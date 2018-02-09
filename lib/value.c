/* hivex - Windows Registry "hive" extraction library.
 * Copyright (C) 2009-2011 Red Hat Inc.
 * Derived from code by Petter Nordahl-Hagen under a compatible license:
 *   Copyright (c) 1997-2007 Petter Nordahl-Hagen.
 * Derived from code by Markus Stephany under a compatible license:
 *   Copyright (c) 2000-2004, Markus Stephany.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation;
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * See file LICENSE for the full license.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <inttypes.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>

#include "hivex.h"
#include "hivex-internal.h"

int
_hivex_get_values (hive_h *h, hive_node_h node,
                   hive_value_h **values_ret, size_t **blocks_ret)
{
  if (!IS_VALID_BLOCK (h, node) || !block_id_eq (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return -1;
  }

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);

  size_t nr_values = le32toh (nk->nr_values);

  DEBUG (2, "nr_values = %zu", nr_values);

  offset_list values, blocks;
  _hivex_init_offset_list (h, &values);
  _hivex_init_offset_list (h, &blocks);

  /* Deal with the common "no values" case quickly. */
  if (nr_values == 0)
    goto ok;

  /* Arbitrarily limit the number of values we will ever deal with. */
  if (nr_values > HIVEX_MAX_VALUES) {
    SET_ERRNO (ERANGE,
               "nr_values > HIVEX_MAX_VALUES (%zu > %d)",
               nr_values, HIVEX_MAX_VALUES);
    goto error;
  }

  /* Preallocate space for the values. */
  if (_hivex_grow_offset_list (&values, nr_values) == -1)
    goto error;

  /* Get the value list and check it looks reasonable. */
  size_t vlist_offset = le32toh (nk->vallist);
  vlist_offset += 0x1000;
  if (!IS_VALID_BLOCK (h, vlist_offset)) {
    SET_ERRNO (EFAULT,
               "value list is not a valid block (0x%zx)", vlist_offset);
    goto error;
  }

  if (_hivex_add_to_offset_list (&blocks, vlist_offset) == -1)
    goto error;

  struct ntreg_value_list *vlist =
    (struct ntreg_value_list *) ((char *) h->addr + vlist_offset);

  size_t len = block_len (h, vlist_offset, NULL);
  if (4 + nr_values * 4 > len) {
    SET_ERRNO (EFAULT, "value list is too long (%zu, %zu)", nr_values, len);
    goto error;
  }

  size_t i;
  for (i = 0; i < nr_values; ++i) {
    hive_node_h value = le32toh (vlist->offset[i]);
    value += 0x1000;
    if (!IS_VALID_BLOCK (h, value)) {
      SET_ERRNO (EFAULT, "value is not a valid block (0x%zx)", value);
      goto error;
    }
    if (_hivex_add_to_offset_list (&values, value) == -1)
      goto error;
  }

 ok:
  *values_ret = _hivex_return_offset_list (&values);
  *blocks_ret = _hivex_return_offset_list (&blocks);
  if (!*values_ret || !*blocks_ret)
    goto error;
  return 0;

 error:
  _hivex_free_offset_list (&values);
  _hivex_free_offset_list (&blocks);
  return -1;
}

hive_value_h *
hivex_node_values (hive_h *h, hive_node_h node)
{
  hive_value_h *values;
  size_t *blocks;

  if (_hivex_get_values (h, node, &values, &blocks) == -1)
    return NULL;

  free (blocks);
  return values;
}

/* Very inefficient, but at least having a separate API call
 * allows us to make it more efficient in future.
 */
hive_value_h
hivex_node_get_value (hive_h *h, hive_node_h node, const char *key)
{
  hive_value_h *values = NULL;
  char *name = NULL;
  hive_value_h ret = 0;

  values = hivex_node_values (h, node);
  if (!values) goto error;

  size_t i;
  for (i = 0; values[i] != 0; ++i) {
    name = hivex_value_key (h, values[i]);
    if (!name) goto error;
    if (STRCASEEQ (name, key)) {
      ret = values[i];
      break;
    }
    free (name); name = NULL;
  }

 error:
  free (values);
  free (name);
  return ret;
}

size_t
hivex_node_nr_values (hive_h *h, hive_node_h node)
{
  if (!IS_VALID_BLOCK (h, node) || !block_id_eq (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return 0;
  }

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);

  size_t nr_values = le32toh (nk->nr_values);

  return nr_values;
}

size_t
hivex_value_struct_length (hive_h *h, hive_value_h value)
{
  size_t key_len;

  errno = 0;
  key_len = hivex_value_key_len (h, value);
  if (key_len == 0 && errno != 0)
    return 0;

  /* -1 to avoid double-counting the first name character */
  return key_len + sizeof (struct ntreg_vk_record) - 1;
}

size_t
hivex_value_key_len (hive_h *h, hive_value_h value)
{
  if (!IS_VALID_BLOCK (h, value) || !block_id_eq (h, value, "vk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'vk' block");
    return 0;
  }

  struct ntreg_vk_record *vk =
    (struct ntreg_vk_record *) ((char *) h->addr + value);

  /* vk->name_len is unsigned, 16 bit, so this is safe ...  However
   * we have to make sure the length doesn't exceed the block length.
   */
  size_t len = le16toh (vk->name_len);

  size_t seg_len = block_len (h, value, NULL);
  if (sizeof (struct ntreg_vk_record) + len - 1 > seg_len) {
    SET_ERRNO (EFAULT, "key length is too long (%zu, %zu)", len, seg_len);
    return 0;
  }
  return _hivex_utf8_strlen (h, vk->name, len, ! (le16toh (vk->flags) & 0x01));
}

char *
hivex_value_key (hive_h *h, hive_value_h value)
{
  if (!IS_VALID_BLOCK (h, value) || !block_id_eq (h, value, "vk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'vk' block");
    return 0;
  }

  struct ntreg_vk_record *vk =
    (struct ntreg_vk_record *) ((char *) h->addr + value);

  size_t flags = le16toh (vk->flags);
  size_t len = le16toh (vk->name_len);

  size_t seg_len = block_len (h, value, NULL);
  if (sizeof (struct ntreg_vk_record) + len - 1 > seg_len) {
    SET_ERRNO (EFAULT, "key length is too long (%zu, %zu)", len, seg_len);
    return NULL;
  }
  if (flags & 0x01) {
    return _hivex_recode (h, latin1_to_utf8, vk->name, len, NULL);
  } else {
    return _hivex_recode (h, utf16le_to_utf8, vk->name, len, NULL);
  }
}

int
hivex_value_type (hive_h *h, hive_value_h value, hive_type *t, size_t *len)
{
  if (!IS_VALID_BLOCK (h, value) || !block_id_eq (h, value, "vk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'vk' block");
    return -1;
  }

  struct ntreg_vk_record *vk =
    (struct ntreg_vk_record *) ((char *) h->addr + value);

  if (t)
    *t = le32toh (vk->data_type);

  if (len) {
    *len = le32toh (vk->data_len);
    *len &= 0x7fffffff;         /* top bit indicates if data is stored inline */
  }

  return 0;
}

hive_value_h
hivex_value_data_cell_offset (hive_h *h, hive_value_h value, size_t *len)
{
  if (!IS_VALID_BLOCK (h, value) || !block_id_eq (h, value, "vk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'vk' block");
    return 0;
  }

  DEBUG (2, "value=0x%zx", value);
  struct ntreg_vk_record *vk =
    (struct ntreg_vk_record *) ((char *) h->addr + value);

  size_t data_len;
  int is_inline;

  data_len = le32toh (vk->data_len);
  is_inline = !!(data_len & 0x80000000);
  data_len &= 0x7fffffff;

  DEBUG (2, "is_inline=%d", is_inline);

  DEBUG (2, "data_len=%zx", data_len);

  if (is_inline && data_len > 4) {
    SET_ERRNO (ENOTSUP, "inline data with declared length (%zx) > 4", data_len);
    return 0;
  }

  if (is_inline) {
    /* There is no other location for the value data. */
    if (len)
      *len = 0;
    errno = 0;
    return 0;
  } else {
    if (len)
      *len = data_len + 4;  /* Include 4 header length bytes */
  }

  DEBUG (2, "proceeding with indirect data");

  size_t data_offset = le32toh (vk->data_offset);
  data_offset += 0x1000;  /* Add 0x1000 because everything's off by 4KiB */
  if (!IS_VALID_BLOCK (h, data_offset)) {
    SET_ERRNO (EFAULT, "data offset is not a valid block (0x%zx)", data_offset);
    return 0;
  }

  DEBUG (2, "data_offset=%zx", data_offset);

  return data_offset;
}

char *
hivex_value_value (hive_h *h, hive_value_h value,
                   hive_type *t_rtn, size_t *len_rtn)
{
  if (!IS_VALID_BLOCK (h, value) || !block_id_eq (h, value, "vk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'vk' block");
    return NULL;
  }

  struct ntreg_vk_record *vk =
    (struct ntreg_vk_record *) ((char *) h->addr + value);

  hive_type t;
  size_t len;
  int is_inline;

  t = le32toh (vk->data_type);

  len = le32toh (vk->data_len);
  is_inline = !!(len & 0x80000000);
  len &= 0x7fffffff;

  DEBUG (2, "value=0x%zx, t=%u, len=%zu, inline=%d",
         value, t, len, is_inline);

  if (t_rtn)
    *t_rtn = t;
  if (len_rtn)
    *len_rtn = len;

  if (is_inline && len > 4) {
    SET_ERRNO (ENOTSUP, "inline data with declared length (%zx) > 4", len);
    return NULL;
  }

  /* Arbitrarily limit the length that we will read. */
  if (len > HIVEX_MAX_VALUE_LEN) {
    SET_ERRNO (ERANGE, "data length > HIVEX_MAX_VALUE_LEN (%zu > %d)",
               len, HIVEX_MAX_VALUE_LEN);
    return NULL;
  }

  char *ret = malloc (len);
  if (ret == NULL)
    return NULL;

  if (is_inline) {
    memcpy (ret, (char *) &vk->data_offset, len);
    return ret;
  }

  size_t data_offset = le32toh (vk->data_offset);
  data_offset += 0x1000;
  if (!IS_VALID_BLOCK (h, data_offset)) {
    SET_ERRNO (EFAULT, "data offset is not a valid block (0x%zx)", data_offset);
    free (ret);
    return NULL;
  }

  /* Check that the declared size isn't larger than the block its in.
   *
   * XXX Some apparently valid registries are seen to have this,
   * so turn this into a warning and substitute the smaller length
   * instead.
   */
  size_t blen = block_len (h, data_offset, NULL);
  if (len <= blen - 4 /* subtract 4 for block header */) {
    char *data = (char *) h->addr + data_offset + 4;
    memcpy (ret, data, len);
    return ret;
  } else {
    if (!IS_VALID_BLOCK (h, data_offset) ||
        !block_id_eq (h, data_offset, "db")) {
      SET_ERRNO (EINVAL,
                 "declared data length is longer than the block and "
                 "block is not a db block (data 0x%zx, data len %zu)",
                 data_offset, len);
      free (ret);
      return NULL;
    }
    struct ntreg_db_record *db =
      (struct ntreg_db_record *) ((char *) h->addr + data_offset);
    size_t blocklist_offset = le32toh (db->blocklist_offset);
    blocklist_offset += 0x1000;
    size_t nr_blocks = le16toh (db->nr_blocks);
    if (!IS_VALID_BLOCK (h, blocklist_offset)) {
      SET_ERRNO (EINVAL,
                 "blocklist is not a valid block "
                 "(db block 0x%zx, blocklist 0x%zx)",
                 data_offset, blocklist_offset);
      free (ret);
      return NULL;
    }
    struct ntreg_value_list *bl =
      (struct ntreg_value_list *) ((char *) h->addr + blocklist_offset);
    size_t i, off;
    for (i = off = 0; i < nr_blocks; ++i) {
      size_t subblock_offset = le32toh (bl->offset[i]);
      subblock_offset += 0x1000;
      if (!IS_VALID_BLOCK (h, subblock_offset)) {
        SET_ERRNO (EINVAL,
                   "subblock is not valid "
                   "(db block 0x%zx, block list 0x%zx, data subblock 0x%zx)",
                   data_offset, blocklist_offset, subblock_offset);
        free (ret);
        return NULL;
      }
      int32_t seg_len = block_len(h, subblock_offset, NULL);
      struct ntreg_db_block *subblock =
        (struct ntreg_db_block *) ((char *) h->addr + subblock_offset);
      int32_t sz = seg_len - 8; /* don't copy the last 4 bytes */
      if (off + sz > len) {
        sz = len - off;
      }
      memcpy (ret + off, subblock->data, sz);
      off += sz;
    }
    if (off != *len_rtn) {
      DEBUG (2, "warning: declared data length "
             "and amount of data found in sub-blocks differ "
             "(db block 0x%zx, data len %zu, found data %zu)",
             data_offset, *len_rtn, off);
      *len_rtn = off;
    }
    return ret;
  }
}

char *
hivex_value_string (hive_h *h, hive_value_h value)
{
  hive_type t;
  size_t len;
  char *data = hivex_value_value (h, value, &t, &len);

  if (data == NULL)
    return NULL;

  if (t != hive_t_string && t != hive_t_expand_string && t != hive_t_link) {
    free (data);
    SET_ERRNO (EINVAL, "type is not string/expand_string/link");
    return NULL;
  }

  /* Deal with the case where Windows has allocated a large buffer
   * full of random junk, and only the first few bytes of the buffer
   * contain a genuine UTF-16 string.
   *
   * In this case, iconv would try to process the junk bytes as UTF-16
   * and inevitably find an illegal sequence (EILSEQ).  Instead, stop
   * after we find the first \0\0.
   *
   * (Found by Hilko Bengen in a fresh Windows XP SOFTWARE hive).
   */
  size_t slen = _hivex_utf16_string_len_in_bytes_max (data, len);
  if (slen < len)
    len = slen;

  char *ret = _hivex_recode (h, utf16le_to_utf8, data, len, NULL);
  free (data);
  if (ret == NULL)
    return NULL;

  return ret;
}

/* Even though
 * http://msdn.microsoft.com/en-us/library/windows/desktop/ms724884.aspx
 * and
 * http://blogs.msdn.com/oldnewthing/archive/2009/10/08/9904646.aspx
 * claim that it is not possible to store empty strings in MULTI_SZ
 * string lists, such lists are used by Windows itself:
 *
 * The MoveFileEx function can schedule files to be renamed (or
 * removed) at restart time by storing pairs of filenames in the
 * HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\PendingFileRenameOperations
 * value.
 *
 * The documentation for MoveFileEx
 * (http://msdn.microsoft.com/en-us/library/windows/desktop/aa365240)
 * states that "[i]f dwFlags specifies MOVEFILE_DELAY_UNTIL_REBOOT,
 * and lpNewFileName is NULL, MoveFileEx registers the
 * lpExistingFileName file to be deleted when the system restarts."
 *
 * For scheduled removals, the second file name of any pair stored in
 * PendingFileRenameOperations is an empty string.
 */
char **
hivex_value_multiple_strings (hive_h *h, hive_value_h value)
{
  hive_type t;
  size_t len;
  char *data = hivex_value_value (h, value, &t, &len);

  if (data == NULL)
    return NULL;

  if (t != hive_t_multiple_strings) {
    free (data);
    SET_ERRNO (EINVAL, "type is not multiple_strings");
    return NULL;
  }

  size_t nr_strings = 0;
  char **ret = malloc ((1 + nr_strings) * sizeof (char *));
  if (ret == NULL) {
    free (data);
    return NULL;
  }
  ret[0] = NULL;

  char *p = data;
  size_t plen;

  while (p < data + len) {
    plen = _hivex_utf16_string_len_in_bytes_max (p, data + len - p);
    nr_strings++;
    char **ret2 = realloc (ret, (1 + nr_strings) * sizeof (char *));
    if (ret2 == NULL) {
      _hivex_free_strings (ret);
      free (data);
      return NULL;
    }
    ret = ret2;

    ret[nr_strings-1] = _hivex_recode (h, utf16le_to_utf8, p, plen, NULL);
    ret[nr_strings] = NULL;
    if (ret[nr_strings-1] == NULL) {
      _hivex_free_strings (ret);
      free (data);
      return NULL;
    }

    p += plen + 2 /* skip over UTF-16 \0\0 at the end of this string */;
  }

  free (data);
  return ret;
}

int32_t
hivex_value_dword (hive_h *h, hive_value_h value)
{
  hive_type t;
  size_t len;
  void *data = hivex_value_value (h, value, &t, &len);

  if (data == NULL)
    return -1;

  if ((t != hive_t_dword && t != hive_t_dword_be) || len < 4) {
    free (data);
    SET_ERRNO (EINVAL, "type is not dword/dword_be");
    return -1;
  }

  int32_t ret = * (int32_t *) data;
  free (data);
  if (t == hive_t_dword)        /* little endian */
    ret = le32toh (ret);
  else
    ret = be32toh (ret);

  return ret;
}

int64_t
hivex_value_qword (hive_h *h, hive_value_h value)
{
  hive_type t;
  size_t len;
  void *data = hivex_value_value (h, value, &t, &len);

  if (data == NULL)
    return -1;

  if (t != hive_t_qword || len < 8) {
    free (data);
    SET_ERRNO (EINVAL, "type is not qword or length < 8");
    return -1;
  }

  int64_t ret = * (int64_t *) data;
  free (data);
  ret = le64toh (ret);          /* always little endian */

  return ret;
}
