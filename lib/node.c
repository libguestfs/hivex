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

#include "c-ctype.h"

#include "hivex.h"
#include "hivex-internal.h"
#include "byte_conversions.h"

hive_node_h
hivex_root (hive_h *h)
{
  hive_node_h ret = h->rootoffs;
  if (!IS_VALID_BLOCK (h, ret)) {
    SET_ERRNO (HIVEX_NO_KEY, "no root key");
    return 0;
  }
  return ret;
}

size_t
hivex_node_struct_length (hive_h *h, hive_node_h node)
{
  if (!IS_VALID_BLOCK (h, node) || !BLOCK_ID_EQ (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return 0;
  }

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);
  size_t name_len = le16toh (nk->name_len);
  /* -1 to avoid double-counting the first name character */
  size_t ret = name_len + sizeof (struct ntreg_nk_record) - 1;
  int used;
  size_t seg_len = block_len (h, node, &used);
  if (ret > seg_len) {
    SET_ERRNO (EFAULT, "node name is too long (%zu, %zu)", name_len, seg_len);
    return 0;
  }
  return ret;
}

char *
hivex_node_name (hive_h *h, hive_node_h node)
{
  if (!IS_VALID_BLOCK (h, node) || !BLOCK_ID_EQ (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return NULL;
  }

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);

  /* AFAIK the node name is always plain ASCII, so no conversion
   * to UTF-8 is necessary.  However we do need to nul-terminate
   * the string.
   */

  /* nk->name_len is unsigned, 16 bit, so this is safe ...  However
   * we have to make sure the length doesn't exceed the block length.
   */
  size_t len = le16toh (nk->name_len);
  size_t seg_len = block_len (h, node, NULL);
  if (sizeof (struct ntreg_nk_record) + len - 1 > seg_len) {
    SET_ERRNO (EFAULT, "node name is too long (%zu, %zu)", len, seg_len);
    return NULL;
  }

  char *ret = malloc (len + 1);
  if (ret == NULL)
    return NULL;
  memcpy (ret, nk->name, len);
  ret[len] = '\0';
  return ret;
}

static int64_t
timestamp_check (hive_h *h, hive_node_h node, int64_t timestamp)
{
  if (timestamp < 0) {
    SET_ERRNO (EINVAL,
               "negative time reported at %zu: %" PRIi64, node, timestamp);
    return -1;
  }

  return timestamp;
}

int64_t
hivex_last_modified (hive_h *h)
{
  return timestamp_check (h, 0, h->last_modified);
}

int64_t
hivex_node_timestamp (hive_h *h, hive_node_h node)
{
  int64_t ret;

  if (!IS_VALID_BLOCK (h, node) || !BLOCK_ID_EQ (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return -1;
  }

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);

  ret = le64toh (nk->timestamp);
  return timestamp_check (h, node, ret);
}

#if 0
/* I think the documentation for the sk and classname fields in the nk
 * record is wrong, or else the offset field is in the wrong place.
 * Otherwise this makes no sense.  Disabled this for now -- it's not
 * useful for reading the registry anyway.
 */

hive_security_h
hivex_node_security (hive_h *h, hive_node_h node)
{
  if (!IS_VALID_BLOCK (h, node) || !BLOCK_ID_EQ (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return 0;
  }

  struct ntreg_nk_record *nk = (struct ntreg_nk_record *) (h->addr + node);

  hive_node_h ret = le32toh (nk->sk);
  ret += 0x1000;
  if (!IS_VALID_BLOCK (h, ret)) {
    SET_ERRNO (EFAULT, "invalid block");
    return 0;
  }
  return ret;
}

hive_classname_h
hivex_node_classname (hive_h *h, hive_node_h node)
{
  if (!IS_VALID_BLOCK (h, node) || !BLOCK_ID_EQ (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return 0;
  }

  struct ntreg_nk_record *nk = (struct ntreg_nk_record *) (h->addr + node);

  hive_node_h ret = le32toh (nk->classname);
  ret += 0x1000;
  if (!IS_VALID_BLOCK (h, ret)) {
    SET_ERRNO (EFAULT, "invalid block");
    return 0;
  }
  return ret;
}
#endif

/* Iterate over children, returning child nodes and intermediate blocks. */
int
_hivex_get_children (hive_h *h, hive_node_h node,
                     hive_node_h **children_ret, size_t **blocks_ret,
                     int flags)
{
  if (!IS_VALID_BLOCK (h, node) || !BLOCK_ID_EQ (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return -1;
  }

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);

  size_t nr_subkeys_in_nk = le32toh (nk->nr_subkeys);

  offset_list children, blocks;
  _hivex_init_offset_list (h, &children);
  _hivex_init_offset_list (h, &blocks);

  /* Deal with the common "no subkeys" case quickly. */
  if (nr_subkeys_in_nk == 0)
    goto ok;

  /* Arbitrarily limit the number of subkeys we will ever deal with. */
  if (nr_subkeys_in_nk > HIVEX_MAX_SUBKEYS) {
    SET_ERRNO (ERANGE,
               "nr_subkeys_in_nk > HIVEX_MAX_SUBKEYS (%zu > %d)",
               nr_subkeys_in_nk, HIVEX_MAX_SUBKEYS);
    goto error;
  }

  /* Preallocate space for the children. */
  if (_hivex_grow_offset_list (&children, nr_subkeys_in_nk) == -1)
    goto error;

  /* The subkey_lf field can point either to an lf-record, which is
   * the common case, or if there are lots of subkeys, to an
   * ri-record.
   */
  size_t subkey_lf = le32toh (nk->subkey_lf);
  subkey_lf += 0x1000;
  if (!IS_VALID_BLOCK (h, subkey_lf)) {
    SET_ERRNO (EFAULT,
               "subkey_lf is not a valid block (0x%zx)", subkey_lf);
    goto error;
  }

  if (_hivex_add_to_offset_list (&blocks, subkey_lf) == -1)
    goto error;

  struct ntreg_hbin_block *block =
    (struct ntreg_hbin_block *) ((char *) h->addr + subkey_lf);

  /* Points to lf-record?  (Note, also "lh" but that is basically the
   * same as "lf" as far as we are concerned here).
   */
  if (block->id[0] == 'l' && (block->id[1] == 'f' || block->id[1] == 'h')) {
    struct ntreg_lf_record *lf = (struct ntreg_lf_record *) block;

    /* Check number of subkeys in the nk-record matches number of subkeys
     * in the lf-record.
     */
    size_t nr_subkeys_in_lf = le16toh (lf->nr_keys);

    if (nr_subkeys_in_nk != nr_subkeys_in_lf) {
      SET_ERRNO (ENOTSUP,
                 "nr_subkeys_in_nk = %zu is not equal to nr_subkeys_in_lf = %zu",
                 nr_subkeys_in_nk, nr_subkeys_in_lf);
      goto error;
    }

    size_t len = block_len (h, subkey_lf, NULL);
    if (8 + nr_subkeys_in_lf * 8 > len) {
      SET_ERRNO (EFAULT, "too many subkeys (%zu, %zu)", nr_subkeys_in_lf, len);
      goto error;
    }

    size_t i;
    for (i = 0; i < nr_subkeys_in_lf; ++i) {
      hive_node_h subkey = le32toh (lf->keys[i].offset);
      subkey += 0x1000;
      if (!(flags & GET_CHILDREN_NO_CHECK_NK)) {
        if (!IS_VALID_BLOCK (h, subkey)) {
          SET_ERRNO (EFAULT, "subkey is not a valid block (0x%zx)", subkey);
          goto error;
        }
      }
      if (_hivex_add_to_offset_list (&children, subkey) == -1)
        goto error;
    }
    goto ok;
  }
  /* Points to ri-record? */
  else if (block->id[0] == 'r' && block->id[1] == 'i') {
    struct ntreg_ri_record *ri = (struct ntreg_ri_record *) block;

    size_t nr_offsets = le16toh (ri->nr_offsets);

    /* Count total number of children. */
    size_t i, count = 0;
    for (i = 0; i < nr_offsets; ++i) {
      hive_node_h offset = le32toh (ri->offset[i]);
      offset += 0x1000;
      if (!IS_VALID_BLOCK (h, offset)) {
        SET_ERRNO (EFAULT, "ri-offset is not a valid block (0x%zx)", offset);
        goto error;
      }
      if (!BLOCK_ID_EQ (h, offset, "lf") && !BLOCK_ID_EQ (h, offset, "lh")) {
        struct ntreg_lf_record *block =
          (struct ntreg_lf_record *) ((char *) h->addr + offset);
        SET_ERRNO (ENOTSUP,
                   "ri-record offset does not point to lf/lh (0x%zx, %d, %d)",
                   offset, block->id[0], block->id[1]);
        goto error;
      }

      if (_hivex_add_to_offset_list (&blocks, offset) == -1)
        goto error;

      struct ntreg_lf_record *lf =
        (struct ntreg_lf_record *) ((char *) h->addr + offset);

      count += le16toh (lf->nr_keys);
    }

    if (nr_subkeys_in_nk != count) {
      SET_ERRNO (ENOTSUP,
                 "nr_subkeys_in_nk = %zu is not equal to counted = %zu",
                 nr_subkeys_in_nk, count);
      goto error;
    }

    /* Copy list of children.  Note nr_subkeys_in_nk is limited to
     * something reasonable above.
     */
    for (i = 0; i < nr_offsets; ++i) {
      hive_node_h offset = le32toh (ri->offset[i]);
      offset += 0x1000;
      if (!IS_VALID_BLOCK (h, offset)) {
        SET_ERRNO (EFAULT, "ri-offset is not a valid block (0x%zx)", offset);
        goto error;
      }
      if (!BLOCK_ID_EQ (h, offset, "lf") && !BLOCK_ID_EQ (h, offset, "lh")) {
        struct ntreg_lf_record *block =
          (struct ntreg_lf_record *) ((char *) h->addr + offset);
        SET_ERRNO (ENOTSUP,
                   "ri-record offset does not point to lf/lh (0x%zx, %d, %d)",
                   offset, block->id[0], block->id[1]);
        goto error;
      }

      struct ntreg_lf_record *lf =
        (struct ntreg_lf_record *) ((char *) h->addr + offset);

      size_t j;
      for (j = 0; j < le16toh (lf->nr_keys); ++j) {
        hive_node_h subkey = le32toh (lf->keys[j].offset);
        subkey += 0x1000;
        if (!(flags & GET_CHILDREN_NO_CHECK_NK)) {
          if (!IS_VALID_BLOCK (h, subkey)) {
            SET_ERRNO (EFAULT,
                       "indirect subkey is not a valid block (0x%zx)",
                       subkey);
            goto error;
          }
        }
        if (_hivex_add_to_offset_list (&children, subkey) == -1)
          goto error;
      }
    }
    goto ok;
  }
  /* else not supported, set errno and fall through */
  SET_ERRNO (ENOTSUP,
             "subkey block is not lf/lh/ri (0x%zx, %d, %d)",
             subkey_lf, block->id[0], block->id[1]);
 error:
  _hivex_free_offset_list (&children);
  _hivex_free_offset_list (&blocks);
  return -1;

 ok:
  *children_ret = _hivex_return_offset_list (&children);
  *blocks_ret = _hivex_return_offset_list (&blocks);
  if (!*children_ret || !*blocks_ret)
    goto error;
  return 0;
}

hive_node_h *
hivex_node_children (hive_h *h, hive_node_h node)
{
  hive_node_h *children;
  size_t *blocks;

  if (_hivex_get_children (h, node, &children, &blocks, 0) == -1)
    return NULL;

  free (blocks);
  return children;
}

/* Very inefficient, but at least having a separate API call
 * allows us to make it more efficient in future.
 */
hive_node_h
hivex_node_get_child (hive_h *h, hive_node_h node, const char *nname)
{
  hive_node_h *children = NULL;
  char *name = NULL;
  hive_node_h ret = 0;

  children = hivex_node_children (h, node);
  if (!children) goto error;

  size_t i;
  for (i = 0; children[i] != 0; ++i) {
    name = hivex_node_name (h, children[i]);
    if (!name) goto error;
    if (STRCASEEQ (name, nname)) {
      ret = children[i];
      break;
    }
    free (name); name = NULL;
  }

 error:
  free (children);
  free (name);
  return ret;
}

hive_node_h
hivex_node_parent (hive_h *h, hive_node_h node)
{
  if (!IS_VALID_BLOCK (h, node) || !BLOCK_ID_EQ (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return 0;
  }

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);

  hive_node_h ret = le32toh (nk->parent);
  ret += 0x1000;
  if (!IS_VALID_BLOCK (h, ret)) {
    SET_ERRNO (EFAULT, "parent is not a valid block (0x%zx)", ret);
    return 0;
  }
  return ret;
}
