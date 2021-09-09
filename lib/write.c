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

#ifdef HAVE_MMAP
#include <sys/mman.h>
#else
/* On systems without mmap (and munmap), use a replacement function. */
#include "mmap.h"
#endif

#include "c-ctype.h"

#include "hivex.h"
#include "hivex-internal.h"

/*----------------------------------------------------------------------
 * Writing.
 */

/* Allocate an hbin (page), extending the malloc'd space if necessary,
 * and updating the hive handle fields (but NOT the hive disk header
 * -- the hive disk header is updated when we commit).  This function
 * also extends the bitmap if necessary.
 *
 * 'allocation_hint' is the size of the block allocation we would like
 * to make.  Normally registry blocks are very small (avg 50 bytes)
 * and are contained in standard-sized pages (4KB), but the registry
 * can support blocks which are larger than a standard page, in which
 * case it creates a page of 8KB, 12KB etc.
 *
 * Returns:
 * > 0 : offset of first usable byte of new page (after page header)
 * 0   : error (errno set)
 */
static size_t
allocate_page (hive_h *h, size_t allocation_hint)
{
  /* In almost all cases this will be 1. */
  size_t nr_4k_pages =
    1 + (allocation_hint + sizeof (struct ntreg_hbin_page) - 1) / 4096;
  assert (nr_4k_pages >= 1);

  /* 'extend' is the number of bytes to extend the file by.  Note that
   * hives found in the wild often contain slack between 'endpages'
   * and the actual end of the file, so we don't always need to make
   * the file larger.
   */
  ssize_t extend = h->endpages + nr_4k_pages * 4096 - h->size;

  DEBUG (2, "current endpages = 0x%zx, current size = 0x%zx",
         h->endpages, h->size);
  DEBUG (2, "extending file by %zd bytes (<= 0 if no extension)",
         extend);

  if (extend > 0) {
    size_t oldsize = h->size;
    size_t newsize = h->size + extend;
    char *newaddr = realloc (h->addr, newsize);
    if (newaddr == NULL)
      return 0;

    size_t oldbitmapsize = 1 + oldsize / 32;
    size_t newbitmapsize = 1 + newsize / 32;
    char *newbitmap = realloc (h->bitmap, newbitmapsize);
    if (newbitmap == NULL) {
      free (newaddr);
      return 0;
    }

    h->addr = newaddr;
    h->size = newsize;
    h->bitmap = newbitmap;

    memset ((char *) h->addr + oldsize, 0, newsize - oldsize);
    memset (h->bitmap + oldbitmapsize, 0, newbitmapsize - oldbitmapsize);
  }

  size_t offset = h->endpages;
  h->endpages += nr_4k_pages * 4096;

  DEBUG (2, "new endpages = 0x%zx, new size = 0x%zx", h->endpages, h->size);

  /* Write the hbin header. */
  struct ntreg_hbin_page *page =
    (struct ntreg_hbin_page *) ((char *) h->addr + offset);
  page->magic[0] = 'h';
  page->magic[1] = 'b';
  page->magic[2] = 'i';
  page->magic[3] = 'n';
  page->offset_first = htole32 (offset - 0x1000);
  page->page_size = htole32 (nr_4k_pages * 4096);
  memset (page->unknown, 0, sizeof (page->unknown));

  DEBUG (2, "new page at 0x%zx", offset);

  /* Offset of first usable byte after the header. */
  return offset + sizeof (struct ntreg_hbin_page);
}

/* Allocate a single block, first allocating an hbin (page) at the end
 * of the current file if necessary.  NB. To keep the implementation
 * simple and more likely to be correct, we do not reuse existing free
 * blocks.
 *
 * seg_len is the size of the block (this INCLUDES the block header).
 * The header of the block is initialized to -seg_len (negative to
 * indicate used).  id[2] is the block ID (type), eg. "nk" for nk-
 * record.  The block bitmap is updated to show this block as valid.
 * The rest of the contents of the block will be zero.
 *
 * **NB** Because allocate_block may reallocate the memory, all
 * pointers into the memory become potentially invalid.  I really
 * love writing in C, can't you tell?
 *
 * Returns:
 * > 0 : offset of new block
 * 0   : error (errno set)
 */
static size_t
allocate_block (hive_h *h, size_t seg_len, const char id[2])
{
  CHECK_WRITABLE (0);

  if (seg_len < 4) {
    /* The caller probably forgot to include the header.  Note that
     * value lists have no ID field, so seg_len == 4 would be possible
     * for them, albeit unusual.
     */
    SET_ERRNO (ERANGE, "refusing too small allocation (%zu)", seg_len);
    return 0;
  }

  /* Refuse really large allocations. */
  if (seg_len > HIVEX_MAX_ALLOCATION) {
    SET_ERRNO (ERANGE, "refusing too large allocation (%zu)", seg_len);
    return 0;
  }

  /* Round up allocation to multiple of 8 bytes.  All blocks must be
   * on an 8 byte boundary.
   */
  seg_len = (seg_len + 7) & ~7;

  /* Allocate a new page if necessary. */
  if (h->endblocks == 0 || h->endblocks + seg_len > h->endpages) {
    size_t newendblocks = allocate_page (h, seg_len);
    if (newendblocks == 0)
      return 0;
    h->endblocks = newendblocks;
  }

  size_t offset = h->endblocks;

  DEBUG (2, "new block at 0x%zx, size %zu", offset, seg_len);

  struct ntreg_hbin_block *blockhdr =
    (struct ntreg_hbin_block *) ((char *) h->addr + offset);

  memset (blockhdr, 0, seg_len);

  blockhdr->seg_len = htole32 (- (int32_t) seg_len);
  if (id[0] && id[1] && seg_len >= sizeof (struct ntreg_hbin_block)) {
    blockhdr->id[0] = id[0];
    blockhdr->id[1] = id[1];
  }

  BITMAP_SET (h->bitmap, offset);

  h->endblocks += seg_len;

  /* If there is space after the last block in the last page, then we
   * have to put a dummy free block header here to mark the rest of
   * the page as free.
   */
  ssize_t rem = h->endpages - h->endblocks;
  if (rem > 0) {
    DEBUG (2, "marking remainder of page free"
           " starting at 0x%zx, size %zd", h->endblocks, rem);

    assert (rem >= 4);

    blockhdr = (struct ntreg_hbin_block *) ((char *) h->addr + h->endblocks);
    blockhdr->seg_len = htole32 ((int32_t) rem);
  }

  return offset;
}

/* 'offset' must point to a valid, used block.  This function marks
 * the block unused (by updating the seg_len field) and invalidates
 * the bitmap.  It does NOT do this recursively, so to avoid creating
 * unreachable used blocks, callers may have to recurse over the hive
 * structures.  Also callers must ensure there are no references to
 * this block from other parts of the hive.
 */
static void
mark_block_unused (hive_h *h, size_t offset)
{
  assert (h->writable);
  assert (IS_VALID_BLOCK (h, offset));

  DEBUG (2, "marking 0x%zx unused", offset);

  struct ntreg_hbin_block *blockhdr =
    (struct ntreg_hbin_block *) ((char *) h->addr + offset);

  size_t seg_len = block_len (h, offset, NULL);
  blockhdr->seg_len = htole32 (seg_len);

  BITMAP_CLR (h->bitmap, offset);
}

/* Delete all existing values at this node. */
static int
delete_values (hive_h *h, hive_node_h node)
{
  assert (h->writable);

  hive_value_h *values;
  size_t *blocks;
  if (_hivex_get_values (h, node, &values, &blocks) == -1)
    return -1;

  size_t i;
  for (i = 0; blocks[i] != 0; ++i)
    mark_block_unused (h, blocks[i]);

  free (blocks);

  for (i = 0; values[i] != 0; ++i) {
    struct ntreg_vk_record *vk =
      (struct ntreg_vk_record *) ((char *) h->addr + values[i]);

    size_t len;
    int is_inline;
    len = le32toh (vk->data_len);
    is_inline = !!(len & 0x80000000); /* top bit indicates is inline */

    if (!is_inline) {           /* non-inline, so remove data block */
      size_t data_offset = le32toh (vk->data_offset);
      data_offset += 0x1000;
      mark_block_unused (h, data_offset);
    }

    /* remove vk record */
    mark_block_unused (h, values[i]);
  }

  free (values);

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);
  nk->nr_values = htole32 (0);
  nk->vallist = htole32 (0xffffffff);

  return 0;
}

/* Calculate the hash for a lf or lh record offset.
 */
static void
calc_hash (const char *type, const char *name, void *ret)
{
  size_t len = strlen (name);

  if (STRPREFIX (type, "lf"))
    /* Old-style, not used in current registries. */
    memcpy (ret, name, len < 4 ? len : 4);
  else {
    /* New-style for lh-records. */
    size_t i, c;
    uint32_t h = 0;
    for (i = 0; i < len; ++i) {
      c = c_toupper (name[i]);
      h *= 37;
      h += c;
    }
    *((uint32_t *) ret) = htole32 (h);
  }
}

/* Create a completely new lh-record containing just the single node. */
static size_t
new_lh_record (hive_h *h, const char *name, hive_node_h node)
{
  static const char id[2] = { 'l', 'h' };
  size_t seg_len = sizeof (struct ntreg_lf_record);
  size_t offset = allocate_block (h, seg_len, id);
  if (offset == 0)
    return 0;

  struct ntreg_lf_record *lh =
    (struct ntreg_lf_record *) ((char *) h->addr + offset);
  lh->nr_keys = htole16 (1);
  lh->keys[0].offset = htole32 (node - 0x1000);
  calc_hash ("lh", name, lh->keys[0].hash);

  return offset;
}

/* Insert node into existing lf/lh-record at position.
 * This allocates a new record and marks the old one as unused.
 */
static size_t
insert_lf_record (hive_h *h, size_t old_offs, size_t posn,
                  const char *name, hive_node_h node)
{
  assert (IS_VALID_BLOCK (h, old_offs));

  /* Work around C stupidity.
   * http://www.redhat.com/archives/libguestfs/2010-February/msg00056.html
   */
  int test = block_id_eq (h, old_offs, "lf") || block_id_eq (h, old_offs, "lh");
  assert (test);

  struct ntreg_lf_record *old_lf =
    (struct ntreg_lf_record *) ((char *) h->addr + old_offs);
  size_t nr_keys = le16toh (old_lf->nr_keys);

  if (nr_keys == UINT16_MAX) {
    SET_ERRNO (EOVERFLOW,
               "cannot extend record because it already contains the maximum number of subkeys (%zu)",
               nr_keys);
    return 0;
  }
  nr_keys++; /* in new record ... */

  size_t seg_len = sizeof (struct ntreg_lf_record) + (nr_keys-1) * 8;

  /* Copy the old_lf->id in case it moves during allocate_block. */
  char id[2];
  memcpy (id, old_lf->id, sizeof id);

  size_t new_offs = allocate_block (h, seg_len, id);
  if (new_offs == 0)
    return 0;

  /* old_lf could have been invalidated by allocate_block. */
  old_lf = (struct ntreg_lf_record *) ((char *) h->addr + old_offs);

  struct ntreg_lf_record *new_lf =
    (struct ntreg_lf_record *) ((char *) h->addr + new_offs);
  new_lf->nr_keys = htole16 (nr_keys);

  /* Copy the keys until we reach posn, insert the new key there, then
   * copy the remaining keys.
   */
  size_t i;
  for (i = 0; i < posn; ++i)
    new_lf->keys[i] = old_lf->keys[i];

  new_lf->keys[i].offset = htole32 (node - 0x1000);
  calc_hash (new_lf->id, name, new_lf->keys[i].hash);

  for (i = posn+1; i < nr_keys; ++i)
    new_lf->keys[i] = old_lf->keys[i-1];

  /* Old block is unused, return new block. */
  mark_block_unused (h, old_offs);
  return new_offs;
}

/* Insert node into existing li-record at position.  Pretty much the
 * same as insert_lf_record above, but the record layout is a bit
 * different.
 */
static size_t
insert_li_record (hive_h *h, size_t old_offs, size_t posn,
                  const char *name, hive_node_h node)
{
  assert (IS_VALID_BLOCK (h, old_offs));
  assert (block_id_eq (h, old_offs, "li"));

  struct ntreg_ri_record *old_li =
    (struct ntreg_ri_record *) ((char *) h->addr + old_offs);
  size_t nr_offsets = le16toh (old_li->nr_offsets);

  if (nr_offsets == UINT16_MAX) {
    SET_ERRNO (EOVERFLOW,
               "cannot extend record because it already contains the maximum number of subkeys (%zu)",
               nr_offsets);
    return 0;
  }
  nr_offsets++; /* in new record ... */

  size_t seg_len = sizeof (struct ntreg_ri_record) + (nr_offsets-1) * 4;

  /* Copy the old_li->id in case it moves during allocate_block. */
  char id[2];
  memcpy (id, old_li->id, sizeof id);

  size_t new_offs = allocate_block (h, seg_len, id);
  if (new_offs == 0)
    return 0;

  /* old_li could have been invalidated by allocate_block. */
  old_li = (struct ntreg_ri_record *) ((char *) h->addr + old_offs);

  struct ntreg_ri_record *new_li =
    (struct ntreg_ri_record *) ((char *) h->addr + new_offs);
  new_li->nr_offsets = htole16 (nr_offsets);

  /* Copy the offsets until we reach posn, insert the new offset
   * there, then copy the remaining offsets.
   */
  size_t i;
  for (i = 0; i < posn; ++i)
    new_li->offset[i] = old_li->offset[i];

  new_li->offset[i] = htole32 (node - 0x1000);

  for (i = posn+1; i < nr_offsets; ++i)
    new_li->offset[i] = old_li->offset[i-1];

  /* Old block is unused, return new block. */
  mark_block_unused (h, old_offs);
  return new_offs;
}

/* Compare name with name in nk-record. */
static int
compare_name_with_nk_name (hive_h *h, const char *name, hive_node_h nk_offs)
{
  assert (IS_VALID_BLOCK (h, nk_offs));
  assert (block_id_eq (h, nk_offs, "nk"));

  /* Name in nk is not necessarily nul-terminated. */
  char *nname = hivex_node_name (h, nk_offs);

  /* Unfortunately we don't have a way to return errors here. */
  if (!nname) {
    perror ("compare_name_with_nk_name");
    return 0;
  }

  /* Perform a limited case-insensitive comparison. ASCII letters will be
   * *upper-cased*. Multibyte sequences will produce nonsensical orderings.
   */
  int r = 0;
  const char *s1 = name;
  const char *s2 = nname;

  for (;;) {
    unsigned char c1 = *(s1++);
    unsigned char c2 = *(s2++);

    if (c1 >= 'a' && c1 <= 'z')
      c1 = 'A' + (c1 - 'a');
    if (c2 >= 'a' && c2 <= 'z')
      c2 = 'A' + (c2 - 'a');
    if (c1 < c2) {
      /* Also covers the case when "name" is a prefix of "nname". */
      r = -1;
      break;
    }
    if (c1 > c2) {
      /* Also covers the case when "nname" is a prefix of "name". */
      r = 1;
      break;
    }
    if (c1 == '\0') {
      /* Both strings end. */
      break;
    }
  }

  free (nname);

  return r;
}

/* See comment about keeping things sorted in hivex_node_add_child. */
static int
insert_subkey (hive_h *h, const char *name,
               size_t parent, size_t nkoffset, size_t *blocks)
{
  size_t old_offs = 0, new_offs = 0;
  size_t i, j = SIZE_MAX;
  struct ntreg_lf_record *old_lf = NULL;
  struct ntreg_ri_record *old_li = NULL;

  /* The caller already dealt with the no subkeys case, so this should
   * be true.
   */
  assert (blocks[0] != 0);

  /* Find the intermediate block which contains a link to the key that
   * is just after the one we are inserting.  This intermediate block
   * might be an lf/lh-record or an li-record (but it won't be a
   * ri-record so we can ignore those).  The lf/lh- and li-records
   * have different formats.  If we cannot find the key after, then we
   * end up at the final block and we have to insert the new key at
   * the end.
   */
  for (i = 0; blocks[i] != 0; ++i) {
    if (block_id_eq (h, blocks[i], "lf") || block_id_eq (h, blocks[i], "lh")) {
      old_offs = blocks[i];
      old_lf = (struct ntreg_lf_record *) ((char *) h->addr + old_offs);
      old_li = NULL;
      for (j = 0; j < le16toh (old_lf->nr_keys); ++j) {
        hive_node_h nk_offs = le32toh (old_lf->keys[j].offset);
        nk_offs += 0x1000;
        if (compare_name_with_nk_name (h, name, nk_offs) < 0)
          goto insert_it;
      }
    }
    else if (block_id_eq (h, blocks[i], "li")) {
      old_offs = blocks[i];
      old_lf = NULL;
      old_li = (struct ntreg_ri_record *) ((char *) h->addr + old_offs);
      for (j = 0; j < le16toh (old_li->nr_offsets); ++j) {
        hive_node_h nk_offs = le32toh (old_li->offset[j]);
        nk_offs += 0x1000;
        if (compare_name_with_nk_name (h, name, nk_offs) < 0)
          goto insert_it;
      }
    }
  }

  /* To insert it at the end, we fall through here. */
  assert (j != SIZE_MAX);

 insert_it:
  /* Verify that the search worked. */
  assert (old_lf || old_li);
  assert (!(old_lf && old_li));

  if (old_lf) {
    DEBUG (2, "insert key in existing lf/lh-record at 0x%zx, posn %zu",
           old_offs, j);

    new_offs = insert_lf_record (h, old_offs, j, name, nkoffset);
    if (new_offs == 0)
      return -1;

    DEBUG (2, "new lf/lh-record at 0x%zx", new_offs);
  }
  else /* old_li */ {
    DEBUG (2, "insert key in existing li-record at 0x%zx, posn %zu",
           old_offs, j);

    new_offs = insert_li_record (h, old_offs, j, name, nkoffset);
    if (new_offs == 0)
      return -1;

    DEBUG (2, "new li-record at 0x%zx", new_offs);
  }

  /* Recalculate pointers that could have been invalidated by
   * previous call to allocate_block (via new_{lf,li}_record).
   */
  struct ntreg_nk_record *parent_nk =
    (struct ntreg_nk_record *) ((char *) h->addr + parent);

  /* Since the lf/lh/li-record has moved, now we have to find the old
   * reference to it and update it.  It might be referenced directly
   * from the parent_nk->subkey_lf, or it might be referenced
   * indirectly from some ri-record in blocks[].  Since we can update
   * either of these in-place, we don't need to do this recursively.
   */
  if (le32toh (parent_nk->subkey_lf) + 0x1000 == old_offs) {
    DEBUG (2, "replacing parent_nk->subkey_lf 0x%zx -> 0x%zx",
           old_offs, new_offs);
    parent_nk->subkey_lf = htole32 (new_offs - 0x1000);
  }
  else {
    for (i = 0; blocks[i] != 0; ++i) {
      if (block_id_eq (h, blocks[i], "ri")) {
        struct ntreg_ri_record *ri =
          (struct ntreg_ri_record *) ((char *) h->addr + blocks[i]);
        for (j = 0; j < le16toh (ri->nr_offsets); ++j)
          if (le32toh (ri->offset[j]) + 0x1000 == old_offs) {
            DEBUG (2, "replacing ri (0x%zx) ->offset[%zu] 0x%zx -> 0x%zx",
                   blocks[i], j, old_offs, new_offs);
            ri->offset[j] = htole32 (new_offs - 0x1000);
            goto found_it;
          }
      }
    }

    /* Not found ..  This is an internal error. */
    SET_ERRNO (ENOTSUP, "could not find ri->lf link");
    return -1;

  found_it:;
  }

  return 0;
}

hive_node_h
hivex_node_add_child (hive_h *h, hive_node_h parent, const char *name)
{
  CHECK_WRITABLE (0);

  if (!IS_VALID_BLOCK (h, parent) || !block_id_eq (h, parent, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return 0;
  }

  if (name == NULL || strlen (name) == 0) {
    SET_ERRNO (EINVAL, "name is NULL or zero length");
    return 0;
  }

  if (hivex_node_get_child (h, parent, name) != 0) {
    SET_ERRNO (EEXIST, "a child with that name exists already");
    return 0;
  }

  size_t recoded_name_len;
  int use_utf16 = 0;
  char *recoded_name =
    _hivex_encode_string (h, name, &recoded_name_len, &use_utf16);
  if (recoded_name == NULL) {
    SET_ERRNO (EINVAL, "malformed name");
    return 0;
  }

  /* Create the new nk-record. */
  static const char nk_id[2] = { 'n', 'k' };
  size_t seg_len = sizeof (struct ntreg_nk_record) + recoded_name_len;
  hive_node_h nkoffset = allocate_block (h, seg_len, nk_id);
  if (nkoffset == 0) {
    free (recoded_name);
    return 0;
  }

  DEBUG (2, "allocated new nk-record for child at 0x%zx", nkoffset);

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + nkoffset);
  if (use_utf16)
    nk->flags = htole16 (0x0000);
  else
    nk->flags = htole16 (0x0020);
  nk->parent = htole32 (parent - 0x1000);
  nk->subkey_lf = htole32 (0xffffffff);
  nk->subkey_lf_volatile = htole32 (0xffffffff);
  nk->vallist = htole32 (0xffffffff);
  nk->classname = htole32 (0xffffffff);
  nk->name_len = htole16 (recoded_name_len);
  memcpy (nk->name, recoded_name, recoded_name_len);
  free (recoded_name);

  /* Inherit parent sk. */
  struct ntreg_nk_record *parent_nk =
    (struct ntreg_nk_record *) ((char *) h->addr + parent);
  size_t parent_sk_offset = le32toh (parent_nk->sk);
  parent_sk_offset += 0x1000;
  if (!IS_VALID_BLOCK (h, parent_sk_offset) ||
      !block_id_eq (h, parent_sk_offset, "sk")) {
    SET_ERRNO (EFAULT,
               "parent sk is not a valid block (%zu)", parent_sk_offset);
    return 0;
  }
  struct ntreg_sk_record *sk =
    (struct ntreg_sk_record *) ((char *) h->addr + parent_sk_offset);
  sk->refcount = htole32 (le32toh (sk->refcount) + 1);
  nk->sk = htole32 (parent_sk_offset - 0x1000);

  /* Inherit parent timestamp. */
  nk->timestamp = parent_nk->timestamp;

  /* What I found out the hard way (not documented anywhere): the
   * subkeys in lh-records must be kept sorted.  If you just add a
   * subkey in a non-sorted position (eg. just add it at the end) then
   * Windows won't see the subkey _and_ Windows will corrupt the hive
   * itself when it modifies or saves it.
   *
   * So use get_children() to get a list of intermediate records.
   * get_children() returns these in reading order (which is sorted),
   * so we look for the lf/lh/li-records in sequence until we find the
   * key name just after the one we are inserting, and we insert the
   * subkey just before it.
   *
   * The only other case is the no-subkeys case, where we have to
   * create a brand new lh-record.
   */
  hive_node_h *unused;
  size_t *blocks;

  if (_hivex_get_children (h, parent, &unused, &blocks, 0) == -1)
    return 0;
  free (unused);

  size_t i;
  size_t nr_subkeys_in_parent_nk = le32toh (parent_nk->nr_subkeys);

  if (nr_subkeys_in_parent_nk == UINT32_MAX) {
    free (blocks);
    SET_ERRNO (EOVERFLOW,
               "too many subkeys (%zu)", nr_subkeys_in_parent_nk);
    return 0;
  }

  if (nr_subkeys_in_parent_nk == 0) { /* No subkeys case. */
    /* Free up any existing intermediate blocks. */
    for (i = 0; blocks[i] != 0; ++i)
      mark_block_unused (h, blocks[i]);
    size_t lh_offs = new_lh_record (h, name, nkoffset);
    if (lh_offs == 0) {
      free (blocks);
      return 0;
    }

    /* Recalculate pointers that could have been invalidated by
     * previous call to allocate_block (via new_lh_record).
     */
    /* nk could be invalid here */
    parent_nk = (struct ntreg_nk_record *) ((char *) h->addr + parent);

    DEBUG (2, "no keys, allocated new lh-record at 0x%zx", lh_offs);

    parent_nk->subkey_lf = htole32 (lh_offs - 0x1000);
  }
  else {                        /* Insert subkeys case. */
    if (insert_subkey (h, name, parent, nkoffset, blocks) == -1) {
      free (blocks);
      return 0;
    }

    /* Recalculate pointers that could have been invalidated by
     * previous call to allocate_block (via new_lh_record).
     */
    /* nk could be invalid here */
    parent_nk = (struct ntreg_nk_record *) ((char *) h->addr + parent);
  }

  free (blocks);

  /* Update nr_subkeys in parent nk. */
  nr_subkeys_in_parent_nk++;
  parent_nk->nr_subkeys = htole32 (nr_subkeys_in_parent_nk);

  /* Update max_subkey_name_len in parent nk. */
  size_t utf16_len = use_utf16 ? recoded_name_len : recoded_name_len * 2;
  if (le16toh (parent_nk->max_subkey_name_len) < utf16_len)
    parent_nk->max_subkey_name_len = htole16 (utf16_len);

  return nkoffset;
}

/* Decrement the refcount of an sk-record, and if it reaches zero,
 * unlink it from the chain and delete it.
 */
static int
delete_sk (hive_h *h, size_t sk_offset)
{
  if (!IS_VALID_BLOCK (h, sk_offset) || !block_id_eq (h, sk_offset, "sk")) {
    SET_ERRNO (EFAULT, "not an sk record: 0x%zx", sk_offset);
    return -1;
  }

  struct ntreg_sk_record *sk =
    (struct ntreg_sk_record *) ((char *) h->addr + sk_offset);

  if (sk->refcount == 0) {
    SET_ERRNO (EINVAL, "sk record already has refcount 0: 0x%zx", sk_offset);
    return -1;
  }

  sk->refcount--;

  if (sk->refcount == 0) {
    size_t sk_prev_offset = sk->sk_prev;
    sk_prev_offset += 0x1000;

    size_t sk_next_offset = sk->sk_next;
    sk_next_offset += 0x1000;

    /* Update sk_prev/sk_next SKs, unless they both point back to this
     * cell in which case we are deleting the last SK.
     */
    if (sk_prev_offset != sk_offset && sk_next_offset != sk_offset) {
      struct ntreg_sk_record *sk_prev =
        (struct ntreg_sk_record *) ((char *) h->addr + sk_prev_offset);
      struct ntreg_sk_record *sk_next =
        (struct ntreg_sk_record *) ((char *) h->addr + sk_next_offset);

      sk_prev->sk_next = htole32 (sk_next_offset - 0x1000);
      sk_next->sk_prev = htole32 (sk_prev_offset - 0x1000);
    }

    /* Refcount is zero so really delete this block. */
    mark_block_unused (h, sk_offset);
  }

  return 0;
}

/* Callback from hivex_node_delete_child which is called to delete a
 * node AFTER its subnodes have been visited.  The subnodes have been
 * deleted but we still have to delete any lf/lh/li/ri records and the
 * value list block and values, followed by deleting the node itself.
 */
static int
delete_node (hive_h *h, void *opaque, hive_node_h node, const char *name)
{
  /* Get the intermediate blocks.  The subkeys have already been
   * deleted by this point, so tell get_children() not to check for
   * validity of the nk-records.
   */
  hive_node_h *unused;
  size_t *blocks;
  if (_hivex_get_children (h, node,
                           &unused, &blocks, GET_CHILDREN_NO_CHECK_NK) == -1)
    return -1;
  free (unused);

  /* We don't care what's in these intermediate blocks, so we can just
   * delete them unconditionally.
   */
  size_t i;
  for (i = 0; blocks[i] != 0; ++i)
    mark_block_unused (h, blocks[i]);

  free (blocks);

  /* Delete the values in the node. */
  if (delete_values (h, node) == -1)
    return -1;

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);

  /* If the NK references an SK, delete it. */
  size_t sk_offs = le32toh (nk->sk);
  if (sk_offs != 0xffffffff) {
    sk_offs += 0x1000;
    if (delete_sk (h, sk_offs) == -1)
      return -1;
    nk->sk = htole32 (0xffffffff);
  }

  /* If the NK references a classname, delete it. */
  size_t cl_offs = le32toh (nk->classname);
  if (cl_offs != 0xffffffff) {
    cl_offs += 0x1000;
    mark_block_unused (h, cl_offs);
    nk->classname = htole32 (0xffffffff);
  }

  /* Delete the node itself. */
  mark_block_unused (h, node);

  return 0;
}

int
hivex_node_delete_child (hive_h *h, hive_node_h node)
{
  CHECK_WRITABLE (-1);

  if (!IS_VALID_BLOCK (h, node) || !block_id_eq (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return -1;
  }

  if (node == hivex_root (h)) {
    SET_ERRNO (EINVAL, "cannot delete root node");
    return -1;
  }

  hive_node_h parent = hivex_node_parent (h, node);
  if (parent == 0)
    return -1;

  /* Delete node and all its children and values recursively. */
  static const struct hivex_visitor visitor = { .node_end = delete_node };
  if (hivex_visit_node (h, node, &visitor, sizeof visitor, NULL, 0) == -1)
    return -1;

  /* Delete the link from parent to child.  We need to find the lf/lh
   * record which contains the offset and remove the offset from that
   * record, then decrement the element count in that record, and
   * decrement the overall number of subkeys stored in the parent
   * node.
   */
  hive_node_h *unused;
  size_t *blocks;
  if (_hivex_get_children (h, parent,
                           &unused, &blocks, GET_CHILDREN_NO_CHECK_NK)== -1)
    return -1;
  free (unused);

  size_t i, j;
  for (i = 0; blocks[i] != 0; ++i) {
    struct ntreg_hbin_block *block =
      (struct ntreg_hbin_block *) ((char *) h->addr + blocks[i]);

    if (block->id[0] == 'l' && (block->id[1] == 'f' || block->id[1] == 'h')) {
      struct ntreg_lf_record *lf = (struct ntreg_lf_record *) block;

      size_t nr_subkeys_in_lf = le16toh (lf->nr_keys);

      for (j = 0; j < nr_subkeys_in_lf; ++j)
        if (le32toh (lf->keys[j].offset) + 0x1000 == node) {
          for (; j < nr_subkeys_in_lf - 1; ++j)
            memcpy (&lf->keys[j], &lf->keys[j+1], sizeof (lf->keys[j]));
          lf->nr_keys = htole16 (nr_subkeys_in_lf - 1);
          goto found;
        }
    }
  }
  free (blocks);
  SET_ERRNO (ENOTSUP, "could not find parent to child link");
  return -1;

 found:;
  free (blocks);

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + parent);
  size_t nr_subkeys_in_nk = le32toh (nk->nr_subkeys);
  nk->nr_subkeys = htole32 (nr_subkeys_in_nk - 1);

  DEBUG (2, "updating nr_subkeys in parent 0x%zx to %zu",
         parent, nr_subkeys_in_nk);

  return 0;
}

int
hivex_node_set_values (hive_h *h, hive_node_h node,
                       size_t nr_values, const hive_set_value *values,
                       int flags)
{
  CHECK_WRITABLE (-1);

  if (!IS_VALID_BLOCK (h, node) || !block_id_eq (h, node, "nk")) {
    SET_ERRNO (EINVAL, "invalid block or not an 'nk' block");
    return -1;
  }

  /* Delete all existing values. */
  if (delete_values (h, node) == -1)
    return -1;

  if (nr_values == 0)
    return 0;

  /* Allocate value list node.  Value lists have no id field. */
  static const char nul_id[2] = { 0, 0 };
  size_t seg_len =
    sizeof (struct ntreg_value_list) + (nr_values - 1) * sizeof (uint32_t);
  size_t vallist_offs = allocate_block (h, seg_len, nul_id);
  if (vallist_offs == 0)
    return -1;

  struct ntreg_nk_record *nk =
    (struct ntreg_nk_record *) ((char *) h->addr + node);
  nk->nr_values = htole32 (nr_values);
  nk->vallist = htole32 (vallist_offs - 0x1000);


  size_t i;
  for (i = 0; i < nr_values; ++i) {
    /* Allocate vk record to store this (key, value) pair. */
    static const char vk_id[2] = { 'v', 'k' };
    size_t recoded_name_len;
    int use_utf16;
    char* recoded_name = _hivex_encode_string (h, values[i].key, &recoded_name_len,
                                               &use_utf16);
    seg_len = sizeof (struct ntreg_vk_record) + recoded_name_len;
    size_t vk_offs = allocate_block (h, seg_len, vk_id);
    if (vk_offs == 0)
      return -1;

    /* Recalculate pointers that could have been invalidated by
     * previous call to allocate_block.
     */
    nk = (struct ntreg_nk_record *) ((char *) h->addr + node);
    struct ntreg_value_list *vallist =
      (struct ntreg_value_list *) ((char *) h->addr + vallist_offs);

    vallist->offset[i] = htole32 (vk_offs - 0x1000);

    struct ntreg_vk_record *vk =
      (struct ntreg_vk_record *) ((char *) h->addr + vk_offs);
    vk->name_len = htole16 (recoded_name_len);
    memcpy (vk->name, recoded_name, recoded_name_len);
    free (recoded_name);
    vk->data_type = htole32 (values[i].t);
    uint32_t len = values[i].len;
    if (len <= 4)               /* store it inline => set MSB flag */
      len |= 0x80000000;
    vk->data_len = htole32 (len);
    if (recoded_name_len == 0)
      vk->flags = 0;
    else
      vk->flags = htole16 (!use_utf16);

    if (values[i].len <= 4)     /* store it inline */
      memcpy (&vk->data_offset, values[i].value, values[i].len);
    else {
      size_t offs = allocate_block (h, values[i].len + 4, nul_id);
      if (offs == 0)
        return -1;

      /* Recalculate pointers that could have been invalidated by
       * previous call to allocate_block.
       */
      nk = (struct ntreg_nk_record *) ((char *) h->addr + node);
      /* vallist could be invalid here */
      vk = (struct ntreg_vk_record *) ((char *) h->addr + vk_offs);

      memcpy ((char *) h->addr + offs + 4, values[i].value, values[i].len);
      vk->data_offset = htole32 (offs - 0x1000);
    }

    size_t utf16_len = use_utf16 ? recoded_name_len : recoded_name_len * 2;
    if (utf16_len > le32toh (nk->max_vk_name_len))
      nk->max_vk_name_len = htole32 (utf16_len);
    if (values[i].len > le32toh (nk->max_vk_data_len))
      nk->max_vk_data_len = htole32 (values[i].len);
  }

  return 0;
}

int
hivex_node_set_value (hive_h *h, hive_node_h node,
                      const hive_set_value *val, int flags)
{
  int retval = -1;
  hive_value_h *prev_values;
  hive_set_value *new_values;
  size_t nr_values;
  size_t i;
  ssize_t idx_of_val;

  prev_values = hivex_node_values (h, node);
  if (prev_values == NULL)
    return -1;

  /* Count number of existing values in this node. */
  nr_values = 0;
  for (i = 0; prev_values[i] != 0; i++)
    nr_values++;

  /* Allocate a new hive_set_value list, with space for all existing
   * values, plus 1 (for the new key if we're not replacing an
   * existing key).
   */
  new_values = calloc (nr_values + 1, sizeof (hive_set_value));
  if (new_values == NULL)
    goto out1;

  /* Copy the old values to the new values.  If we find the key along
   * the way, note its index in 'idx_of_val'.
   */
  idx_of_val = -1;
  for (i = 0; prev_values[i] != 0; i++) {
    size_t len;
    hive_type t;
    char *valkey, *valval;

    valval = hivex_value_value (h, prev_values[i], &t, &len);
    if (valval == NULL) goto out2;

    new_values[i].value = valval;
    new_values[i].t = t;
    new_values[i].len = len;

    valkey = hivex_value_key (h, prev_values[i]);
    if (valkey == NULL) goto out2;

    new_values[i].key = valkey;

    if (STRCASEEQ (valkey, val->key))
      idx_of_val = i;
  }

  if (idx_of_val > -1) {
    free (new_values[idx_of_val].key);
    free (new_values[idx_of_val].value);
  } else { /* insert it at the end */
    idx_of_val = nr_values;
    nr_values++;
  }

  new_values[idx_of_val].key = strdup (val->key);
  new_values[idx_of_val].value = malloc (val->len);
  new_values[idx_of_val].len = val->len;
  new_values[idx_of_val].t = val->t;

  if (new_values[idx_of_val].key == NULL ||
      new_values[idx_of_val].value == NULL)
    goto out2;
  memcpy (new_values[idx_of_val].value, val->value, val->len);

  retval = hivex_node_set_values (h, node, nr_values, new_values, 0);

 out2:
  for (i = 0; i < nr_values; ++i) {
    free (new_values[i].key);
    free (new_values[i].value);
  }
  free (new_values);

 out1:
  free (prev_values);

  return retval;
}
