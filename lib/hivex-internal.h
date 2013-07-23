/* hivex internal header
 * Copyright (C) 2009-2011 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation;
 * version 2.1 of the License only.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef HIVEX_INTERNAL_H_
#define HIVEX_INTERNAL_H_

#include <stddef.h>

struct hive_h {
  char *filename;
  int fd;
  size_t size;
  int msglvl;
  int writable;

  /* Registry file, memory mapped if read-only, or malloc'd if writing. */
  union {
    void *addr;
    struct ntreg_header *hdr;
  };

  /* Use a bitmap to store which file offsets are valid (point to a
   * used block).  We only need to store 1 bit per 32 bits of the file
   * (because blocks are 4-byte aligned).  We found that the average
   * block size in a registry file is ~50 bytes.  So roughly 1 in 12
   * bits in the bitmap will be set, making it likely a more efficient
   * structure than a hash table.
   */
  char *bitmap;
#define BITMAP_SET(bitmap,off) (bitmap[(off)>>5] |= 1 << (((off)>>2)&7))
#define BITMAP_CLR(bitmap,off) (bitmap[(off)>>5] &= ~ (1 << (((off)>>2)&7)))
#define BITMAP_TST(bitmap,off) (bitmap[(off)>>5] & (1 << (((off)>>2)&7)))
#define IS_VALID_BLOCK(h,off)               \
  (((off) & 3) == 0 &&                      \
   (off) >= 0x1000 &&                       \
   (off) < (h)->size &&                     \
   BITMAP_TST((h)->bitmap,(off)))

  /* Fields from the header, extracted from little-endianness hell. */
  size_t rootoffs;              /* Root key offset (always an nk-block). */
  size_t endpages;              /* Offset of end of pages. */
  int64_t last_modified;        /* mtime of base block. */

  /* For writing. */
  size_t endblocks;             /* Offset to next block allocation (0
                                   if not allocated anything yet). */

#ifndef HAVE_MMAP
  /* Internal data for mmap replacement */
  void *p_winmap;
#endif
};

/* offset-list.c */
typedef struct offset_list offset_list;
struct offset_list {
  hive_h *h;
  size_t *offsets;
  size_t len;
  size_t alloc;
  size_t limit;
};
extern void _hivex_init_offset_list (hive_h *h, offset_list *list);
extern int _hivex_grow_offset_list (offset_list *list, size_t alloc);
extern int _hivex_add_to_offset_list (offset_list *list, size_t offset);
extern size_t _hivex_get_offset_list_length (offset_list *list);
extern void _hivex_set_offset_list_limit (offset_list *list, size_t limit);
extern void _hivex_free_offset_list (offset_list *list);
extern size_t * _hivex_return_offset_list (offset_list *list);

#define STREQ(a,b) (strcmp((a),(b)) == 0)
#define STRCASEEQ(a,b) (strcasecmp((a),(b)) == 0)
#define STRNEQ(a,b) (strcmp((a),(b)) != 0)
#define STRCASENEQ(a,b) (strcasecmp((a),(b)) != 0)
#define STREQLEN(a,b,n) (strncmp((a),(b),(n)) == 0)
#define STRCASEEQLEN(a,b,n) (strncasecmp((a),(b),(n)) == 0)
#define STRNEQLEN(a,b,n) (strncmp((a),(b),(n)) != 0)
#define STRCASENEQLEN(a,b,n) (strncasecmp((a),(b),(n)) != 0)
#define STRPREFIX(a,b) (strncmp((a),(b),strlen((b))) == 0)

#endif /* HIVEX_INTERNAL_H_ */
