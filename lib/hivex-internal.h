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

#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <iconv.h>
#include <pthread.h>

#include "byte_conversions.h"

#define STREQ(a,b) (strcmp((a),(b)) == 0)
#define STRCASEEQ(a,b) (strcasecmp((a),(b)) == 0)
#define STRNEQ(a,b) (strcmp((a),(b)) != 0)
#define STRCASENEQ(a,b) (strcasecmp((a),(b)) != 0)
#define STREQLEN(a,b,n) (strncmp((a),(b),(n)) == 0)
#define STRCASEEQLEN(a,b,n) (strncasecmp((a),(b),(n)) == 0)
#define STRNEQLEN(a,b,n) (strncmp((a),(b),(n)) != 0)
#define STRCASENEQLEN(a,b,n) (strncasecmp((a),(b),(n)) != 0)
#define STRPREFIX(a,b) (strncmp((a),(b),strlen((b))) == 0)

typedef enum {
  utf8_to_latin1 = 0,
  latin1_to_utf8,
  utf8_to_utf16le,
  utf16le_to_utf8,
  nr_recode_types,
} recode_type;

struct hive_h {
  char *filename;
  int fd;
  size_t size;
  int msglvl;                   /* 1 = verbose, 2 or 3 = debug */
  int writable;
  int unsafe;

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

  struct {
    pthread_mutex_t mutex;
    iconv_t *handle;
  } iconv_cache[nr_recode_types];
};

/* Format of registry blocks. NB. All fields are little endian. */
struct ntreg_header {
  char magic[4];                /* "regf" */
  uint32_t sequence1;
  uint32_t sequence2;
  int64_t last_modified;
  uint32_t major_ver;           /* 1 */
  uint32_t minor_ver;           /* 3 */
  uint32_t unknown5;            /* 0 */
  uint32_t unknown6;            /* 1 */
  uint32_t offset;              /* offset of root key record - 4KB */
  uint32_t blocks;              /* pointer AFTER last hbin in file - 4KB */
  uint32_t unknown7;            /* 1 */
  /* 0x30 */
  char name[64];                /* original file name of hive */
  char unknown_guid1[16];
  char unknown_guid2[16];
  /* 0x90 */
  uint32_t unknown8;
  char unknown_guid3[16];
  uint32_t unknown9;
  /* 0xa8 */
  char unknown10[340];
  /* 0x1fc */
  uint32_t csum;                /* checksum: xor of dwords 0-0x1fb. */
  /* 0x200 */
  char unknown11[3528];
  /* 0xfc8 */
  char unknown_guid4[16];
  char unknown_guid5[16];
  char unknown_guid6[16];
  uint32_t unknown12;
  uint32_t unknown13;
  /* 0x1000 */
} __attribute__((__packed__));

struct ntreg_hbin_page {
  char magic[4];                /* "hbin" */
  uint32_t offset_first;        /* offset from 1st block */
  uint32_t page_size;           /* size of this page (multiple of 4KB) */
  char unknown[20];
  /* Linked list of blocks follows here. */
} __attribute__((__packed__));

struct ntreg_hbin_block {
  int32_t seg_len;              /* length of this block (-ve for used block) */
  char id[2];                   /* the block type (eg. "nk" for nk record) */
  /* Block data follows here. */
} __attribute__((__packed__));

static inline int
block_id_eq (const hive_h *h, size_t offs, const char *eqid)
{
  return (STREQLEN (((struct ntreg_hbin_block *)((char *) (h)->addr + (offs)))->id, (eqid), 2));
}

struct ntreg_nk_record {
  int32_t seg_len;              /* length (always -ve because used) */
  char id[2];                   /* "nk" */
  uint16_t flags;               /* bit 1: HiveExit
                                   bit 2: HiveEntry == root key
                                   bit 3: NoDelete
                                   bit 4: SymbolicLink
                                   bit 5: CompressedName: Name is encoded
                                          in ASCII (actually: Latin-1)
                                          rather than UTF-16.
                                   bit 6: PredefinedHandle
                                   bit 7: VirtMirrored
                                   bit 8: VirtTarget
                                   bit 9: VirtualStore */
  /* Information from: Peter Norris: The Internal Structure of the
     Windows Registry, 2008, p.220 ff. */
  int64_t timestamp;
  uint32_t unknown1;
  uint32_t parent;              /* offset of owner/parent */
  uint32_t nr_subkeys;          /* number of subkeys */
  uint32_t nr_subkeys_volatile;
  uint32_t subkey_lf;           /* lf record containing list of subkeys */
  uint32_t subkey_lf_volatile;
  uint32_t nr_values;           /* number of values */
  uint32_t vallist;             /* value-list record */
  uint32_t sk;                  /* offset of sk-record */
  uint32_t classname;           /* offset of classname record */
  uint16_t max_subkey_name_len; /* maximum length of a subkey name in bytes
                                   if the subkey was reencoded as UTF-16LE */
  uint16_t unknown2;
  uint32_t unknown3;
  uint32_t max_vk_name_len;     /* maximum length of any vk name in bytes
                                   if the name was reencoded as UTF-16LE */
  uint32_t max_vk_data_len;     /* maximum length of any vk data in bytes */
  uint32_t unknown6;
  uint16_t name_len;            /* length of name */
  uint16_t classname_len;       /* length of classname */
  char name[1];                 /* name follows here */
} __attribute__((__packed__));

struct ntreg_lf_record {
  int32_t seg_len;
  char id[2];                   /* "lf"|"lh" */
  uint16_t nr_keys;             /* number of keys in this record */
  struct {
    uint32_t offset;            /* offset of nk-record for this subkey */
    char hash[4];               /* hash of subkey name */
  } __attribute__((__packed__)) keys[1];
} __attribute__((__packed__));

struct ntreg_ri_record {
  int32_t seg_len;
  char id[2];                   /* "ri" */
  uint16_t nr_offsets;          /* number of pointers to lh records */
  uint32_t offset[1];           /* list of pointers to lh records */
} __attribute__((__packed__));

/* This has no ID header. */
struct ntreg_value_list {
  int32_t seg_len;
  uint32_t offset[1];           /* list of pointers to vk records */
} __attribute__((__packed__));

struct ntreg_vk_record {
  int32_t seg_len;              /* length (always -ve because used) */
  char id[2];                   /* "vk" */
  uint16_t name_len;            /* length of name */
  /* length of the data:
   * If data_len is <= 4, then it's stored inline.
   * Top bit is set to indicate inline.
   */
  uint32_t data_len;
  uint32_t data_offset;         /* pointer to the data (or data if inline) */
  uint32_t data_type;           /* type of the data */
  uint16_t flags;               /* bit 0 set => key name ASCII,
                                   bit 0 clr => key name UTF-16.
                                   Only seen ASCII here in the wild.
                                   NB: this is CLEAR for default key. */
  uint16_t unknown2;
  char name[1];                 /* key name follows here */
} __attribute__((__packed__));

struct ntreg_sk_record {
  int32_t seg_len;              /* length (always -ve because used) */
  char id[2];                   /* "sk" */
  uint16_t unknown1;
  uint32_t sk_next;             /* linked into a circular list */
  uint32_t sk_prev;
  uint32_t refcount;            /* reference count */
  uint32_t sec_len;             /* length of security info */
  char sec_desc[1];             /* security info follows */
} __attribute__((__packed__));

struct ntreg_db_record {
  int32_t seg_len;              /* length (always -ve because used) */
  char id[2];                   /* "db" */
  uint16_t nr_blocks;
  uint32_t blocklist_offset;
  uint32_t unknown1;
} __attribute__((__packed__));

struct ntreg_db_block {
  int32_t seg_len;
  char data[1];
} __attribute__((__packed__));

static inline size_t
block_len (hive_h *h, size_t blkoff, int *used)
{
  struct ntreg_hbin_block *block;
  block = (struct ntreg_hbin_block *) ((char *) h->addr + blkoff);

  int32_t len = le32toh (block->seg_len);
  if (len < 0) {
    if (used) *used = 1;
    len = -len;
  } else {
    if (used) *used = 0;
  }

  return (size_t) len;
}

/* node.c */
#define GET_CHILDREN_NO_CHECK_NK 1
extern int _hivex_get_children (hive_h *h, hive_node_h node, hive_node_h **children_ret, size_t **blocks_ret, int flags);

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
extern void _hivex_print_offset_list (offset_list *list, FILE *fp);

/* handle.c */
extern iconv_t * _hivex_get_iconv (hive_h *h, recode_type r);
extern void  _hivex_release_iconv (hive_h *h, recode_type r);

/* utf16.c */
extern char * _hivex_recode (hive_h *h, recode_type r,
                             const char *input, size_t input_len, size_t *output_len);
extern char* _hivex_encode_string (hive_h *h, const char *str, size_t *size, int *utf16);
extern size_t _hivex_utf16_string_len_in_bytes_max (const char *str, size_t len);
extern size_t _hivex_utf8_strlen (hive_h *h, const char* str, size_t len, int utf16);

/* util.c */
extern void _hivex_free_strings (char **argv);

/* value.c */
extern int _hivex_get_values (hive_h *h, hive_node_h node, hive_value_h **values_ret, size_t **blocks_ret);

#define DEBUG(lvl,fs,...)                                       \
  do {                                                          \
    if (h->msglvl >= (lvl)) {                                   \
      fprintf (stderr, "%s: %s: " fs "\n",                      \
               "hivex", __func__, ## __VA_ARGS__);              \
    }                                                           \
  } while (0)

#define SET_ERRNO(errval,fs,...)                                        \
  do {                                                                  \
    DEBUG (1, "returning " #errval " because: " fs, ## __VA_ARGS__);    \
    errno = errval;                                                     \
  } while (0)

#define CHECK_WRITABLE(retcode)                                         \
  do {                                                                  \
    if (!h->writable) {                                                 \
      SET_ERRNO (EROFS,                                                 \
                 "HIVEX_OPEN_WRITE flag was not specified when opening this hive"); \
      return (retcode);                                                 \
    }                                                                   \
  } while (0)

/* These limits are in place to stop really stupid stuff and/or exploits. */
#define HIVEX_MAX_SUBKEYS       70000
#define HIVEX_MAX_VALUES       110000
#define HIVEX_MAX_VALUE_LEN   8000000
#define HIVEX_MAX_ALLOCATION  1000000

#endif /* HIVEX_INTERNAL_H_ */
