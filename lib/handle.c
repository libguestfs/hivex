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
#include <inttypes.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <assert.h>
#include <iconv.h>
#include <pthread.h>

#ifdef HAVE_MMAP
#include <sys/mman.h>
#else
/* On systems without mmap (and munmap), use a replacement function. */
#include "mmap.h"
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef O_NOCTTY
#define O_NOCTTY 0
#endif

#include "full-read.h"
#include "full-write.h"
#include "c-ctype.h"

#include "hivex.h"
#include "hivex-internal.h"

static uint32_t
header_checksum (const hive_h *h)
{
  uint32_t *daddr = (uint32_t *) h->addr;
  size_t i;
  uint32_t sum = 0;

  for (i = 0; i < 0x1fc / 4; ++i) {
    sum ^= le32toh (*daddr);
    daddr++;
  }

  return sum;
}

#define HIVEX_OPEN_MSGLVL_MASK (HIVEX_OPEN_VERBOSE|HIVEX_OPEN_DEBUG)

iconv_t *
_hivex_get_iconv (hive_h *h, recode_type t)
{
  pthread_mutex_lock (&h->iconv_cache[t].mutex);
  if (h->iconv_cache[t].handle == NULL) {
    if (t == utf8_to_latin1)
      h->iconv_cache[t].handle = iconv_open ("LATIN1", "UTF-8");
    else if (t == latin1_to_utf8)
      h->iconv_cache[t].handle = iconv_open ("UTF-8", "LATIN1");
    else if (t == utf8_to_utf16le)
      h->iconv_cache[t].handle = iconv_open ("UTF-16LE", "UTF-8");
    else if (t == utf16le_to_utf8)
      h->iconv_cache[t].handle = iconv_open ("UTF-8", "UTF-16LE");
  } else {
    /* reinitialize iconv context */
    iconv (h->iconv_cache[t].handle, NULL, 0, NULL, 0);
  }
  return h->iconv_cache[t].handle;
}

void
_hivex_release_iconv (hive_h *h, recode_type t)
{
  pthread_mutex_unlock (&h->iconv_cache[t].mutex);
}

hive_h *
hivex_open (const char *filename, int flags)
{
  hive_h *h = NULL;

  assert (sizeof (struct ntreg_header) == 0x1000);
  assert (offsetof (struct ntreg_header, csum) == 0x1fc);

  h = calloc (1, sizeof *h);
  if (h == NULL)
    goto error;

  h->msglvl = flags & HIVEX_OPEN_MSGLVL_MASK;

  const char *debug = getenv ("HIVEX_DEBUG");
  if (debug && STREQ (debug, "1"))
    h->msglvl = 2;

  DEBUG (2, "created handle %p", h);

  h->writable = !!(flags & HIVEX_OPEN_WRITE);
  h->unsafe = !!(flags & HIVEX_OPEN_UNSAFE);
  h->filename = strdup (filename);
  if (h->filename == NULL)
    goto error;

#ifdef O_CLOEXEC
  h->fd = open (filename, O_RDONLY | O_CLOEXEC | O_BINARY);
#else
  h->fd = open (filename, O_RDONLY | O_BINARY);
#endif
  if (h->fd == -1)
    goto error;
#ifdef FD_CLOEXEC
  fcntl (h->fd, F_SETFD, FD_CLOEXEC);
#endif

  struct stat statbuf;
  if (fstat (h->fd, &statbuf) == -1)
    goto error;

  h->size = statbuf.st_size;

  if (h->size < 0x2000) {
    SET_ERRNO (EINVAL,
               "%s: file is too small to be a Windows NT Registry hive file",
               filename);
    goto error;
  }

  if (!h->writable) {
    h->addr = mmap (NULL, h->size, PROT_READ, MAP_SHARED, h->fd, 0);
    if (h->addr == MAP_FAILED)
      goto error;

    DEBUG (2, "mapped file at %p", h->addr);
  } else {
    h->addr = malloc (h->size);
    if (h->addr == NULL)
      goto error;

    if (full_read (h->fd, h->addr, h->size) < h->size)
      goto error;

    /* We don't need the file descriptor along this path, since we
     * have read all the data.
     */
    if (close (h->fd) == -1)
      goto error;
    h->fd = -1;
  }

  /* Check header. */
  if (h->hdr->magic[0] != 'r' ||
      h->hdr->magic[1] != 'e' ||
      h->hdr->magic[2] != 'g' ||
      h->hdr->magic[3] != 'f') {
    SET_ERRNO (ENOTSUP,
               "%s: not a Windows NT Registry hive file", filename);
    goto error;
  }

  /* Check major version. */
  uint32_t major_ver = le32toh (h->hdr->major_ver);
  if (major_ver != 1) {
    SET_ERRNO (ENOTSUP,
               "%s: hive file major version %" PRIu32 " (expected 1)",
               filename, major_ver);
    goto error;
  }

  h->bitmap = calloc (1 + h->size / 32, 1);
  if (h->bitmap == NULL)
    goto error;

  /* Header checksum. */
  uint32_t sum = header_checksum (h);
  if (sum != le32toh (h->hdr->csum)) {
    SET_ERRNO (EINVAL, "%s: bad checksum in hive header", filename);
    goto error;
  }

  for (int t = 0; t < nr_recode_types; t++) {
    pthread_mutex_init (&h->iconv_cache[t].mutex, NULL);
    h->iconv_cache[t].handle = NULL;
  }

  /* Last modified time. */
  h->last_modified = le64toh ((int64_t) h->hdr->last_modified);

  if (h->msglvl >= 2) {
    char *name = _hivex_recode (h, utf16le_to_utf8,
                                h->hdr->name, 64, NULL);

    fprintf (stderr,
             "hivex_open: header fields:\n"
             "  file version             %" PRIu32 ".%" PRIu32 "\n"
             "  sequence nos             %" PRIu32 " %" PRIu32 "\n"
             "    (sequences nos should match if hive was synched at shutdown)\n"
             "  last modified            %" PRIi64 "\n"
             "    (Windows filetime, x 100 ns since 1601-01-01)\n"
             "  original file name       %s\n"
             "    (only 32 chars are stored, name is probably truncated)\n"
             "  root offset              0x%x + 0x1000\n"
             "  end of last page         0x%x + 0x1000 (total file size 0x%zx)\n"
             "  checksum                 0x%x (calculated 0x%x)\n",
             major_ver, le32toh (h->hdr->minor_ver),
             le32toh (h->hdr->sequence1), le32toh (h->hdr->sequence2),
             h->last_modified,
             name ? name : "(conversion failed)",
             le32toh (h->hdr->offset),
             le32toh (h->hdr->blocks), h->size,
             le32toh (h->hdr->csum), sum);
    free (name);
  }

  h->rootoffs = le32toh (h->hdr->offset) + 0x1000;
  h->endpages = le32toh (h->hdr->blocks) + 0x1000;

  DEBUG (2, "root offset = 0x%zx", h->rootoffs);

  /* We'll set this flag when we see a block with the root offset (ie.
   * the root block).
   */
  int seen_root_block = 0, bad_root_block = 0;

  /* Collect some stats. */
  size_t pages = 0;           /* Number of hbin pages read. */
  size_t smallest_page = SIZE_MAX, largest_page = 0;
  size_t blocks = 0;          /* Total number of blocks found. */
  size_t smallest_block = SIZE_MAX, largest_block = 0, blocks_bytes = 0;
  size_t used_blocks = 0;     /* Total number of used blocks found. */
  size_t used_size = 0;       /* Total size (bytes) of used blocks. */

  /* Read the pages and blocks.  The aim here is to be robust against
   * corrupt or malicious registries.  So we make sure the loops
   * always make forward progress.  We add the address of each block
   * we read to a hash table so pointers will only reference the start
   * of valid blocks.
   */
  size_t off;
  struct ntreg_hbin_page *page;
  for (off = 0x1000; off < h->size; off += le32toh (page->page_size)) {
    if (off >= h->endpages)
      break;

    page = (struct ntreg_hbin_page *) ((char *) h->addr + off);
    if (page->magic[0] != 'h' ||
        page->magic[1] != 'b' ||
        page->magic[2] != 'i' ||
        page->magic[3] != 'n') {

      if (!h->unsafe) {
        SET_ERRNO (ENOTSUP,
                   "%s: trailing garbage at end of file "
                   "(at 0x%zx, after %zu pages)",
                   filename, off, pages);
        goto error;
      }

      DEBUG (2,
             "page not found at expected offset 0x%zx, "
             "seeking until one is found or EOF is reached",
             off);

      int found = 0;
      while (off < h->size) {
        off += 0x1000;

        if (off >= h->endpages)
          break;

        page = (struct ntreg_hbin_page *) ((char *) h->addr + off);
        if (page->magic[0] == 'h' &&
            page->magic[1] == 'b' &&
            page->magic[2] == 'i' &&
            page->magic[3] == 'n') {
          DEBUG (2, "found next page by seeking at 0x%zx", off);
          found = 1;
          break;
        }
      }

      if (!found) {
        DEBUG (2, "page not found and end of pages section reached");
        break;
      }
    }

    size_t page_size = le32toh (page->page_size);
    DEBUG (2, "page at 0x%zx, size %zu", off, page_size);
    pages++;
    if (page_size < smallest_page) smallest_page = page_size;
    if (page_size > largest_page) largest_page = page_size;

    if (page_size <= sizeof (struct ntreg_hbin_page) ||
        (page_size & 0x0fff) != 0) {
      SET_ERRNO (ENOTSUP,
                 "%s: page size %zu at 0x%zx, bad registry",
                 filename, page_size, off);
      goto error;
    }

    if (off + page_size > h->size) {
      SET_ERRNO (ENOTSUP,
                 "%s: page size %zu at 0x%zx extends beyond end of file, bad registry",
                 filename, page_size, off);
      goto error;
    }

    size_t page_offset = le32toh(page->offset_first) + 0x1000;

    if (page_offset != off) {
      SET_ERRNO (ENOTSUP,
                 "%s: declared page offset (0x%zx) does not match computed "
                 "offset (0x%zx), bad registry",
                 filename, page_offset, off);
      goto error;
    }

    /* Read the blocks in this page. */
    size_t blkoff;
    struct ntreg_hbin_block *block;
    size_t seg_len;
    for (blkoff = off + 0x20;
         blkoff < off + page_size;
         blkoff += seg_len) {
      blocks++;

      int is_root = blkoff == h->rootoffs;
      if (is_root)
        seen_root_block = 1;

      block = (struct ntreg_hbin_block *) ((char *) h->addr + blkoff);
      int used;
      seg_len = block_len (h, blkoff, &used);
/* https://gcc.gnu.org/bugzilla/show_bug.cgi?id=78665 */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-overflow"
      if (seg_len <= 4 || (seg_len & 3) != 0) {
#pragma GCC diagnostic pop
        if (is_root || !h->unsafe) {
          SET_ERRNO (ENOTSUP,
                     "%s, the block at 0x%zx size %" PRIu32
                     " <= 4 or not a multiple of 4, bad registry",
                     filename, blkoff, le32toh (block->seg_len));
          goto error;
        } else {
          DEBUG (2,
                 "%s: block at 0x%zx has invalid size %" PRIu32 ", skipping",
                 filename, blkoff, le32toh (block->seg_len));
          break;
        }
      }

      if (blkoff + seg_len > off + page_size) {
        SET_ERRNO (ENOTSUP,
                   "%s, the block at 0x%zx size %" PRIu32
                   " extends beyond the current page, bad registry",
                   filename, blkoff, le32toh (block->seg_len));
        goto error;
      }

      if (h->msglvl >= 2) {
        unsigned char *id = (unsigned char *) block->id;
        int id0 = id[0], id1 = id[1];

        fprintf (stderr, "%s: %s: "
                 "%s block id %d,%d (%c%c) at 0x%zx size %zu%s\n",
                 "hivex", __func__,
                 used ? "used" : "free",
                 id0, id1,
                 c_isprint (id0) ? id0 : '.',
                 c_isprint (id1) ? id1 : '.',
                 blkoff,
                 seg_len, is_root ? " (root)" : "");
      }

      blocks_bytes += seg_len;
      if (seg_len < smallest_block) smallest_block = seg_len;
      if (seg_len > largest_block) largest_block = seg_len;

      if (is_root && !used)
        bad_root_block = 1;

      if (used) {
        used_blocks++;
        used_size += seg_len;

        /* Root block must be an nk-block. */
        if (is_root && (block->id[0] != 'n' || block->id[1] != 'k'))
          bad_root_block = 1;

        /* Note this blkoff is a valid address. */
        BITMAP_SET (h->bitmap, blkoff);
      }
    }
  }

  if (!seen_root_block) {
    SET_ERRNO (ENOTSUP, "%s: no root block found", filename);
    goto error;
  }

  if (bad_root_block) {
    SET_ERRNO (ENOTSUP, "%s: bad root block (free or not nk)", filename);
    goto error;
  }

  DEBUG (1, "successfully read Windows Registry hive file:\n"
         "  pages:          %zu [sml: %zu, lge: %zu]\n"
         "  blocks:         %zu [sml: %zu, avg: %zu, lge: %zu]\n"
         "  blocks used:    %zu\n"
         "  bytes used:     %zu",
         pages, smallest_page, largest_page,
         blocks, smallest_block, blocks_bytes / blocks, largest_block,
         used_blocks, used_size);

  return h;

 error:;
  int err = errno;
  if (h) {
    free (h->bitmap);
    if (h->addr && h->size && h->addr != MAP_FAILED) {
      if (!h->writable)
        munmap (h->addr, h->size);
      else
        free (h->addr);
    }
    if (h->fd >= 0)
      close (h->fd);
    free (h->filename);
    free (h);
  }
  errno = err;
  return NULL;
}

int
hivex_close (hive_h *h)
{
  int r;

  DEBUG (1, "hivex_close");

  free (h->bitmap);
  if (!h->writable)
    munmap (h->addr, h->size);
  else
    free (h->addr);
  if (h->fd >= 0)
    r = close (h->fd);
  else
    r = 0;
  free (h->filename);
  for (int t=0; t<nr_recode_types; t++) {
    if (h->iconv_cache[t].handle != NULL) {
      iconv_close (h->iconv_cache[t].handle);
      h->iconv_cache[t].handle = NULL;
    }
  }
  free (h);

  return r;
}

int
hivex_commit (hive_h *h, const char *filename, int flags)
{
  int fd;

  if (flags != 0) {
    SET_ERRNO (EINVAL, "flags != 0");
    return -1;
  }

  CHECK_WRITABLE (-1);

  filename = filename ? : h->filename;
#ifdef O_CLOEXEC
  fd = open (filename, O_WRONLY|O_CREAT|O_TRUNC|O_NOCTTY|O_CLOEXEC|O_BINARY,
             0666);
#else
  fd = open (filename, O_WRONLY|O_CREAT|O_TRUNC|O_NOCTTY|O_BINARY, 0666);
#endif
  if (fd == -1)
    return -1;
#ifdef FD_CLOEXEC
  fcntl (fd, F_SETFD, FD_CLOEXEC);
#endif

  /* Update the header fields. */
  uint32_t sequence = le32toh (h->hdr->sequence1);
  sequence++;
  h->hdr->sequence1 = htole32 (sequence);
  h->hdr->sequence2 = htole32 (sequence);
  /* XXX Ought to update h->hdr->last_modified. */
  h->hdr->blocks = htole32 (h->endpages - 0x1000);

  /* Recompute header checksum. */
  uint32_t sum = header_checksum (h);
  h->hdr->csum = htole32 (sum);

  DEBUG (2, "hivex_commit: new header checksum: 0x%x", sum);

  if (full_write (fd, h->addr, h->size) != h->size) {
    int err = errno;
    close (fd);
    errno = err;
    return -1;
  }

  if (close (fd) == -1)
    return -1;

  return 0;
}
