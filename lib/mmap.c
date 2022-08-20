/* mmap replacement for mingw.
 *
 * Copyright (C) 2011 by Daniel Gillen <gillen (dot) dan (at) pinguin (dot) lu>
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
 */

#include <config.h>

#include "hivex.h"
#include "hivex-internal.h"
#include "mmap.h"

#include <windows.h>
#include <io.h>

void *
hivex__rpl_mmap (hive_h *h,
                 void *p_addr, size_t len, int prot, int flags, int fd, off_t offset)
{
  void *p_map;

  // Check parameters for unsupported values
  if (p_addr != NULL)
    return MAP_FAILED;
  if (prot != PROT_READ)
    return MAP_FAILED;
  if (flags != MAP_SHARED)
    return MAP_FAILED;

  // Create file mapping
  h->p_winmap = CreateFileMapping ((HANDLE)_get_osfhandle(fd),
                                   NULL, PAGE_READONLY, 0, 0, NULL);
  if (h->p_winmap == NULL)
    return MAP_FAILED;

  // Create map view
  p_map = MapViewOfFile (h->p_winmap, FILE_MAP_READ, 0, 0, len);
  if (p_map == NULL) {
    CloseHandle (h->p_winmap);
    return MAP_FAILED;
  }

  return p_map;
}

int
hivex__rpl_munmap (hive_h *h, void *p_addr, size_t len)
{
  if (p_addr == NULL || h->p_winmap == NULL)
    return -1;

  // Close map view
  if (UnmapViewOfFile (p_addr) == 0)
    return -1;

  // Close file mapping
  if (CloseHandle (h->p_winmap) == 0)
    return -1;

  h->p_winmap = NULL;
  return 0;
}
