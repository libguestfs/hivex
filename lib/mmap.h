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

#ifndef HIVEX_MMAP_H_
#define HIVEX_MMAP_H_

#include <stdlib.h>
#include <sys/types.h>

#include "hivex.h"
#include "hivex-internal.h"

/* Hack to pass the hive handle to the replacement mmap and munmap
 * functions. XXX
 */
#define mmap(a,b,c,d,e,f) hivex__rpl_mmap(h,(a),(b),(c),(d),(e),(f))
#define munmap(a,b) hivex__rpl_munmap(h,(a),(b))

// Supported map protections.
#define PROT_READ   0x1  /* Page can be read.  */

// Supported sharing types (must choose one and only one of these).
#define MAP_SHARED  0x01 /* Share changes.  */

// Value that is returned when mapping failed
#define MAP_FAILED  NULL

/*
 * hivex replacement mmap
 *
 * Parameters:
 *   h                : Hive handle
 *   void *p_addr     : Preferred starting address for the mapping. Unsupported
 *                      and must be NULL.
 *   size_t len       : Mapping length (From offset to offset+len-1).
 *   int prot         : Flags that control what kind of access is permitted.
 *                      Must be PROT_READ.
 *   int flags        : Flags that control the nature of the map. Must be
 *                      MAP_SHARED.
 *   int fd           : File descriptor of file to be mapped.
 *   off_t offset     : Mapping offset.
 *
 * Returns:
 *   Map address on success or MAP_FAILED on error.
 */
extern void *hivex__rpl_mmap (hive_h *h, void *p_addr, size_t len, int prot, int flags, int fd, off_t offset);

/*
 * hivex replacement munmap
 *
 * Parameters:
 *   h               : Hive handle
 *   void *p_addr    : Startaddress of mapping created with mmap
 *   size_t len      : Lenght of mapping to be unmapped. Unsupported. The whole
 *                     mapping will always be unmapped.
 *
 * Returns:
 *   0 on success or -1 on error.
 */
extern int hivex__rpl_munmap (hive_h *h, void *p_addr, size_t len);

#endif /* HIVEX_MMAP_H_ */
