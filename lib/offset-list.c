/* hivex - Windows Registry "hive" extraction library.
 * Copyright (C) 2013 Red Hat Inc.
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

/* Structure for returning 0-terminated lists of offsets (nodes,
 * values, etc).
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <assert.h>

#include "hivex.h"
#include "hivex-internal.h"

void
_hivex_init_offset_list (hive_h *h, offset_list *list)
{
  list->h = h;
  list->len = 0;
  list->alloc = 0;
  list->offsets = NULL;
  list->limit = SIZE_MAX;
}

/* Preallocates the offset_list, but doesn't make the contents longer. */
int
_hivex_grow_offset_list (offset_list *list, size_t alloc)
{
  assert (alloc >= list->len);
  size_t *p = realloc (list->offsets, alloc * sizeof (size_t));
  if (p == NULL)
    return -1;
  list->offsets = p;
  list->alloc = alloc;
  return 0;
}

static int
add_to_offset_list (offset_list *list, size_t offset)
{
  if (list->len >= list->alloc) {
    if (_hivex_grow_offset_list (list, list->alloc ? list->alloc * 2 : 4) == -1)
      return -1;
  }
  list->offsets[list->len] = offset;
  list->len++;
  return 0;
}

int
_hivex_add_to_offset_list (offset_list *list, size_t offset)
{
  assert (offset != 0);         /* internal error if this happens */

  if (list->len >= list->limit) {
    hive_h *h = list->h;        /* for SET_ERRNO macro */
    SET_ERRNO (ERANGE,
               "list of offsets has exceeded limit (limit = %zu)",
               list->limit);
    return -1;
  }

  return add_to_offset_list (list, offset);
}

size_t
_hivex_get_offset_list_length (offset_list *list)
{
  return list->len;
}

void
_hivex_set_offset_list_limit (offset_list *list, size_t limit)
{
  list->limit = limit;
}

void
_hivex_free_offset_list (offset_list *list)
{
  free (list->offsets);
}

size_t *
_hivex_return_offset_list (offset_list *list)
{
  if (add_to_offset_list (list, 0) == -1)
    return NULL;
  return list->offsets;         /* caller frees */
}

void
_hivex_print_offset_list (offset_list *list, FILE *fp)
{
  size_t i;

  fprintf (fp, "[");
  for (i = 0; i < list->len; ++i) {
    if (i > 0)
      fputc (',', fp);
    fprintf (fp, "%zu", list->offsets[i]);
  }
  fprintf (fp, "]");
}
