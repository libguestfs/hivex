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
hivex_visit (hive_h *h, const struct hivex_visitor *visitor, size_t len,
             void *opaque, int flags)
{
  return hivex_visit_node (h, hivex_root (h), visitor, len, opaque, flags);
}

static int hivex__visit_node (hive_h *h, hive_node_h node,
                              const struct hivex_visitor *vtor,
                              char *unvisited, void *opaque, int flags);

int
hivex_visit_node (hive_h *h, hive_node_h node,
                  const struct hivex_visitor *visitor, size_t len, void *opaque,
                  int flags)
{
  struct hivex_visitor vtor;
  memset (&vtor, 0, sizeof vtor);

  /* Note that len might be larger *or smaller* than the expected size. */
  size_t copysize = len <= sizeof vtor ? len : sizeof vtor;
  memcpy (&vtor, visitor, copysize);

  /* This bitmap records unvisited nodes, so we don't loop if the
   * registry contains cycles.
   */
  char *unvisited = malloc (1 + h->size / 32);
  if (unvisited == NULL)
    return -1;
  memcpy (unvisited, h->bitmap, 1 + h->size / 32);

  int r = hivex__visit_node (h, node, &vtor, unvisited, opaque, flags);
  free (unvisited);
  return r;
}

static int
hivex__visit_node (hive_h *h, hive_node_h node,
                   const struct hivex_visitor *vtor, char *unvisited,
                   void *opaque, int flags)
{
  int skip_bad = flags & HIVEX_VISIT_SKIP_BAD;
  char *name = NULL;
  hive_value_h *values = NULL;
  hive_node_h *children = NULL;
  char *key = NULL;
  char *str = NULL;
  char **strs = NULL;
  int i;

  /* Return -1 on all callback errors.  However on internal errors,
   * check if skip_bad is set and suppress those errors if so.
   */
  int ret = -1;

  if (!BITMAP_TST (unvisited, node)) {
    SET_ERRNO (ELOOP, "contains cycle: visited node 0x%zx already", node);
    return skip_bad ? 0 : -1;
  }
  BITMAP_CLR (unvisited, node);

  name = hivex_node_name (h, node);
  if (!name) return skip_bad ? 0 : -1;
  if (vtor->node_start && vtor->node_start (h, opaque, node, name) == -1)
    goto error;

  values = hivex_node_values (h, node);
  if (!values) {
    ret = skip_bad ? 0 : -1;
    goto error;
  }

  for (i = 0; values[i] != 0; ++i) {
    hive_type t;
    size_t len;

    if (hivex_value_type (h, values[i], &t, &len) == -1) {
      ret = skip_bad ? 0 : -1;
      goto error;
    }

    key = hivex_value_key (h, values[i]);
    if (key == NULL) {
      ret = skip_bad ? 0 : -1;
      goto error;
    }

    if (vtor->value_any) {
      str = hivex_value_value (h, values[i], &t, &len);
      if (str == NULL) {
        ret = skip_bad ? 0 : -1;
        goto error;
      }
      if (vtor->value_any (h, opaque, node, values[i], t, len, key, str) == -1)
        goto error;
      free (str); str = NULL;
    }
    else {
      switch (t) {
      case hive_t_none:
        str = hivex_value_value (h, values[i], &t, &len);
        if (str == NULL) {
          ret = skip_bad ? 0 : -1;
          goto error;
        }
        if (t != hive_t_none) {
          ret = skip_bad ? 0 : -1;
          goto error;
        }
        if (vtor->value_none &&
            vtor->value_none (h, opaque, node, values[i], t, len, key, str) == -1)
          goto error;
        free (str); str = NULL;
        break;

      case hive_t_string:
      case hive_t_expand_string:
      case hive_t_link:
        str = hivex_value_string (h, values[i]);
        if (str == NULL) {
          if (errno != EILSEQ && errno != EINVAL) {
            ret = skip_bad ? 0 : -1;
            goto error;
          }
          if (vtor->value_string_invalid_utf16) {
            str = hivex_value_value (h, values[i], &t, &len);
            if (vtor->value_string_invalid_utf16 (h, opaque, node, values[i],
                                                  t, len, key, str) == -1)
              goto error;
            free (str); str = NULL;
          }
          break;
        }
        if (vtor->value_string &&
            vtor->value_string (h, opaque, node, values[i],
                                t, len, key, str) == -1)
          goto error;
        free (str); str = NULL;
        break;

      case hive_t_dword:
      case hive_t_dword_be: {
        int32_t i32 = hivex_value_dword (h, values[i]);
        if (vtor->value_dword &&
            vtor->value_dword (h, opaque, node, values[i],
                               t, len, key, i32) == -1)
          goto error;
        break;
      }

      case hive_t_qword: {
        int64_t i64 = hivex_value_qword (h, values[i]);
        if (vtor->value_qword &&
            vtor->value_qword (h, opaque, node, values[i],
                               t, len, key, i64) == -1)
          goto error;
        break;
      }

      case hive_t_binary:
        str = hivex_value_value (h, values[i], &t, &len);
        if (str == NULL) {
          ret = skip_bad ? 0 : -1;
          goto error;
        }
        if (t != hive_t_binary) {
          ret = skip_bad ? 0 : -1;
          goto error;
        }
        if (vtor->value_binary &&
            vtor->value_binary (h, opaque, node, values[i],
                                t, len, key, str) == -1)
          goto error;
        free (str); str = NULL;
        break;

      case hive_t_multiple_strings:
        strs = hivex_value_multiple_strings (h, values[i]);
        if (strs == NULL) {
          if (errno != EILSEQ && errno != EINVAL) {
            ret = skip_bad ? 0 : -1;
            goto error;
          }
          if (vtor->value_string_invalid_utf16) {
            str = hivex_value_value (h, values[i], &t, &len);
            if (vtor->value_string_invalid_utf16 (h, opaque, node, values[i],
                                                  t, len, key, str) == -1)
              goto error;
            free (str); str = NULL;
          }
          break;
        }
        if (vtor->value_multiple_strings &&
            vtor->value_multiple_strings (h, opaque, node, values[i],
                                          t, len, key, strs) == -1)
          goto error;
        _hivex_free_strings (strs); strs = NULL;
        break;

      case hive_t_resource_list:
      case hive_t_full_resource_description:
      case hive_t_resource_requirements_list:
      default:
        str = hivex_value_value (h, values[i], &t, &len);
        if (str == NULL) {
          ret = skip_bad ? 0 : -1;
          goto error;
        }
        if (vtor->value_other &&
            vtor->value_other (h, opaque, node, values[i],
                               t, len, key, str) == -1)
          goto error;
        free (str); str = NULL;
        break;
      }
    }

    free (key); key = NULL;
  }

  children = hivex_node_children (h, node);
  if (children == NULL) {
    ret = skip_bad ? 0 : -1;
    goto error;
  }

  for (i = 0; children[i] != 0; ++i) {
    DEBUG (2, "%s: visiting subkey %d (0x%zx)", name, i, children[i]);

    if (hivex__visit_node (h, children[i], vtor, unvisited, opaque, flags) == -1)
      goto error;
  }

  if (vtor->node_end && vtor->node_end (h, opaque, node, name) == -1)
    goto error;

  ret = 0;

 error:
  free (name);
  free (values);
  free (children);
  free (key);
  free (str);
  _hivex_free_strings (strs);
  return ret;
}
