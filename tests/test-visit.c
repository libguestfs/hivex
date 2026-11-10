/* hivex
 * Copyright (C) 2010-2026 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/* Simple test of hivex_visit API. */

#include <stdio.h>
#include <stdlib.h>

#include "hivex.h"

static struct hivex_visitor visitor = {
/*
  .node_start = node_start,
  .node_end = node_end,
  .value_string = value_string,
  .value_multiple_strings = value_multiple_strings,
  .value_string_invalid_utf16 = value_string_invalid_utf16,
  .value_dword = value_dword,
  .value_qword = value_qword,
  .value_binary = value_binary,
  .value_none = value_none,
  .value_other = value_other
*/
};

int
main (int argc, char *argv[])
{
  hive_h *h = hivex_open ("../images/large", 0);
  if (!h) {
    perror ("hivex_open");
    exit (EXIT_FAILURE);
  }

  if (hivex_visit (h, &visitor, sizeof visitor, NULL, 0) == -1) {
    perror ("hivex_visit");
    exit (EXIT_FAILURE);
  }

  hivex_close (h);
  exit (EXIT_SUCCESS);
}
