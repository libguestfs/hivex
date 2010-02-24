/* mklarge - Make a large hive for testing purposes.
 * Copyright (C) 2010 Red Hat Inc.
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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <hivex.h>

static int degrees[] = { 3, 1, 4, 1, 5, 9, 2 }; /* ~1000 nodes */
static const int nr_degrees = sizeof degrees / sizeof degrees[0];
static const char *strings[][10] = {
  { "The", "A", "Another" },  /* level 0 */
  { "giant" },
  { "mongoose", "goat", "zebra", "elephant" },
  { "was" },
  { "found in", "seen in", "spotted over", "sent to", "fired at" },
  { "Paris", "London", "Rome", "Oslo", "Madrid", "Nicosia", "Amsterdam",
    "Moscow", "Riga" },
  { "today", "yesterday" } /* level 6 */
};
static hive_set_value values[] = {
  /* char* casts are needed to work around a stupidity of C */
  { (char *) "A", hive_t_REG_SZ, 4, (char *) "a\0\0\0" },
  { (char *) "B", hive_t_REG_DWORD, 4, (char *) "\x78\x56\x34\x12" },
  { (char *) "C", hive_t_REG_EXPAND_SZ, 6, (char *) "c\0c\0\0\0" },
  { (char *) "D", hive_t_REG_SZ, 8, (char *) "d\0d\0d\0\0\0" },
  { (char *) "E", hive_t_REG_QWORD, 8, (char *) "\xf0\xde\xbc\x9a\x78\x56\x34\x12" },
  { (char *) "F", hive_t_REG_SZ, 4, (char *) "f\0\0\0" },
  { (char *) "G", hive_t_REG_EXPAND_SZ, 4, (char *) "g\0\0\0" }
};

static void
iter (hive_h *h, int depth, int posn, hive_node_h parent, char *name)
{
  if (depth < nr_degrees) {
    int degree = degrees[depth];
    int i, len;
    hive_node_h node;

    len = strlen (name);
    if (len > 0) name[len++] = ' ';

    for (i = 0; i < degree; ++i) {
      strcpy (&name[len], strings[depth][i]);
      node = hivex_node_add_child (h, parent, name);
      if (node == 0) {
        perror ("mklarge: hivex_node_add_child");
        exit (1);
      }
      iter (h, depth+1, i, node, name);
    }

    if (hivex_node_set_values (h, parent, depth, values, 0) == -1) {
      perror ("mklarge: hivex_node_set_values");
      exit (1);
    }
  }
}

int
main (int argc, char *argv[])
{
  hive_h *h;
  char name[4096] = { '\0' };

  h = hivex_open (argv[1], HIVEX_OPEN_WRITE);
  if (h == NULL) {
    perror (argv[1]);
    exit (1);
  }

  iter (h, 0, 0, hivex_root (h), name);

  if (hivex_commit (h, argv[2], 0) == -1) {
    perror (argv[2]);
    exit (1);
  }

  if (hivex_close (h) == -1) {
    perror ("mklarge: close");
    exit (1);
  }

  exit (0);
}
