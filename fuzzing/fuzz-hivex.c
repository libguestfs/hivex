/* hivex persistent mode fuzzer
 * Copyright (C) 2009-2026 Red Hat Inc.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "hivex.h"

/* To ensure checks are not optimized out it is recommended to disable
 * code optimization for the fuzzer harness main()
 */
#pragma clang optimize off
#pragma GCC optimize("O0")

static int node_start (hive_h *, void *opaque, hive_node_h, const char *name);
static int node_end (hive_h *, void *opaque, hive_node_h, const char *name);
static int value_string (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *str);
static int value_multiple_strings (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, char **argv);
static int value_string_invalid_utf16 (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *str);
static int value_dword (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, int32_t);
static int value_qword (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, int64_t);
static int value_binary (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
static int value_none (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
static int value_other (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);

static struct hivex_visitor visitor = {
  .node_start = node_start,
  .node_end = node_end,
  .value_string = value_string,
  .value_multiple_strings = value_multiple_strings,
  .value_string_invalid_utf16 = value_string_invalid_utf16,
  .value_dword = value_dword,
  .value_qword = value_qword,
  .value_binary = value_binary,
  .value_none = value_none,
  .value_other = value_other,
};

__AFL_FUZZ_INIT();

static int dummy;

int
main (int argc, char **argv)
{
  __AFL_INIT();

  unsigned char *buf = __AFL_FUZZ_TESTCASE_BUF;

  while (__AFL_LOOP(1000)) {
    int len = __AFL_FUZZ_TESTCASE_LEN;

    /* For the reason why we copy, see (5) on:
     * https://nullprogram.com/blog/2025/02/05/
     */
    void *copy = malloc (len);
    memcpy (copy, buf, len);

    /* Errors in the hive are expected, so ignore then. */
    hive_h *h = hivex_open_memory (copy, len, 0);
    if (h) {
      hivex_visit (h, &visitor, sizeof visitor, NULL,
                   HIVEX_VISIT_SKIP_BAD);
      hivex_close (h);
    }

    free (copy);
  }

  /* Just to be sure this is not optimized out. */
  printf ("%d\n", dummy);

  /* Once the loop is exited, terminate normally - AFL will restart
   * the process when this happens, with a clean slate when it comes
   * to allocated memory, leftover file descriptors, etc.
   */
  return 0;
}

/* These callbacks should do minimal checks that at least the strings
 * or memory regions passed to us are bounded.
 */

static void
check_valid (const void *p, size_t len)
{
  const char *b = p;
  for (size_t i = 0; i < len; ++i)
    dummy += b[i];
}

static int
node_start (hive_h *h, void *opaque, hive_node_h node, const char *name)
{
  dummy += strlen (name);
  return 0;
}

static int
node_end (hive_h *h, void *opaque, hive_node_h node, const char *name)
{
  dummy += strlen (name);
  return 0;
}

static int
value_string (hive_h *h, void *opaque,
              hive_node_h node, hive_value_h val,
              hive_type t, size_t len,
              const char *key, const char *str)
{
  dummy += strlen (key);
  dummy += strlen (str);
  return 0;
}

static int
value_multiple_strings (hive_h *h, void *opaque,
                        hive_node_h node, hive_value_h val,
                        hive_type t, size_t len,
                        const char *key, char **argv)
{
  dummy += strlen (key);
  for (size_t i = 0; argv[i]; ++i)
    dummy += strlen (argv[i]);
  return 0;
}

static int
value_string_invalid_utf16 (hive_h *h, void *opaque,
                            hive_node_h node, hive_value_h val,
                            hive_type t, size_t len,
                            const char *key, const char *str)
{
  dummy += strlen (key);
  check_valid (str, len);
  return 0;
}

static int
value_dword (hive_h *h, void *opaque,
             hive_node_h node, hive_value_h val,
             hive_type t, size_t len,
             const char *key, int32_t dw)
{
  dummy += strlen (key);
  return 0;
}

static int
value_qword (hive_h *h, void *opaque,
             hive_node_h node, hive_value_h val,
             hive_type t, size_t len,
             const char *key, int64_t qw)
{
  dummy += strlen (key);
  return 0;
}

static int
value_binary (hive_h *h, void *opaque,
              hive_node_h node, hive_value_h val,
              hive_type t, size_t len,
              const char *key, const char *value)
{
  dummy += strlen (key);
  check_valid (value, len);
  return 0;
}

static int
value_none (hive_h *h, void *opaque,
            hive_node_h node, hive_value_h val,
            hive_type t, size_t len,
            const char *key, const char *value)
{
  dummy += strlen (key);
  check_valid (value, len);
  return 0;
}

static int
value_other (hive_h *h, void *opaque,
             hive_node_h node, hive_value_h val,
             hive_type t, size_t len,
             const char *key, const char *value)
{
  dummy += strlen (key);
  check_valid (value, len);
  return 0;
}
