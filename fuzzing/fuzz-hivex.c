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
#include <fcntl.h>
#include <unistd.h>

#include "hivex.h"

/* To ensure checks are not optimized out it is recommended to disable
 * code optimization for the fuzzer harness main()
 */
#pragma clang optimize off
#pragma GCC optimize("O0")

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

__AFL_FUZZ_INIT();

int
main (int argc, char **argv)
{
  __AFL_INIT();

  unsigned char *buf = __AFL_FUZZ_TESTCASE_BUF;

  while (__AFL_LOOP(1000)) {
    int len = __AFL_FUZZ_TESTCASE_LEN;
    hive_h *h;

    /* Errors in the hive are expected, so ignore then. */
    h = hivex_open_memory (buf, len, 0);
    if (h) {
      hivex_visit (h, &visitor, sizeof visitor, NULL, 0);
      hivex_close (h);
    }
  }

  /* Once the loop is exited, terminate normally - AFL will restart
   * the process when this happens, with a clean slate when it comes
   * to allocated memory, leftover file descriptors, etc.
   */
  return 0;
}

