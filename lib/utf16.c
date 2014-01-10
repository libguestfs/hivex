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
#include <iconv.h>
#include <string.h>

#include "hivex.h"
#include "hivex-internal.h"

char *
_hivex_recode (const char *input_encoding, const char *input, size_t input_len,
               const char *output_encoding, size_t *output_len)
{
  iconv_t ic = iconv_open (output_encoding, input_encoding);
  if (ic == (iconv_t) -1)
    return NULL;

  /* iconv(3) has an insane interface ... */

  size_t outalloc = input_len;

 again:;
  size_t inlen = input_len;
  size_t outlen = outalloc;
  char *out = malloc (outlen + 1);
  if (out == NULL) {
    int err = errno;
    iconv_close (ic);
    errno = err;
    return NULL;
  }
  const char *inp = input;
  char *outp = out;

  /* Surely iconv doesn't really modify the input buffer? XXX */
  size_t r = iconv (ic, (char **) &inp, &inlen, &outp, &outlen);
  if (r == (size_t) -1) {
    if (errno == E2BIG) {
      int err = errno;
      size_t prev = outalloc;
      /* Try again with a larger output buffer. */
      free (out);
      outalloc *= 2;
      if (outalloc < prev) {
        iconv_close (ic);
        errno = err;
        return NULL;
      }
      goto again;
    }
    else {
      /* Else some conversion failure, eg. EILSEQ, EINVAL. */
      int err = errno;
      iconv_close (ic);
      free (out);
      errno = err;
      return NULL;
    }
  }

  *outp = '\0';
  iconv_close (ic);
  if (output_len != NULL)
    *output_len = outp - out;

  return out;
}

/* Encode a given UTF-8 string to Latin1 (preferred) or UTF-16 for
 * storing in the hive file, as needed.
 */
char*
_hivex_encode_string(const char *str, size_t *size, int *utf16)
{
  char* outstr;
  *utf16 = 0;
  outstr = _hivex_recode ("UTF-8", str, strlen(str),
                          "LATIN1", size);
  if (outstr != NULL)
    return outstr;
  *utf16 = 1;
  outstr = _hivex_recode ("UTF-8", str, strlen(str),
                          "UTF-16LE", size);
  return outstr;
}

/* Get the length of a UTF-16 format string.  Handle the string as
 * pairs of bytes, looking for the first \0\0 pair.  Only read up to
 * 'len' maximum bytes.
 */
size_t
_hivex_utf16_string_len_in_bytes_max (const char *str, size_t len)
{
  size_t ret = 0;

  while (len >= 2 && (str[0] || str[1])) {
    str += 2;
    ret += 2;
    len -= 2;
  }

  return ret;
}

size_t
_hivex_utf8_strlen (const char* str, size_t len, int utf16)
{
  const char *encoding = utf16 ? "UTF-16LE" : "LATIN1";
  size_t ret;
  char *buf = _hivex_recode(encoding, str, len, "UTF-8", &ret);
  free(buf);
  return ret;
}
