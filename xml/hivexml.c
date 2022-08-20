/* hivexml - Convert Windows Registry "hive" to XML file.
 * Copyright (C) 2009 Red Hat Inc.
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
#include <stdint.h>
#include <inttypes.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <locale.h>

#ifdef HAVE_LIBINTL_H
#include <libintl.h>
#endif

#include <getopt.h>

#include <libxml/xmlwriter.h>

#include "hivex.h"

#ifdef HAVE_GETTEXT
#include "gettext.h"
#define _(str) dgettext(PACKAGE, (str))
//#define N_(str) dgettext(PACKAGE, (str))
#else
#define _(str) str
//#define N_(str) str
#endif

static char *filetime_to_8601 (int64_t windows_ticks);

/* Callback functions. */
static int node_start (hive_h *, void *, hive_node_h, const char *name);
static int node_end (hive_h *, void *, hive_node_h, const char *name);
static int value_string (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *str);
static int value_multiple_strings (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, char **argv);
static int value_string_invalid_utf16 (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *str);
static int value_dword (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, int32_t);
static int value_qword (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, int64_t);
static int value_binary (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
static int value_none (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
static int value_other (hive_h *, void *, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);

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
  .value_other = value_other
};

#define XML_CHECK(proc, args)                                           \
  do {                                                                  \
    if ((proc args) == -1) {                                            \
      fprintf (stderr, _("%s: failed to write XML document\n"), #proc); \
      exit (EXIT_FAILURE);                                              \
    }                                                                   \
  } while (0)

int
main (int argc, char *argv[])
{
  setlocale (LC_ALL, "");
#ifdef HAVE_BINDTEXTDOMAIN
  bindtextdomain (PACKAGE, LOCALEBASEDIR);
  textdomain (PACKAGE);
#endif

  int c;
  int open_flags = 0;
  int visit_flags = 0;

  while ((c = getopt (argc, argv, "dku")) != EOF) {
    switch (c) {
    case 'd':
      open_flags |= HIVEX_OPEN_DEBUG;
      break;
    case 'k':
      visit_flags |= HIVEX_VISIT_SKIP_BAD;
      break;
    case 'u':
      open_flags |= HIVEX_OPEN_UNSAFE;
      break;
    default:
      fprintf (stderr, "hivexml [-dk] regfile > output.xml\n");
      exit (EXIT_FAILURE);
    }
  }

  if (optind + 1 != argc) {
    fprintf (stderr, _("hivexml: missing name of input file\n"));
    exit (EXIT_FAILURE);
  }

  hive_h *h = hivex_open (argv[optind], open_flags);
  if (h == NULL) {
    fprintf (stderr, "hivex_open: %s: %m\n", argv[optind]);
    exit (EXIT_FAILURE);
  }

  /* Note both this macro, and xmlTextWriterStartDocument leak memory.  There
   * doesn't seem to be any way to recover that memory, but it's not a
   * large amount.
   */
  LIBXML_TEST_VERSION;

  xmlTextWriterPtr writer;
  writer = xmlNewTextWriterFilename ("/dev/stdout", 0);
  if (writer == NULL) {
    fprintf (stderr, _("xmlNewTextWriterFilename: failed to create XML writer\n"));
    exit (EXIT_FAILURE);
  }

  XML_CHECK (xmlTextWriterStartDocument, (writer, NULL, "utf-8", NULL));
  XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "hive"));

  int64_t hive_mtime = hivex_last_modified (h);
  if (hive_mtime >= 0) {
    char *timebuf = filetime_to_8601 (hive_mtime);
    if (timebuf) {
      XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "mtime"));
      XML_CHECK (xmlTextWriterWriteString, (writer, BAD_CAST timebuf));
      XML_CHECK (xmlTextWriterEndElement, (writer));
      free (timebuf);
    }
  }

  if (hivex_visit (h, &visitor, sizeof visitor, writer, visit_flags) == -1) {
    fprintf (stderr, "hivex_visit: %s: %m\n", argv[optind]);
    exit (EXIT_FAILURE);
  }

  if (hivex_close (h) == -1) {
    fprintf (stderr, "hivex_close: %s: %m\n", argv[optind]);
    exit (EXIT_FAILURE);
  }

  XML_CHECK (xmlTextWriterEndElement, (writer));
  XML_CHECK (xmlTextWriterEndDocument, (writer));
  xmlFreeTextWriter (writer);

  exit (EXIT_SUCCESS);
}

/* Convert Windows filetime to ISO 8601 format.
 * http://stackoverflow.com/questions/6161776/convert-windows-filetime-to-second-in-unix-linux/6161842#6161842
 *
 * Source for time_t->char* conversion: Fiwalk version 0.6.14's
 * fiwalk.cpp.
 *
 * The caller should free the returned buffer.
 *
 * This function returns NULL on a 0 input.  In the context of
 * hives, which only have mtimes, 0 will always be a complete
 * absence of data.
 */

#define WINDOWS_TICK 10000000LL
#define SEC_TO_UNIX_EPOCH 11644473600LL
#define TIMESTAMP_BUF_LEN 32

static char *
filetime_to_8601 (int64_t windows_ticks)
{
  char *ret;
  time_t t;
  struct tm *tm;

  if (windows_ticks == 0LL)
    return NULL;

  t = windows_ticks / WINDOWS_TICK - SEC_TO_UNIX_EPOCH;
  tm = gmtime (&t);
  if (tm == NULL)
    return NULL;

  ret = malloc (TIMESTAMP_BUF_LEN);
  if (ret == NULL) {
    perror ("malloc");
    exit (EXIT_FAILURE);
  }

  if (strftime (ret, TIMESTAMP_BUF_LEN, "%Y-%m-%dT%H:%M:%SZ", tm) == 0) {
    perror ("strftime");
    exit (EXIT_FAILURE);
  }

  return ret;
}

#define BYTE_RUN_BUF_LEN 32

static int
node_byte_runs (hive_h *h, void *writer_v, hive_node_h node)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  char buf[1+BYTE_RUN_BUF_LEN];
  size_t node_struct_length = hivex_node_struct_length (h, node);
  if (node_struct_length == 0) {
    fprintf (stderr, "node_byte_runs: hivex_node_struct_length: %m\n");
    return -1;
  }
  /* A node has one byte run. */
  XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "byte_runs"));
  XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "byte_run"));
  memset (buf, 0, 1+BYTE_RUN_BUF_LEN);
  snprintf (buf, 1+BYTE_RUN_BUF_LEN, "%zu", node);
  XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "file_offset", BAD_CAST buf));
  snprintf (buf, 1+BYTE_RUN_BUF_LEN, "%zu", node_struct_length);
  XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "len", BAD_CAST buf));
  XML_CHECK (xmlTextWriterEndElement, (writer));
  XML_CHECK (xmlTextWriterEndElement, (writer));
  return 0;
}

static int
node_start (hive_h *h, void *writer_v, hive_node_h node, const char *name)
{
  int64_t last_modified;
  char *timebuf;
  int ret = 0;

  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "node"));
  XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "name", BAD_CAST name));

  if (node == hivex_root (h)) {
    XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "root", BAD_CAST "1"));
  }

  last_modified = hivex_node_timestamp (h, node);
  if (last_modified >= 0) {
    timebuf = filetime_to_8601 (last_modified);
    if (timebuf) {
      XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "mtime"));
      XML_CHECK (xmlTextWriterWriteString, (writer, BAD_CAST timebuf));
      XML_CHECK (xmlTextWriterEndElement, (writer));
      free (timebuf);
    }
  }

  ret = node_byte_runs (h, writer_v, node);
  return ret;
}

static int
node_end (hive_h *h, void *writer_v, hive_node_h node, const char *name)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  XML_CHECK (xmlTextWriterEndElement, (writer));
  return 0;
}

static void
start_value (xmlTextWriterPtr writer,
             const char *key, const char *type, const char *encoding)
{
  XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "value"));
  XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "type", BAD_CAST type));
  if (encoding)
    XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "encoding", BAD_CAST encoding));
  if (*key)
    XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "key", BAD_CAST key));
  else                          /* default key */
    XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "default", BAD_CAST "1"));
}

static void
end_value (xmlTextWriterPtr writer)
{
  XML_CHECK (xmlTextWriterEndElement, (writer));
}

static int
value_byte_runs (hive_h *h, void *writer_v, hive_value_h value) {
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  char buf[1+BYTE_RUN_BUF_LEN];
  size_t value_data_cell_length;
  size_t value_data_structure_length = hivex_value_struct_length (h, value);
  if (value_data_structure_length == 0) {
    fprintf (stderr, "value_byte_runs: hivex_value_struct_length: %m\n");
    return -1;
  }
  errno = 0;
  hive_value_h value_data_cell_offset =
    hivex_value_data_cell_offset (h, value, &value_data_cell_length);
  if (value_data_cell_offset == 0 && errno != 0) {
    fprintf (stderr, "value_byte_runs: hivex_value_data_cell_offset: %m\n");
    return -1;
  }

  XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "byte_runs"));
  memset (buf, 0, 1+BYTE_RUN_BUF_LEN);

  /* Write first byte run for data structure */
  XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "byte_run"));
  snprintf (buf, 1+BYTE_RUN_BUF_LEN, "%zu", value);
  XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "file_offset", BAD_CAST buf));
  snprintf (buf, 1+BYTE_RUN_BUF_LEN, "%zu", value_data_structure_length);
  XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "len", BAD_CAST buf));
  XML_CHECK (xmlTextWriterEndElement, (writer));

  /* Write second byte run for longer values */
  if (value_data_cell_length > 4) {
    XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "byte_run"));
    snprintf (buf, 1+BYTE_RUN_BUF_LEN, "%zu", value_data_cell_offset);
    XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "file_offset", BAD_CAST buf));
    snprintf (buf, 1+BYTE_RUN_BUF_LEN, "%zu", value_data_cell_length);
    XML_CHECK (xmlTextWriterWriteAttribute, (writer, BAD_CAST "len", BAD_CAST buf));
    XML_CHECK (xmlTextWriterEndElement, (writer));
  }
  XML_CHECK (xmlTextWriterEndElement, (writer));
  return 0;
}

static int
value_string (hive_h *h, void *writer_v, hive_node_h node, hive_value_h value,
              hive_type t, size_t len, const char *key, const char *str)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  const char *type;
  int ret = 0;

  switch (t) {
  case hive_t_string: type = "string"; break;
  case hive_t_expand_string: type = "expand"; break;
  case hive_t_link: type = "link"; break;

  case hive_t_none:
  case hive_t_binary:
  case hive_t_dword:
  case hive_t_dword_be:
  case hive_t_multiple_strings:
  case hive_t_resource_list:
  case hive_t_full_resource_description:
  case hive_t_resource_requirements_list:
  case hive_t_qword:
    abort ();                   /* internal error - should not happen */

  default:
    type = "unknown";
  }

  start_value (writer, key, type, NULL);
  XML_CHECK (xmlTextWriterStartAttribute, (writer, BAD_CAST "value"));
  XML_CHECK (xmlTextWriterWriteString, (writer, BAD_CAST str));
  XML_CHECK (xmlTextWriterEndAttribute, (writer));
  ret = value_byte_runs (h, writer_v, value);
  end_value (writer);
  return ret;
}

static int
value_multiple_strings (hive_h *h, void *writer_v, hive_node_h node,
                        hive_value_h value, hive_type t, size_t len,
                        const char *key, char **argv)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  int ret = 0;
  start_value (writer, key, "string-list", NULL);

  size_t i;
  for (i = 0; argv[i] != NULL; ++i) {
    XML_CHECK (xmlTextWriterStartElement, (writer, BAD_CAST "string"));
    XML_CHECK (xmlTextWriterWriteString, (writer, BAD_CAST argv[i]));
    XML_CHECK (xmlTextWriterEndElement, (writer));
  }

  ret = value_byte_runs (h, writer_v, value);
  end_value (writer);
  return ret;
}

static int
value_string_invalid_utf16 (hive_h *h, void *writer_v, hive_node_h node,
                            hive_value_h value, hive_type t, size_t len,
                            const char *key,
                            const char *str /* original data */)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  const char *type;
  int ret = 0;

  switch (t) {
  case hive_t_string: type = "bad-string"; break;
  case hive_t_expand_string: type = "bad-expand"; break;
  case hive_t_link: type = "bad-link"; break;
  case hive_t_multiple_strings: type = "bad-string-list"; break;

  case hive_t_none:
  case hive_t_binary:
  case hive_t_dword:
  case hive_t_dword_be:
  case hive_t_resource_list:
  case hive_t_full_resource_description:
  case hive_t_resource_requirements_list:
  case hive_t_qword:
    abort ();                   /* internal error - should not happen */

  default:
    type = "unknown";
  }

  start_value (writer, key, type, "base64");
  XML_CHECK (xmlTextWriterStartAttribute, (writer, BAD_CAST "value"));
  XML_CHECK (xmlTextWriterWriteBase64, (writer, str, 0, len));
  XML_CHECK (xmlTextWriterEndAttribute, (writer));
  ret = value_byte_runs (h, writer_v, value);
  end_value (writer);

  return ret;
}

static int
value_dword (hive_h *h, void *writer_v, hive_node_h node, hive_value_h value,
             hive_type t, size_t len, const char *key, int32_t v)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  int ret = 0;
  start_value (writer, key, "int32", NULL);
  XML_CHECK (xmlTextWriterWriteFormatAttribute, (writer, BAD_CAST "value", "%" PRIi32, v));
  ret = value_byte_runs (h, writer_v, value);
  end_value (writer);
  return ret;
}

static int
value_qword (hive_h *h, void *writer_v, hive_node_h node, hive_value_h value,
             hive_type t, size_t len, const char *key, int64_t v)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  int ret = 0;
  start_value (writer, key, "int64", NULL);
  XML_CHECK (xmlTextWriterWriteFormatAttribute, (writer, BAD_CAST "value", "%" PRIi64, v));
  ret = value_byte_runs (h, writer_v, value);
  end_value (writer);
  return ret;
}

static int
value_binary (hive_h *h, void *writer_v, hive_node_h node, hive_value_h value,
              hive_type t, size_t len, const char *key, const char *v)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  int ret = 0;
  start_value (writer, key, "binary", "base64");
  XML_CHECK (xmlTextWriterStartAttribute, (writer, BAD_CAST "value"));
  XML_CHECK (xmlTextWriterWriteBase64, (writer, v, 0, len));
  XML_CHECK (xmlTextWriterEndAttribute, (writer));
  ret = value_byte_runs (h, writer_v, value);
  end_value (writer);
  return ret;
}

static int
value_none (hive_h *h, void *writer_v, hive_node_h node, hive_value_h value,
            hive_type t, size_t len, const char *key, const char *v)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  int ret = 0;
  start_value (writer, key, "none", "base64");
  if (len > 0) {
    XML_CHECK (xmlTextWriterStartAttribute, (writer, BAD_CAST "value"));
    XML_CHECK (xmlTextWriterWriteBase64, (writer, v, 0, len));
    XML_CHECK (xmlTextWriterEndAttribute, (writer));
    ret = value_byte_runs (h, writer_v, value);
  }
  end_value (writer);
  return ret;
}

static int
value_other (hive_h *h, void *writer_v, hive_node_h node, hive_value_h value,
             hive_type t, size_t len, const char *key, const char *v)
{
  xmlTextWriterPtr writer = (xmlTextWriterPtr) writer_v;
  const char *type;
  int ret = 0;

  switch (t) {
  case hive_t_none:
  case hive_t_binary:
  case hive_t_dword:
  case hive_t_dword_be:
  case hive_t_qword:
  case hive_t_string:
  case hive_t_expand_string:
  case hive_t_link:
  case hive_t_multiple_strings:
    abort ();                   /* internal error - should not happen */

  case hive_t_resource_list: type = "resource-list"; break;
  case hive_t_full_resource_description: type = "resource-description"; break;
  case hive_t_resource_requirements_list: type = "resource-requirements"; break;

  default:
    type = "unknown";
  }

  start_value (writer, key, type, "base64");
  if (len > 0) {
    XML_CHECK (xmlTextWriterStartAttribute, (writer, BAD_CAST "value"));
    XML_CHECK (xmlTextWriterWriteBase64, (writer, v, 0, len));
    XML_CHECK (xmlTextWriterEndAttribute, (writer));
    ret = value_byte_runs (h, writer_v, value);
  }
  end_value (writer);

  return ret;
}
