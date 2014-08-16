# Test various possible types for assignment to setvalue
# Copyright (C) 2014 Peter Wu <peter@lekensteyn.nl>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

import hivex
from hivex.hive_types import *
import os

srcdir = "."
if "srcdir" in os.environ and os.environ["srcdir"]:
    srcdir = os.environ["srcdir"]

h = hivex.Hivex ("%s/../images/minimal" % srcdir,
                 write = True)

def set_value (key = "test key", t = REG_BINARY, value = b"Val"):
    global h
    h.node_set_value (h.root (), {
        "key": key,
        "t": t,
        "value": value
    })

def test_pass (key = "test key", t = REG_BINARY, value = b"Val"):
    global h
    set_value (key, t, value)
    val = h.node_get_value (h.root (), key)
    ret_type, ret_value = h.value_value (val)
    assert t == ret_type, \
        "expected type {0}, got {1}".format(t, ret_type)
    assert value == ret_value, \
        "expected value {0}, got {1}".format(value, ret_value)

def test_exception (exception_type, **kwargs):
    try:
        set_value (**kwargs)
        raise AssertionError("expected {0}".format(exception_type))
    except exception_type:
        pass


# Good weather tests
# Accept either bytes or unicode for ASCII string
# TODO: fix node_get_value to handle UTF-16 string in bytes
#test_pass (t = REG_BINARY,  key = b"\x01\x02")
test_pass (t = REG_SZ,      key = u"ASCII key")
# Try a byte with upper bit set
test_pass (t = REG_DWORD,   value = b"\xaa\xbb\xcc")


# Bad weather tests
# Invalid 'key' type
test_exception (TypeError, key = 1)
test_exception (TypeError, key = 1)
# TODO: should non-ASCII strings be rejected?
#test_exception (ValueError, key = u"Euro: \u20ac")

# Invalid 't' type
test_exception (TypeError, t = b"meh")

# Invalid 'value' types
test_exception (TypeError, t = REG_BINARY,      value = 1)
test_exception (TypeError, t = REG_DWORD,       value = 1)
test_exception (TypeError, t = REG_SZ,          value = 1)
test_exception (TypeError, t = REG_DWORD,       value = None)
# Unicode strings should be rejected, bytes only!
test_exception (TypeError, t = REG_SZ,          value = u"some text")
