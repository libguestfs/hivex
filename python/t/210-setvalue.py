# hivex Python bindings
# Copyright (C) 2010 Red Hat Inc.
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

import sys
import os
import hivex

if sys.version >= '3':
    import codecs
    def b(x):
        return codecs.encode(x)
else:
    def b(x):
        return x

srcdir = os.environ["srcdir"]
if not srcdir:
    srcdir = "."

h = hivex.Hivex ("%s/../images/minimal" % srcdir,
                 write = True)
assert h

root = h.root ()
assert root

h.node_add_child (root, "B")

B = h.node_get_child (root, "B")
assert B

values = [
    { "key": "Key1", "t": 3, "value": "ABC" },
    { "key": "Key2", "t": 3, "value": "DEF" }
]
h.node_set_values (B, values)

value1 = { "key": "Key3", "t": 3, "value": "GHI" }
h.node_set_value (B, value1)

value1 = { "key": "Key1", "t": 3, "value": "JKL" }
h.node_set_value (B, value1)

val = h.node_get_value (B, "Key1")
t_data = h.value_value (val)
assert t_data[0] == 3
assert t_data[1] == b("JKL")

val = h.node_get_value (B, "Key3")
t_data = h.value_value (val)
assert t_data[0] == 3
assert t_data[1] == b("GHI")
