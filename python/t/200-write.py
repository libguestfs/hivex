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

import os
import hivex

srcdir = os.environ["srcdir"]
if not srcdir:
    srcdir = "."

h = hivex.Hivex ("%s/../images/minimal" % srcdir,
                 write = True)
assert h

root = h.root ()
assert root

h.node_add_child (root, "A")

h.node_add_child (root, "B")

b = h.node_get_child (root, "B")
assert b

values = [
    { "key": "Key1", "t": 3, "value": b"ABC" },
    { "key": "Key2", "t": 3, "value": b"DEF" }
]
h.node_set_values (b, values)
