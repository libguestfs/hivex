#!/usr/bin/env python
import os
import hivex

srcdir = os.environ.get("srcdir")
if not srcdir:
    srcdir = "."

h = hivex.Hivex ("%s/../images/minimal" % srcdir,
                 write = True)
assert h

root = h.root ()
assert root

h.node_add_child (root, "ModerateValueParent")

mvp = h.node_get_child (root, "ModerateValueParent")
assert mvp

moderate_value = "0123456789ABCDEF"

values = [
    { "key": "3Bytes", "t": 3, "value": moderate_value[:3] },
    { "key": "16Bytes", "t": 3, "value": moderate_value },
    { "key": "30Bytes", "t": 3, "value": (moderate_value*2)[:30] },
    { "key": "31Bytes", "t": 3, "value": (moderate_value*2)[:31] },
    { "key": "32Bytes", "t": 3, "value": moderate_value*2 },
    { "key": "33Bytes", "t": 3, "value": (moderate_value*3)[:33] },
]
h.node_set_values (mvp, values)

new_moderate_value = h.node_get_value (mvp, "16Bytes")

assert h.value_value (new_moderate_value)[1] == moderate_value

h.commit ("%s/../images/rlenvalue_test_hive" % srcdir)
