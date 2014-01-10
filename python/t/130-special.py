# coding: utf-8

import os
import hivex

srcdir = os.environ["srcdir"]
if not srcdir:
    srcdir = "."

h = hivex.Hivex ("%s/../images/special" % srcdir)
assert h

root = h.root ()
assert root

ns = [ n for n in h.node_children (root) if h.node_name(n) == u"abcd_äöüß" ]
assert len (ns) == 1
vs = [ v for v in h.node_values (ns[0]) if h.value_key(v) == u"abcd_äöüß" ]
assert len (vs) == 1
ns = [ n for n in h.node_children (root) if h.node_name(n) == u"zero\0key" ]
assert len (ns) == 1
vs = [ v for v in h.node_values (ns[0]) if h.value_key(v) == u"zero\0val" ]
assert len (vs) == 1
ns = [ n for n in h.node_children (root) if h.node_name(n) == u"weird™" ]
assert len (ns) == 1
vs = [ v for v in h.node_values (ns[0]) if h.value_key(v) == u"symbols $£₤₧€" ]
assert len (vs) == 1

