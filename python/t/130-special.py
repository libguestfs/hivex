# coding: utf-8
# http://stackoverflow.com/questions/6625782/unicode-literals-that-work-in-python-3-and-2
import sys
if sys.version < '3':
    import codecs
    def u(x):
        return codecs.unicode_escape_decode(x)[0]
else:
    def u(x):
        return x

import os
import hivex

srcdir = os.environ["srcdir"]
if not srcdir:
    srcdir = "."

h = hivex.Hivex ("%s/../images/special" % srcdir)
assert h

root = h.root ()
assert root

# "abcd_äöüß"
ns = [ n for n in h.node_children (root) if h.node_name(n) == u("abcd_\u00e4\u00f6\u00fc\u00df") ]
assert len (ns) == 1
# "abcd_äöüß"
vs = [ v for v in h.node_values (ns[0]) if h.value_key(v) == u("abcd_\u00e4\u00f6\u00fc\u00df") ]
assert len (vs) == 1
ns = [ n for n in h.node_children (root) if h.node_name(n) == u("zero\0key") ]
assert len (ns) == 1
vs = [ v for v in h.node_values (ns[0]) if h.value_key(v) == u("zero\0val") ]
assert len (vs) == 1
# "weird™"
ns = [ n for n in h.node_children (root) if h.node_name(n) == u("weird\u2122") ]
assert len (ns) == 1
# "symbols $£₤₧€"
vs = [ v for v in h.node_values (ns[0]) if h.value_key(v) == u("symbols \u0024\u00a3\u20a4\u20a7\u20ac") ]
assert len (vs) == 1

