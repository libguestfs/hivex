# hivex Ruby bindings -*- ruby -*-
# Copyright (C) 2009-2011 Red Hat Inc.
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

# Demonstrate value_data_cell_offset by looking at the value data at
# "\$$$PROTO.HIV\ModerateValueParent\33Bytes", verified to be at file
# offset 8680 (0x21e8) of the hive rlenvalue_test_hive.  The returned
# length and offset for this value cell should be 37 bytes, position
# 8712.

require 'test/unit'
require 'hivex'

class TestRLenValue < Test::Unit::TestCase
  def test_RLenValue
    h = Hivex::open(File::join(ENV['abs_srcdir'], '..', 'images', 'rlenvalue_test_hive'), {})
    assert_not_nil(h)

    root = h.root()
    assert_not_nil(root)

    moderate_value_node = h.node_get_child(root, "ModerateValueParent")
    assert_not_nil(moderate_value_node)

    moderate_value_value = h.node_get_value(moderate_value_node, "33Bytes")

    r = h.value_data_cell_offset(moderate_value_value)
    assert_equal(r[:len], 37)
    assert_equal(r[:off], 8712)
  end
end
