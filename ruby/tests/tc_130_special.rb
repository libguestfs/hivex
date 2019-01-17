# -*- coding: utf-8 -*-
# -*- ruby -*-

require File::join(File::dirname(__FILE__), 'test_helper')

class TestRLenValue < MiniTest::Unit::TestCase
  def test_RLenValue
    h = Hivex::open(File::join(ENV['abs_srcdir'], '..', 'images', 'special'), {})
    refute_nil(h)

    root = h.root()
    refute_nil(root)

    nodes = h.node_children (root)
    node = nodes.find { |n| h.node_name(n) == "abcd_äöüß" }
    refute_nil(node)
    value = h.node_values(node).find { |v| h.value_key(v) == "abcd_äöüß" }
    refute_nil(value)
    node = nodes.find { |n| h.node_name(n) == "zero\0key" }
    refute_nil(node)
    value = h.node_values(node).find { |v| h.value_key(v) == "zero\0val" }
    refute_nil(value)
    node = nodes.find { |n| h.node_name(n) == "weird™" }
    refute_nil(node)
    value = h.node_values(node).find { |v| h.value_key(v) == "symbols $£₤₧€" }
    refute_nil(value)

  end
end
