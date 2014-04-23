# -*- coding: utf-8 -*-
# -*- ruby -*-

require File::join(File::dirname(__FILE__), 'test_helper')

class TestRLenValue < Test::Unit::TestCase
  def test_RLenValue
    h = Hivex::open(File::join(ENV['abs_srcdir'], '..', 'images', 'special'), {})
    assert_not_nil(h)

    root = h.root()
    assert_not_nil(root)

    nodes = h.node_children (root)
    node = nodes.find { |n| h.node_name(n) == "abcd_äöüß" }
    assert node != nil
    value = h.node_values(node).find { |v| h.value_key(v) == "abcd_äöüß" }
    assert value != nil
    node = nodes.find { |n| h.node_name(n) == "zero\0key" }
    assert node != nil
    value = h.node_values(node).find { |v| h.value_key(v) == "zero\0val" }
    assert value != nil
    node = nodes.find { |n| h.node_name(n) == "weird™" }
    assert node != nil
    value = h.node_values(node).find { |v| h.value_key(v) == "symbols $£₤₧€" }
    assert value != nil

  end
end
