defmodule NamexTest do
  use ExUnit.Case
  doctest Namex

  test "greets the world" do
    assert Namex.hello() == :world
  end
end
