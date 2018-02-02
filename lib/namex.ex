defmodule Namex do
  @moduledoc """
  Documentation for Namex.
  """

  require Record
  alias Namex.Name

  @doc """
  Hello world.

  ## Examples

      iex> Namex.hello
      :world
  """

  Record.defrecord(
    :name,
    family: nil,
    given: nil,
    suffix: nil,
    particle: nil,
    nick: nil,
    appellation: nil,
    title: nil
  )

  def parse(string) do
    with tokens <- Namex.Tokenize.call(string),
         {:ok, records} <- :namex_parser.parse(tokens),
         do:
           Enum.map(records, fn record ->
             %Name{
               family: name(record, :family),
               given: name(record, :given),
               suffix: name(record, :suffix),
               particle: name(record, :particle),
               nick: name(record, :nick),
               appellation: name(record, :appellation),
               title: name(record, :title)
             }
           end)
  end
end
