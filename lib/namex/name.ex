defmodule Namex.Name do
  defstruct family: nil,
            given: nil,
            suffix: nil,
            particle: nil,
            nick: nil,
            appellation: nil,
            title: nil

  @default_initial_opts [expand: false, dots: true, spaces: false]

  def sort_order(name, delimiter \\ ", ") do
    compact_and_join([family_part(name), name.given], delimiter)
  end

  def display_order(name) do
    compact_and_join([name.given, family_part(name), name.suffix])
  end

  def initials(name, opts \\ []) do
    opts = Keyword.merge(@default_initial_opts, opts)

    if Keyword.get(opts, :expand) do
      compact_and_join([initials_of(name.given, opts), name.family])
    else
      [name.given, family_part(name)]
      |> compact_and_join()
      |> initials_of(opts)
    end
  end

  defp initials_of(name, opts) do
    dots? = Keyword.get(opts, :dots)
    spaces? = Keyword.get(opts, :spaces)
    replacement = if dots?, do: "\\1.", else: "\\1"

    i = Regex.replace(~r/([[:upper:]])[[:lower:]]+/, name, replacement)
    if !spaces?, do: Regex.replace(~r/\s+/, i, ""), else: i
  end

  defp family_part(%__MODULE__{particle: particle, family: family}) do
    compact_and_join([particle, family])
  end

  defp compact_and_join(list, delimiter \\ " ") do
    list
    |> Enum.reject(&is_nil/1)
    |> Enum.join(delimiter)
  end
end
