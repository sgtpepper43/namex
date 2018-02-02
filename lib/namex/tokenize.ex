defmodule Namex.Tokenize do
  @moduledoc """
  States
  """

  alias Namex.Tokenize, as: State

  defstruct commas: 0, words: 0, initials: 0, suffices: 0, stack: [], string: ''

  @comma ~r/^\s*,\s*/
  @stops ",;"
  @seperator ~r/^\s*(\band\b|\&|;)\s*/i
  @title ~r/^\s*\b(sir|lord|count(ess)?|(gen|adm|col|maj|capt|cmdr|lt|sgt|cpl|pvt|pastor|pr|reverend|rev|elder|deacon|deaconess|father|fr|rabbi|cantor|vicar|esq|esquire|prof|dr|md|ph\.?d)\.?)(\s+|$)/i
  @suffix ~r/^\s*\b(JR|Jr|jr|SR|Sr|sr|[IVX]{2,})(\.|\b)/
  @appellation ~r/^\s*\b((mrs?|ms|fr|hr)\.?|miss|herr|frau)(\s+|$)/i
  @uword ~r/^((\\\w+)?\{[^\}]*\})*[[:upper:]][^\s#{@stops}]*/
  @lword ~r/^((\\\w+)?\{[^\}]*\})*[[:lower:]][^\s#{@stops}]*/
  @pword ~r/^(\\\w+)?\{[^\}]*\}[^\s#{@stops}]*/
  @nick ~r/^('[^'\n]+')|("[^"\n]+")/

  def call(string) do
    next_token(%State{string: string}).stack
    |> Enum.reverse()
    |> Enum.map(fn {token, value} -> {token, 1, value} end)
  end

  defp next_token(%State{string: ""} = state), do: state
  defp next_token(%State{string: nil} = state), do: state

  defp next_token(state) do
    state
    |> scan(@seperator, &consume_seperator/2)
    |> scan(@comma, &handle_comma/2)
    |> scan(~r/^\s+/, fn _, new_state -> next_token(new_state) end)
    |> scan(@title, &consume_word(:TITLE, String.trim(&1), &2))
    |> scan(@suffix, &consume_word(:SUFFIX, String.trim(&1), &2))
    |> scan(@appellation, &handle_appellation/2)
    |> scan(@uword, &consume_word(:UWORD, String.trim(&1), &2))
    |> scan(@lword, &consume_word(:LWORD, String.trim(&1), &2))
    |> scan(@pword, &consume_word(:PWORD, String.trim(&1), &2))
    |> scan(@nick, &consume_word(:NICK, &1 |> String.trim("\"") |> String.trim("'"), &2))
  end

  defp scan(%State{string: ""} = state, _, _), do: state
  defp scan(%State{string: nil} = state, _, _), do: state

  defp scan(state, regex, callback) do
    case Regex.run(regex, state.string, return: :index) do
      nil ->
        state

      [{_, offset} | _] ->
        {match, string} = String.split_at(state.string, offset)
        callback.(match, %State{state | string: string})
    end
  end

  defp handle_comma(_match, state) do
    if state.commas == 0 || (state.commas == 1 && suffix?(state)) do
      %State{state | commas: state.commas + 1, stack: [{:COMMA, :COMMA} | state.stack]}
      |> next_token
    else
      consume_seperator(nil, state)
    end
  end

  defp handle_appellation(match, %State{words: 0} = state) do
    %State{state | stack: [{:APPELLATION, String.trim(match)} | state.stack]}
    |> next_token
  end

  defp handle_appellation(match, state), do: consume_word(:UWORD, match, state)

  defp consume_seperator(_match, state) do
    if seen_seperator?(state) do
      next_token(state)
    else
      %State{
        state
        | commas: 0,
          words: 0,
          initials: 0,
          suffices: 0,
          stack: [{:AND, :AND} | state.stack]
      }
      |> next_token
    end
  end

  defp consume_word(type, word, state) do
    state = %State{state | words: state.words + 1, stack: [{type, word} | state.stack]}

    state =
      case type do
        :UWORD ->
          if Regex.match?(~r/^[[:upper:]]+\b/, word) do
            %State{state | initials: state.initials + 1}
          else
            state
          end

        :SUFFIX ->
          %State{state | suffices: state.suffices + 1}

        _ ->
          state
      end

    next_token(state)
  end

  defp seen_seperator?(%State{stack: []}), do: false
  defp seen_seperator?(%State{stack: [{:and, :and} | _]}), do: true
  defp seen_seperator?(_), do: false

  defp suffix?(%State{suffices: 0} = state), do: will_see_suffix?(state)
  defp suffix?(_), do: true

  defp will_see_suffix?(%State{string: string}) do
    suffix =
      string
      |> String.slice(0..8)
      |> String.trim()
      |> String.split(~r/\s+/)
      |> List.first()

    Regex.match?(@suffix, suffix)
  end
end
