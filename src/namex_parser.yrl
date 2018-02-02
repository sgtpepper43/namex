Nonterminals names name word words honorific display_order sort_order von last
  first u_words u_word suffices titles nick first_name_only.
Terminals LWORD UWORD PWORD AND APPELLATION TITLE NICK COMMA SUFFIX.

Rootsymbol names. 

names -> name : ['$1'].
names -> names AND name : lists:flatten(['$1', '$3']).
names -> honorific AND honorific display_order :
  ['$4'#name{appellation = '$1'#name.appellation, title = '$1'#name.title},
  '$3'#name{family = '$4'#name.family}].

names -> first_name_only AND name : ['$1'#name{family = '$3'#name.family}, '$3'].


name -> first_name_only : '$1'.
name -> display_order : '$1'.
name -> honorific word : '$1'#name{family = '$2'}.
name -> honorific display_order :
  '$2'#name{appellation = '$1'#name.appellation, title = '$1'#name.title}.
name -> sort_order : '$1'.

first_name_only -> word : #name{given = '$1'}.
first_name_only -> word suffices : #name{given = '$1', suffix = '$2'}.

honorific -> APPELLATION : #name{appellation = extract_token('$1')}.
honorific -> TITLE : #name{title = extract_token('$1')}.


display_order -> u_words word : #name{given = '$1', family = '$2'}.
display_order -> u_words word suffices :
  #name{given = '$1', family = '$2', suffix = '$3'}.
display_order -> u_words word titles :
  #name{given = '$1', family = '$2', title = '$3'}.
display_order -> u_words word suffices titles : 
  #name{given = '$1', family = '$2', suffix = '$3', title = '$4'}.

display_order -> u_words nick last :
  #name{given = '$1', nick = '$2', family = '$3'}.
display_order -> u_words nick last suffices :
  #name{given = '$1', nick = '$2', family = '$3', suffix = '$4'}.
display_order -> u_words nick last titles :
  #name{given = '$1', nick = '$2', family = '$3', title = '$4'}.
display_order -> u_words nick last suffices titles :
  #name{given = '$1', nick = '$2', family = '$3', suffix = '$4', title = '$5'}.

display_order -> u_words nick von last :
  #name{given = '$1', nick = '$2', particle = '$3', family = '$4'}.
display_order -> u_words nick von last suffices :
  #name{given = '$1', nick = '$2', particle = '$3', family = '$4',
    suffix = '$5'}.
display_order -> u_words nick von last titles :
  #name{given = '$1', nick = '$2', particle = '$3', family = '$4',
    title = '$5'}.
display_order -> u_words nick von last suffices titles :
  #name{given = '$1', nick = '$2', particle = '$3', family = '$4',
    suffix = '$5', title = '$5'}.

display_order -> u_words von last :
  #name{given = '$1', particle = '$2', family = '$3'}.
display_order -> von last : #name{particle = '$1', family = '$2'}.


sort_order -> last COMMA first :
  {Suffix, Given} = '$3',
  new_name(#name{family = '$1', suffix = Suffix, given = Given}, Suffix).
sort_order -> von last COMMA first :
  {Suffix, Given} = '$4',
  new_name(
    #name{particle = '$1', family = '$2', suffix = Suffix, given = Given},
    Suffix
  ).
sort_order -> u_words von last COMMA first :
  {Suffix, Given} = '$5',
  new_name(
    #name{particle = join(['$1', '$2']), family = '$3', suffix = Suffix,
      given = Given},
    Suffix
  ).


von -> LWORD : extract_token('$1').
von -> von LWORD : join(['$1', '$2']).
von -> von u_words LWORD : join(['$1', '$2', '$3']).


last -> LWORD : extract_token('$1').
last -> u_words : '$1'.

first -> words : {nil, '$1'}.
first -> words suffices : {'$2', '$1'}.
first -> words COMMA suffices : {'$3', '$1'}.
first -> suffices : {'$1', nil}.
first -> suffices COMMA words : {'$1', '$3'}.

u_words -> u_word : '$1'.
u_words -> u_words u_word : join(['$1', '$2']).

u_word -> UWORD : extract_token('$1').
u_word -> PWORD : extract_token('$1').

words -> word : '$1'.
words -> words word : join(['$1', '$2']).

word -> LWORD : extract_token('$1').
word -> UWORD : extract_token('$1').
word -> PWORD : extract_token('$1').

nick -> NICK : extract_token('$1').

suffices -> SUFFIX : extract_token('$1').
suffices -> suffices SUFFIX : join(['$1', '$2']).

titles -> TITLE : extract_token('$1').
titles -> titles TITLE : join(['$1', '$2']).


Erlang code.

-record(name, {family = nil, given = nil, suffix = nil, particle = nil,
  nick = nil, appellation = nil, title = nil}).

extract_token({_Token, _Line, Value}) -> Value.

new_name(Record, Sanitize) ->
  case not is_nil(Sanitize) and not is_nil(Record#name.suffix) and is_nil(Record#name.given) and not is_nil(Record#name.family) of
    true ->
      case re:split(Record#name.family, "\s+") of
        [Family | Given] -> Record#name{family = Family, given = join(Given)};
        _ -> Record
      end;
    false -> Record
  end.

is_nil(nil) -> true;
is_nil(_) -> false.

join(Words) -> binary_join(Words, <<" ">>).

binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).
