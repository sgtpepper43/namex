-module(namex_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/namex_parser.yrl", 118).

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

-file("/home/trevor/.asdf/installs/erlang/20.1/lib/erlang/lib/parsetools-2.1.5/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/namex_parser.erl", 210).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'APPELLATION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_cont_0(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_first_name_only(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_2/7}).
yeccpars2_cont_2(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_3(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'NICK', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_last(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_u_words(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_name(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
yeccpars2_6(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_6(S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_names(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_8(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_name(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_name(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_honorific(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_13_\'$end\''(Stack),
 yeccgoto_word(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_13(_S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_13_\'AND\''(Stack),
 yeccgoto_word(hd(Ss), 'AND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_13(_S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_13_\'SUFFIX\''(Stack),
 yeccgoto_word(hd(Ss), 'SUFFIX', Ss, NewStack, T, Ts, Tzr);
yeccpars2_13(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_13_\'COMMA\''(Stack),
 yeccgoto_last(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_von(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_14_\'$end\''(Stack),
 yeccgoto_word(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_14(_S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_14_\'AND\''(Stack),
 yeccgoto_word(hd(Ss), 'AND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_14(_S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_14_\'SUFFIX\''(Stack),
 yeccgoto_word(hd(Ss), 'SUFFIX', Ss, NewStack, T, Ts, Tzr);
yeccpars2_14(_S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_14_\'TITLE\''(Stack),
 yeccgoto_word(hd(Ss), 'TITLE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_u_word(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_honorific(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_16_\'$end\''(Stack),
 yeccgoto_word(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_16(_S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_16_\'AND\''(Stack),
 yeccgoto_word(hd(Ss), 'AND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_16(_S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_16_\'SUFFIX\''(Stack),
 yeccgoto_word(hd(Ss), 'SUFFIX', Ss, NewStack, T, Ts, Tzr);
yeccpars2_16(_S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_16_\'TITLE\''(Stack),
 yeccgoto_word(hd(Ss), 'TITLE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_u_word(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_17: see yeccpars2_0

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_names(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_19(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_name(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_name(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_22: see yeccpars2_2

yeccpars2_23(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'NICK', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_name(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_25_\'$end\''(Stack),
 yeccgoto_word(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_25_\'AND\''(Stack),
 yeccgoto_word(hd(Ss), 'AND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_25_\'SUFFIX\''(Stack),
 yeccgoto_word(hd(Ss), 'SUFFIX', Ss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_25_\'TITLE\''(Stack),
 yeccgoto_word(hd(Ss), 'TITLE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_von(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_27: see yeccpars2_2

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_u_words(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_29(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_nick(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_31: see yeccpars2_2

yeccpars2_32(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_last(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_34_\'$end\''(Stack),
 yeccgoto_last(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_34_\'AND\''(Stack),
 yeccgoto_last(hd(Ss), 'AND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_34_\'SUFFIX\''(Stack),
 yeccgoto_last(hd(Ss), 'SUFFIX', Ss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_34_\'TITLE\''(Stack),
 yeccgoto_last(hd(Ss), 'TITLE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_von(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_u_word(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_u_word(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_suffices(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_titles(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_41(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_suffices(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_titles(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_last(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_46_\'$end\''(Stack),
 yeccgoto_last(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_46(_S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_46_\'AND\''(Stack),
 yeccgoto_last(hd(Ss), 'AND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_46(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_46_\'COMMA\''(Stack),
 yeccgoto_last(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_46(_S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_46_\'SUFFIX\''(Stack),
 yeccgoto_last(hd(Ss), 'SUFFIX', Ss, NewStack, T, Ts, Tzr);
yeccpars2_46(_S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_46_\'TITLE\''(Stack),
 yeccgoto_last(hd(Ss), 'TITLE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_von(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_49(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_von(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_52(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_54(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_56/7}).
yeccpars2_56(S, 'APPELLATION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'TITLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_57(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_names(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_von(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_60(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_first(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_words(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_first(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_sort_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_word(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_word(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_word(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_68/7}).
yeccpars2_68(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_69(S, 'LWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'PWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'UWORD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_first(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_words(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_first(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_72/7}).
yeccpars2_72(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_73(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_first(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_74: see yeccpars2_0

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_names(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_76: see yeccpars2_2

yeccpars2_77(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_78: see yeccpars2_60

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_sort_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_80(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_display_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_81: see yeccpars2_60

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_sort_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, 'SUFFIX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_first_name_only(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_display_order/7}).
yeccgoto_display_order(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_display_order(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_display_order(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_display_order(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_display_order(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_display_order(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_first/7}).
yeccgoto_first(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_first(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_first(81=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_first_name_only/7}).
yeccgoto_first_name_only(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_first_name_only(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_first_name_only(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_honorific/7}).
yeccgoto_honorific(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_honorific(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_honorific(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_honorific(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_last/7}).
yeccgoto_last(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_last(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_name/7}).
yeccgoto_name(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_names/7}).
yeccgoto_names(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_nick/7}).
yeccgoto_nick(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nick(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_sort_order/7}).
yeccgoto_sort_order(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sort_order(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sort_order(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_suffices/7}).
yeccgoto_suffices(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_suffices(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_titles/7}).
yeccgoto_titles(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_titles(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_titles(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_titles(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_titles(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_titles(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_u_word/7}).
yeccgoto_u_word(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_word(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_u_words/7}).
yeccgoto_u_words(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_u_words(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_von/7}).
yeccgoto_von(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(22, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(22, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(27, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(22, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_von(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_word/7}).
yeccgoto_word(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(26, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(26, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(68=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_word(81=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_words/7}).
yeccgoto_words(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_words(68, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_words(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_words(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-file("src/namex_parser.yrl", 19).
yeccpars2_1_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 }
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("src/namex_parser.yrl", 3).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("src/namex_parser.yrl", 22).
yeccpars2_12_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # name { appellation = extract_token ( __1 ) }
  end | __Stack].

-compile({inline,'yeccpars2_13_\'$end\''/1}).
-file("src/namex_parser.yrl", 101).
'yeccpars2_13_\'$end\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_13_\'AND\''/1}).
-file("src/namex_parser.yrl", 101).
'yeccpars2_13_\'AND\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_13_\'SUFFIX\''/1}).
-file("src/namex_parser.yrl", 101).
'yeccpars2_13_\'SUFFIX\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_13_\'COMMA\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_13_\'COMMA\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("src/namex_parser.yrl", 78).
yeccpars2_13_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_14_\'$end\''/1}).
-file("src/namex_parser.yrl", 103).
'yeccpars2_14_\'$end\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_14_\'AND\''/1}).
-file("src/namex_parser.yrl", 103).
'yeccpars2_14_\'AND\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_14_\'SUFFIX\''/1}).
-file("src/namex_parser.yrl", 103).
'yeccpars2_14_\'SUFFIX\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_14_\'TITLE\''/1}).
-file("src/namex_parser.yrl", 103).
'yeccpars2_14_\'TITLE\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_14_/1}).
-file("src/namex_parser.yrl", 96).
yeccpars2_14_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("src/namex_parser.yrl", 23).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # name { title = extract_token ( __1 ) }
  end | __Stack].

-compile({inline,'yeccpars2_16_\'$end\''/1}).
-file("src/namex_parser.yrl", 102).
'yeccpars2_16_\'$end\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_16_\'AND\''/1}).
-file("src/namex_parser.yrl", 102).
'yeccpars2_16_\'AND\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_16_\'SUFFIX\''/1}).
-file("src/namex_parser.yrl", 102).
'yeccpars2_16_\'SUFFIX\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_16_\'TITLE\''/1}).
-file("src/namex_parser.yrl", 102).
'yeccpars2_16_\'TITLE\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/namex_parser.yrl", 95).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("src/namex_parser.yrl", 9).
yeccpars2_18_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 # name { family = __3 # name .family } , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("src/namex_parser.yrl", 14).
yeccpars2_21_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 # name { family = __2 }
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("src/namex_parser.yrl", 16).
yeccpars2_24_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2 # name { appellation = __1 # name .appellation , title = __1 # name .title }
  end | __Stack].

-compile({inline,'yeccpars2_25_\'$end\''/1}).
-file("src/namex_parser.yrl", 101).
'yeccpars2_25_\'$end\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_25_\'AND\''/1}).
-file("src/namex_parser.yrl", 101).
'yeccpars2_25_\'AND\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_25_\'SUFFIX\''/1}).
-file("src/namex_parser.yrl", 101).
'yeccpars2_25_\'SUFFIX\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_25_\'TITLE\''/1}).
-file("src/namex_parser.yrl", 101).
'yeccpars2_25_\'TITLE\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("src/namex_parser.yrl", 78).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("src/namex_parser.yrl", 26).
yeccpars2_26_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , family = __2 }
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("src/namex_parser.yrl", 93).
yeccpars2_28_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   join ( [ __1 , __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("src/namex_parser.yrl", 105).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("src/namex_parser.yrl", 35).
yeccpars2_33_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , family = __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\'$end\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_34_\'$end\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_34_\'AND\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_34_\'AND\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_34_\'SUFFIX\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_34_\'SUFFIX\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_34_\'TITLE\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_34_\'TITLE\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("src/namex_parser.yrl", 78).
yeccpars2_34_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("src/namex_parser.yrl", 96).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("src/namex_parser.yrl", 95).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("src/namex_parser.yrl", 39).
yeccpars2_37_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , family = __3 , title = __4 }
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("src/namex_parser.yrl", 37).
yeccpars2_38_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , family = __3 , suffix = __4 }
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("src/namex_parser.yrl", 107).
yeccpars2_39_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("src/namex_parser.yrl", 110).
yeccpars2_40_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("src/namex_parser.yrl", 41).
yeccpars2_41_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , family = __3 , suffix = __4 , title = __5 }
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("src/namex_parser.yrl", 108).
yeccpars2_42_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   join ( [ __1 , __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("src/namex_parser.yrl", 111).
yeccpars2_43_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   join ( [ __1 , __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("src/namex_parser.yrl", 44).
yeccpars2_45_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , particle = __3 , family = __4 }
  end | __Stack].

-compile({inline,'yeccpars2_46_\'$end\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_46_\'$end\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_46_\'AND\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_46_\'AND\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_46_\'COMMA\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_46_\'COMMA\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_46_\'SUFFIX\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_46_\'SUFFIX\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,'yeccpars2_46_\'TITLE\''/1}).
-file("src/namex_parser.yrl", 83).
'yeccpars2_46_\'TITLE\''(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("src/namex_parser.yrl", 79).
yeccpars2_46_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   join ( [ __1 , __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("src/namex_parser.yrl", 49).
yeccpars2_47_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , particle = __3 , family = __4 ,
    title = __5 }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/namex_parser.yrl", 46).
yeccpars2_48_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , particle = __3 , family = __4 ,
    suffix = __5 }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("src/namex_parser.yrl", 52).
yeccpars2_49_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , nick = __2 , particle = __3 , family = __4 ,
    suffix = __5 , title = __5 }
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("src/namex_parser.yrl", 80).
yeccpars2_50_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   join ( [ __1 , __2 , __3 ] )
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("src/namex_parser.yrl", 56).
yeccpars2_51_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , particle = __2 , family = __3 }
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("src/namex_parser.yrl", 30).
yeccpars2_52_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , family = __2 , title = __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("src/namex_parser.yrl", 28).
yeccpars2_53_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , family = __2 , suffix = __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_/1}).
-file("src/namex_parser.yrl", 32).
yeccpars2_54_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , family = __2 , suffix = __3 , title = __4 }
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-file("src/namex_parser.yrl", 57).
yeccpars2_55_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # name { particle = __1 , family = __2 }
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("src/namex_parser.yrl", 6).
yeccpars2_58_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __4 # name { appellation = __1 # name .appellation , title = __1 # name .title } ,
    __3 # name { family = __4 # name .family } ]
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-file("src/namex_parser.yrl", 78).
yeccpars2_59_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("src/namex_parser.yrl", 86).
yeccpars2_61_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { nil , __1 }
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("src/namex_parser.yrl", 89).
yeccpars2_63_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { __1 , nil }
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-file("src/namex_parser.yrl", 61).
yeccpars2_64_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Suffix , Given } = __3 ,
    new_name ( # name { family = __1 , suffix = Suffix , given = Given } , Suffix )
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-file("src/namex_parser.yrl", 101).
yeccpars2_65_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-file("src/namex_parser.yrl", 103).
yeccpars2_66_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("src/namex_parser.yrl", 102).
yeccpars2_67_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   extract_token ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("src/namex_parser.yrl", 90).
yeccpars2_69_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("src/namex_parser.yrl", 99).
yeccpars2_70_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   join ( [ __1 , __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("src/namex_parser.yrl", 87).
yeccpars2_71_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("src/namex_parser.yrl", 88).
yeccpars2_73_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __3 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-file("src/namex_parser.yrl", 4).
yeccpars2_75_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : flatten ( [ __1 , __3 ] )
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-file("src/namex_parser.yrl", 56).
yeccpars2_77_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , particle = __2 , family = __3 }
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-file("src/namex_parser.yrl", 70).
yeccpars2_79_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Suffix , Given } = __5 ,
    new_name (
    # name { particle = join ( [ __1 , __2 ] ) , family = __3 , suffix = Suffix ,
    given = Given } ,
    Suffix
    )
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-file("src/namex_parser.yrl", 57).
yeccpars2_80_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # name { particle = __1 , family = __2 }
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("src/namex_parser.yrl", 64).
yeccpars2_82_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Suffix , Given } = __4 ,
    new_name (
    # name { particle = __1 , family = __2 , suffix = Suffix , given = Given } ,
    Suffix
    )
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("src/namex_parser.yrl", 20).
yeccpars2_83_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # name { given = __1 , suffix = __2 }
  end | __Stack].


-file("src/namex_parser.yrl", 151).
