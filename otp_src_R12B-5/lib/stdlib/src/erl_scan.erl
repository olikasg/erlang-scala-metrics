%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%% Erlang token scanning functions of io library.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037	NUL - US	control
%% 040 - 057	SPC - /		punctuation
%% 060 - 071	0 - 9		digit
%% 072 - 100	: - @		punctuation
%% 101 - 132	A - Z		uppercase
%% 133 - 140	[ - `		punctuation
%% 141 - 172	a - z		lowercase
%% 173 - 176	{ - ~		punctuation
%% 177		DEL		control
%% 200 - 237			control
%% 240 - 277	NBSP - �	punctuation
%% 300 - 326	� - �		uppercase
%% 327		�		punctuation
%% 330 - 336	� - �		uppercase
%% 337 - 366	� - �		lowercase
%% 367		�		punctuation
%% 370 - 377	� - �		lowercase
%%
%% Many punctuation characters region have special meaning.  Must
%% watch using � \327, bvery close to x \170

-module(erl_scan).

-export([string/1,string/2,tokens/3,format_error/1,reserved_word/1]).

-import(lists, [reverse/1]).

%% format_error(Error)
%%  Return a string describing the error.

format_error({string,Quote,Head}) ->
    ["unterminated " ++ string_thing(Quote) ++
     " starting with " ++ io_lib:write_string(Head,Quote)];
format_error({illegal,Type}) -> io_lib:fwrite("illegal ~w", [Type]);
format_error(char) -> "unterminated character";
format_error(scan) -> "premature end";
format_error({base,Base}) -> io_lib:fwrite("illegal base '~w'", [Base]);
format_error(float) -> "bad float";
%%
format_error(Other) -> io_lib:write(Other).

string_thing($') -> "atom";   %' Stupid Emacs
string_thing(_) -> "string".


%% string(CharList, StartPos)
%%  Takes a list of characters and tries to tokenise them.
%%
%%  Returns:
%%	{ok,[Tok]}
%%	{error,{ErrorPos,?MODULE,What},EndPos}

string(Cs) ->
    string(Cs, 1).

string(Cs, Pos) when is_list(Cs), is_integer(Pos) ->
%     %% Debug replacement line for chopping string into 1-char segments
%     scan([], [], [], Pos, Cs, []).
    scan(Cs, [], [], Pos, [], []).

%% tokens(Continuation, CharList, StartPos) ->
%%	{done, {ok, [Tok], EndPos}, Rest} |
%%	{done, {error,{ErrorPos,?MODULE,What}, EndPos}, Rest} |
%%	{more, Continuation'}
%%  This is the main function into the re-entrant scanner. 
%%
%%  The continuation has the form:
%%      {RestChars,ScanStateStack,ScannedTokens,
%%       CurrentPos,ContState,ErrorStack,ContFunArity5}

tokens([], Chars, Pos) ->
    tokens({[],[],[],Pos,io,[],fun scan/6}, Chars, Pos);
tokens({Cs,_Stack,_Toks,Pos,eof,_Fun}, eof, _) ->
    {done,{eof,Pos},Cs};
tokens({Cs,Stack,Toks,Pos,_State,Errors,Fun}, eof, _) ->
    Fun(Cs++eof, Stack, Toks, Pos, eof, Errors);
tokens({Cs,Stack,Toks,Pos,State,Errors,Fun}, Chars, _) ->
    Fun(Cs++Chars, Stack, Toks, Pos, State, Errors).


%% Scan loop.
%%
%% The scan_*/6 and sub_scan_*/6 functions does tail recursive calls 
%% between themselves to change state. State data is kept on the Stack. 
%% Results are passed on the Stack and on the stream (Cs). The variable 
%% State in this loop is not the scan loop state, but the state for 
%% instream handling by more/7 and done/5. The variable Stack is not
%% always a stack, it is just stacked state data for the scan loop, and
%% the variable Errors is a reversed list of scan error {Error,Pos} tuples.
%%
%% All the scan_*/6 functions have the same arguments (in the same order), 
%% to keep the tail recursive calls (jumps) fast.
%%
%% When more data is needed from the stream, the tail recursion loop is
%% broken by calling more/7 that either returns to the I/O-server to 
%% get more data or fetches it from a string, or by calling done/5 when
%% scanning is done.
%%
%% The last argument to more/7 is a fun to jump back to with more data 
%% to continue scanning where it was interrupted.
%%
%% more/7 and done/5 handles scanning from I/O-server (Stream) or from String.
%%

%% String
more(Cs, Stack, Toks, Pos, eos, Errors, Fun) ->
    erlang:error(badstate, [Cs,Stack,Toks,Pos,eos,Errors,Fun]);
% %% Debug clause for chopping string into 1-char segments
% more(Cs, Stack, Toks, Pos, [H|T], Errors, Fun) ->
%     Fun(Cs++[H], Stack, Toks, Pos, T, Errors);
more(Cs, Stack, Toks, Pos, [], Errors, Fun) ->
    Fun(Cs++eof, Stack, Toks, Pos, eos, Errors);
%% Stream
more(Cs, Stack, Toks, Pos, eof, Errors, Fun) ->
    erlang:error(badstate, [Cs,Stack,Toks,Pos,eof,Errors,Fun]);
more(Cs, Stack, Toks, Pos, io, Errors,Fun) ->
    {more,{Cs,Stack,Toks,Pos,io,Errors,Fun}}.

%% String
done(eof, [], Toks, Pos, eos) ->
    {ok,reverse(Toks),Pos};
done(eof, Errors, _Toks, Pos, eos) ->
    {Error,ErrorPos} = lists:last(Errors),
    {error,{ErrorPos,?MODULE,Error},Pos};
done(Cs, Errors, Toks, Pos, eos) ->
    scan(Cs, [], Toks, Pos, eos, Errors);
% %% Debug clause for chopping string into 1-char segments
% done(Cs, Errors, Toks, Pos, [H|T]) ->
%    scan(Cs++[H], [], Toks, Pos, T, Errors);
done(Cs, Errors, Toks, Pos, []) ->
    scan(Cs++eof, [], Toks, Pos, eos, Errors);
%% Stream
done(Cs, [], [{dot,_}|_]=Toks, Pos, io) ->
    {done,{ok,reverse(Toks),Pos},Cs};
done(Cs, [], [_|_], Pos, io) ->
    {done,{error,{Pos,?MODULE,scan},Pos},Cs};
done(Cs, [], [], Pos, eof) ->
    {done,{eof,Pos},Cs};
done(Cs, [], [{dot,_}|_]=Toks, Pos, eof) ->
    {done,{ok,reverse(Toks),Pos},Cs};
done(Cs, [], _Toks, Pos, eof) ->
    {done,{error,{Pos,?MODULE,scan},Pos},Cs};
done(Cs, Errors, _Toks, Pos, io) ->
    {Error,ErrorPos} = lists:last(Errors),
    {done,{error,{ErrorPos,?MODULE,Error},Pos},Cs};
done(Cs, Errors, _Toks, Pos, eof) ->
    {Error,ErrorPos} = lists:last(Errors),
    {done,{error,{ErrorPos,?MODULE,Error},Pos},Cs}.


%% The actual scan loop
%% Stack is assumed to be [].

scan([$\n|Cs], Stack, Toks, Pos, State, Errors) ->      % Newline - skip
    scan(Cs, Stack, Toks, Pos+1, State, Errors);
scan([C|Cs], Stack, Toks, Pos, State, Errors) 
  when C >= $\000, C =< $\s ->                          % Control chars - skip
    scan(Cs, Stack, Toks, Pos, State, Errors);
scan([C|Cs], Stack, Toks, Pos, State, Errors) 
  when C >= $\200, C =< $\240 ->                        % Control chars -skip
    scan(Cs, Stack, Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors) 
  when C >= $a, C =< $z ->                              % Atoms
    sub_scan_name(Cs, [C,fun scan_atom/6], Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors) 
  when C >= $�, C =< $�, C =/= $� ->                     % Atoms
    sub_scan_name(Cs, [C,fun scan_atom/6], Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors) 
  when C >= $A, C =< $Z ->                              % Variables
    sub_scan_name(Cs, [C,fun scan_variable/6], Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors) 
  when C >= $�, C =< $�, C =/= $� ->                     % Variables
    sub_scan_name(Cs, [C,fun scan_variable/6], Toks, Pos, State, Errors);
scan([$_|Cs], _Stack, Toks, Pos, State, Errors) ->      % _Variables
    sub_scan_name(Cs, [$_,fun scan_variable/6], Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors)
    when C >= $0, C =< $9 ->                            % Numbers
    scan_number(Cs, [C], Toks, Pos, State, Errors);
scan([$$|Cs], Stack, Toks, Pos, State, Errors) ->	% Character constant
    scan_char(Cs, Stack, Toks, Pos, State, Errors);
scan([$'|Cs], _Stack, Toks, Pos, State, Errors) ->      % Quoted atom
    scan_qatom(Cs, [$',Pos], Toks, Pos, State, Errors);
scan([$"|Cs], _Stack, Toks, Pos, State, Errors) ->      % String
    scan_string(Cs, [$",Pos], Toks, Pos, State, Errors);
scan([$%|Cs], Stack, Toks, Pos, State, Errors) ->       % Comment
    scan_comment(Cs, Stack, Toks, Pos, State, Errors);
%% Punctuation characters and operators, first recognise multiples.
%% Clauses are rouped by first character (a short with the same head has
%% to come after a longer).
%%
%% << <- <=
scan("<<"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'<<',Pos}|Toks], Pos, State, Errors);
scan("<-"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'<-',Pos}|Toks], Pos, State, Errors);
scan("<="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'<=',Pos}|Toks], Pos, State, Errors);
scan("<"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% >> >=
scan(">>"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'>>',Pos}|Toks], Pos, State, Errors);
scan(">="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'>=',Pos}|Toks], Pos, State, Errors);
scan(">"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% -> --
scan("->"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'->',Pos}|Toks], Pos, State, Errors);
scan("--"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'--',Pos}|Toks], Pos, State, Errors);
scan("-"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% ++
scan("++"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'++',Pos}|Toks], Pos, State, Errors);
scan("+"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% =:= =/= =< ==
scan("=:="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'=:=',Pos}|Toks], Pos, State, Errors);
scan("=:"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
scan("=/="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'=/=',Pos}|Toks], Pos, State, Errors);
scan("=/"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
scan("=<"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'=<',Pos}|Toks], Pos, State, Errors);
scan("=="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'==',Pos}|Toks], Pos, State, Errors);
scan("="=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% /=
scan("/="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'/=',Pos}|Toks], Pos, State, Errors);
scan("/"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% ||
scan("||"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'||',Pos}|Toks], Pos, State, Errors);
scan("|"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% :-
scan(":-"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{':-',Pos}|Toks], Pos, State, Errors);
%% :: for typed records
scan("::"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'::',Pos}|Toks], Pos, State, Errors);
%%
scan(":"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% Full stop and plain '.'
scan("."++Cs, Stack, Toks, Pos, State, Errors) ->
    scan_dot(Cs, Stack, Toks, Pos, State, Errors);
%% All single-char punctuation characters and operators (except '.')
scan([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{list_to_atom([C]),Pos}|Toks], Pos, State, Errors);
%%
scan([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan/6);
scan(Eof, _Stack, Toks, Pos, State, Errors) ->
    done(Eof, Errors, Toks, Pos, State).


scan_atom(Cs, Name, Toks, Pos, State, Errors) ->
    case catch list_to_atom(Name) of
	Atom when is_atom(Atom) ->
	    case reserved_word(Atom) of
		true ->
		    scan(Cs, [], [{Atom,Pos}|Toks], Pos, State, Errors);
		false ->
		    scan(Cs, [], [{atom,Pos,Atom}|Toks], Pos, State, Errors)
	    end;
	_ ->
	    scan(Cs, [], Toks, Pos, State, [{{illegal,atom},Pos}|Errors])
    end.

scan_variable(Cs, Name, Toks, Pos, State, Errors) ->
    case catch list_to_atom(Name) of
	A when is_atom(A) ->
	    scan(Cs, [], [{var,Pos,A}|Toks], Pos, State, Errors);
	_ ->
	    scan(Cs, [], Toks, Pos, State, [{{illegal,var},Pos}|Errors])
    end.


%% Scan for a name - unqouted atom or variable, after the first character.
%%
%% Stack argument: return fun.
%% Returns the scanned name on the stack, unreversed.
%%
sub_scan_name([C|Cs]=Css, Stack, Toks, Pos, State, Errors) ->
    case name_char(C) of
	true ->
	    sub_scan_name(Cs, [C|Stack], Toks, Pos, State, Errors);
	false ->
	    [Fun|Name] = reverse(Stack),
	    Fun(Css, Name, Toks, Pos, State, Errors)
    end;
sub_scan_name([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun sub_scan_name/6);
sub_scan_name(Eof, Stack, Toks, Pos, State, Errors) ->
    [Fun|Name] = reverse(Stack),
    Fun(Eof, Name, Toks, Pos, State, Errors).

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $�, C =< $�, C =/= $� -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $�, C =< $�, C =/= $� -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.


scan_char([$\\|Cs], Stack, Toks, Pos, State, Errors) ->
    sub_scan_escape(Cs,[fun scan_char_escape/6|Stack], 
		    Toks, Pos, State, Errors);
scan_char([$\n|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{char,Pos,$\n}|Toks], Pos+1, State, Errors);
scan_char([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_char/6);
scan_char(Cs, Stack, Toks, Pos, State, Errors) ->
    scan_char_escape(Cs, Stack, Toks, Pos, State, Errors).

scan_char_escape([nl|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{char,Pos,$\n}|Toks], Pos+1, State, Errors);
scan_char_escape([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{char,Pos,C}|Toks], Pos, State, Errors);
scan_char_escape(Eof, _Stack, _Toks, Pos, State, Errors) ->
    done(Eof, [{char,Pos}|Errors], [], Pos, State).



scan_string([$"|Cs], Stack, Toks, Pos, State, Errors) ->
    [StartPos,$"|S] = reverse(Stack),
    scan(Cs, [], [{string,StartPos,S}|Toks], Pos, State, Errors);
scan_string([$\n|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_string(Cs, [$\n|Stack], Toks, Pos+1, State, Errors);
scan_string([$\\|Cs], Stack, Toks, Pos, State, Errors) ->
    sub_scan_escape(Cs, [fun scan_string_escape/6|Stack], 
		    Toks, Pos, State, Errors);
scan_string([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_string(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_string([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_string/6);
scan_string(Eof, Stack, _Toks, Pos, State, Errors) ->
    [StartPos,$"|S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string,$",SS},StartPos}|Errors], [], Pos, State).

scan_string_escape([nl|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_string(Cs, [$\n|Stack], Toks, Pos+1, State, Errors);
scan_string_escape([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_string(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_string_escape(Eof, Stack, _Toks, Pos, State, Errors) ->
    [StartPos,$"|S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string,$",SS},StartPos}|Errors], [], Pos, State).



scan_qatom([$'|Cs], Stack, Toks, Pos, State, Errors) ->
    [StartPos,$'|S] = reverse(Stack),
    case catch list_to_atom(S) of
	A when is_atom(A) ->
	    scan(Cs, [], [{atom,StartPos,A}|Toks], Pos, State, Errors);
	_ ->
	    scan(Cs, [], Toks, Pos, State, [{{illegal,atom},StartPos}|Errors])
    end;
scan_qatom([$\n|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_qatom(Cs, [$\n|Stack], Toks, Pos+1, State, Errors);
scan_qatom([$\\|Cs], Stack, Toks, Pos, State, Errors) ->
    sub_scan_escape(Cs, [fun scan_qatom_escape/6|Stack], 
		    Toks, Pos, State, Errors);
scan_qatom([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_qatom(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_qatom([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_qatom/6);
scan_qatom(Eof, Stack, _Toks, Pos, State, Errors) ->
    [StartPos,$'|S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string,$',SS},StartPos}|Errors], [], Pos, State).

scan_qatom_escape([nl|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_qatom(Cs, [$\n|Stack], Toks, Pos+1, State, Errors);
scan_qatom_escape([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_qatom(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_qatom_escape(Eof, Stack, _Toks, Pos, State, Errors) ->
    [StartPos,$'|S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string,$',SS},StartPos}|Errors], [], Pos, State).


%% Scan for a character escape sequence, in character literal or string. 
%% A string is a syntactical sugar list (e.g "abc") 
%% or a quoted atom (e.g 'EXIT').
%%
%% Stack argument: return fun.
%% Returns the resulting escape character on the stream.
%% The return atom 'nl' means that the escape sequence Backslash Newline
%% was found, i.e an actual Newline in the input.
%%
%% \<1-3> octal digits
sub_scan_escape([O1,O2,O3|Cs], [Fun|Stack], Toks, Pos, State, Errors) 
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    Val = (O1*8 + O2)*8 + O3 - 73*$0,
    Fun([Val|Cs], Stack, Toks, Pos, State, Errors);
sub_scan_escape([O1,O2]=Cs, Stack, Toks, Pos, State, Errors) 
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun sub_scan_escape/6);
sub_scan_escape([O1,O2|Cs], [Fun|Stack], Toks, Pos, State, Errors) 
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    Val = (O1*8 + O2) - 9*$0,
    Fun([Val|Cs], Stack, Toks, Pos, State, Errors);
sub_scan_escape([O1]=Cs, Stack, Toks, Pos, State, Errors) 
  when O1 >= $0, O1 =< $7 ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun sub_scan_escape/6);
sub_scan_escape([O1|Cs], [Fun|Stack], Toks, Pos, State, Errors) 
  when O1 >= $0, O1 =< $7 ->
    Val = O1 - $0,
    Fun([Val|Cs], Stack, Toks, Pos, State, Errors);
%% \^X -> CTL-X
sub_scan_escape([$^,C|Cs], [Fun|Stack], Toks, Pos, State, Errors) ->
    Val = C band 31,
    Fun([Val|Cs], Stack, Toks, Pos, State, Errors);
sub_scan_escape([$^]=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun sub_scan_escape/6);
sub_scan_escape([$^|Eof], [Fun|Stack], Toks, Pos, State, Errors) ->
    Fun(Eof, Stack, Toks, Pos, State, Errors);
%% \NL (backslash newline)
sub_scan_escape([$\n|Cs],[Fun|Stack], Toks, Pos, State, Errors) ->
    Fun([nl|Cs], Stack, Toks, Pos, State, Errors);
%% \X - familiar escape sequences
sub_scan_escape([C|Cs], [Fun|Stack], Toks, Pos, State, Errors) ->
    Val = escape_char(C),
    Fun([Val|Cs], Stack, Toks, Pos, State, Errors);
%%
sub_scan_escape([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun sub_scan_escape/6);
sub_scan_escape(Eof, [Fun|Stack], Toks, Pos, State, Errors) ->
    Fun(Eof, Stack, Toks, Pos, State, Errors).

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.


scan_number([$.,C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C,$.|Stack], Toks, Pos, State, Errors);
scan_number([$.]=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan_number/6);
scan_number([C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_number(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_number([$#|Cs], Stack, Toks, Pos, State, Errors) ->
    case catch list_to_integer(reverse(Stack)) of
	B when is_integer(B), B >= 2, B =< 1+$Z-$A+10 ->
	    scan_based_int(Cs, [B], Toks, Pos, State, Errors);
	B ->
	    scan(Cs, [], Toks, Pos, State, [{{base,B},Pos}|Errors])
    end;
scan_number([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_number/6);
scan_number(Cs, Stack, Toks, Pos, State, Errors) ->
    case catch list_to_integer(reverse(Stack)) of
	N when is_integer(N) ->
	    scan(Cs, [], [{integer,Pos,N}|Toks], Pos, State, Errors);
	_ ->
	    scan(Cs, [], Toks, Pos, State, [{{illegal,integer},Pos}|Errors])
    end.

scan_based_int([C|Cs], [B|Stack], Toks, Pos, State, Errors) 
  when C >= $0, C =< $9, C < $0+B ->
    scan_based_int(Cs, [B,C|Stack], Toks, Pos, State, Errors);
scan_based_int([C|Cs], [B|Stack], Toks, Pos, State, Errors) 
  when C >= $A, B > 10, C < $A+B-10 ->
    scan_based_int(Cs, [B,C|Stack], Toks, Pos, State, Errors);
scan_based_int([C|Cs], [B|Stack], Toks, Pos, State, Errors) 
  when C >= $a, B > 10, C < $a+B-10 ->
    scan_based_int(Cs, [B,C|Stack], Toks, Pos, State, Errors);
scan_based_int([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_based_int/6);
scan_based_int(Cs, [B|Stack], Toks, Pos, State, Errors) ->
    case catch erlang:list_to_integer(reverse(Stack), B) of
	N when is_integer(N) ->
	    scan(Cs, [], [{integer,Pos,N}|Toks], Pos, State, Errors);
	_ ->
	    scan(Cs, [], Toks, Pos, State, [{{illegal,integer},Pos}|Errors])
    end.

scan_fraction([C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_fraction([$e|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent_sign(Cs, [$E|Stack], Toks, Pos, State, Errors);
scan_fraction([$E|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent_sign(Cs, [$E|Stack], Toks, Pos, State, Errors);
scan_fraction([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_fraction/6);
scan_fraction(Cs, Stack, Toks, Pos, State, Errors) ->
    case catch list_to_float(reverse(Stack)) of
	F when is_float(F) ->
	    scan(Cs, [], [{float,Pos,F}|Toks], Pos, State, Errors);
	_ ->
	    scan(Cs, [], Toks, Pos, State, [{{illegal,float},Pos}|Errors])
    end.

scan_exponent_sign([$+|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent(Cs, [$+|Stack], Toks, Pos, State, Errors);
scan_exponent_sign([$-|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent(Cs, [$-|Stack], Toks, Pos, State, Errors);
scan_exponent_sign([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_exponent_sign/6);
scan_exponent_sign(Cs, Stack, Toks, Pos, State, Errors) ->
    scan_exponent(Cs, Stack, Toks, Pos, State, Errors).
        
scan_exponent([C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_exponent(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_exponent([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_exponent/6);
scan_exponent(Cs, Stack, Toks, Pos, State, Errors) ->
    case catch list_to_float(reverse(Stack)) of
	F when is_float(F) ->
	    scan(Cs, [], [{float,Pos,F}|Toks], Pos, State, Errors);
	_ ->
	    scan(Cs, [], Toks, Pos, State, [{{illegal,float},Pos}|Errors])
    end.


scan_comment([$\n|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, Toks, Pos+1, State, Errors);
scan_comment([_|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_comment(Cs, Stack, Toks, Pos, State, Errors);
scan_comment([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_comment/6);
scan_comment(Eof, _Stack, Toks, Pos, State, Errors) ->
    done(Eof, Errors, Toks, Pos, State).



scan_dot([$%|_]=Cs, _Stack, Toks, Pos, State, Errors) ->
    done(Cs, Errors, [{dot,Pos}|Toks], Pos, State);
scan_dot([$\n|Cs], _Stack, Toks, Pos, State, Errors) ->
    done(Cs, Errors, [{dot,Pos}|Toks], Pos+1, State);
scan_dot([C|Cs], _Stack, Toks, Pos, State, Errors) when C>=$\000, C=<$\s ->
    done(Cs, Errors, [{dot,Pos}|Toks], Pos, State);
scan_dot([C|Cs], _Stack, Toks, Pos, State, Errors) when C>=$\200, C=<$\240 ->
    done(Cs, Errors, [{dot,Pos}|Toks], Pos, State);
scan_dot([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_dot/6);
scan_dot(eof, _Stack, Toks, Pos, State, Errors) ->
    done(eof, Errors, [{dot,Pos}|Toks], Pos, State);
scan_dot(Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'.',Pos}|Toks], Pos, State, Errors).


%% reserved_word(Atom) -> Bool
%%   return 'true' if Atom is an Erlang reserved word, else 'false'.

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') -> true;
reserved_word('cond') -> true;
reserved_word('catch') -> true;
reserved_word('andalso') -> true;
reserved_word('orelse') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('query') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word('spec') -> true;
reserved_word(_) -> false.
