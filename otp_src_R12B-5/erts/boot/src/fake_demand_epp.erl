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
-module(epp).

%% An Erlang code preprocessor.

-export([open/2,open/3,close/1,format_error/1]).
-export([open_io/2,open_io/3]).
-export([scan_erl_form/1,parse_erl_form/1,macro_defs/1]).
-export([parse_file/3]).

%% Epp state record.
-record(epp, {file,				%Current file
	      line=1,				%Current line number
	      name="",				%Current file name
	      istk=[],				%Ifdef stack
	      sstk=[],				%State stack
	      macs=dict:new(),			%Macros
	      uses=dict:new()			%Macro use structure
	     }).

%%% Note on representation: as tokens, both {var, Line, Name} and
%%% {atom, Line, Name} can occur as macro identifiers. However, keeping
%%% this distinction here is done for historical reasons only: previously,
%%% ?FOO and ?'FOO' were not the same, but now they are. Removing the
%%% distinction in the internal representation would simplify the code
%%% a little.

%% open(FileName, IncludePath)
%% open(FileName, IncludePath, PreDefMacros)
%% close(Epp)
%% scan_erl_form(Epp)
%% parse_erl_form(Epp)
%% macro_defs(Epp)

open(File, Path) ->
    open(File, Path, []).

open(File, Path, Pdm) ->
    Self = self(),				%The caller's pid
    Epp = spawn_link(fun () -> file_server(Self, File, Path, Pdm) end),
    wait_epp_reply(Epp).

close(Epp) ->
    epp_request(Epp, close),
    wait_epp_reply(Epp).

scan_erl_form(Epp) ->
    epp_request(Epp, scan_erl_form),
    wait_epp_reply(Epp).

parse_erl_form(Epp) ->
    epp_request(Epp, scan_erl_form),
    case wait_epp_reply(Epp) of
	{ok,Toks} ->
	    erl_parse:parse_form(Toks);
	Other ->
	    Other
    end.

macro_defs(Epp) ->
    epp_request(Epp, macro_defs),
    wait_epp_reply(Epp).

%% format_error(ErrorDescriptor) -> String
%%  Return a string describing the error.

format_error({bad,W}) ->
    io_lib:format("badly formed '~s'", [W]);
format_error({call,What}) ->
    io_lib:format("illegal macro call '~s'",[What]);
format_error({undefined,M}) ->
    io_lib:format("undefined macro '~w'", [M]);
format_error({depth,What}) ->
    io_lib:format("~s too deep",[What]);
format_error({mismatch,M}) ->
    io_lib:format("argument mismatch for macro '~w'", [M]);
format_error({arg_error,M}) ->
    io_lib:format("badly formed argument for macro '~w'", [M]);
format_error({redefine,M}) ->
    io_lib:format("redefining macro '~w'", [M]);
format_error({circular,M}) ->
    io_lib:format("circular macro '~w'", [M]);
format_error({include,W,F}) ->
    io_lib:format("can't find include ~s \"~s\"", [W,F]);
format_error({illegal,How,What}) ->
    io_lib:format("~s '-~s'", [How,What]);
format_error({'NYI',What}) ->
    io_lib:format("not yet implemented '~s'", [What]).

%% parse_file(FileName, IncludePath, [PreDefMacro]) ->
%%	{ok,[Form]} | {error,OpenError}

parse_file(Ifile, Path, Predefs) ->
    case open(Ifile, Path, Predefs) of
	{ok,Epp} ->
	    Forms = parse_file(Epp),
	    close(Epp),
	    {ok,Forms};
	{error,E} ->
	    {error,E}
    end.

%% parse_file(Epp) ->
%%	[Form]

parse_file(Epp) ->
    case parse_erl_form(Epp) of
	{ok,Form} ->
	    [Form|parse_file(Epp)];
	{error,E} ->
	    [{error,E}|parse_file(Epp)];
	{eof,Line} ->
	    [{eof,Line}]
    end.

%% open_io(IoStream, IncludePath)
%% open_io(IoStream, IncludePath, PredefMacros)

open_io(Io, Path) ->
    open_io(Io, Path, []).

open_io(Io, Path, Pdm) ->
    Self = self(),				%The caller's pid
    Epp = spawn(fun () -> io_server(Self, Io, "iostream", Path, Pdm) end),
    wait_epp_reply(Epp).

%% file_server(StarterPid, FileName, Path, PreDefMacros)
%% io_server(StarterPid, IoStream, FileName, Path, PreDefMacros)

file_server(Pid, Name, Path, Pdm) ->
    process_flag(trap_exit, true),
    case file:open(Name, read) of
	{ok,File} ->
	    io_server(Pid, File, Name, Path, Pdm);
	{error,E} ->
	    epp_reply(Pid, {error,E})
    end.

io_server(Pid, Io, Name, Path, Pdm) ->
    put(sys_path, Path),
    put(user_path, Path),
    Ms0 = predef_macros(Name),
    case user_predef(Pdm, Ms0) of
	{ok,Ms1} ->
	    epp_reply(Pid, {ok,self()}),
	    St = #epp{file=Io,name=Name,macs=Ms1},
	    From = wait_request(St),
	    enter_file_reply(From, Name, 1),
	    wait_req_scan(St);
	{error,E} ->
	    epp_reply(Pid, {error,E})
    end.

%% predef_macros(FileName) -> Macrodict
%%  Initialise the macro dictionary with the default predefined macros,
%%  FILE, LINE, MODULE as undefined, MACHINE and MACHINE value.

predef_macros(File) ->
    Ms0 = dict:new(),
    Ms1 = dict:store({atom,'FILE'}, {none,[{string,1,File}]}, Ms0),
    Ms2 = dict:store({atom,'LINE'}, {none,[{integer,1,1}]}, Ms1),
    Ms3 = dict:store({atom,'MODULE'}, undefined, Ms2),
    Ms31 = dict:store({atom,'MODULE_STRING'}, undefined, Ms3),
    Machine = list_to_atom(erlang:system_info(machine)),
    Ms4 = dict:store({atom,'MACHINE'}, {none,[{atom,1,Machine}]}, Ms31),
    dict:store({atom,Machine}, {none,[{atom,1,true}]}, Ms4).

%% user_predef(PreDefMacros, Macros) ->
%%	{ok,MacroDict} | {error,E}
%%  Add the predefined macros to the macros dictionary. A macro without a
%%  value gets the value 'true'.

user_predef([{M,Val}|Pdm], Ms) when atom(M) ->
    case dict:find({atom,M}, Ms) of
	{ok,Def} ->
	    {error,{redefine,M}};
	error ->
	    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
	    user_predef(Pdm, dict:store({atom,M}, {none,Exp}, Ms))
    end;
user_predef([M|Pdm], Ms) when atom(M) ->
    case dict:find({atom,M}, Ms) of
	{ok,Def} ->
	    {error,{redefine,M}};
	error ->
	    user_predef(Pdm, dict:store({atom,M}, {none,[{atom,1,true}]}, Ms))
    end;
user_predef([Md|Pdm], Ms) -> {error,{bad,Md}};
user_predef([], Ms) -> {ok,Ms}.

%% wait_request(EppState) -> RequestFrom
%% wait_req_scan(EppState)
%% wait_req_skip(EppState, SkipIstack)
%%  Handle requests, processing trivial requests directly. Either return
%%  requestor or scan/skip tokens.

wait_request(St) ->
    receive
	{epp_request,From,scan_erl_form} -> From;
	{epp_request,From,macro_defs} ->
	    epp_reply(From, dict:dict_to_list(St#epp.macs)),
	    wait_request(St);
	{epp_request,From,close} ->
	    epp_reply(From, ok),
	    exit(normal);
	{'EXIT',_,R} ->
	    exit(R);
	Other ->
	    io:fwrite("Epp: unknown '~w'\n", [Other]),
	    wait_request(St)
    end.

wait_req_scan(St) ->
    From = wait_request(St),
    scan_toks(From, St).

wait_req_skip(St, Sis) ->
    From = wait_request(St),
    skip_toks(From, St, Sis).
	
%% enter_file(Path, FileName, IncludeLine, From, EppState)
%% leave_file(From, EppState)
%%  Handle entering and leaving included files. Notify caller when the
%%  current file is changed. Note it is an error to exit a file if we are
%%  in a conditional. These functions never return.

enter_file(Path, NewName, Li, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{Li,epp,{depth,"include"}}}),
    wait_req_scan(St);
enter_file(Path, NewName, Li, From, St) ->
    case file:path_open(Path, NewName, read) of
	{ok,NewF,Pname} ->
	    wait_req_scan(enter_file(NewF, Pname, From, St));
	{error,E} ->
	    epp_reply(From, {error,{Li,epp,{include,file,NewName}}}),
	    wait_req_scan(St)
    end.

%% enter_file(File, FullName, From, EppState) -> EppState.
%%  Set epp to use this file and "enter" it.

enter_file(NewF, Pname, From, St) ->
    enter_file_reply(From, Pname, 1),
    Ms = dict:store({atom,'FILE'}, {none,[{string,1,Pname}]}, St#epp.macs),
    #epp{file=NewF,name=Pname,sstk=[St|St#epp.sstk],macs=Ms}.

enter_file_reply(From, Name, Line) ->
    Rep = {ok, [{'-',Line},{atom,Line,file},{'(',Line},
		{string,Line,file_name(Name)},{',',Line},
		{integer,Line,Line},{')',Line},{dot,Line}]},
    epp_reply(From, Rep).

%% Flatten filename to a string. Must be a valid filename.

file_name([C | T]) when integer(C), C > 0, C =< 255 ->
    [C | file_name(T)];
file_name([H|T]) ->
    file_name(H) ++ file_name(T);
file_name([]) ->
    [];
file_name(N) when atom(N) ->
    atom_to_list(N).

leave_file(From, St) ->
    case St#epp.istk of
	[I|Cis] -> 
	    epp_reply(From,
		      {error,{St#epp.line,epp, {illegal,"unterminated",I}}}),
	    leave_file(wait_request(St),St#epp{istk=Cis});
	[] ->
	    case St#epp.sstk of
		[OldSt|Sts] ->
		    file:close(St#epp.file),
		    enter_file_reply(From, OldSt#epp.name, OldSt#epp.line),
		    Ms = dict:store({atom,'FILE'},
				    {none,
				     [{string,OldSt#epp.line,OldSt#epp.name}]},
				    St#epp.macs),
		    wait_req_scan(OldSt#epp{sstk=Sts,macs=Ms});
		[] ->
		    epp_reply(From, {eof,St#epp.line}),
		    wait_req_scan(St)
	    end
    end.

%% scan_toks(From, EppState)
%% scan_toks(Tokens, From, EppState)

scan_toks(From, St) ->
    case io:scan_erl_form(St#epp.file, '', St#epp.line) of
	{ok,Toks,Cl} ->
	    scan_toks(Toks, From, St#epp{line=Cl});
	{error,E,Cl} ->
	    epp_reply(From, {error,E}),
	    wait_req_scan(St#epp{line=Cl});
	{eof,Cl} ->
	    leave_file(From, St#epp{line=Cl});
	{error,E} ->
	    epp_reply(From, {error,E}),
	    true				%This serious, just exit!
    end.

scan_toks([{'-',Lh},{atom,Ld,define}|Toks], From, St) ->
    scan_define(Toks, Ld, From, St);
scan_toks([{'-',Lh},{atom,Ld,undef}|Toks], From, St) ->
    scan_undef(Toks, Ld, From, St);
scan_toks([{'-',Lh},{atom,Li,include}|Toks], From, St) ->
    scan_include(Toks, Li, From, St);
scan_toks([{'-',Lh},{atom,Li,include_lib}|Toks], From, St) ->
    scan_include_lib(Toks, Li, From, St);
scan_toks([{'-',Lh},{atom,Li,ifdef}|Toks], From, St) ->
    scan_ifdef(Toks, Li, From, St);
scan_toks([{'-',Lh},{atom,Li,ifndef}|Toks], From, St) ->
    scan_ifndef(Toks, Li, From, St);
scan_toks([{'-',Lh},{atom,Le,else}|Toks], From, St) ->
    scan_else(Toks, Le, From, St);
scan_toks([{'-',Lh},{atom,Le,'if'}|Toks], From, St) ->
    scan_if(Toks, Le, From, St);
scan_toks([{'-',Lh},{atom,Le,elif}|Toks], From, St) ->
    scan_elif(Toks, Le, From, St);
scan_toks([{'-',Lh},{atom,Le,endif}|Toks], From, St) ->
    scan_endif(Toks, Le, From, St);
scan_toks(Toks0, From, St) ->
    case catch expand_macros(Toks0, {St#epp.macs, St#epp.uses}) of
	Toks1 when list(Toks1) ->
	    epp_reply(From, {ok,Toks1}),
	    wait_req_scan(St#epp{macs=scan_module(Toks1, St#epp.macs)});
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end.

scan_module([{'-',_Lh},{atom,_Lm,module},{'(',_Ll}|Ts], Ms) ->
    scan_module_1(Ts,[],Ms);
scan_module(_Ts, Ms) -> Ms.

scan_module_1([{atom,Ln,A},{')',_Lr}|_Ts], As, Ms) ->
    Mod = lists:concat(lists:reverse([A|As])),
    Ms1 = dict:store({atom,'MODULE'}, {none,[{atom,Ln,list_to_atom(Mod)}]},
		     Ms),
    dict:store({atom,'MODULE_STRING'}, {none,[{string,Ln,Mod}]},Ms1);
scan_module_1([{atom,_Ln,A},{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, [".",A|As], Ms);
scan_module_1([{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, As, Ms);
scan_module_1(_Ts, _As, Ms) -> Ms.

%% scan_define(Tokens, DefineLine, From, EppState)

scan_define([{'(',Lp},{atom,Lm,M},{',',Lc}|Toks], Ld, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    epp_reply(From, {error,{Lm,epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    scan_define_cont(From, St,
			     {atom, M},
			     {none,macro_expansion(Toks)})
    end;
scan_define([{'(',Lp},{atom,Lm,M},{'(',Lc}|Toks], Ld, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    epp_reply(From, {error,{Lm,epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    case catch macro_pars(Toks, []) of
		{ok, {As, Me}} ->
		    scan_define_cont(From, St,
				    {atom, M},
				    {As, Me});
		_ ->
		    epp_reply(From, {error,{Ld,epp,{bad,define}}}),
		    wait_req_scan(St)
	    end
    end;
scan_define([{'(',Lp},{var,Lm,M},{',',Lc}|Toks], Ld, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    epp_reply(From, {error,{Lm,epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    scan_define_cont(From, St,
			     {atom, M},
			     {none,macro_expansion(Toks)})
    end;
scan_define([{'(',Lp},{var,Lm,M},{'(',Lc}|Toks], Ld, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    epp_reply(From, {error,{Lm,epp,{redefine,M}}}),
	    wait_req_scan(St);
	error ->
	    case catch macro_pars(Toks, []) of
		{ok, {As, Me}} ->
		    scan_define_cont(From, St,
				     {atom, M},
				     {As, Me});
		_ ->
		    epp_reply(From, {error,{Ld,epp,{bad,define}}}),
		    wait_req_scan(St)
	    end
    end;
scan_define(Toks, Ld, From, St) ->
    epp_reply(From, {error,{Ld,epp,{bad,define}}}),
    wait_req_scan(St).

%%% Detection of circular macro expansions (which would either keep
%%% the compiler looping forever, or run out of memory):
%%% When a macro is defined, we store the names of other macros it
%%% uses in St#epp.uses. If any macro is undef'ed, that information
%%% becomes invalid, so we redo it for all remaining macros.
%%% The circularity detection itself is done when a macro is expanded:
%%% the information from St#epp.uses is traversed, and if a circularity
%%% is detected, an error message is thrown.

scan_define_cont(F, St, M, Def) ->
    Ms = dict:store(M, Def, St#epp.macs),
    U = dict:store(M, macro_uses(Def), St#epp.uses),
    scan_toks(F, St#epp{uses=U, macs=Ms}).

macro_uses({Args, Tokens}) ->
    Uses0 = macro_ref(Tokens),
    lists:usort(Uses0).

macro_ref([]) ->
    [];
macro_ref([{'?', _}, {atom, _, A} | Rest]) ->
    [{atom, A} | macro_ref(Rest)];
macro_ref([{'?', _}, {var, _, A} | Rest]) ->
    [{atom, A} | macro_ref(Rest)];
macro_ref([Token | Rest]) ->
    macro_ref(Rest).

all_macro_uses(D0) ->
    L = dict:dict_to_list(D0),
    D = dict:new(),
    add_macro_uses(L, D).

add_macro_uses([], D) ->
    D;
add_macro_uses([{Key, Def} | Rest], D0) ->
    add_macro_uses(Rest, dict:store(Key, macro_uses(Def), D0)).

%% scan_undef(Tokens, UndefLine, From, EppState)

scan_undef([{'(',Llp},{atom,Lm,M},{')',Lrp},{dot,Ld}], Lu, From, St) ->
    scan_toks(From, St#epp{macs=dict:erase({atom,M}, St#epp.macs),
			   uses=all_macro_uses(St#epp.macs)});
scan_undef([{'(',Llp},{var,Lm,M},{')',Lrp},{dot,Ld}], Lu, From,St) ->
    scan_toks(From, St#epp{macs=dict:erase({atom,M}, St#epp.macs),
			   uses=all_macro_uses(St#epp.macs)});
scan_undef(Toks, Lu, From,St) ->
    epp_reply(From, {error,{Lu,epp,{bad,undef}}}),
    wait_req_scan(St).

%% scan_include(Tokens, IncludeLine, From, St)

scan_include([{'(',Llp},{string,Lf,NewName},{')',Lrp},{dot,Ld}], Li, From, St) ->
    enter_file(get(user_path), NewName, Li, From, St);
scan_include(Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,include}}}),
    wait_req_scan(St).

%% scan_include_lib(Tokens, IncludeLine, From, EppState)
%%  For include_lib we first test if we can find the file through the
%%  normal search path, if not we assume that the first directory name
%%  is a library name, find its true directory and try with that.

find_lib_dir(NewName) ->
    Sc = string:chr(NewName, $/),
    Lib = string:substr(NewName, 1, Sc-1),
    Rest = string:substr(NewName, Sc+1),
    {Lib, Rest}.

scan_include_lib([{'(',Llp},{string,Lf,NewName},{')',Lrp},{dot,Ld}], Li,
		 From, St) ->
    case catch find_lib_dir(NewName) of
	{LibDir, Rest} when list(LibDir) ->
	    Inc = filename:rootname(filename:basename(Rest)),
	    %% erlang:display({include_lib,here1, LibDir,Inc}),
	    case boot_code_loader:include_file(LibDir, Inc) of
		{ok, Bin} ->
		    %% io:format("Found it in a binary! -- hack city~n"),
		    {ok, NewF} = bin_io:open_read(Bin),
		    wait_req_scan(enter_file(NewF, LibDir, From, St));
		error ->
		    epp_reply(From, {error,{Li,epp,{include,lib,NewName}}}),
		    wait_req_scan(St);
		Error ->
		    epp_reply(From, {error,{Li,epp,{include,lib,NewName}}}),
		    wait_req_scan(St)
	    end
    end;
scan_include_lib(Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,include_lib}}}),
    wait_req_scan(St).

%% scan_ifdef(Tokens, IfdefLine, From, EppState)
%% scan_ifndef(Tokens, IfdefLine, From, EppSate)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if[n]def test and then treat as undefined macro.

scan_ifdef([{'(',Llp},{atom,Lm,M},{')',Lrp},{dot,Ld}], Li, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
	error ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef([{'(',Llp},{var,Lm,M},{')',Lrp},{dot,Ld}], Li, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
	error ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef(Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,ifdef}}}),
    wait_req_skip(St, [ifdef]).

scan_ifndef([{'(',Llp},{atom,Lm,M},{')',Lrp},{dot,Ld}], Li, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    skip_toks(From, St, [ifndef]);
	error ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef([{'(',Llp},{var,Lm,M},{')',Lrp},{dot,Ld}], Li, From, St) ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,Def} ->
	    skip_toks(From, St, [ifndef]);
	error ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef(Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,ifndef}}}),
    wait_req_scan(St).

%% scan_else(Tokens, ElseLine, From, EppState)
%%  If we are in an if body then convert to else and skip, if we are in an
%%  else or not in anything report an error.

scan_else([{dot,Ld}], Le, From, St) ->
    case St#epp.istk of
	[else|Cis] ->
	    epp_reply(From, {error,{Le,epp,{illegal,"repeated",else}}}),
	    wait_req_skip(St#epp{istk=Cis}, [else]);
	[I|Cis] ->
	    skip_toks(From, St#epp{istk=Cis}, [else]);
	[] ->
	    epp_reply(From, {error,{Le,epp,{illegal,"unbalanced",else}}}),
	    wait_req_scan(St)
    end;
scan_else(Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{bad,else}}}),
    wait_req_scan(St).

%% scan_if(Tokens, EndifLine, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_if(Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{'NYI','if'}}}),
    wait_req_skip(St, ['if']).

%% scan_elif(Tokens, EndifLine, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_elif(Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{'NYI','elif'}}}),
    wait_req_skip(St, ['elif']).

%% scan_endif(Tokens, EndifLine, From, EppState)
%%  If we are in an if body then exit it, else report an error.

scan_endif([{dot,Ld}], Le, From, St) ->
    case St#epp.istk of
	[I|Cis] ->
	    scan_toks(From, St#epp{istk=Cis});
	[] ->
	    epp_reply(From, {error,{Le,epp,{illegal,"unbalanced",endif}}}),
	    wait_req_scan(St)
    end;
scan_endif(Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{bad,endif}}}),
    wait_req_scan(St).

%% skip_toks(From, EppState, SkipIstack)
%%  Skip over forms until current conditional has been exited. Handle
%%  nested conditionals and repeated 'else's.

skip_toks(From, St, [I|Sis]) ->
    case io:scan_erl_form(St#epp.file, '', St#epp.line) of
	{ok,[{'-',Lh},{atom,Li,ifdef}|Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [ifdef,I|Sis]);
	{ok,[{'-',Lh},{atom,Li,ifndef}|Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [ifndef,I|Sis]);
	{ok,[{'-',Lh},{atom,Li,'if'}|Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, ['if',I|Sis]);
	{ok,[{'-',Lh},{atom,Le,else}|Toks],Cl}->
	    skip_else(Le, From, St#epp{line=Cl}, [I|Sis]);
	{ok,[{'-',Lh},{atom,Le,endif}|Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, Sis);
	{ok,Toks,Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [I|Sis]);
	{error,E,Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [I|Sis]);
	{eof,Cl} ->
	    leave_file(From, St#epp{line=Cl,istk=[I|Sis]});
	{error,E} ->
	    epp_reply(From, {error,E}),
	    true				%This serious, just exit!
    end;
skip_toks(From, St, []) ->
    scan_toks(From, St).

skip_else(Le, From, St, [else|Sis]) ->
    epp_reply(From, {error,{Le,epp,{illegal,"repeated",else}}}),
    wait_req_skip(St, [else|Sis]);
skip_else(Le, From, St, [I]) ->
    scan_toks (From, St#epp{istk=[else|St#epp.istk]});
skip_else(Le, From, St, Sis) ->
    skip_toks(From, St, Sis).

%% macro_pars(Tokens, ArgStack)
%% macro_expansion(Tokens)
%%  Extract the macro parameters and the expansion from a macro definition.

macro_pars([{')',Lp}, {',',Ld}|Ex], Args) ->
    {ok, {lists:reverse(Args), macro_expansion(Ex)}};
macro_pars([{var,_,Name}, {')',Lp}, {',',Ld}|Ex], Args) ->
    false = lists:member(Name, Args),		%Prolog is nice
    {ok, {lists:reverse([Name|Args]), macro_expansion(Ex)}};
macro_pars([{var,L,Name}, {',',_}|Ts], Args) ->
    false = lists:member(Name, Args),           
    macro_pars(Ts, [Name|Args]).

macro_expansion([{')',Lp},{dot,Ld}]) -> [];
macro_expansion([{dot,Ld}]) -> [];		%Be nice, allow no right paren!
macro_expansion([T|Ts]) ->
    [T|macro_expansion(Ts)].

%% expand_macros(Tokens, Macros)
%% expand_macro(Tokens, MacroLine, RestTokens)
%%  Expand the macros in a list of tokens, making sure that an expansion
%%  gets the same line number as the macro call.

expand_macros(Type, Lm, M, Toks, Ms0) ->
    %% (Type will always be 'atom')
    {Ms, U} = Ms0,
    check_uses([{Type,M}], [], U, Lm),
    case dict:find({Type,M}, Ms) of
	{ok,{none,Exp}} ->
	    expand_macros(expand_macro(Exp, Lm, Toks, dict:new()), Ms0);
	{ok,{As,Exp}} ->
	    {Bs,Toks1} = bind_args(Toks, Lm, M, As, dict:new()),
	    %%io:format("Bound arguments to macro ~w (~w)~n", [M,Bs]),
	    expand_macros(expand_macro(Exp, Lm, Toks1, Bs), Ms0);
	{ok,undefined} ->
	    throw({error,Lm,{undefined,M}});
	error ->
	    throw({error,Lm,{undefined,M}})
    end.

check_uses([], Anc, U, Lm) ->
    ok;
check_uses([M|Rest], Anc, U, Lm) ->
    case lists:member(M, Anc) of
	true ->
	    {_, Name} = M,
	    throw({error,Lm,{circular,Name}});
	false ->
	    L = get_macro_uses(M, U),
	    check_uses(L, [M|Anc], U, Lm),
	    check_uses(Rest, Anc, U, Lm)
    end.
    
get_macro_uses(M, U) ->
    case dict:find(M, U) of
	error ->
	    [];
	{ok, L} ->
	    L
    end.

%% Macro expansion
expand_macros([{'?',Lq},{atom,Lm,M}|Toks], Ms) ->
    expand_macros(atom, Lm, M, Toks, Ms);
%% Special macros
expand_macros([{'?',Lq},{var,Lm,'LINE'}|Toks], Ms) ->
    [{integer,Lm,Lm}|expand_macros(Toks, Ms)];
expand_macros([{'?',Lq},{var,Lm,M}|Toks], Ms) ->
    expand_macros(atom, Lm, M, Toks, Ms);
%% Illegal macros
expand_macros([{'?',Lq},{Type,Lt}|Toks], Ms) ->
    throw({error,Lt,{call,[$?|atom_to_list(Type)]}});
expand_macros([{'?',Lq},{Type,Lt,What}|Toks], Ms) ->
    throw({error,Lt,{call,[$?|io_lib:write(What)]}});

expand_macros([T|Ts], Ms) ->
    [T|expand_macros(Ts, Ms)];
expand_macros([], Ms) -> [].

%% bind_args(Tokens, MacroLine, MacroName, ArgumentVars, Bindings)
%%  Collect the arguments to a macro call and check for correct number.

bind_args([{'(',Llp},{')',Lrp}|Toks], Lm, M, [], Bs) ->
    {Bs,Toks};
bind_args([{'(',Llp}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, dict:store(A, Arg, Bs));
bind_args(Toks, Lm, M, As, Bs) ->
    throw({error,Lm,{mismatch,M}}).

macro_args([{')',Lrp}|Toks], Lm, M, [], Bs) ->
    {Bs,Toks};
macro_args([{',',Lc}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, dict:store(A, Arg, Bs));
macro_args([], Lm, M, As, Bs) ->
    throw({error,Lm,{arg_error,M}});
macro_args(Toks, Lm, M, As, Bs) ->
    throw({error,Lm,{mismatch,M}}).

%% macro_arg([Tok], [ClosePar], [ArgTok]) -> {[ArgTok],[RestTok]}.
%%  Collect argument tokens until we hit a ',' or a ')'. We know a
%%  enough about syntax to recognise "open parentheses" and keep
%%  scanning until matching "close parenthesis".

macro_arg([{',',Lc}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{',',Lc}|Toks]};
macro_arg([{')',Lrp}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{')',Lrp}|Toks]};
macro_arg([{'(',Llp}|Toks], E, Arg) ->
    macro_arg(Toks, [')'|E], [{'(',Llp}|Arg]);
macro_arg([{'<<',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, ['>>'|E], [{'<<',Lls}|Arg]);
macro_arg([{'[',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, [']'|E], [{'[',Lls}|Arg]);
macro_arg([{'{',Llc}|Toks], E, Arg) ->
    macro_arg(Toks, ['}'|E], [{'{',Llc}|Arg]);
macro_arg([{'begin',Lb}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'begin',Lb}|Arg]);
macro_arg([{'if',Li}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'if',Li}|Arg]);
macro_arg([{'case',Lc}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'case',Lc}|Arg]);
macro_arg([{'receive',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'receive',Lr}|Arg]);
macro_arg([{Rb,Lrb}|Toks], [Rb|E], Arg) ->	%Found matching close
    macro_arg(Toks, E, [{Rb,Lrb}|Arg]);
macro_arg([T|Toks], E, Arg) ->
    macro_arg(Toks, E, [T|Arg]);
macro_arg([], E, Arg) ->
    {lists:reverse(Arg),[]}.

%% expand_macro(MacroDef, MacroLine, RestTokens, Bindings)
%% expand_arg(Argtokens, MacroTokens, MacroLine, RestTokens, Bindings)
%%  Insert the macro expansion replacing macro parameters with their
%%  argument values, inserting the line number of first the macro call
%%  and then the macro arguments, i.e. simulate textual expansion.

expand_macro([{var,Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
	    expand_arg(Val, Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([{'?', _}, {'?', _}, {var,Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
	    expand_arg(stringify(Val), Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([T|Ts], L, Rest, Bs) ->
    [setelement(2, T, L)|expand_macro(Ts, L, Rest, Bs)];
expand_macro([], L, Rest, Bs) -> Rest.

expand_arg([A|As], Ts, L, Rest, Bs) ->
    [A|expand_arg(As, Ts, element(2, A), Rest, Bs)];
expand_arg([], Ts, L, Rest, Bs) ->
    expand_macro(Ts, L, Rest, Bs).

%%% stringify(L) returns a list of one token: a string which when
%%% tokenized would yield the token list L.

%tst(Str) ->
%    {ok, T, _} = erl_scan:string(Str),
%    [{string, _, S}] = stringify(T),
%    S.

token_src({dot, _}) ->
    ".";
token_src({X, _}) when atom(X) ->
    atom_to_list(X);
token_src({var, _, X}) ->
    atom_to_list(X);
token_src({string, _, X}) ->
    lists:flatten(io_lib:format("~p", [X]));
token_src({_, _, X}) ->
    lists:flatten(io_lib:format("~w", [X])).

stringify1([]) ->
    [];
stringify1([T | Tokens]) ->
    [io_lib:format(" ~s", [token_src(T)]) | stringify1(Tokens)].

stringify(L) ->
    [$\s | S] = lists:flatten(stringify1(L)),
    [{string, 1, S}].

%% epp_request(Epp, Request)
%% epp_reply(From, Reply)
%% wait_epp_reply(Epp)
%%  Handle communication with the epp.

epp_request(Epp, Req) ->
    Epp ! {epp_request,self(),Req}.

epp_reply(From, Rep) ->
    From ! {epp_reply,self(),Rep}.

wait_epp_reply(Epp) ->
    receive
	{epp_reply,Epp,Rep} -> Rep
    end.
