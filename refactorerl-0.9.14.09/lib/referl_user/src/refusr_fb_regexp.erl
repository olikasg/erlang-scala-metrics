%%% This file is part of RefactorErl.
%%%
%%% RefactorErl is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published
%%% by the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% RefactorErl is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @doc Functionblocks filtered by regular expressions. Heavily relies on module 
%%% {@link refusr_fb_relations} at the relationship examinations.
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_fb_regexp).
-vsn("$Rev: 9568 $").
-export([re/1]).
-export([error_text/2]).


-include("user.hrl").
-include("refusr_lib_relations.hrl").

-define(Basic, ["^(/)[0-9a-zA-Z_./]+/common/[0-9a-zA-Z_.]+/(lib/)?(ebin)$"]).


error_text(written, Message = [C|_]) when is_integer(C)->
    Message;
error_text(_,_)->
    unknown.


%% @spec re(Options::proplists()) -> any()
%% @doc Operates on functional blocks filtered by regular expressions. The type of examination
%% is given in a proplist.<br/> 
%% The Options are the following:
%% ```
%% {type, Type}
%% Type = list | get_rel | cycle | draw
%% '''
%%      - <b>list</b> -Prints out every function block which matches the basic regular expression.<br/>
%%	- <b>get_rel</b> - Decides whether there is a connection between the two given functionblocks. <br/>
%%	- <b>cycle</b> - Checks for cycles in the dependencies between the given functionblock list.<br/>
%%	- <b>draw</b>- Prints out the entire graph or creates a subgraph drawing from the given functionblock list.
%%		 Output file is fb_relations.dot.<br/>
%%
%% ```
%% {regexp, Value}
%% Value = File::string() || [RegExp::string()]
%% '''
%% Unless this option (tuple) is given, the program works with a basic regular expression.
%% The basic rule: <i>&lt;functionblock&gt;/common/&lt;service&gt;/ebin</i>
%% or <i>&lt;functionblock&gt;/common/&lt;service&gt;/lib/ebin</i>.<br/>
%% and regular expression saved for these: ``^(/)[0-9a-zA-Z_./]+/common/[0-9a-zA-Z_.]+/(lib/)?(ebin)$''.
%%
%%      -<b>Value </b> - If the regular expression is given in a file then every single regexp has to be in a newline
%%	    in the file and must follow Perl syntax and semantics 
%%          as the "<a href="http://www.erlang.org/doc/man/re.html"><tt>re</tt></a>" erlang module resembles so.<br/>
%%	    However, the user can give the regular expressions in a list as well.
%%          If there is an error with a regular expression in the file or in the list, it prints out the regexp, 
%%          the error specification, and the position. <br/>
re([])->
    {error, no_options_list_given};
re(Options)->
    ?CatchInternal(
    begin
    	Value = proplists:get_value(regexp, Options), %%user defined regexp
    	Modules = mods_from_fbs(Value, []),		%%modules matching to the regexp
        do_re(Options, Modules)
    end).


do_re(Options, Modules)->
    case proplists:get_value(type, Options) of
    list -> Args = [{Regexp, [element(2,?Graph:data(Mod))|| Mod<-ModList]} || {Regexp, ModList}<-Modules],
        io:format("Matched modules: ~p~n", [Args]),
        io:format("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n"),
        Modules;
    get_rel -> do_fb_relations(get_relations, Modules, Options); 
    check_cycle -> do_fb_relations(check_cycle, Modules, Options); 
    draw -> do_fb_relations(draw, Modules, Options);
    undefined -> {error, no_type_key_given};
    _ -> {error, wrong_type_key}
    end.

mods_from_fbs(undefined, _)->
	mods_from_fbs(?Basic, []);
mods_from_fbs([], Mods)->
	lists:reverse(Mods);
mods_from_fbs([[Element | _] = Hd | Rest], Mods) when is_atom(Element)->
	mods_from_fbs(Rest, [{Hd, get_modules(Hd)} | Mods]);
mods_from_fbs([Hd | Rest],Mods)->
	case filename:extension(Hd) of
		".erl" ->
			mods_from_fbs(Rest, [{Hd, 
				get_files(filename:dirname(Hd),
			 	 filename:basename(Hd))} | Mods]);
		_ ->
			mods_from_fbs(Rest,[{Hd, 
			get_directory_files(Hd)} | Mods])
	end.
			
get_modules(ModuleList)->
	[Mod || {_, Mod} <- ls_path(),
		lists:member(?Mod:name(Mod), ModuleList) == true].

get_files(Directory, Basename)->
	Paths= lists:merge([[filename:join(Dir, File)||
		   File<-element(2, file:list_dir(Dir))] 
			|| Dir<-gen_list([Directory], false)]),
	dir_files2(match(Paths,
		  [Basename]), []).

get_directory_files(Directory)->
	dir_files([element(2, file:list_dir(Dir))
			|| Dir<-gen_list([Directory], false)], []). 

dir_files([], Result)->
	Result;
dir_files([Hd | Rest], Result)->
	dir_files(Rest, dir_files2(Hd, Result)).

dir_files2([], Result)->
	Result;
dir_files2([Hd | Rest], Result)->
	Element = ?Query:exec(
			   ?Mod:find(
			     list_to_atom(
				filename:rootname(
					filename:basename((Hd)))))), %% .erl and .beam files could exist
	case {lists:member(Element, Result), Element} of
		{_, []} ->
			dir_files2(Rest, Result);
		{true, _}->
			dir_files2(Rest, Result);
		{false, _} ->
			dir_files2(Rest, [hd(Element) | Result])			
	end.


%%===========================================================================
gen_list(Value, Bool)->
	sets:to_list(sets:from_list(list(Value, Bool))).

%list(undefined, true)->
%	[];
%list(undefined, false)->
%      match(ls_path(), ?Basic);
%list([], _)->
%      match(ls_path(), ?Basic);
list(Value, _)->
	case file:read_file(Value) of 		%%decide whether regexp is from file or shell
	   {ok, _} ->
    		match(ls_path(), read(Value));
	   {error, _} ->
		match(ls_path(), valid_regexp(Value, []))
	end.

valid_regexp([], Valid)->
	Valid;
valid_regexp([Hd|Rest], Valid)->
	valid_regexp(Rest, 
			  check(re:compile(Hd), Hd,  Valid)).


do_fb_relations(Fun, RegexpMods, Options)->
	io:format("Matched modules: ~p~n", [RegexpMods]),
	Args = [[element(2,?Graph:data(Mod))|| Mod<-ModList] || {_, ModList}<-RegexpMods],
	Custom = lists:flatten([
				[{filename:dirname(
					?File:path(
						hd(?Query:exec(Mod, ?Mod:file())))), Name}
				 || Mod<-ModList]
				|| {{_, ModList}, Name}<-lists:zip(RegexpMods, Args)]
			      ),    %%for fb_relations, examination is not on the whole db
	refusr_fb_relations:Fun([{fb_list, Args}, {custom, Custom} | proplists:delete(type, Options)]).


%% ============================================================================
%% Reading file
read(File)->
    case file:open(File, [read]) of
	{ok, Dev}->
	   try
	      read_line(file:read_line(Dev), Dev, [])
	   after
	       file:close(Dev)
	   end;
	{error, Reason} ->
	    io:format(file:format_error(Reason))
    end.

read_line({ok, "\n"}, File, List)->
    read_line(file:read_line(File), File, List);
read_line({ok, Data}, File,  List)->
    NewData = leave(Data),
    read_line(file:read_line(File), File, check(re:compile(NewData), NewData,  List));
read_line(eof, _, List) ->
    List;
read_line({error, Reason}, _, _) ->
    io:format(file:format_error(Reason)).

check({ok, _}, Data, List)->
    [Data | List];
check({error, {ErrString, Position}}, Data, List)->
    io:format("In regexp: ~s~nError: ~s at position ~p~n ", [Data, ErrString, Position]),
    List.

leave(Data)->
    string:substr(Data, 1, string:len(Data)-1).


%%==========================================================================
%% Matching with regular expressions
match([{_, _} | _] = PathList, RegExpList)->
    [Path|| {Path, _} <- PathList,
	    RegExp <- RegExpList,
	    match_single(Path, RegExp) == true];
match(PathList, RegExpList)->
    [Path|| Path <- PathList,
	    RegExp <- RegExpList,
	    match_single(Path, RegExp) == true].

match_single(Path, RegExp)->
     regexp(re:run(Path, RegExp, [])).


regexp(nomatch)-> false;
regexp({match, [{0, _} | _]})-> true;
regexp({match, [{_, _} | _]})-> true.

%%======================================================================
%% Listing every path
ls_path()->
    lists:usort([{filename:dirname(?File:path(Path)), Mod} || Mod <- ?Query:exec(?Mod:all()), 
						 Path <- ?Query:exec(Mod, ?Mod:file())]).
