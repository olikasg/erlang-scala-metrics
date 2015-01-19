%%<copyright>
%% <year>1997-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
-module(klas3).
-compile(export_all).

ftab(new) ->
   mnesia:create_table([{name, friendsTable3},
			{snmp, [{key, integer}]},
			{attributes, [a1,a2]}]);

ftab(delete) ->
    ok.


ftab(is_set_ok, [2], _Cols) ->
%    io:format("calling ftab is_set_ok - fail~n"),
    {inconsistentValue, 2};
ftab(is_set_ok, _, _Cols) ->
%    io:format("calling ftab is_set_ok - ok~n"),
    {noError, 0};

ftab(undo, [3], _Cols) ->
%    io:format("calling ftab undo - fail~n"),
    {undoFailed, 2};

ftab(undo, [1], _Cols) ->
%    io:format("calling ftab undo~n"),
    {noError, 0};

ftab(set, [4], _Cols) ->
    {commitFailed, 2};

ftab(Op, RowIndex, Cols) ->
    snmp_generic:table_func(Op, RowIndex, Cols, {friendsTable3, mnesia}).


fname(new) -> ok;
fname(delete) -> ok;
fname(get) ->
    Str2 = (catch begin
        case snmpa:current_request_id() of
	    {value, Int} when integer(Int) -> ok;
	    {value, _} -> throw("bad_int");
	    _ -> throw("bad_req")
	end,
	case snmpa:current_community() of
	    {value, Str} when list(Str) -> Str;
	    {value, _} -> throw("bad_str");
	    _ -> throw("bad_com")
	end,
	case snmpa:current_address() of
	    {value, {[_A,_B,_C,_D], E}} when integer(E) -> ok;
	    {value, _} -> throw("bad_ip");
	    _ -> throw("bad_adr")
	end,
	case snmpa:current_net_if_data() of
	    {value, []} -> ok;
	    {value, _} -> throw("bad_nil");
	    _ -> throw("bad_nid")
	end,
	"ok"
    end),
    {value, Str2}.

fname(is_set_ok, "hoj") ->
    inconsistentValue;
fname(is_set_ok, "xfail") ->
    i_know_this_is_wrong_it_should_be_user_error;
fname(is_set_ok, _) ->
%    io:format("calling fname is_set_ok~n"),
    noError;
fname(set, "fel") -> commitFailed;
fname(set, _) -> noError;
fname(undo, "ufail") ->
%    io:format("calling fname undo - fail~n"),
    undoFailed;
fname(undo, _) ->
%    io:format("calling fname undo~n"),
    noError.

%% snmp_mgr:s([{[fStatus2, 1], 4}, {[fname2,0], "ok"}]). -> noError
%% snmp_mgr:s([{[fStatus2, 1], 4}, {[fname2,0], "hoj"}]). -> {badValue, 2}
%% snmp_mgr:s([{[fStatus2, 3], 4}, {[fname2,0], "hoj"}]). -> {genErr, 1}
%% snmp_mgr:s([{[fStatus2, 4], 4}, {[fname2,0], "ok"}]). -> {genErr, 1}
%% snmp_mgr:s([{[fStatus2, 4], 4}, {[fname2,0], "ufail"}]). -> {genErr, 1}
%% snmp_mgr:s([{[fStatus2, 1], 4}, {[fname2,0], "xfail"}]). -> {genErr, 2}


fname4(get) ->
    {value, none}.


ftab2(_) ->
    ok.


%% Following 2 clauses is for OTP-1222
ftab2(is_set_ok, [1], _Cols) ->
    % bad column - In: col 2 & 3
    io:format("** Here comes Error Report is_set_ok bad column~n"),
    {inconsistentValue, 1};
ftab2(is_set_ok, _, _Cols) ->
    {noError, 0};

ftab2(set, [2], _Cols) ->
    % bad column - In: col 2 & 3
    io:format("** Here comes Error Report set bad column~n"),
    {commitFailed, 4};
ftab2(set, _, _Cols) ->
    {noError, 0};

%% Unfortunatly we can't force the undo - we don't know which var
%% is tried first.
%ftab2(undo, [3], Cols) ->
%    % bad column - In: col 2 & 3
%    io:format("** Here comes (no Error Report) undo bad column~n"),
%    {undoFailed, 5};
%ftab2(undo, _, Cols) ->
%    {noError, 0};

ftab2(get, [4], _Cols) ->
    % bad return value
    io:format("** Here comes Error Report get 1 bad return~n"),
    [];
ftab2(get, [5], _Cols) ->
    % bad return value
    io:format("** Here comes Error Report get 2 bad return~n"),
    [{value, 1}];
ftab2(get, [6], _Cols) ->
    % bad return value
    io:format("** Here comes Error Report get 3 bad return~n"),
    [{value,1},{value,2},{value,3}];

ftab2(get_next, [7], _Cols) ->
    % bad return value
    io:format("** Here comes Error Report get_next 1 bad return~n"),
    [];
ftab2(get_next, [8], _Cols) ->
    % bad return value
    io:format("** Here comes Error Report get_next 2 bad return~n"),
    [endOfTable];
ftab2(get_next, [9], _Cols) ->
    % bad return value
    io:format("** Here comes Error Report get_next 3 bad return~n"),
    [{[1,5],1},{[2,5],3},{[2,6],3}].    
