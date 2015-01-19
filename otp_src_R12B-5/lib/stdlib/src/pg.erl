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
-module(pg).

%% pg provides a process group facility. Messages 
%% can be multicasted to all members in the group

-export([create/1,
	 create/2,
	 standby/2,
	 join/2,
	 send/2,
	 esend/2,
	 members/1,
	 name_to_pid/1,
	 master/1]).


%% Create a brand new empty process group with the master residing 
%% at the local node

create(PgName)    -> 
    catch begin check(PgName),
    Pid = spawn(pg,master,[PgName]),
    global:register_name(PgName,Pid),
    ok end.

%% Create a brand new empty process group with the master 
%% residing at Node

create(PgName,Node)    -> 
    catch begin check(PgName),
    Pid = spawn(Node,pg,master,[PgName]),
    global:register_name(PgName,Pid),
    ok end.

%% Have a process on Node that will act as a standby for
%% the process group manager. So if the node where the
%% manager runs fails, the process group will continue 
%% to function.

standby(_PgName, _Node) ->
    ok.


%% Tell process group PgName that Pid is a new member of the group
%% synchronously return a list of all old members in the group

join(PgName,Pid)  when is_atom(PgName) -> 
    global:send(PgName, {join,self(),Pid}),
    receive
	{_P,{members,Members}} ->
	    Members
    end.

%% Multi cast Mess to all members in the group

send(PgName,Mess) when is_atom(PgName) ->
    global:send(PgName, {send, self(), Mess});
send(Pg,Mess) when is_pid(Pg) ->
    Pg ! {send,self(),Mess}.


%% multi cast a message to all members in the group but ourselves
%% If we are a member

esend(PgName,Mess) when is_atom(PgName) ->
    global:send(PgName,{esend,self(),Mess});
esend(Pg,Mess) when is_pid(Pg) ->
    Pg ! {esend,self(),Mess}.

%% Return the members of the group

members(PgName) when is_atom(PgName) ->
    global:send(PgName, {self() ,members}),
    receive
	{_P,{members,Members}} ->
	    Members
    end;
members(Pg) when is_pid(Pg) ->
    Pg ! {self,members},
    receive
	{_P,{members,Members}} ->
	    Members
    end.

name_to_pid(PgName) when is_atom(PgName) ->
    global:whereis_name(PgName).

master(PgName) ->
    process_flag(trap_exit,true),
    master_loop(PgName,[]).

master_loop(PgName,Members) ->
    receive
	{send,From,Message} ->
	    send_all(Members,{pg_message,From,PgName,Message}),
	    master_loop(PgName,Members);
	{esend,From,Message} ->
	    send_all(lists:delete(From,Members),
		     {pg_message,From,PgName,Message}),
	    master_loop(PgName,Members);
	{join,From,Pid} ->
	    link(Pid),
	    send_all(Members,{new_member,PgName,Pid}),
	    From ! {self(),{members,Members}},
	    master_loop(PgName,[Pid|Members]);
	{From,members} ->
	    From ! {self(),{members,Members}},
	    master_loop(PgName,Members);
	{'EXIT',From,_} ->
	    L =
		case lists:member(From,Members) of
		    true ->
			NewMembers = lists:delete(From,Members),
			send_all(NewMembers, {crashed_member,PgName,From}),
			NewMembers;
		    false ->
			Members
		end,
	    master_loop(PgName,L)
	    
    end.

send_all([],_) -> done;
send_all([P|Tail],M) ->
    P ! M,
    send_all(Tail,M).

%% Check if the process group already exists

check(PgName) ->
    case global:whereis_name(PgName) of
        Pid when is_pid(Pid) -> 
            throw({error,already_created});
        undefined ->
	    ok
    end.
	    
		    
	    

