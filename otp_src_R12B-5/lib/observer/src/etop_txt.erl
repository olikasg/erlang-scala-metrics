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
-module(etop_txt).
-author('siri@erix.ericsson.se').

%%-compile(export_all).
-export([init/1,stop/1]).
-export([do_update/3]).

-include("etop.hrl").
-include("etop_defs.hrl").

-import(etop,[loadinfo/1,meminfo/2]).
-import(etop_gui,[formatmfa/1,to_list/1]).

-define(PROCFORM,"~-15w~-20s~8w~8w~8w~8w ~-20s~n").

stop(Pid) -> Pid ! stop.

init(Config) ->
    loop(Config).

loop(Config) ->
    Info = do_update(Config),
    receive 
	stop -> stopped;
	{dump,Fd} -> do_update(Fd,Info,Config), loop(Config); 
	{config,_,Config1} -> loop(Config1)
    after Config#opts.intv -> loop(Config)
    end.

do_update(Config) ->
    Info = etop:update(Config),
    do_update(standard_io,Info,Config).

do_update(Fd,Info,Config) ->
    {Cpu,NProcs,RQ,Clock} = loadinfo(Info),
    [Tot,Procs,Atom,Bin,Code,Ets] = 
	meminfo(Info#etop_info.memi,[total,processes,atom,binary,code,ets]),
    io:nl(Fd),
    writedoubleline(Fd),
    io:fwrite(Fd,?SYSFORM, [Config#opts.node,Clock,
			    Cpu,Tot,Bin,
			    NProcs,Procs,Code,
			    RQ,Atom,Ets]),
    io:nl(Fd),
    writepinfo_header(Fd),
    writesingleline(Fd),
    writepinfo(Fd,Info#etop_info.procinfo),
    writedoubleline(Fd),
    io:nl(Fd),
    Info.

writepinfo_header(Fd) ->
    io:fwrite(Fd,"Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function~n",[]).

writesingleline(Fd) ->
    io:fwrite(Fd,"----------------------------------------------------------------------------------------~n",[]).
writedoubleline(Fd) ->
    io:fwrite(Fd,"========================================================================================~n",[]).
 
writepinfo(Fd,[#etop_proc_info{pid=Pid,
			       mem=Mem,
			       reds=Reds,
			       name=Name,
			       runtime=Time,
			       cf=MFA,
			       mq=MQ}
	       |T]) ->
    io:fwrite(Fd,?PROCFORM,[Pid,to_list(Name),Time,Reds,Mem,MQ,formatmfa(MFA)]), 
    writepinfo(Fd,T);
writepinfo(_Fd,[]) ->
    ok.

