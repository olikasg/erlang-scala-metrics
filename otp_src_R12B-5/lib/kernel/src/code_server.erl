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
-module(code_server).

%% This file holds the server part of the code_server.

-export([start_link/1,
	 call/2,
	 system_continue/3, 
	 system_terminate/4,
	 system_code_change/4,
	 error_msg/2, info_msg/2
	]).

-include_lib("kernel/include/file.hrl").

-import(lists, [foreach/2]).

-record(state,{supervisor,
	       root,
	       path,
	       moddb,
	       namedb,
	       cache = no_cache,
	       mode=interactive}).

start_link(Args) ->
    Ref = make_ref(),
    Parent = self(),
    Init = fun() -> init(Ref, Parent, Args) end,
    spawn_link(Init),
    receive 
	{Ref,Res} -> Res
    end.


%% -----------------------------------------------------------
%% Init the code_server process.
%% -----------------------------------------------------------

init(Ref, Parent, [Root,Mode0]) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),

    Db = ets:new(code, [private]),
    foreach(fun (M) -> ets:insert(Db, {M,preloaded}) end, erlang:pre_loaded()),
    ets:insert(Db, init:fetch_loaded()),

    Mode = 
	case Mode0 of
	    minimal    -> interactive;
	    _          -> Mode0
	end,

    IPath =
	case Mode of
	    interactive ->
		LibDir = filename:append(Root, "lib"),
		{ok,Dirs} = erl_prim_loader:list_dir(LibDir),
		{Paths,_Libs} = make_path(LibDir,Dirs),
		UserLibPaths = get_user_lib_dirs(),
		["."] ++ UserLibPaths ++ Paths;
	    _ ->
		[]
	end,

    Path = add_loader_path(IPath, Mode),
    State0 = #state{root = Root,
		    path = Path,
		    moddb = Db,
		    namedb = init_namedb(Path),
		    mode = Mode},

    State =
	case init:get_argument(code_path_cache) of
	    {ok, _} -> 
		create_cache(State0);
	    error -> 
		State0
	end,

    Parent ! {Ref,{ok,self()}},
    loop(State#state{supervisor=Parent}).

get_user_lib_dirs() ->
    case os:getenv("ERL_LIBS") of
	LibDirs0 when is_list(LibDirs0) ->
	    Sep =
		case os:type() of
		    {win32, _} -> $;;
		    _          -> $:
		end,
	    LibDirs = split_paths(LibDirs0, Sep, [], []),
	    get_user_lib_dirs_1(LibDirs);
	false ->
	    []
    end.

get_user_lib_dirs_1([Dir|DirList]) ->
    case erl_prim_loader:list_dir(Dir) of
	{ok, Dirs} ->
	    {Paths,_Libs} = make_path(Dir, Dirs),
	    %% Only add paths trailing with ./ebin.
	    [P || P <- Paths, filename:basename(P) =:= "ebin"] ++
		get_user_lib_dirs_1(DirList);
	error ->
	    get_user_lib_dirs_1(DirList)
    end;
get_user_lib_dirs_1([]) -> [].


split_paths([S|T], S, Path, Paths) ->
    split_paths(T, S, [], [lists:reverse(Path) | Paths]);
split_paths([C|T], S, Path, Paths) ->
    split_paths(T, S, [C|Path], Paths);
split_paths([], _S, Path, Paths) ->
    lists:reverse(Paths, [lists:reverse(Path)]).

call(Name, Req) ->
    Name ! {code_call, self(), Req},
    receive 
	{?MODULE, Reply} ->
	    Reply
    end.

reply(Pid, Res) ->
    Pid ! {?MODULE, Res}.

loop(#state{supervisor=Supervisor}=State0) ->
    receive 
	{code_call, Pid, Req} ->
	    case handle_call(Req, {Pid, call}, State0) of
		{reply, Res, State} ->
		    reply(Pid, Res),
		    loop(State);
		{noreply, State} ->
		    loop(State);
		{stop, Why, stopped, State} ->
		    system_terminate(Why, Supervisor, [], State)
	    end;
	{'EXIT', Supervisor, Reason} ->
	    system_terminate(Reason, Supervisor, [], State0);
	{system, From, Msg} ->
	    handle_system_msg(running,Msg, From, Supervisor, State0);
	_Msg ->
	    loop(State0)
    end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

handle_system_msg(SysState,Msg,From,Parent,Misc) ->
    case do_sys_cmd(SysState,Msg,Parent, Misc) of
	{suspended, Reply, NMisc} ->
	    gen_reply(From, Reply),
	    suspend_loop(suspended, Parent, NMisc);
	{running, Reply, NMisc} ->
	    gen_reply(From, Reply),
	    system_continue(Parent, [], NMisc)
    end.

gen_reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%%-----------------------------------------------------------------
%% When a process is suspended, it can only respond to system
%% messages.
%%-----------------------------------------------------------------
suspend_loop(SysState, Parent, Misc) ->
    receive
	{system, From, Msg} ->
	    handle_system_msg(SysState, Msg, From, Parent, Misc);
	{'EXIT', Parent, Reason} ->
	    system_terminate(Reason, Parent, [], Misc)
    end.

do_sys_cmd(_, suspend, _Parent, Misc) ->
    {suspended, ok, Misc};
do_sys_cmd(_, resume, _Parent, Misc) ->
    {running, ok, Misc};
do_sys_cmd(SysState, get_status, Parent, Misc) ->
    Status = {status, self(), {module, ?MODULE},
	      [get(), SysState, Parent, [], Misc]},
    {SysState, Status, Misc};
do_sys_cmd(SysState, {debug, _What}, _Parent, Misc) ->
    {SysState,ok,Misc};
do_sys_cmd(suspended, {change_code, Module, Vsn, Extra}, _Parent, Misc0) ->
    {Res, Misc} = 
	case catch ?MODULE:system_code_change(Misc0, Module, Vsn, Extra)  of
	    {ok, Misc1} -> {ok, Misc1};
	    Else -> {{error, Else}, Misc0}
	end,
    {suspended, Res, Misc};
do_sys_cmd(SysState, Other, _Parent, Misc) ->
    {SysState, {error, {unknown_system_msg, Other}}, Misc}.

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(_Reason, _Parent, _Debug, _State) ->
%    error_msg("~p terminating: ~p~n ",[?MODULE,Reason]),
    exit(shutdown).

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

%%
%% The gen_server call back functions.
%%

handle_call({stick_dir,Dir}, {_From,_Tag}, S) ->
    {reply,stick_dir(Dir, true, S),S};

handle_call({unstick_dir,Dir}, {_From,_Tag}, S) ->
    {reply,stick_dir(Dir, false, S),S};

handle_call({stick_mod,Mod}, {_From,_Tag}, S) ->
    {reply,stick_mod(Mod, true, S),S};

handle_call({unstick_mod,Mod}, {_From,_Tag}, S) ->
    {reply,stick_mod(Mod, false, S),S};

handle_call({dir,Dir},{_From,_Tag}, S) ->
    Root = S#state.root,
    Resp = do_dir(Root,Dir,S#state.namedb),
    {reply,Resp,S};

handle_call({load_file,Mod},{_From,_Tag}, S) ->
    case modp(Mod) of
	false ->
	    {reply,{error, badarg},S};
	true ->
	    {St,Status} = load_file(Mod, S),
	    {reply,Status,St}
    end;

handle_call({add_path,Where,Dir0}, {_From,_Tag}, S=#state{cache=Cache0}) ->
    case Cache0 of
	no_cache ->
	    {Resp,Path} = add_path(Where, Dir0, S#state.path, S#state.namedb),
	    {reply,Resp,S#state{path=Path}};
	_ ->
	    Dir = absname(Dir0), %% Cache always expands the path 
	    {Resp,Path} = add_path(Where, Dir, S#state.path, S#state.namedb),
	    Cache=update_cache([Dir],Where,Cache0),
	    {reply,Resp,S#state{path=Path,cache=Cache}}
    end;

handle_call({add_paths,Where,Dirs0}, {_From,_Tag}, S=#state{cache=Cache0}) ->
    case Cache0 of
	no_cache ->
	    {Resp,Path} = add_paths(Where,Dirs0,S#state.path,S#state.namedb),
	    {reply,Resp, S#state{path=Path}};
	_ ->
	    %% Cache always expands the path 
	    Dirs = [absname(Dir) || Dir <- Dirs0], 
	    {Resp,Path} = add_paths(Where, Dirs, S#state.path, S#state.namedb),
	    Cache=update_cache(Dirs,Where,Cache0),
	    {reply,Resp,S#state{cache=Cache,path=Path}}
    end;

handle_call({set_path,PathList}, {_From,_Tag}, S) ->
    Path = S#state.path,
    {Resp, NewPath,NewDb} = set_path(PathList, Path, S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = NewPath, namedb=NewDb})};

handle_call({del_path,Name}, {_From,_Tag}, S) ->
    {Resp,Path} = del_path(Name,S#state.path,S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = Path})};

handle_call({replace_path,Name,Dir}, {_From,_Tag}, S) ->
    {Resp,Path} = replace_path(Name,Dir,S#state.path,S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = Path})};

handle_call(rehash, {_From,_Tag}, S0) ->
    S = create_cache(S0),
    {reply,ok,S};

handle_call(get_path, {_From,_Tag}, S) ->
    {reply,S#state.path,S};

%% Messages to load, delete and purge modules/files.
handle_call({load_abs,File,Mod}, {_From,_Tag}, S) ->
    case modp(File) of
	false ->
	    {reply,{error,badarg},S};
	true ->
	    Status = load_abs(File,Mod,S#state.moddb),
	    {reply,Status,S}
    end;

handle_call({load_binary,Mod,File,Bin}, {_From,_Tag}, S) ->
    Status = do_load_binary(Mod,File,Bin,S#state.moddb),
    {reply,Status,S};

handle_call({load_native_partial,Mod,Bin}, {_From,_Tag}, S) ->
    Result = (catch hipe_unified_loader:load(Mod,Bin)),
    Status = hipe_result_to_status(Result),
    {reply,Status,S};

handle_call({load_native_sticky,Mod,Bin,WholeModule}, {_From,_Tag}, S) ->
    Result = (catch hipe_unified_loader:load_module(Mod,Bin,WholeModule)),
    Status = hipe_result_to_status(Result),
    {reply,Status,S};

handle_call({ensure_loaded,Mod0}, {_From,_Tag}, St0) ->
    Fun = fun (M, St) ->
		  case erlang:module_loaded(M) of
		      true ->
			  {St, {module,M}};
		      false when St#state.mode =:= interactive ->
			  load_file(M, St);
		      false -> 
			  {St, {error,embedded}}
		  end
	  end,
    do_mod_call(Fun, Mod0, {error,badarg}, St0);

handle_call({delete,Mod0}, {_From,_Tag}, S) ->
    Fun = fun (M, St) ->
		  case catch erlang:delete_module(M) of
		      true ->
			  ets:delete(St#state.moddb, M),
			  {St, true};
		      _ -> 
			  {St, false}
		  end
	  end,
    do_mod_call(Fun, Mod0, false, S);

handle_call({purge,Mod0}, {_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) -> {St, do_purge(M)} end, Mod0, false, St0);

handle_call({soft_purge,Mod0},{_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) -> {St,do_soft_purge(M)} end, Mod0, true, St0);

handle_call({is_loaded,Mod0},{_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) -> {St, is_loaded(M, St#state.moddb)} end, Mod0, false, St0);

handle_call(all_loaded, {_From,_Tag}, S) ->
    Db = S#state.moddb,
    {reply,all_loaded(Db),S};

handle_call({get_object_code,Mod0}, {_From,_Tag}, St0) ->
    Fun = fun(M, St) ->
		  Path = St#state.path,
		  case mod_to_bin(Path, atom_to_list(M)) of
		      {_,Bin,FName} -> {St,{M,Bin,FName}};
		      Error -> {St,Error}
		  end
	  end,
    do_mod_call(Fun, Mod0, error, St0);

handle_call({is_sticky, Mod}, {_From,_Tag}, S) ->
    Db = S#state.moddb,
    {reply, is_sticky(Mod,Db), S};

handle_call(stop,{_From,_Tag}, S) ->
    {stop,normal,stopped,S};

handle_call({is_cached,_File}, {_From,_Tag}, S=#state{cache=no_cache}) ->
    {reply, no, S};

handle_call({set_primary_archive, File, ArchiveBin}, {_From,_Tag}, S=#state{mode=Mode}) ->
    case erl_prim_loader:set_primary_archive(File, ArchiveBin) of
	{ok, Files} ->
	    {reply, {ok, Mode, Files}, S};
	{error, Reason} ->
	    {reply, {error, Reason}, S}
    end;

handle_call({is_cached,File}, {_From,_Tag}, S=#state{cache=Cache}) ->
    ObjExt = objfile_extension(),
    Ext = filename:extension(File),
    Type = case Ext of
	       ObjExt -> obj;
	       ".app" -> app;
	       _ -> undef
	   end,
    if Type =:= undef -> 
	    {reply, no, S};
       true ->
	    Key = {Type,list_to_atom(filename:rootname(File, Ext))},
	    case ets:lookup(Cache, Key) of
		[] -> 
		    {reply, no, S};
		[{Key,Dir}] ->
		    {reply, Dir, S}
	    end
    end;

handle_call(Other,{_From,_Tag}, S) ->			
    error_msg(" ** Codeserver*** ignoring ~w~n ",[Other]),
    {noreply,S}.

% handle_cast(_,S) ->
%     {noreply,S}.
% handle_info(_,S) ->
%     {noreply,S}.

do_mod_call(Action, Module, _Error, St0) when is_atom(Module) ->
    {St, Res} = Action(Module, St0),
    {reply,Res,St};
do_mod_call(Action, Module, Error, St0) ->
    case catch list_to_atom(Module) of
	{'EXIT',_} ->
	    {reply,Error,St0};
	Atom when is_atom(Atom) ->
	    {St, Res} = Action(Atom, St0),
	    {reply,Res,St}
    end.

%% --------------------------------------------------------------
%% Cache functions 
%% --------------------------------------------------------------

create_cache(St = #state{cache = no_cache}) ->
    Cache = ets:new(code_cache, [protected]),
    rehash_cache(Cache, St);
create_cache(St) ->
    rehash_cache(St).

rehash_cache(St = #state{cache = no_cache}) ->
    St;
rehash_cache(St = #state{cache = OldCache}) ->
    ets:delete(OldCache), 
    Cache = ets:new(code_cache, [protected]),
    rehash_cache(Cache, St).

rehash_cache(Cache, St = #state{path = Path}) ->
    Exts = [{obj,objfile_extension()}, {app,".app"}],
    {Cache,NewPath} = locate_mods(lists:reverse(Path), first, Exts, Cache, []),
    St#state{cache = Cache, path=NewPath}.

update_cache(Dirs, Where, Cache0) ->
    Exts = [{obj,objfile_extension()}, {app,".app"}],
    {Cache, _} = locate_mods(Dirs, Where, Exts, Cache0, []),
    Cache.

locate_mods([Dir0|Path], Where, Exts, Cache, Acc) ->
    Dir = absname(Dir0), %% Cache always expands the path 
    case erl_prim_loader:list_dir(Dir) of
	{ok, Files} -> 
	    Cache = filter_mods(Files, Where, Exts, Dir, Cache),
	    locate_mods(Path, Where, Exts, Cache, [Dir|Acc]);
	error ->
	    locate_mods(Path, Where, Exts, Cache, Acc)
    end;
locate_mods([], _, _, Cache, Path) ->
    {Cache,Path}.

filter_mods([File|Rest], Where, Exts, Dir, Cache) ->
    Ext = filename:extension(File),
    Root = list_to_atom(filename:rootname(File, Ext)),
    case lists:keysearch(Ext, 2, Exts) of
	{value,{Type,_}} ->
	    Key = {Type,Root},
	    case Where of
		first ->
		    true = ets:insert(Cache, {Key,Dir});
		last ->
		    case ets:lookup(Cache, Key) of
			[] ->
			    true = ets:insert(Cache, {Key,Dir});
			_ ->
			    ignore
		    end
	    end;
	false ->
	    ok
    end,
    filter_mods(Rest, Where, Exts, Dir, Cache);

filter_mods([], _, _, _, Cache) ->
    Cache.

%% --------------------------------------------------------------
%% Path handling functions.
%% --------------------------------------------------------------

%%
%% Create the initial path. 
%%
make_path(BundleDir,Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir,Bundles,[],[]).

choose_bundles(Bundles) ->
    ArchiveExt = archive_extension(),
    Bs = lists:sort([create_bundle(B,ArchiveExt) || B <- Bundles]),
    [FullName || {_Name,_NumVsn,FullName} <-
		     choose(lists:reverse(Bs), [], ArchiveExt)].

create_bundle(FullName,ArchiveExt) ->
    BaseName = filename:basename(FullName,ArchiveExt),
    case split(BaseName, "-") of
	Toks when length(Toks) > 1 ->
	    VsnStr = lists:last(Toks),
	    case vsn_to_num(VsnStr) of
		{ok, VsnNum} ->
		    Name = join(lists:sublist(Toks,length(Toks)-1),"-"),
		    {Name,VsnNum,FullName};
		false ->
		    {FullName, [0], FullName}
	    end;
	_ ->
	    {FullName,[0],FullName}
    end.

%% Convert "X.Y.Z. ..." to [K, L, M| ...]
vsn_to_num(Vsn) ->
    case is_vsn(Vsn) of
	true ->
	    {ok, [list_to_integer(S) || S <- split(Vsn, ".")]};
	_  ->
	    false
    end.

is_vsn(Str) when is_list(Str) ->
    Vsns = split(Str, "."),
    lists:all(fun is_numstr/1, Vsns).

is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> true; 
		  (_)                       -> false
	      end, Cs).

split(Cs, S) ->
    split1(Cs, S, []).

split1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
	true -> split1(S, Seps, Toks);
	false -> split2(S, Seps, Toks, [C])
    end;
split1([], _Seps, Toks) ->
    lists:reverse(Toks).

split2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
	true -> split1(S, Seps, [lists:reverse(Cs)|Toks]);
	false -> split2(S, Seps, Toks, [C|Cs])
    end;
split2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).
   
join([H1, H2| T], S) ->
    H1 ++ S ++ join([H2| T], S);
join([H], _) ->
    H;
join([], _) ->
    [].

choose([{Name,NumVsn,NewFullName}=New|Bs], Acc, ArchiveExt) ->
    case lists:keysearch(Name,1,Acc) of
	{value, {_, NV, OldFullName}} when NV =:= NumVsn ->
	    case filename:extension(OldFullName) =:= ArchiveExt of
		false ->
		    choose(Bs,Acc, ArchiveExt);
		true ->
		    Acc2 = lists:keystore(Name, 1, Acc, New),
		    choose(Bs,Acc2, ArchiveExt)
	    end;
	{value, {_, _, _}} ->
	    choose(Bs,Acc, ArchiveExt);
	false ->
	    choose(Bs,[{Name,NumVsn,NewFullName}|Acc], ArchiveExt)
    end;
choose([],Acc, _ArchiveExt) ->
    Acc.

make_path(_,[],Res,Bs) ->
    {Res,Bs};
make_path(BundleDir,[Bundle|Tail],Res,Bs) ->
    Dir = filename:append(BundleDir,Bundle),
    Ebin = filename:append(Dir,"ebin"),
    %% First try with /ebin
    case erl_prim_loader:read_file_info(Ebin) of
	{ok,#file_info{type=directory}} ->
	    make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
	_ ->
	    %% Second try with archive
	    case {filename:extension(Dir), archive_extension()} of
		{Ext, Ext} ->
		    Base = filename:basename(Dir, Ext),
		    Ebin2 = filename:join([Dir, Base, "ebin"]),
		    %% Add the dir in archive if it exists
		    case erl_prim_loader:read_file_info(Ebin2) of
			{ok,#file_info{type=directory}} -> 
			    make_path(BundleDir,Tail,[Ebin2|Res],[Bundle|Bs]);
			_ ->
			    make_path(BundleDir,Tail,Res,Bs)
		    end;
		_ ->
		    case erl_prim_loader:read_file_info(Dir) of
			{ok,#file_info{type=directory}} ->
			    make_path(BundleDir,Tail,[Dir|Res],[Bundle|Bs]);
			_ ->
			    make_path(BundleDir,Tail,Res,Bs)
		    end
	    end
    end.

%%
%% Add the erl_prim_loader path.
%% 
%%
add_loader_path(IPath0,Mode) ->
    {ok,PrimP0} = erl_prim_loader:get_path(),
    case Mode of
        embedded ->
            strip_path(PrimP0, Mode);  % i.e. only normalize
        _ ->
            Pa0 = get_arg(pa),
            Pz0 = get_arg(pz),

            Pa = patch_path(Pa0),
            Pz = patch_path(Pz0),
	    PrimP = patch_path(PrimP0),
	    IPath = patch_path(IPath0),

            P = exclude_pa_pz(PrimP,Pa,Pz),
            Path0 = strip_path(P, Mode),
            Path = add(Path0, IPath, []),
            add_pa_pz(Path,Pa,Pz)
    end.

patch_path(Path) ->
    case check_path(Path) of
	{ok, NewPath} -> NewPath;
	{error, _Reason} -> Path
    end.	    

%% As the erl_prim_loader path includes the -pa and -pz
%% directories they have to be removed first !!
exclude_pa_pz(P0,Pa,Pz) ->
    P1 = excl(Pa, P0),
    P = excl(Pz, lists:reverse(P1)),
    lists:reverse(P).

excl([], P) -> 
    P;
excl([D|Ds], P) ->
    excl(Ds, lists:delete(D, P)).

%%
%% Keep only 'valid' paths in code server.
%% Only if mode is interactive, in an embedded
%% system we can't rely on file.
%%

strip_path([P0|Ps], Mode) ->
    P = filename:join([P0]), % Normalize
    case check_path([P]) of
	{ok, [NewP]} ->
	    [NewP|strip_path(Ps, Mode)];
	_ when Mode =:= embedded ->
	    [P|strip_path(Ps, Mode)];
	_ ->
	    strip_path(Ps, Mode)
    end;
strip_path(_, _) ->
    [].
    
%%
%% Add only non-existing paths.
%% Also delete other versions of directories,
%% e.g. .../test-3.2/ebin should exclude .../test-*/ebin (and .../test/ebin).
%% Put the Path directories first in resulting path.
%%
add(Path,["."|IPath],Acc) ->
    RPath = add1(Path,IPath,Acc),
    ["."|lists:delete(".",RPath)];
add(Path,IPath,Acc) ->
    add1(Path,IPath,Acc).

add1([P|Path],IPath,Acc) ->
    case lists:member(P,Acc) of
	true ->
	    add1(Path,IPath,Acc); % Already added
	false ->
	    IPath1 = exclude(P,IPath),
	    add1(Path,IPath1,[P|Acc])
    end;
add1(_,IPath,Acc) ->
    lists:reverse(Acc) ++ IPath.

add_pa_pz(Path0, Patha, Pathz) ->
    {_,Path1} = add_paths(first,Patha,Path0,false),
    {_,Path2} = add_paths(first,Pathz,lists:reverse(Path1),false),
    lists:reverse(Path2).

get_arg(Arg) ->
    case init:get_argument(Arg) of
	{ok, Values} ->
	    lists:append(Values);
	_ ->
	    []
    end.

%%
%% Exclude other versions of Dir or duplicates.
%% Return a new Path.
%%
exclude(Dir,Path) ->
    Name = get_name(Dir),
    [D || D <- Path, 
	  D =/= Dir, 
	  get_name(D) =/= Name].

%%
%% Get the "Name" of a directory. A directory in the code server path
%% have the following form: .../Name-Vsn or .../Name
%% where Vsn is any sortable term (the newest directory is sorted as
%% the greatest term).
%%
%%
get_name(Dir) ->
    get_name2(get_name1(Dir), []).

get_name1(Dir) ->
    case lists:reverse(filename:split(Dir)) of
	["ebin",DirName|_] -> DirName;
	[DirName|_]        -> DirName;
	_                  -> ""        % No name !
    end.

get_name2([$-|_],Acc) -> lists:reverse(Acc);
get_name2([H|T],Acc)  -> get_name2(T,[H|Acc]);
get_name2(_,Acc)      -> lists:reverse(Acc).

check_path(Path) ->
    PathChoice = init:code_path_choice(),
    ArchiveExt = archive_extension(),
    do_check_path(Path, PathChoice, ArchiveExt, []).
    
do_check_path([], _PathChoice, _ArchiveExt, Acc) -> 
    {ok, lists:reverse(Acc)};
do_check_path([Dir | Tail], PathChoice = strict, ArchiveExt, Acc) ->
    %% Be strict. Use dir as explicitly as stated
    case catch erl_prim_loader:read_file_info(Dir) of
	{ok, #file_info{type=directory}} -> 
	    do_check_path(Tail, PathChoice, ArchiveExt, [Dir | Acc]);
	_ ->
	    {error, bad_directory}
    end;
do_check_path([Orig | Tail], PathChoice = relaxed, ArchiveExt, Acc) ->
    %% Be relaxed
    case catch lists:reverse(filename:split(Orig)) of
	{'EXIT', _} ->
	    {error, bad_directory};
	["ebin", App, OptArchive | RevTop] ->
	    Ext = filename:extension(OptArchive),
	    Base = filename:basename(OptArchive, Ext),
	    if
		Ext =:= ArchiveExt, Base =:= App ->
		    %% Orig archive
		    Archive = Orig,
		    Top = lists:reverse(RevTop),
		    Dir = filename:join(Top ++ [App, "ebin"]);
		true ->
		    %% Orig directory
		    Dir = Orig,
		    Top = lists:reverse([OptArchive | RevTop]),
		    Archive = filename:join(Top ++ [App ++ Ext, App, "ebin"])
	    end,
	    %% First try dir, second try archive and at last use orig if both fails.
	    case erl_prim_loader:read_file_info(Dir) of
		{ok, #file_info{type = directory}} ->
		    do_check_path(Tail, PathChoice, ArchiveExt, [Dir | Acc]);
		_ ->
		    case erl_prim_loader:read_file_info(Archive) of
			{ok, #file_info{type = directory}} ->
			    do_check_path(Tail, PathChoice, ArchiveExt, [Archive | Acc]);
			_ ->
			    do_check_path(Tail, PathChoice, ArchiveExt, [Orig | Acc])
		    end
	    end;    
	_ ->
	    do_check_path(Tail, PathChoice, ArchiveExt, [Orig | Acc])
    end.

%%
%% Add new path(s).
%%
add_path(Where,Dir,Path,NameDb) when is_atom(Dir) ->
    add_path(Where,atom_to_list(Dir),Path,NameDb);
add_path(Where,Dir0,Path,NameDb) when is_list(Dir0) ->
    case int_list(Dir0) of
	true ->
	    Dir = filename:join([Dir0]), % Normalize
	    case check_path([Dir]) of
		{ok, [NewDir]} ->
		    {true, do_add(Where,NewDir,Path,NameDb)};
		Error ->
		    {Error, Path}
	    end;
	false ->
	    {{error, bad_directory}, Path}
    end;
add_path(_,_,Path,_) ->
    {{error, bad_directory}, Path}.


%%
%% If the new directory is added first or if the directory didn't exist
%% the name-directory table must be updated.
%% If NameDb is false we should NOT update NameDb as it is done later
%% then the table is created :-)
%%
do_add(first,Dir,Path,NameDb) ->
    update(Dir,NameDb),
    [Dir|lists:delete(Dir,Path)];
do_add(last,Dir,Path,NameDb) ->
    case lists:member(Dir,Path) of
	true ->
	    Path;
	false ->
	    maybe_update(Dir,NameDb),
	    Path ++ [Dir]
    end.

%% Do not update if the same name already exists !
maybe_update(Dir,NameDb) ->
    case lookup_name(get_name(Dir),NameDb) of
        false -> update(Dir,NameDb);
        _     -> false
    end.

update(_Dir, false) ->
    ok;
update(Dir,NameDb) ->
    replace_name(Dir,NameDb).



%%
%% Set a completely new path.
%%
set_path(NewPath0, OldPath, NameDb) ->
    NewPath = normalize(NewPath0),
    case check_path(NewPath) of
	{ok, NewPath2} ->
	    ets:delete(NameDb),
	    NewDb = init_namedb(NewPath2),
	    {true, NewPath2, NewDb};
	Error ->
	    {Error, OldPath, NameDb}
    end.

%%
%% Normalize the given path.
%% The check_path function catches erroneous path,
%% thus it is ignored here.
%%
normalize([P|Path]) when is_atom(P) ->
    normalize([atom_to_list(P)|Path]);
normalize([P|Path]) when is_list(P) ->
    case int_list(P) of
	true  -> [filename:join([P])|normalize(Path)];
	false -> [P|normalize(Path)]
    end;
normalize([P|Path]) ->
    [P|normalize(Path)];
normalize([]) ->
    [];
normalize(Other) ->
    Other.

%% Handle a table of name-directory pairs.
%% The priv_dir/1 and lib_dir/1 functions will have
%% an O(1) lookup.
init_namedb(Path) ->
    Db = ets:new(code_names,[private]),
    init_namedb(lists:reverse(Path), Db),
    Db.
    
init_namedb([P|Path], Db) ->
    insert_name(P, Db),
    init_namedb(Path, Db);
init_namedb([], _) ->
    ok.

-ifdef(NOTUSED).
clear_namedb([P|Path], Db) ->
    delete_name_dir(P, Db),
    clear_namedb(Path, Db);
clear_namedb([], _) ->
    ok.
-endif.

insert_name(Dir, Db) ->
    case get_name(Dir) of
	Dir  -> false;
	Name -> insert_name(Name, Dir, Db)
    end.

insert_name(Name, Dir, Db) ->
    AppDir = del_ebin(Dir),
    SubDirs = archive_subdirs(AppDir),
    ets:insert(Db, {Name, AppDir, SubDirs}),
    true.

archive_subdirs(AppDir) ->
    IsDir =
	fun(RelFile) ->
		File = filename:join([AppDir, RelFile]),
		case erl_prim_loader:read_file_info(File) of
		    {ok, #file_info{type = directory}} ->
			false;
		    _ ->
			true
		end
	end,
    ArchiveDirs = all_archive_subdirs(AppDir),
    lists:filter(IsDir, ArchiveDirs).

all_archive_subdirs(AppDir) ->
    Ext = archive_extension(),
    ArchiveDir = filename:join([AppDir ++ Ext, 
				filename:basename(AppDir)]),
    case erl_prim_loader:list_dir(ArchiveDir) of
	{ok, Files} ->
	    IsDir =
		fun(RelFile) ->
			File = filename:join([ArchiveDir, RelFile]),
			case erl_prim_loader:read_file_info(File) of
			    {ok, #file_info{type = directory}} ->
				true;
			    _ ->
				false
			end
		end,
	    lists:filter(IsDir, Files);
	_ ->
	    []
    end.

%%
%% Delete a directory from Path.
%% Name can be either the the name in .../Name[-*] or
%% the complete directory name.
%%
del_path(Name0,Path,NameDb) ->
    case catch to_list(Name0)of
	{'EXIT',_} ->
	    {{error,bad_name},Path};
	Name ->
	    case del_path1(Name,Path,NameDb) of
		Path -> % Nothing has changed
		    {false,Path};
		NewPath ->
		    {true,NewPath}
	    end
    end.

del_path1(Name,[P|Path],NameDb) ->
    case get_name(P) of
	Name ->
	    delete_name(Name, NameDb),
	    insert_old_shadowed(Name, Path, NameDb),
	    Path;
	_ when Name =:= P ->
	    case delete_name_dir(Name, NameDb) of
		true -> insert_old_shadowed(get_name(Name), Path, NameDb);
		false -> ok
	    end,
	    Path;
	_ ->
	    [P|del_path1(Name,Path,NameDb)]
    end;
del_path1(_,[],_) ->
    [].

insert_old_shadowed(Name, [P|Path], NameDb) ->
    case get_name(P) of
	Name -> insert_name(Name, P, NameDb);
	_    -> insert_old_shadowed(Name, Path, NameDb)
    end;
insert_old_shadowed(_, [], _) ->
    ok.

%%
%% Replace an old occurrence of an directory with name .../Name[-*].
%% If it does not exist, put the new directory last in Path.
%%
replace_path(Name,Dir,Path,NameDb) ->
    case catch check_pars(Name,Dir) of
	{ok,N,D} ->
	    {true,replace_path1(N,D,Path,NameDb)};
	{'EXIT',_} ->
	    {{error,{badarg,[Name,Dir]}},Path};
	Error ->
	    {Error,Path}
    end.

replace_path1(Name,Dir,[P|Path],NameDb) ->
    case get_name(P) of
	Name ->
	    insert_name(Name, Dir, NameDb),
	    [Dir|Path];
	_ ->
	    [P|replace_path1(Name,Dir,Path,NameDb)]
    end;
replace_path1(Name, Dir, [], NameDb) ->
    insert_name(Name, Dir, NameDb),
    [Dir].

check_pars(Name,Dir) ->
    N = to_list(Name),
    D = filename:join([to_list(Dir)]), % Normalize
    case get_name(Dir) of
	N ->
	    case check_path([D]) of
		{ok, [NewD]} ->
		    {ok,N,NewD};
		Error ->
		    Error
	    end;
	_ ->
	    {error,bad_name}
    end.


del_ebin(Dir) ->
    case filename:basename(Dir) of
	"ebin" -> 
	    Dir2 = filename:dirname(Dir),
	    Dir3 = filename:dirname(Dir2),
	    Ext = archive_extension(),
	    case filename:extension(Dir3) of
		E when E =:= Ext ->
		    %% Strip archive extension
		    filename:join([filename:dirname(Dir3), 
				   filename:basename(Dir3, Ext)]);
		_ ->
		    Dir2
	    end;
	_ ->
	    Dir
    end.



replace_name(Dir, Db) ->
    case get_name(Dir) of
	Dir ->
	    false;
	Name ->
	    delete_name(Name, Db),
	    insert_name(Name, Dir, Db)
    end.

delete_name(Name, Db) ->
    ets:delete(Db, Name).

delete_name_dir(Dir, Db) ->
    case get_name(Dir) of
	Dir  -> false;
	Name ->
	    Dir0 = del_ebin(Dir),
	    case lookup_name(Name, Db) of
		{ok, Dir0, _SubDirs} ->
		    ets:delete(Db, Name), 
		    true;
		_ -> false
	    end
    end.

lookup_name(Name, Db) ->
    case ets:lookup(Db, Name) of
	[{Name, Dir, SubDirs}] -> {ok, Dir, SubDirs};
	_ -> false
    end.


%%
%% Fetch a directory.
%%
do_dir(Root,lib_dir,_) ->
    filename:append(Root, "lib");
do_dir(Root,root_dir,_) ->
    Root;
do_dir(_Root,compiler_dir,NameDb) ->
    case lookup_name("compiler", NameDb) of
	{ok, Dir,_SubDirs} -> Dir;
	_  -> ""
    end;
do_dir(_Root,{lib_dir,Name},NameDb) ->
    case catch lookup_name(to_list(Name), NameDb) of
	{ok, Dir, _SubDirs} -> Dir;
	_         -> {error, bad_name}
    end;
do_dir(_Root,{lib_dir,Name,SubDir0},NameDb) ->
    SubDir = atom_to_list(SubDir0),
    case catch lookup_name(to_list(Name), NameDb) of
	{ok, Dir, SubDirs} -> 
	    case lists:member(SubDir, SubDirs) of
		true ->
		    %% Subdir is in archive
		    filename:join([Dir ++ archive_extension(),
				   filename:basename(Dir),
				   SubDir]);
		false ->
		    %% Subdir is regular directory
		    filename:join([Dir, SubDir])
	    end;
	_  -> 
	    {error, bad_name}
    end;
do_dir(_Root,{priv_dir,Name},NameDb) ->
    do_dir(_Root,{lib_dir,Name,priv},NameDb);
do_dir(_, _, _) ->
    'bad request to code'.

stick_dir(Dir, Stick, St) ->
    case erl_prim_loader:list_dir(Dir) of
	{ok,Listing} ->
	    Mods = get_mods(Listing, objfile_extension()),
	    Db = St#state.moddb,
	    case Stick of
		true ->
		    foreach(fun (M) -> ets:insert(Db, {{sticky,M},true}) end, Mods);
		false ->
		    foreach(fun (M) -> ets:delete(Db, {sticky,M}) end, Mods)
	    end;
	Error -> 
	    Error
    end.

stick_mod(M, Stick, St) ->
    Db = St#state.moddb,
    case Stick of
	true ->
	    ets:insert(Db, {{sticky,M},true});
	false ->
	    ets:delete(Db, {sticky,M})
    end.

get_mods([File|Tail], Extension) ->
    case filename:extension(File) of
	Extension ->
	    [list_to_atom(filename:basename(File, Extension)) |
	     get_mods(Tail, Extension)];
	_ ->
	    get_mods(Tail, Extension)
    end;
get_mods([], _) -> [].

is_sticky(Mod, Db) ->
    case erlang:module_loaded(Mod) of
	true ->
	    case ets:lookup(Db, {sticky,Mod}) of
		[] -> false;
		_  -> true
	    end;
	false ->
	    false
    end.

add_paths(Where,[Dir|Tail],Path,NameDb) ->
    {_,NPath} = add_path(Where,Dir,Path,NameDb),
    add_paths(Where,Tail,NPath,NameDb);
add_paths(_,_,Path,_) ->
    {ok,Path}.


do_load_binary(Module,File,Binary,Db) ->
    case {modp(Module),modp(File)} of
	{true, true} when is_binary(Binary) ->
	    case erlang:module_loaded(to_atom(Module)) of
		true ->
		    do_purge(Module);
		false ->
		    ok
	    end,
	    try_load_module(File, Module, Binary, Db);
	_ ->
	    {error, badarg}
    end.

modp(Atom) when is_atom(Atom) -> true;
modp(List) when is_list(List) -> int_list(List);
modp(_)                       -> false.


load_abs(File, Mod0, Db) ->
    Ext = objfile_extension(),
    FileName0 = lists:concat([File, Ext]),
    FileName = absname(FileName0),
    Mod = if Mod0 =:= [] ->
		  list_to_atom(filename:basename(FileName0, Ext));
	     true ->
		  Mod0
	  end,
    case erl_prim_loader:get_file(FileName) of
	{ok,Bin,_} ->
	    try_load_module(FileName, Mod, Bin, Db);
	error ->
	    {error,nofile}
    end.

try_load_module(Mod, Dir, Db) ->
    File = filename:append(Dir, to_path(Mod) ++ 
			   objfile_extension()),
    case erl_prim_loader:get_file(File) of
	error -> 
	    %% No cache case tries with erl_prim_loader here
	    %% Should the caching code do it??
%           File2 = to_path(Mod) ++ objfile_extension(),
% 	    case erl_prim_loader:get_file(File2) of
% 		error -> 
	    error;     % No more alternatives !
% 		{ok,Bin,FName} ->
% 		    	    try_load_module(absname(FName), Mod, Binary, Db)
% 	    end;
	{ok,Binary,FName} ->
	    try_load_module(absname(FName), Mod, Binary, Db)
    end.

try_load_module(File, Mod, Bin, Db) ->
    M = to_atom(Mod),

    case is_sticky(M, Db) of
	true ->                         %% Sticky file reject the load
	    error_msg("Can't load module that resides in sticky dir\n",[]),
	    {error, sticky_directory};
	false ->
	    case catch load_native_code(Mod, Bin) of
		{module,M} ->
		    ets:insert(Db, {M,File}),
		    {module,Mod};
		no_native ->
		    case erlang:load_module(M, Bin) of
			{module,M} ->
			    ets:insert(Db, {M,File}),
			    post_beam_load(Mod),
			    {module,Mod};
			{error,What} ->
			    error_msg("Loading of ~s failed: ~p\n", [File, What]),
			    {error,What}
		    end;
		Error ->
		    error_msg("Native loading of ~s failed: ~p\n", [File, Error])
	    end
    end.

load_native_code(Mod, Bin) ->
    %% During bootstrapping of Open Source Erlang, we don't have any hipe
    %% loader modules, but the Erlang emulator might be hipe enabled.
    %% Therefore we must test for that the loader modules are available
    %% before trying to to load native code.
    case erlang:module_loaded(hipe_unified_loader) of
	false -> no_native;
	true -> hipe_unified_loader:load_native_code(Mod, Bin)
    end.

hipe_result_to_status(Result) ->
    case Result of
	{module,_} -> Result;
	_ -> {error,Result}
    end.

post_beam_load(Mod) ->
    case erlang:module_loaded(hipe_unified_loader) of
	false -> ok;
	true -> hipe_unified_loader:post_beam_load(Mod)
    end.

int_list([H|T]) when is_integer(H) -> int_list(T);
int_list([_|_])                    -> false;
int_list([])                       -> true.


load_file(Mod, St=#state{path=Path,moddb=Db,cache=no_cache}) ->
    case mod_to_bin(Path, Mod) of
	error -> {St, {error,nofile}};
	{Mod,Binary,File} -> {St,try_load_module(File, Mod, Binary, Db)}
    end;
load_file(Mod, St0=#state{moddb=Db,cache=Cache}) ->
    Key = {obj,Mod},
    case ets:lookup(Cache, Key) of
	[] -> 
	    St = rehash_cache(St0),
	    case ets:lookup(St#state.cache, Key) of
		[] -> 
		    {St, {error,nofile}};
		[{Key,Dir}] ->
		    {St, try_load_module(Mod, Dir, Db)}
	    end;
	[{Key,Dir}] ->
	    {St0, try_load_module(Mod, Dir, Db)}
    end.

mod_to_bin([Dir|Tail], Mod) ->
    File = filename:append(Dir, to_path(Mod) ++ objfile_extension()),
    case erl_prim_loader:get_file(File) of
	error -> 
	    mod_to_bin(Tail, Mod);
	{ok,Bin,FName} ->
	    {Mod,Bin,absname(FName)}
    end;
mod_to_bin([], Mod) ->
    %% At last, try also erl_prim_loader's own method
    File = to_path(Mod) ++ objfile_extension(),
    case erl_prim_loader:get_file(File) of
	error -> 
	    error;     % No more alternatives !
	{ok,Bin,FName} ->
	    {Mod,Bin,absname(FName)}
    end.

absname(File) ->
    case erl_prim_loader:get_cwd() of
	{ok,Cwd} -> absname(File, Cwd);
	_Error -> File
    end.

absname(Name, AbsBase) ->
    case filename:pathtype(Name) of
	relative ->
	    filename:absname_join(AbsBase, Name);
	absolute ->
	    %% We must flatten the filename before passing it into join/1,
	    %% or we will get slashes inserted into the wrong places.
	    filename:join([filename:flatten(Name)]);
	volumerelative ->
	    absname_vr(filename:split(Name), filename:split(AbsBase), AbsBase)
    end.

%% Handles volumerelative names (on Windows only).

absname_vr(["/"|Rest1], [Volume|_], _AbsBase) ->
    %% Absolute path on current drive.
    filename:join([Volume|Rest1]);
absname_vr([[X, $:]|Rest1], [[X|_]|_], AbsBase) ->
    %% Relative to current directory on current drive.
    absname(filename:join(Rest1), AbsBase);
absname_vr([[X, $:]|Name], _, _AbsBase) ->
    %% Relative to current directory on another drive.
    Dcwd =
	case erl_prim_loader:get_cwd([X, $:]) of
	    {ok, Dir}  -> Dir;
	    error -> [X, $:, $/]
    end,
    absname(filename:join(Name), Dcwd).


%% do_purge(Module)
%%  Kill all processes running code from *old* Module, and then purge the
%%  module. Return true if any processes killed, else false.

do_purge(Mod) ->
    do_purge(processes(), to_atom(Mod), false).

do_purge([P|Ps], Mod, Purged) ->
    case erlang:check_process_code(P, Mod) of
	true ->
	    Ref = erlang:monitor(process, P),
	    exit(P, kill),
	    receive
		{'DOWN',Ref,process,_Pid,_} -> ok
	    end,
	    do_purge(Ps, Mod, true);
	false ->
	    do_purge(Ps, Mod, Purged)
    end;
do_purge([], Mod, Purged) ->
    catch erlang:purge_module(Mod),
    Purged.

%% do_soft_purge(Module)
%% Purge old code only if no procs remain that run old code
%% Return true in that case, false if procs remain (in this
%% case old code is not purged)

do_soft_purge(Mod) ->
    catch do_soft_purge(processes(), Mod).

do_soft_purge([P|Ps], Mod) ->
    case erlang:check_process_code(P, Mod) of
	true -> throw(false);
	false -> do_soft_purge(Ps, Mod)
    end;
do_soft_purge([], Mod) ->
    catch erlang:purge_module(Mod),
    true.

is_loaded(M, Db) ->
    case ets:lookup(Db, M) of
	[{M,File}] -> {file,File};
	[] -> false
    end.

%% -------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------

all_loaded(Db) ->
    all_l(Db, ets:slot(Db, 0), 1, []).

all_l(_Db, '$end_of_table', _, Acc) ->
    Acc;
all_l(Db, ModInfo, N, Acc) ->
    NewAcc = strip_mod_info(ModInfo,Acc),
    all_l(Db, ets:slot(Db, N), N + 1, NewAcc).


strip_mod_info([{{sticky,_},_}|T], Acc) -> strip_mod_info(T, Acc);
strip_mod_info([H|T], Acc)              -> strip_mod_info(T, [H|Acc]);
strip_mod_info([], Acc)                 -> Acc.

% error_msg(Format) ->
%     error_msg(Format,[]).
error_msg(Format, Args) ->
    Msg = {notify,{error, group_leader(), {self(), Format, Args}}},
    error_logger ! Msg,
    ok.

info_msg(Format, Args) ->
    Msg = {notify,{info_msg, group_leader(), {self(), Format, Args}}},
    error_logger ! Msg,
    ok.

objfile_extension() ->
    init:objfile_extension().

archive_extension() ->
    init:archive_extension().

to_list(X) when is_list(X) -> X;
to_list(X) when is_atom(X) -> atom_to_list(X).

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_atom(X).

to_path(X) ->
    filename:join(packages:split(X)).
