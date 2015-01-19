%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotifyComm_PullSupplier
%% Source: /ldisk/daily_build/otp_prebuild_r12b.2008-11-05_12/otp_src_R12B-5/lib/cosNotification/src/CosNotifyComm.idl
%% IC vsn: 4.2.19
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotifyComm_PullSupplier').
-ic_compiled("4_2_19").


%% Interface functions

%% Exports from "CosNotifyComm::NotifySubscribe"
-export([subscription_change/3, subscription_change/4]).

%% Exports from "CosEventComm::PullSupplier"
-export([pull/1, pull/2, try_pull/1]).
-export([try_pull/2, disconnect_pull_supplier/1, disconnect_pull_supplier/2]).

%% Type identification function
-export([typeID/0]).

%% Used to start server
-export([oe_create/0, oe_create_link/0, oe_create/1]).
-export([oe_create_link/1, oe_create/2, oe_create_link/2]).

%% TypeCode Functions and inheritance
-export([oe_tc/1, oe_is_a/1, oe_get_interface/0]).

%% gen server export stuff
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3]).

-include_lib("orber/include/corba.hrl").


%%------------------------------------------------------------
%%
%% Object interface functions.
%%
%%------------------------------------------------------------



%%%% Operation: subscription_change
%% 
%%   Returns: RetVal
%%   Raises:  CosNotifyComm::InvalidEventType
%%
subscription_change(OE_THIS, Added, Removed) ->
    corba:call(OE_THIS, subscription_change, [Added, Removed], ?MODULE).

subscription_change(OE_THIS, OE_Options, Added, Removed) ->
    corba:call(OE_THIS, subscription_change, [Added, Removed], ?MODULE, OE_Options).

%%%% Operation: pull
%% 
%%   Returns: RetVal
%%   Raises:  CosEventComm::Disconnected
%%
pull(OE_THIS) ->
    corba:call(OE_THIS, pull, [], ?MODULE).

pull(OE_THIS, OE_Options) ->
    corba:call(OE_THIS, pull, [], ?MODULE, OE_Options).

%%%% Operation: try_pull
%% 
%%   Returns: RetVal, Has_event
%%   Raises:  CosEventComm::Disconnected
%%
try_pull(OE_THIS) ->
    corba:call(OE_THIS, try_pull, [], ?MODULE).

try_pull(OE_THIS, OE_Options) ->
    corba:call(OE_THIS, try_pull, [], ?MODULE, OE_Options).

%%%% Operation: disconnect_pull_supplier
%% 
%%   Returns: RetVal
%%
disconnect_pull_supplier(OE_THIS) ->
    corba:call(OE_THIS, disconnect_pull_supplier, [], ?MODULE).

disconnect_pull_supplier(OE_THIS, OE_Options) ->
    corba:call(OE_THIS, disconnect_pull_supplier, [], ?MODULE, OE_Options).

%%------------------------------------------------------------
%%
%% Inherited Interfaces
%%
%%------------------------------------------------------------
oe_is_a("IDL:omg.org/CosNotifyComm/PullSupplier:1.0") -> true;
oe_is_a("IDL:omg.org/CosNotifyComm/NotifySubscribe:1.0") -> true;
oe_is_a("IDL:omg.org/CosEventComm/PullSupplier:1.0") -> true;
oe_is_a(_) -> false.

%%------------------------------------------------------------
%%
%% Interface TypeCode
%%
%%------------------------------------------------------------
oe_tc(subscription_change) -> 'CosNotifyComm_NotifySubscribe':oe_tc(subscription_change);
oe_tc(pull) -> 'CosEventComm_PullSupplier':oe_tc(pull);
oe_tc(try_pull) -> 'CosEventComm_PullSupplier':oe_tc(try_pull);
oe_tc(disconnect_pull_supplier) -> 'CosEventComm_PullSupplier':oe_tc(disconnect_pull_supplier);
oe_tc(_) -> undefined.

oe_get_interface() -> 
	[{"disconnect_pull_supplier", 'CosEventComm_PullSupplier':oe_tc(disconnect_pull_supplier)},
	{"try_pull", 'CosEventComm_PullSupplier':oe_tc(try_pull)},
	{"pull", 'CosEventComm_PullSupplier':oe_tc(pull)},
	{"subscription_change", 'CosNotifyComm_NotifySubscribe':oe_tc(subscription_change)}].




%%------------------------------------------------------------
%%
%% Object server implementation.
%%
%%------------------------------------------------------------


%%------------------------------------------------------------
%%
%% Function for fetching the interface type ID.
%%
%%------------------------------------------------------------

typeID() ->
    "IDL:omg.org/CosNotifyComm/PullSupplier:1.0".


%%------------------------------------------------------------
%%
%% Object creation functions.
%%
%%------------------------------------------------------------

oe_create() ->
    corba:create(?MODULE, "IDL:omg.org/CosNotifyComm/PullSupplier:1.0").

oe_create_link() ->
    corba:create_link(?MODULE, "IDL:omg.org/CosNotifyComm/PullSupplier:1.0").

oe_create(Env) ->
    corba:create(?MODULE, "IDL:omg.org/CosNotifyComm/PullSupplier:1.0", Env).

oe_create_link(Env) ->
    corba:create_link(?MODULE, "IDL:omg.org/CosNotifyComm/PullSupplier:1.0", Env).

oe_create(Env, RegName) ->
    corba:create(?MODULE, "IDL:omg.org/CosNotifyComm/PullSupplier:1.0", Env, RegName).

oe_create_link(Env, RegName) ->
    corba:create_link(?MODULE, "IDL:omg.org/CosNotifyComm/PullSupplier:1.0", Env, RegName).

%%------------------------------------------------------------
%%
%% Init & terminate functions.
%%
%%------------------------------------------------------------

init(Env) ->
%% Call to implementation init
    corba:handle_init('CosNotifyComm_PullSupplier_impl', Env).

terminate(Reason, State) ->
    corba:handle_terminate('CosNotifyComm_PullSupplier_impl', Reason, State).


%%%% Operation: subscription_change
%% 
%%   Returns: RetVal
%%   Raises:  CosNotifyComm::InvalidEventType
%%
handle_call({_, OE_Context, subscription_change, [Added, Removed]}, _, OE_State) ->
  corba:handle_call('CosNotifyComm_PullSupplier_impl', subscription_change, [Added, Removed], OE_State, OE_Context, false, false);

%%%% Operation: pull
%% 
%%   Returns: RetVal
%%   Raises:  CosEventComm::Disconnected
%%
handle_call({_, OE_Context, pull, []}, _, OE_State) ->
  corba:handle_call('CosNotifyComm_PullSupplier_impl', pull, [], OE_State, OE_Context, false, false);

%%%% Operation: try_pull
%% 
%%   Returns: RetVal, Has_event
%%   Raises:  CosEventComm::Disconnected
%%
handle_call({_, OE_Context, try_pull, []}, _, OE_State) ->
  corba:handle_call('CosNotifyComm_PullSupplier_impl', try_pull, [], OE_State, OE_Context, false, false);

%%%% Operation: disconnect_pull_supplier
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, disconnect_pull_supplier, []}, _, OE_State) ->
  corba:handle_call('CosNotifyComm_PullSupplier_impl', disconnect_pull_supplier, [], OE_State, OE_Context, false, false);



%%%% Standard gen_server call handle
%%
handle_call(stop, _, State) ->
    {stop, normal, ok, State};

handle_call(_, _, State) ->
    {reply, catch corba:raise(#'BAD_OPERATION'{minor=1163001857, completion_status='COMPLETED_NO'}), State}.


%%%% Standard gen_server cast handle
%%
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.


%%%% Standard gen_server handles
%%
handle_info(_, State) ->
    {noreply, State}.


code_change(OldVsn, State, Extra) ->
    corba:handle_code_change('CosNotifyComm_PullSupplier_impl', OldVsn, State, Extra).

