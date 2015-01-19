%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_ceach.erl
%%  Module   :	hipe_ceach
%%  Purpose  :  Compile each function in a module, possibly
%%              applying a fun between each compilation.
%%              Useful for bug-hunting by pinpointing a function 
%%              that when compiled causes a bug.
%%
%%  Notes    : 
%%  History  :	* 2001-12-11 Erik Johansson (happi@it.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2008/09/14 11:36:25 $
%%              $Revision: 1.8 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_ceach).

-export([c/1, c/2, c/3]).

-include("../main/hipe.hrl").

-spec c(atom()) -> 'ok'.
c(M) ->
  lists:foreach(fun({F,A}) -> comp(M, F, A) end,
		M:module_info(functions)).

-spec c(atom(), comp_options()) -> 'ok'.
c(M, Opts) ->
  lists:foreach(fun({F,A}) -> comp(M, F, A, Opts) end,
		M:module_info(functions)).

-spec c(atom(), comp_options(), fun(() -> any())) -> 'ok'.
c(M, Opts, Fn) ->
  lists:foreach(fun({F,A}) -> comp(M, F, A, Opts), Fn() end,
		M:module_info(functions)).

-spec comp(atom(), atom(), byte()) -> 'ok'.
comp(M, F, A) ->
  io:format("~w:~w/~w... ", [M, F, A]),
  MFA = {M, F, A},
  {ok, MFA} = hipe:c(MFA),
  io:format("OK\n").

-spec comp(atom(), atom(), byte(), comp_options()) -> 'ok'.
comp(M, F, A, Opts) ->
  io:format("~w:~w/~w... ", [M, F, A]),
  MFA = {M, F, A},
  {ok, MFA} = hipe:c(MFA, Opts),
  io:format("OK\n").
