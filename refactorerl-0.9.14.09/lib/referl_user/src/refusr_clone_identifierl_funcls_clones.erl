%%% -*- coding: latin-1 -*-

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
%%% The Initial Developer of the Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.
%%% ============================================================================
%%% Module information

%%% @doc Identifying code clones based on software metrics

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_clone_identifierl_funcls_clones).
-vsn("$Rev: 9316 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").
-export([get_clones/0, get_clones/1, default_options/0, find_clones/2]).
-export([make_storage/1, free_storage/0, search/1]).
% @private
-export([classification_worker/3, filter_worker/1]).

-define(unit_tab, clone_candidates_store).

-define(default_subject, all).
-define(default_unit, form).
-define(default_max_rank, 5).

-define(classification_reqs,
        [{5, [{cf, promising}, {exp, perhaps}]},
         {4, [{cf, promising}, {exp, promising}, {lay, impossible}]},
         {3, [{cf, promising}, {exp, promising}, {lay, perhaps}]},
         {2, [{cf, promising}, {exp, promising}, {lay, promising}, {name, impossible}]},
         {1, [{cf, promising}, {exp, promising}, {lay, promising}, {name, promising}]}]).

%%% ============================================================================
%%% Interface

get_clones()->
    get_clones(default_options()).

get_clones(Options) when is_list(Options) ->
    search(Options).

default_options()->
    [{subject, ?default_subject},
     {unit, ?default_unit},
     {max_rank, ?default_max_rank}].

%%% ============================================================================
%%% Controllers

search(Options)->
    NumOfUnits = make_storage(Options),
    Candidates = NumOfUnits * (NumOfUnits -1) /2,
    Clones = find_clones(NumOfUnits, Options),
    free_storage(),
    FilteredClones = filter_out_clones(Clones, Options),
    OrdClones = order_clones(FilteredClones),
    ListClones = lists:map(fun(K) -> [K] end, OrdClones),
    GroupedClones = ?Lib:group_clones(ListClones),
    [{analysed_candidates_num, Candidates},
     {detected_clones_num, length(GroupedClones)},
     {detected_clones, GroupedClones}].

filter_out_clones(Clones, Options) ->
    case proplists:get_value(unit, Options) of
        form ->
            make_filter_cache(),
            MakeProperArgs = fun(Arg)-> [Arg] end,
            Res = ?MISC:parallelise(Clones, ?MODULE, filter_worker, MakeProperArgs, false, low),
            free_filter_cache(),
            Res;
        _ ->
            Clones
    end.

order_clones(Clones) ->
    Ordering = fun(#clone_item{score=A}, #clone_item{score=B}) ->
                   A=<B;
                 (A,B) ->
                     A=<B
                end,
    lists:sort(Ordering, Clones).

find_clones(NumOfUnits, Options)->
    Candidates = candidates(NumOfUnits,
                            proplists:get_value(subject, Options, ?default_subject)),
    make_caches(),
    Result = find_clones0(Candidates, Options),
    free_caches(),
    Result.

find_clones0(Cands, Options)->
    Unit = proplists:get_value(unit, Options, ?default_unit),
    MaxRank =  proplists:get_value(max_rank, Options, ?default_max_rank),

    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end,
    Result = ?MISC:parallelise(Cands, ?MODULE, classification_worker, MakeProperArgs, low),
    {_, DupsLists} = lists:unzip(Result),
    [Dup || Dup <-DupsLists, Dup/=[]].

candidates(Max, all)->
    lists:flatten([lists:zip(lists:duplicate(Max-I, I), lists:seq(I+1, Max))
        || I <- lists:seq(1, Max-1)]);
candidates(Max, Units)->
    MatchSpec = ?Lib:make_match_spec(Units),
    Indexes = proplists:get_keys(ets:select(?unit_tab, MatchSpec)),
    lists:flatten([lists:zip(lists:duplicate(Max, Index), lists:seq(1,Max))
        || Index <- Indexes]) -- [{Index, Index} || Index <- Indexes].
%%% ============================================================================
%%% Storage

make_storage(Options)->
    Filter = case proplists:get_value(unit, Options, ?default_unit) of
                 form -> [{file_filter, fun()->[file,moddef] end},
                          % to filter out functions without definition (etc: record_info)
                          {form_filter, fun()->?Query:seq([?Mod:locals(),
                                                           ?Fun:definition(),
                                                           ?Form:func()
                                                           ]) end}];
                            % {form_filter, fun()->?Query:seq([?File:forms(),
                            %                                  ?Form:func()]) end}];
                 _ -> []
             end,
    AlphabetOpts = [{unit, proplists:get_value(unit, Options, ?default_unit)},
                    {type, to_unit}] ++ Filter,
    StorageOpts = [{alphabet_opts, AlphabetOpts},
                   {storage_opts, [named_table]}],
    {_,NumOfUnits}=?Lib:make_unit_storage(?unit_tab, StorageOpts),
    NumOfUnits.

free_storage()->
    ?Lib:free_storage(?unit_tab).

make_caches()->
    ?Metric:make_cache(?cf_metric_tab, #cf_metric_values.node),
    ?Metric:make_cache(?exp_metric_tab, #exp_metric_values.node),
    ?Metric:make_cache(?lay_metric_tab, #lay_metric_values.node),
    ?Metric:make_cache(?name_metric_tab, #name_metric_values.node).

free_caches()->
    Caches = [?cf_metric_tab, ?exp_metric_tab, ?lay_metric_tab, ?name_metric_tab],
    [?Metric:free_cache(Cache) || Cache <- Caches].

make_filter_cache()->
    ?Metric:make_cache(?sw_filter_tab, #sw_filter_values.node).

free_filter_cache()->
    ?Lib:free_storage(?sw_filter_tab).

%%% ============================================================================
%%% Filtering
filter_worker(Clones)->
    lists:filter(fun evaluate_cat_dep_filters/1, Clones).

evaluate_cat_dep_filters(#clone_item{score=1, items = [#unit{id=A}, #unit{id=B}]})->
        % we only want to evaluate nearby identical (located in group 'name')
        % specific filters here
    evaluate_cat_dep_filters0([cf, exp, lay, name_stricter], A, B);
evaluate_cat_dep_filters(#clone_item{score=2, items = [#unit{id=A}, #unit{id=B}]})->
        % we only want to evaluate identical (located in group 'name_stricter')
        % and layout specific filters here
        evaluate_cat_dep_filters0([cf, exp, name], A, B);
evaluate_cat_dep_filters(#clone_item{score=Score, items = [#unit{id=A}, #unit{id=B}]})->
    Reqs = proplists:get_value(Score, ?classification_reqs,[]),
    NeqCats = lists:flatten([
        case proplists:get_value(Cat, Reqs, impossible) of
            impossible -> Cat;
            _ -> []
        end || Cat <- ?categories]),
    evaluate_cat_dep_filters0(NeqCats, A, B);
evaluate_cat_dep_filters(_)->
    false.

evaluate_cat_dep_filters0(NeqCats, A, B)->
     Metrics = [Ms || {Cat, Ms} <- ?Metric:available_sw_filters(),
                                         not lists:member(Cat, NeqCats)],
    lists:all(fun({Calc, Arb})->
                try
                    Arb({Calc(A), Calc(B)})
                catch
                    _:_ -> false
                end
              end, lists:flatten(Metrics)).


%%% ============================================================================
%%% Classification
classification_worker(Unit, MaxRank, Candidates) ->
    classification_worker(Unit, MaxRank+1, Candidates, 1, []).

classification_worker(_, _, [], _, Clones) ->
    Clones;

classification_worker(Unit, MaxRank, [_ | Candidates], MaxRank, Clones) ->
    classification_worker(Unit, MaxRank, Candidates, 1, Clones++[[]]);

classification_worker(Unit, MaxRank, Cands = [{K1,K2} | TCands], Rank, Clones) ->
    {U1,U2} = to_unit_pair(K1,K2),
    case catch can_classify(Rank, U1, U2, Unit) of
        false ->
            classification_worker(Unit, MaxRank, Cands, Rank+1, Clones);
        true ->
            NewClones = Clones ++ [#clone_item{items = [U1, U2], score = Rank}],
            classification_worker(Unit, MaxRank, TCands, 1, NewClones);
        _ ->
            classification_worker(Unit, MaxRank, TCands, 1, Clones++[[]])
    end.

can_classify(Rank, U1, U2, Unit) ->
    Reqs = proplists:get_value(Rank, ?classification_reqs, []),
    lists:all(fun(Req)-> sat_req(Req, U1#unit.id, U2#unit.id, Unit) end, Reqs).

to_unit_pair(K1,K2) ->
    Unit1 = ets:lookup_element(?unit_tab, K1, ?unit_data_pos),
    Unit2 = ets:lookup_element(?unit_tab, K2, ?unit_data_pos),
    {Unit1, Unit2}.

% If the arbitrator has detected the maximum amount of similarity between the elements
% of a clone candidate, then its vote is promising.
sat_req({ReqType, promising}, U1, U2, Unit)->
    lists:all(fun({Metric, _})->Metric(U1, Unit) == Metric(U2, Unit) end,
              ?Metric:available_metrics(ReqType));
% The vote is perhaps, if the computed metric values of the elements of
% the investigated clone candidate deviate from each other less than
% the corresponding maximum value.
sat_req({ReqType, perhaps}, U1, U2, Unit)->
    lists:all(fun({Metric, Delta})->
                abs(diff(Metric(U1, Unit) , Metric(U2, Unit)))=< Delta
              end, ?Metric:available_metrics(ReqType));
% If the arbitrator has found no similarities among the elements of a clone candidate,
% then its vote is impossible.
sat_req({ReqType, impossible}, U1, U2, Unit)->
    lists:any(fun({Metric, Delta})->
                abs(diff(Metric(U1, Unit) , Metric(U2, Unit))) > Delta
              end, ?Metric:available_metrics(ReqType)).

%%% ============================================================================
%%% Helpers
diff(A, B) when is_number(A), is_number(B) ->
    A-B;
diff(A,A) when is_list(A)->
    0;
diff(A,B) when is_list(A), is_list(B)->
    length(A -- B);
diff(A,B) when is_atom(A)->
    diff(io_lib:format("~p", [A]), B);
diff(A,B) when is_atom(B)->
    diff(A, io_lib:format("~p", [B])).
