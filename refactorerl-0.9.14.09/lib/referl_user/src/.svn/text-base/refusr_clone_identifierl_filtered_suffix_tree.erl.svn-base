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

%%% @doc A duplicated code search based on suffix_tree but with different filtering.

%%% @author Szabo Bence <szbtadi@caesar.elte.hu>

-module(refusr_clone_identifierl_filtered_suffix_tree).
-vsn("$Rev: 11297 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

-export([get_clones/0, get_clones/1, default_options/0]).

% @private
-export([clique_worker/1, inv_seq_loop/3, generate_groups/2, filter_worker/3]).

%%miért nem tempfile
-define(IGRAPH_NAME,
        filename:join([?MISC:data_dir(), "clone_identifierl_igraph"])).

-record(group_item, {rows , cols}).
%%% ============================================================================
%%% Interface

get_clones()->
    get_clones(default_options()).

get_clones(Options) when is_list(Options) ->
    search(Options).

default_options() ->
    ?STree:default_options() ++ 
     [{max_invalid_seq_length, 1}].


search(Options) ->
    CloneResult = ?STree:get_clones(Options), 
    Clones = proplists:get_value(detected_clones, CloneResult),
    NormClones = ?Lib:to_tlexpr_clones(Clones, fstree),
    Storage = ?Metric:make_cache(?filter_tab, #filter_values.node),
    FilteredClones = filter_out_clones(NormClones, Options),
    ?Metric:free_cache(Storage),
    [{analysed_candidates_num, no_data},
      {detected_clones_num, length(FilteredClones)},
      {detected_clones, FilteredClones}].


filter_out_clones(Clones, Options) ->
    MakeProperArgs = fun(Arg) -> [Arg, Options, []] end,
    _FilteredClones = 
        ?MISC:parallelise(Clones, ?MODULE, filter_worker, MakeProperArgs, false, normal).


%% @doc Main filtering unit.
filter_worker([], _, Res) -> Res;
filter_worker([Clone | Clones], Options, Res) ->
    Length = length(Clone),
    MaxInvalidSeqLength = proplists:get_value(max_invalid_seq_length, Options),
    Cliques = get_cliques(Clone, Length),
    Isl = lists:reverse(lists:seq(0, MaxInvalidSeqLength)),

    MakeProperArgs = fun(Arg) -> [Cliques, Arg, []] end,
    SGS2 = ?MISC:parallelise(Isl, ?MODULE, inv_seq_loop, MakeProperArgs, false, low),
    SGS2E = make_max_ordset(SGS2++[Cliques]),
    
    SGSn = get_sgsn(SGS2E, Length),
    MakeProperArgs1 = fun(Arg) -> [Arg, Clone] end,
    GroupedClones = ?MISC:parallelise(SGSn, ?MODULE, generate_groups, MakeProperArgs1, false, low),

    Minlen = proplists:get_value(minlen, Options),
    FilterLength = filter_by_length(GroupedClones,Minlen),
    filter_worker(Clones, Options, Res++FilterLength).


%% @doc Loops through the InvalidSequenceLength list and groups.
inv_seq_loop(_, [], Res) -> Res;
inv_seq_loop(Cliques, [Isl | Tail], Res) ->
    NewRes = inv_seq_group(Cliques, Isl, []),
    inv_seq_loop(Cliques, Tail, Res++[NewRes]).


%% @doc Groups the cliques for a specific InvalidSequenceLength.
inv_seq_group([], _, Res) -> Res;
inv_seq_group([Clique | Tail], Isl, Res) ->
    #group_item{rows = Rows, cols = Cols} = Clique,
    Cands = ordsets:filter(
                fun(#group_item{rows = HRows, cols = HCols}) ->
                    lists:min(HRows) - lists:max(Rows) == Isl+1 andalso
                    ordsets:size(ordsets:intersection(Cols,HCols)) >= 2 
            end, Tail),
    NewClique = lists:map(fun(K) -> union_and_intersect(Clique, [K]) end, Cands),
    NewRes = insert_to_res(Res, NewClique),
    inv_seq_group(Tail, Isl, NewRes).


%% @doc Filters clones by lenght.
filter_by_length([], _) -> [];
filter_by_length([Clone | Clones], Minlen) ->
    Length = lists:foldl(fun(X, Sum) ->
                    Items = X#clone_item.items,
                    First = hd(Items),
                    length(First#unit.alphabet) + Sum
                 end, 0, Clone),

    case Length < Minlen of
        true -> filter_by_length(Clones, Minlen);
        _ -> [Clone] ++ filter_by_length(Clones, Minlen)
    end.


%% @doc Makes a maximal ordset from the list.
make_max_ordset(Res) -> 
     make_max_ordset(Res, []).

make_max_ordset([], Res) -> Res;
make_max_ordset([Head | Tail], Res) ->
    NewRes = insert_to_res(Head, Res),
    make_max_ordset(Tail, lists:flatten(NewRes)).


%% @doc Inserts elements to Res if the conditions are good.
insert_to_res([], Res) -> Res;
insert_to_res([H|T], Res) ->
    #group_item{rows = Rows, cols = Cols} = H,
    CanGoIn = lists:any(
                fun(#group_item{rows = KRows, cols = KCols}) -> 
                    ordsets:is_subset(Rows, KRows) andalso
                    ordsets:is_subset(Cols, KCols)
                end, T++Res),
    NewRes = case CanGoIn of
            false -> FilteredRes =
                         lists:filter(
                            fun(#group_item{rows = KRows, cols = KCols}) -> 
                                not(ordsets:is_subset(KRows, Rows)
                                andalso ordsets:is_subset(KCols, Cols))
                            end, Res),
                     ordsets:add_element(H,FilteredRes);
            _ -> Res
        end,
    insert_to_res(T, NewRes).


%% @doc Get the cliques of the Clones and returns them as group_item.
get_cliques(Clones, Length) ->
    CloneNums = lists:zip(Clones, lists:seq(1, Length)),
    MakeProperArgs = fun(Arg)-> [Arg] end,
    Result = ?MISC:parallelise(CloneNums, ?MODULE, clique_worker, MakeProperArgs, false, low),
    lists:flatten(Result).


%% @doc Worker unit of the filtering process.
clique_worker([]) -> [];
clique_worker([{Clone, N}| Tail]) ->
    Items = Clone#clone_item.items,
    ZippedItems = lists:zip(Items, lists:seq(1,length(Items))),
    ItemPairs = combos(2, ZippedItems),
    FilteredPairs = filtermap(fun(K) -> filter_return_id(K) end, ItemPairs), 
    IgrpahList = lists:flatten(FilteredPairs),
    Cliques = ?Igraph:maximal_cliques(IgrpahList), 
    OrdCliques = ordsets:from_list(Cliques),
    FullOrdCliques =
        lists:map(
            fun(K) ->
                #group_item{rows = [N], cols = ordsets:from_list(K)}
            end, OrdCliques),
    [FullOrdCliques]++clique_worker(Tail).


%% @doc Calculates the filter value on a pair of items.
filter_return_id([{I1,Id1}, {I2,Id2}]) ->
    case eval_conditions([I1,I2],one_element) of
        false -> {true, [Id1,Id2]};
        _ -> false
    end.


%% @doc Iterates throught Max..0 interval and connects the partresults if possible.
get_sgsn(SGSn, Max) when Max == 0 -> SGSn;
get_sgsn(SGSn, Max) -> 
    SGSnE = seq_group(SGSn,[]),
    NewSGS = make_max_ordset([SGSn]++SGSnE),
    get_sgsn(NewSGS, Max-1).

%% @doc Does an iteration of glueing the SGSn-s.
seq_group(Cliques, Res) ->
    NewRes = seq_group0(Cliques, []),
    Res++[NewRes].
seq_group0([], Res) -> Res;
seq_group0([Clique | Tail], Res) ->
    #group_item{rows = Rows, cols = Cols} = Clique,
    Cands = ordsets:filter(
                fun(#group_item{rows = HRows, cols = HCols}) ->
                    ordsets:intersection(HRows, Rows) /= [] andalso
                    ordsets:size(ordsets:intersection(Cols, HCols)) >= 2 
            end, Tail),
    NewClique = lists:map(fun(K) -> union_and_intersect(Clique, [K]) end, Cands),
    NewRes = insert_to_res(Res, NewClique),
    seq_group0(Tail, NewRes).

%% @doc Creates a group item by intersecting the cols and make min..max rows.
union_and_intersect(Comp, Cands) ->
    Full = ordsets:add_element(Comp,Cands),
    First = hd(Full), 
    MinRow = hd(First#group_item.rows),
    Last = lists:last(Full),
    MaxRow = lists:last(Last#group_item.rows),
    NewRow = lists:seq(MinRow, MaxRow),
    NewCol = intersect_cols(Full),
    NewGroupItem = #group_item{rows = NewRow, cols = NewCol},
    NewGroupItem.


%% @doc Intersects the list of group_item-s cols.
intersect_cols([H|T]) ->
    Cols = H#group_item.cols,
    intersect_cols(T,Cols).
intersect_cols([], Result) -> Result;
intersect_cols([H|T], Result) ->
    Cols = H#group_item.cols,
    intersect_cols(T, ordsets:intersection(Cols,Result)).


%% @doc Generates clone groups from group_items.
generate_groups(SGSnc, Clones) -> 
    lists:map(fun(K) -> generate_group(K, Clones) end, SGSnc).

%% @doc Generates clone group from a gorup_item.
generate_group(Group, Clones) ->
    #group_item{rows = Rows, cols = Cols} = Group,
    get_ci_from_row(Rows,Cols, Clones).

%% @doc Gets the clone_group by rows.
get_ci_from_row([],_,_) -> [];
get_ci_from_row([H|T], Cols, Clones) ->
    ActRow = lists:nth(H, Clones),
    ActRowItems = ActRow#clone_item.items,
    NewItems = lists:map(fun(K) -> lists:nth(K, ActRowItems) end, Cols),
   [#clone_item{items = NewItems}] ++ get_ci_from_row(T, Cols, Clones).



%% @doc Generates every combinations of the list to K length lists.
combos(1, L) -> [[X] || X <-L];
combos(K, L) when K == length(L) -> [L];
combos(K, [H|T]) ->
    [[H | Subcombos] || Subcombos <- combos(K-1, T)]
    ++(combos(K, T)).


%% @doc Evaluates the filtering result to pair.
eval_conditions(Params, one_element)-> 
    eval_conditions(Params, ?Metric:filter(filters_for_one_element_clones_light),false).

eval_conditions(_, _,true)->
    true;
eval_conditions(_, [],Result)->
    Result;
eval_conditions([Unit1,Unit2]=Items, [Filter | Filters], _)-> 
        #filtering_metric{key=Key, calc_fun=CalcFun, arbitrate_fun=ArbFun} = Filter,
    Val1 = ?Metric:filter_value(Unit1, Key, CalcFun),
    Val2 = ?Metric:filter_value(Unit2, Key, CalcFun),
    Result = ArbFun({Val1, Val2}),
    eval_conditions(Items, Filters, Result).


%% @doc Filtermap function from Erlang lists module.
filtermap(F, [Hd|Tail]) ->
    case F(Hd) of
	true ->
	    [Hd|filtermap(F, Tail)];
	{true,Val} ->
	    [Val|filtermap(F, Tail)];
	false ->
	    filtermap(F, Tail)
    end;
filtermap(F, []) when is_function(F, 1) -> [].
