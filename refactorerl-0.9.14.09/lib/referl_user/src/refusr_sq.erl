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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @author Lilla Haj�s <lya@elte.hu>

-module(refusr_sq).
-vsn("$Rev: 12359 $ ").

-export([run/3]).
-export([format_nodes/2]).
-export([prepare/1, error_text/2]).
-export([closure_worker/5, chains_worker/5, chains_worker_first/5]).
-export([pmap_helper/2]).
-export([process_query/3, process_parallel/1, process_parallel/3]).

-include("user.hrl").
-include("sq_lib.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(Lib, refusr_sq_lib).
-define(Format, refusr_sq_format).


%%% ============================================================================
%%% Errors

error_text(lexical_error, Error) ->
    refusr_sq_lexer:format_error(Error);
error_text(syntax_error, Error) ->
    refusr_ac_parser:format_error(Error);
error_text(illegal_property, Params) ->
    io_lib:format("illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("illegal ~p selector: ~p  ", Params);
error_text(illegal_operator, Params) ->
    io_lib:format("illegal ~p operator: ~p  ", Params);
error_text(bad_type, [Type, Expr]) ->
    io_lib:format("unexpected type (~p) for ~p", [Type, Expr]);
error_text(type_mismatch, [At, Of, ExpType]) ->
    [ case At of
          {Pl,De} -> io_lib:format("type mismatch at ~p ~p :\n    ", [Pl,De]);
          Pl -> io_lib:format("type mismatch at ~p:\n    ", [Pl])
      end,
      case Of of
          {Ent, Type} -> io_lib:format("the type of ~p is ~s, not ~s",
                                       [Ent, Type, ExpType]);
          Ent -> io_lib:format("the type of ~p is not ~s", [Ent, ExpType])
      end ];
error_text(no_property_in_comparison, []) ->
    "no property in comparison";
error_text(statistics_error, []) ->
    "statistics are only available for properties with numeric values";
error_text(bad_regexp, [])->
    "illegal regexp";
error_text(bad_typing,[]) ->
    "bad grouping type, else the value is less than or equal to 0.";    
error_text(grouping_error, []) ->
     "grouping value run out of the query's length";
error_text(mult_error, []) ->
    "multiplicity must be greater than 0";
error_text(unbound_variable, Variable) ->
    io_lib:format("unbound variable: '~s' must be bound with a property first",
                  [Variable]);
error_text(unsupported_feature, Feature) ->
    io_lib:format("unsupported feature: ~s", [Feature]);
error_text(ets_limit_reached, []) ->
    "Query cannot be executed, because system limit (related to the number of ETS tables) would be reached. You might try it again using smaller database.".
%%% ============================================================================
%%% Callbacks

%% @spec run(DisplayOpt::proplist(), Params::proplist(), Query::string()) ->
%%           QueryResult::term()
%% @doc Returns the result of `Query' starting from the initial state given by
%%      `Params'. The format of the result is determined by `DisplayOpt'.
%%
%%      `Params' contains either the
%%          - (optional) `file' and `position' keys or
%%          - the `node_list' and (optional) `node_type' key.
%%       The possible values for
%%          - `node_type': file|function|record|field|macro|variable|expression
%%
%%      `DisplayOpt' contains the keys `positions' and `output'.
%%      The possible values for
%%          - `positions': none|scalar|linecol|both
%%          - `output': stdio|{iodev,io_device()}|msg|other|nodes
%%
%%      The `QueryResult' depends on the `output' key in `DisplayOpt'.
%%          - stdio: a formatted text written to stdio
%%          - {iodev,Dev::io_device()}: a formatted text written to Dev
%%          - msg: a message containing a list with the following types of
%%                 elements: {eq, Name, Value} |
%%                           {list, [{Position, Text}]} |
%%                           {chain, [{Position, Text}], PostWS} |
%%                           {group_by, {Position, Text}, eq, Name, Value} |
%%                           {group_by, {Position, Text},
%%                            list, [{Position, Text}]}
%%          - other: the same list that is otherwise sent by a message
%%          - nodes: a proplist with the keys: `nodes' for a list of nodes and
%%                                             `text' for a formatted text
%%      The format of positions depends on the `positions' key in `DisplayOpt'.
%%          - none: nopos
%%          - scalar: {File::string(), PosFrom::integer(), PosTo::integer()}
%%          - linecol: {File::string(), PosFrom::{integer(), integer()},
%%                                      PosTo::{integer(), integer()}}

run(DisplayOpt, Params, Query) when is_list(Query) ->
    Tokens   = tokenize(?Lib:replace_special_chars(Query)),
    SynTree  = parse(Tokens),
    SynTree2 = make_internal_representation(SynTree),
    Result = process_semantic_query(Params, SynTree2, DisplayOpt),    % ?d(Result),
    ?Format:result(Result, DisplayOpt).

make_internal_representation(SynTree) ->
    case refusr_ac:validate_query(SynTree) of
        {ok, Res} ->
            Res;
        {error, E} ->
            throw(?LocalError(syntax_error, E))
    end.

parse(Tokens) ->
    case refusr_ac_parser:parse(Tokens) of
        {ok, SynTree} ->
            SynTree;
        {error, {_, _, Err}} ->
            case lists:flatten(Err) of
                "{quoted," ++ A ->
                    A1 = lists:reverse(tl(lists:reverse(A))),
                    throw(?LocalError(syntax_error, A1));
                _ ->
                    throw(?LocalError(syntax_error, Err))
            end
    end.

tokenize(Query) ->
    case refusr_sq_lexer:string(Query) of
        {ok, Tokens, _} ->
            Tokens;
        {error, {_, _, Error}, _} ->
            throw(?LocalError(lexical_error, Error))
    end.
 
%% @private
prepare(Args) ->
    fun () ->
        [DisplayOpt, StartOpt, QueryStr] =
            ?MISC:pgetu([display_opt, start_opt, querystr], Args),
        run(DisplayOpt, StartOpt, QueryStr)
    end.

%%% ============================================================================
%%% Implementation

process_semantic_query(Params, SemanticQuery, DisplayOpt) ->
    {Seq, Last} = split_semantic_query(SemanticQuery),
    SeqLength = entitynumber_increment(SemanticQuery),
    {GroupByParam, GroupBy} = case proplists:get_value(groupby, DisplayOpt) of
        undefined ->
            {default, SeqLength};
        GroupByOpt ->
            ?Check(is_integer(GroupByOpt) andalso (GroupByOpt > 0),
                   ?LocalError(bad_typing, [])),
            case SeqLength >= GroupByOpt of
                true ->
                    {GroupByOpt, GroupByOpt};
                _ ->
                    case Seq of
                        [{initial_selector, {set_op, _}} | _] -> 
                            {GroupByOpt, -42};
                        _ ->
                            throw(?LocalError(grouping_error, []))
                    end
            end
    end,
    Skeleton = #state{groupby_place = GroupBy,
                      params = [{groupby, GroupByParam} | Params]},
    SeqState = lists:foldl(fun check/2, Skeleton, Seq),
    LastState = lists:foldl(fun check/2, SeqState#state{checked_query = []}, Last),
    case find_help(LastState) of
        [] -> 
            Query = lists:reverse(SeqState#state.checked_query),
            QueryLastPart = lists:reverse(LastState#state.checked_query),
            InitStateWithVariables = Skeleton#state{
                                       checked_query = [],
                                       variables = LastState#state.variables},
            Processed = process_full_query(InitStateWithVariables,
                                           Query, 
                                           QueryLastPart),
            simplify_prop_query_result(filter_empty_groups(Processed));
        [Help | _] -> 
            Help
    end.

partition_query(SemanticQuery) ->
    lists:foldr(
      fun(Filter = {filter, _F}, {undef, undef, [], Last}) ->
          {undef, undef, [], [Filter|Last]};
         (InitSel = {initial_selector, _}, {undef, Type, Mid, Last}) ->
              {InitSel, Type, Mid, Last};
         (Closure = {closure, _, _}, {undef, _Type, [], Last = [{filter,_} | _]}) ->
          {undef, mid, [Closure], Last};
         (Elem, {undef, undef, [], Last}) ->
          {undef, element(1,Elem), [], [Elem|Last]};
         (Filter = {filter, _F}, {undef, Type,Mid, Last}) ->
          {undef, Type, [Filter|Mid], Last};
         (Closure = {closure, _S, _M}, {undef, closure, Mid, Last}) ->
          {undef, closure, [], [Closure|Mid++Last]};
         (Iteration = {iteration, _S, _M}, {undef, iteration, Mid, Last}) ->
          {undef, iteration, [], [Iteration|Mid++Last]};
         (Elem, {undef, _Type, Mid, Last}) ->
          {undef, mid, [Elem|Mid], Last}
      end,
      {undef, undef, [], []},
      SemanticQuery).

split_semantic_query(SemanticQuery) ->
    Split = partition_query(SemanticQuery),

    {InitialSelector, QuerySeq, QuerySeqLastPart} = 
        case Split of
            {undef, _, [{set_op, _} = SetOp | T], L} ->
                {{initial_selector, SetOp}, T, L};
            {undef, _, [], [{set_op, _} = SetOp | T]} ->
                {{initial_selector, SetOp}, [], T};
            {I, _, S, L} ->
                {I, S, L}
        end,
    {[InitialSelector | QuerySeq], QuerySeqLastPart}.

process_full_query(InitialState, QuerySeq, LastQuery) ->
    merge_states(process_query(InitialState, QuerySeq, LastQuery)).

process_query(InitialState, QuerySeq, QuerySeqLast) ->
    StartState = InitialState#state{semquery_last = QuerySeqLast},
    State = process_query_seq(StartState, QuerySeq),
    Processed = process_last_query(State, QuerySeqLast),
    EveryState = append_parallel_states(Processed),
    QueryBody = QuerySeq ++ QuerySeqLast,
    SeqLength = entitynumber_increment(QueryBody),
    GroupBy = InitialState#state.groupby_place,
    case (GroupBy == undefined) orelse 
        (SeqLength == 0) orelse (SeqLength == GroupBy) of
        true ->
            EveryState;
        _ ->
            DropInitialSelector = case QueryBody of
                                      [{initial_selector, _} | Query] ->
                                          Query;
                                      _ ->
                                          QueryBody
                                  end,
            lists:map(fun(EndState) ->                              
                              grouped_prop_query_naming(EndState, GroupBy - 1,
                                                        DropInitialSelector)
                      end,
                      EveryState)
    end.

process_query_seq(State, QuerySeq) ->
    process_query_seq_worker(State, QuerySeq, QuerySeq).

process_closure_or_iteration_body(State, BodyQuery) ->
    FullQuery = BodyQuery ++ State#state.semquery,
    process_query_seq_worker(State, FullQuery, BodyQuery).

process_query_seq_worker(FirstState, RemainingQuery, QuerySeqToProcess) ->
    {LastState, _, ParallelKeys} =
        lists:foldl(
          fun (QueryElement, {ActualState, RemainingQueryPlusQueryElement,
                              ParallelKeys})->
                  ActualRemainingQuery = tl(RemainingQueryPlusQueryElement),
                  RemainingQueryForBinding =
                      remaining_query(RemainingQueryPlusQueryElement),
                  NewState =
                      process(QueryElement,
                              ActualState#state{
                                semquery = RemainingQueryForBinding,
                                parallel_key = []}),
                  NewKeys = NewState#state.parallel_key,
                  {NewState, ActualRemainingQuery, NewKeys ++ ParallelKeys}
          end,
          {FirstState, RemainingQuery, FirstState#state.parallel_key},
          QuerySeqToProcess),    
    LastState#state{semquery = [], parallel_key = ParallelKeys}.

remaining_query([{closure_seq, _QSLst, infinite, _} | _] = FullQuery) ->
    FullQuery;
remaining_query([{closure_seq, QSLst, Mult, VariablesToBind} | Query]) ->
    [{closure_seq, QSLst, Mult - 1, VariablesToBind} | Query];
remaining_query([{iteration, QSLst, Mult} | Query]) ->
    [{iteration, QSLst, Mult - 1} | Query];
remaining_query(Query) ->
    tl(Query).

process_last_query(State, LastQuery) ->
    {LastState, _, ParallelKeys} =
        lists:foldl(
          fun (QueryElement, {ActualState, RemainingQueryPlusQueryElement,
                             ParallelKeys})->
                  ActualRemainingQuery = tl(RemainingQueryPlusQueryElement),
                  RemainingQueryForBinding =
                      remaining_query(RemainingQueryPlusQueryElement),
                  NewState =
                      process_last(QueryElement,
                                   ActualState#state{
                                     semquery_last = RemainingQueryForBinding,
                                     parallel_key = []}),
                  NewKeys = NewState#state.parallel_key,
                  {NewState, ActualRemainingQuery, NewKeys ++ ParallelKeys}
          end,
          {State, LastQuery, State#state.parallel_key},
          LastQuery),
    LastState#state{semquery_last = [], parallel_key = ParallelKeys}.

is_empty_state(#state{res = Chains}) when is_record(Chains, chains) ->
    #chains{complete = Completes, 
            incomplete = Incompletes, 
            recursive = Recursives} = Chains,
    (Completes == []) andalso (Incompletes == []) andalso (Recursives == []);
is_empty_state(#state{res = []}) ->
    true;
is_empty_state(#state{res = [H | _] = Result}) when is_list(H) ->
    lists:all(fun (Group) -> Group == [] end, Result);
is_empty_state(State) when is_record(State, state) ->
    false.

not_empty_states(States)->
    lists:filter(fun(State) -> not is_empty_state(State) end, States).

merge_states([State]) ->
    State;
merge_states(States = [#state{action = property_query,
                              entitynumber = EntNumber,
                              groupby_place = GbNumber} | _])
  when ((GbNumber + 1) == EntNumber) ->
    ?Check(length(ets:all())  < ?ets_limit,
           ?LocalError(ets_limit_reached, [])),
    Results = ets:new(results, [ordered_set]),
    Insert = fun(Entity, Property)->
                     ets:insert_new(Results, {Entity, Property})
            end,
    ProcessState = fun(State, VariableValues) -> 
                           foreach2(Insert,
                                    State#state.groupby_res,
                                    State#state.res),
                           store_variable_values(State, VariableValues)
                   end,
    First = hd(States),
    InitialVariableStore = initial_store(First#state.variables),
    CollectedVariableValues = lists:foldl(ProcessState, InitialVariableStore,
                                          States),
    {Entities, Properties} = lists:unzip(ets:tab2list(Results)),
    ets:delete(Results),
    First#state{
      res = Properties,
      groupby_res = Entities,
      variables = CollectedVariableValues};
merge_states(States = [#state{action = Action} | _]) 
  when Action == iteration orelse Action == closure ->
    First = hd(States),
    InitialVariableStore = initial_store(First#state.variables),
    Empty = gb_sets:empty(),
    Add = fun (Chain, Results) ->
                  lists:foldl(fun gb_sets:add_element/2, Results, Chain) end,
    {Complete, Incomplete, Recursive, CollectedVariableValues} = 
        lists:foldl(fun(State, {Complete, Incomplete, Recursive, VariableValues})->
                            Chain = State#state.res,
                            {Add(Chain#chains.complete, Complete),
                             Add(Chain#chains.incomplete, Incomplete),
                             Add(Chain#chains.recursive, Recursive),
                             store_variable_values(State, VariableValues)}
                    end,
                    {Empty, Empty, Empty, InitialVariableStore},
                    States),
    First#state{res = #chains{complete = gb_sets:to_list(Complete),
                              incomplete = gb_sets:to_list(Incomplete),
                              recursive = gb_sets:to_list(Recursive)},
                variables = CollectedVariableValues};
merge_states(States = [#state{action = selection,
                              groupby_type = []} | _]) ->
    First = hd(States),
    VariablesInitialStore = initial_store(First#state.variables),
    Union = fun (#state{res = Entities}, Set) ->
                    SetOfState = gb_sets:from_list(Entities),
                    gb_sets:union(Set, SetOfState) end,
    {Entities, CollectedVariableValues} = 
        lists:foldl(fun (State, {Results, VariableValues}) ->
                            {Union(State, Results),
                             store_variable_values(State, VariableValues)} end,
                    {gb_sets:empty(), VariablesInitialStore},
                    States),
    First#state{res = gb_sets:to_list(Entities),
                variables = CollectedVariableValues};
merge_states(States) ->
    Union = fun (Ordset, List)->
                    ordsets:union(Ordset, ordsets:from_list(List)) end,
    Append =
        fun(_GroupingEntity, [], Results) ->
                Results;
           (GroupingEntity, Group, Results) ->
                case gb_trees:is_defined(GroupingEntity, Results) of
                    true ->
                        OldGroup = gb_trees:get(GroupingEntity, Results),
                        NewGroup = Union(OldGroup, Group),
                        gb_trees:update(GroupingEntity, NewGroup, Results);
                    false ->
                        OrderedGroup = ordsets:from_list(Group),
                        gb_trees:insert(GroupingEntity, OrderedGroup, Results)
                end
        end,
    First = hd(States),
    InitialVariableStore = initial_store(First#state.variables),
    {Results, CollectedVariableValues} =
        lists:foldl(
          fun(State, {Results, VariableValues}) ->
                  #state{res = Groups, groupby_res = GroupEntities} = State,
                  {foldl2(Append, Results, GroupEntities, Groups), 
                   store_variable_values(State, VariableValues)}
          end,
          {gb_trees:empty(), InitialVariableStore},
          States),
    {GroupingEntities, EntityGroups} =
        lists:unzip(gb_trees:to_list(Results)),
    to_composite_prop_query(
        First#state{
            res = EntityGroups,
            groupby_res = GroupingEntities,
            variables = CollectedVariableValues}).

to_composite_prop_query(#state{res = []} = State) -> State;
to_composite_prop_query(#state{action = property_query} = State) ->
    #state{res = Result, type = Type} = State,
    IsGroupedResult =
        fun ([Head | _]) ->
                case Type of
                    string -> is_list(Head);
                    _ -> true
                end;
            (_) -> false
        end,
    case lists:any(IsGroupedResult, Result) of
        true -> State#state{action = composite_property_query};
        false -> State
    end;
to_composite_prop_query(State) -> State.

initial_store(Variables) ->
    ToUndefined = fun (_Variable, {Type, _Value}) ->
                          {Type, undefined} end,
    orddict:map(ToUndefined, Variables).

store_variable_values(#state{res = []}, Store) ->
    Store;
store_variable_values(State, Store) ->
    VariableValues = State#state.variables,
    UpdateStore = 
        fun (_Variable, {_Type, undefined}, StoredInfo) ->
                StoredInfo;
            (_Variable, {_Type, valueless}, StoredInfo) ->
                StoredInfo;
            (_Variable, {Type, {value, Values}} = Info, {Type, undefined})
              when is_list(Values) ->
                Info;
            (_Variable, {Type, {value, Values}}, {Type, {value, StoredValues}}) 
              when is_list(Values) ->
                {Type, {value, ordsets:union(Values, StoredValues)}};
            (_Variable, {Type, {value, Value}}, {Type, undefined}) ->
                {Type, {value, ordsets:add_element(Value, ordsets:new())}};
            (_variable, {Type, {value, Value}}, {Type, {value, StoredValues}}) ->
                {Type, {value, ordsets:add_element(Value, StoredValues)}} end,
    orddict:merge(UpdateStore, VariableValues, Store).

collect_variable_values([#state{variables = Variables} | _] = States) ->
    InitialStore = initial_store(Variables),
    lists:foldl(fun store_variable_values/2, InitialStore, States).    

foreach2(_Fun, [], []) ->
    ok;
foreach2(Fun, [ElemA | ListA], [ElemB | ListB]) when is_function(Fun, 2) ->
    Fun(ElemA, ElemB),
    foreach2(Fun, ListA, ListB).

foldl2(_Fun, E, [], [])->
    E;
foldl2(Fun, E, [ElemA | ListA], [ElemB | ListB]) when is_function(Fun, 3)->
    foldl2(Fun, Fun(ElemA, ElemB, E), ListA, ListB).

find_help(E) when is_list(E) ->
    lists:flatmap(fun find_help/1, E);
find_help(E) when is_tuple(E) ->
    case E of
        {help, {_, _}} -> [E];
        {help, _} -> [{help, {initial_selectors, []}}];
        _ -> find_help(tuple_to_list(E))
    end;
find_help(_) ->
    [].

grouped_prop_query_naming(State, Skip, QuerySeq) ->
    case is_property_query(State) of
        true ->
            Strings = lists:map(fun query_selectors_to_string/1, QuerySeq),
            Filtered = lists:filter(fun (String) -> String /= [] end,
                                    Strings),
            QueryName = 
                if Skip > length(Filtered) ->
                        [];
                   true ->
                        lists:append(lists:nthtail(Skip, Filtered))
                end,
            State#state{name = QueryName};
        _ ->
            State
    end.

query_selectors_to_string(QueryElement) ->
    case QueryElement of
        {selector, Sel, _} ->
            "." ++ atom_to_list(Sel);
        {property, Property, _} ->
            "." ++ atom_to_list(Property);
        {initial_selector, Sel} ->
            atom_to_list(Sel);
        {iteration, Sels, Mult} ->
            ".{" ++
            tl(lists:flatmap(fun query_selectors_to_string/1, Sels)) ++
            "}" ++ integer_to_list(Mult);
        {closure, Sels, Mult} ->
            ".(" ++
            tl(lists:flatmap(fun query_selectors_to_string/1, Sels)) ++
            ")" ++
            case Mult of
                infinite -> 
                    "+";
                N ->
                    integer_to_list(N)
            end;
        {closure_seq, Sels, Mult, _} ->
            ".(" ++
            tl(lists:flatmap(fun query_selectors_to_string/1, Sels)) ++
            ")" ++
            case Mult of
                infinite -> 
                    "+";
                N ->
                    integer_to_list(N)
            end;
        {set_op, {_, Q1, Q2}} ->
            case hd(Q1) of
                {initial_selector, _} ->
                    lists:flatmap(fun query_selectors_to_string/1, Q2);
                _ ->
                    lists:flatmap(fun query_selectors_to_string/1, Q1)
            end;
        _ ->
            []
    end.

merged_type(Type1, Type2) ->
    case Type1 of
        Type2 -> Type2;
        _ ->
            case ordsets:is_subset(
                    ordsets:from_list([Type1, Type2]),
                    [atom, bool, int, string]) of
                true -> string;
                _ -> 
                    case {Type1, Type2} of
                        {any, T} -> T;
                        {T, any} -> T;
                        {help, _} -> help;
                        {_, help} -> help;
                        _ -> null
                    end
            end
    end.


%%% ============================================================================
%%% Preprocessing of queries

merge_results(SetOp, Res1, Res2) when is_record(Res1, state) andalso
                                      is_record(Res2, state) ->
    SetopFun = setop_merge_fun(SetOp),
    if Res1#state.groupby_place > Res1#state.entitynumber ->
            SetopFun(Res1#state.res, Res2#state.res);
       true ->
            lists:zipwith(SetopFun, Res1#state.res, Res2#state.res)
    end;

merge_results(SetOp, Res1, Res2) ->
    case SetOp of
        union -> ordsets:union(
                    ordsets:from_list(Res1),
                    ordsets:from_list(Res2)
                 );
        intersect -> ordsets:intersection(
                    ordsets:from_list(Res1),
                    ordsets:from_list(Res2)
                 );
        minus -> ordsets:subtract(
                    ordsets:from_list(Res1),
                    ordsets:from_list(Res2)
                 )
    end.

setop_merge_fun(union) ->
    fun (Res1, Res2) ->
            ordsets:union(
              ordsets:from_list(Res1),
              ordsets:from_list(Res2))
    end;
setop_merge_fun(intersect) ->
    fun (Res1, Res2) ->
            ordsets:intersection(
              ordsets:from_list(Res1),
              ordsets:from_list(Res2))
    end;
setop_merge_fun(minus) ->
    fun (Res1, Res2) ->
                ordsets:subtract(
                  ordsets:from_list(Res1),
                  ordsets:from_list(Res2))
    end.

combine_same_groups(Group1, Group2) ->
    Same_added =
    [{ordsets:from_list(Ents ++
        case lists:keyfind(G, 2, Group2) of
            false -> [];
            {E, _} -> E
        end
        ), G} || {Ents, G} <- Group1],

    Only_in_G2 =
    lists:filter(
        fun({_, Key})->
            not(lists:keymember(Key, 2, Group1))
        end,
        Group2),

    lists:unzip(Same_added ++ Only_in_G2).

group_by_mod(State) ->
    St = 
    case State#state.type of
        file ->
            State#state{
                groupby_type = file,
                groupby_res = lists:append(State#state.res),
                res=[[E]||E <- lists:append(State#state.res)]
            };
        _ -> case State#state.groupby_type of
                [] -> State#state{groupby_res = State#state.res};
                _ -> State
             end
    end,

    Mods = [?Lib:node_file(N) || N <- St#state.groupby_res],

    Merge_same =
    fun(Groups) ->
        Gs = lists:keysort(1, Groups),
        lists:ukeysort(1,
            [ {K, ordsets:union([ordsets:from_list(E)|| {Key, E}<-Gs, Key==K])}
                || {K, _} <- Gs]
        )
    end,

    {PrevRes, Res} = 
        lists:unzip(
            Merge_same(lists:zip(Mods, St#state.res))
        ),
    St#state{groupby_type=file, groupby_res=PrevRes, res=Res}.

merge_states(SetOp, S1, S2) ->
%?d({is_group_state(S1), is_group_state(S2)}),
%?d({to_group_state(S1), to_group_state(S2)}),
    case {to_group_state(S1), to_group_state(S2)} of

        {#state{action=A1, res=R1} = A,
            #state{action=A2, res=R2}}
            when A1==iteration;A1==closure, A2==iteration;A2==closure ->
            Action = case A1 of
                        A2 -> A2;
                        _ -> closure
                     end,
            A#state{
                action=Action,
                res = {chains,
                    merge_results(SetOp, element(2, R1), element(2, R2)),
                    merge_results(SetOp, element(3, R1), element(3, R2)),
                    merge_results(SetOp, element(4, R1), element(4, R2))},
                groupby_res = [],
                groupby_type = []
            };

        {#state{groupby_type = GType} = A,
            #state{groupby_type = PrevType} = B}
            when GType==[] orelse PrevType==[] ->
            A#state{
                res = merge_results(SetOp, lists:append(A#state.res),
                                           lists:append(B#state.res)),
                groupby_res = [],
                groupby_type = []
            };

        {State1, State2} when SetOp/=union ->
            State1#state{
                res = [merge_results(SetOp, E, lists:append(State2#state.res))
                        || E<-State1#state.res]
            };

        {#state{action=Action, type=Type, groupby_type=GType} = A,
            #state{action=Action, type=Type, groupby_type=GType} = B} ->
                {F_res, F_groupby_res} =
                    combine_same_groups(
                        lists:zip(A#state.res, A#state.groupby_res),
                        lists:zip(B#state.res, B#state.groupby_res)
                    ),
                A#state{
                  res =  F_res,
                  groupby_res = F_groupby_res};

        {#state{action = Action, groupby_type = GType} = A,
            #state{action=Action, groupby_type=PrevType} = B}
            when GType==[] orelse PrevType==[] orelse GType /= PrevType ->
                merge_states(SetOp, group_by_mod(A), group_by_mod(B));

        {A, B} ->
            case A#state.res of
                [] -> B;
                _  -> A
            end
    end.

property_to_string(State1 = #state{type = Type}, 
                   State2 = #state{type = Type}) ->
    {State1, State2};

property_to_string(State1, State2) ->
    {property_to_string(State1), property_to_string(State2)}.

property_to_string(#state{action=property_query}=State) ->
    State#state{
        type = string,
        name = value,
        res = [?MISC:any_to_string(E) || E<-State#state.res]
    };

property_to_string(#state{action=composite_property_query}=State) ->
    State#state{
        type = string,
        name = value,
        res = [[?MISC:any_to_string(E) || E<-G] || G<-State#state.res]
    };

property_to_string(State) ->
    State.

all_to_string(L) when is_list(L) ->
    [to_string(E) || E<-L].

to_string({quoted, Val}) ->
    to_string(Val);

to_string(E) -> ?MISC:any_to_string(E).

to_nongrouped_state(State = #state{groupby_place = undefined}) ->
    State#state{groupby_res = [],
                groupby_type = []};
to_nongrouped_state(State) ->
    State#state{groupby_res = [],
                groupby_type = [],
                entitynumber = State#state.groupby_place - 1}.

to_group_state(#state{action=property_query}=State) ->
    State#state{
        action = composite_property_query,
        res = [[E] || E <- State#state.res]
    };
to_group_state(State) ->
    case is_group_state(State) of
        true -> State;
        _ ->
            State#state{
                res = [[E] || E <- State#state.res]
            }
    end.

is_group_state(#state{action=closure}) -> true;
is_group_state(#state{action=iteration}) -> true;
is_group_state(#state{action=composite_property_query}) -> true;
is_group_state(#state{action=property_query}) -> false;
is_group_state(#state{action=statistics}) -> false;
is_group_state(#state{res=[H|_]}) -> is_list(H);
is_group_state(_) -> false.

is_property_query(#state{action=property_query}) -> true;
is_property_query(#state{action=composite_property_query}) -> true;
is_property_query(_) -> false.

filter_empty_results(#state{res=Res}=State)->
    case is_list(Res) of
        true -> 
            State#state{
                res = lists:filter(fun([]) -> false; (_) -> true end, Res)};
        _ -> State
    end.

filter_empty_groups(#state{res=Res, groupby_res=GRes}=State) when is_list(Res) ->
    case is_group_state(State) of
        true ->
            Zip = lists:zip(Res, GRes),
            Filtered = lists:filter(fun({[],_})->false; (_)->true end, Zip),
            {NRes, NGRes} = lists:unzip(Filtered),
            State#state{
                res = NRes,
                groupby_res = NGRes};
        _ -> filter_empty_results(State)
    end;
filter_empty_groups(State) -> State.

simplify_prop_query_result(#state{action=composite_property_query, res=Res}=State)->
    OneElementGroups = lists:all(
                        fun ([_])   -> true;
                            (_)     -> false end, Res),
    case OneElementGroups of
        true ->
            State#state{
                action = property_query,
                res = lists:map(fun([A]) -> A end, Res)
            };
        _ -> State
    end;

simplify_prop_query_result(State) -> State.

%todo:  node list <-> query format check
check({initial_selector, {set_op, {SetOp, Q1, Q2} = Q}}, State) ->
    ?Check(not (SetOp == any_in orelse SetOp == all_in),
           ?LocalError(illegal_operator, [SetOp, Q])),
    CheckedQ1 = lists:foldl(fun check/2, State, Q1),
    CheckQ2 = State#state{variables = CheckedQ1#state.variables},
    CheckedQ2 = lists:foldl(fun check/2, CheckQ2, Q2),

    Type1 = CheckedQ1#state.type,
    Type2 = CheckedQ2#state.type,

    IsStatistics = fun([{statistics, _}]) -> true;
                      (_) -> false end,
    
    MergedType = merged_type(Type1, Type2),
    ?Check((MergedType /= null)
           andalso (not IsStatistics(CheckedQ1#state.checked_query))
           andalso (not IsStatistics(CheckedQ2#state.checked_query)),
           ?LocalError(unsupported_feature, "set operations do not support "
                       "the mixing of selectors, properties and statistics")),
    ?Check(CheckedQ2#state.variables == State#state.variables,
           ?LocalError(unsupported_feature, "set operand with variable binding")),

    CheckedQuery = {set_op, {SetOp,
                             lists:reverse(CheckedQ1#state.checked_query),
                             lists:reverse(CheckedQ2#state.checked_query)}},
    InitialSelector = {initial_selector, CheckedQuery},
    State#state{type = MergedType, checked_query = [InitialSelector],
                variables = CheckedQ2#state.variables};

check({initial_selector, {quoted, Selector}}, State) ->
    check({initial_selector, Selector}, State);

check({initial_selector, Selector} = InitialSelector, State) ->
    State#state{type = ?Lib:init_sel_type(Selector),
                checked_query = [InitialSelector]};

check({set_op, {SetOp, Q1, Q2}}, State) ->
    StartState1 = State#state{checked_query = []},
    CheckedQ1 = lists:foldl(fun check/2, StartState1, Q1),
    
    StartState2 = State#state{checked_query = [],
                              variables = CheckedQ1#state.variables},
    CheckedQ2 = lists:foldl(fun check/2, StartState2, Q2),
    
    Type1 = CheckedQ1#state.type,
    Type2 = CheckedQ2#state.type,

    IsStatistics = fun([{statistics, _}]) -> true;
                      (_) -> false end,

    MergedType = merged_type(Type1, Type2),
    ?Check((MergedType /= null)
           andalso (not IsStatistics(CheckedQ1#state.checked_query))
           andalso (not IsStatistics(CheckedQ2#state.checked_query)),
           ?LocalError(unsupported_feature, "set operations do not support "
                       "the mixing of selectors, properties and statistics")),
    ?Check(CheckedQ2#state.variables == State#state.variables,
           ?LocalError(unsupported_feature, "set operand with variable binding")),

    State#state{
      type = merged_type(Type1, Type2),
      checked_query = [{set_op, 
                        {SetOp,
                         lists:reverse(CheckedQ1#state.checked_query),
                         lists:reverse(CheckedQ2#state.checked_query)
                        }} | State#state.checked_query],
      variables = CheckedQ2#state.variables};

check({selector, Sel}, #state{type=Type, checked_query=Lst}=State) ->
    Selector =
        case Sel of
            {quoted, Atom} -> Atom;
            Atom -> Atom
        end,
    case ?Lib:sel_type(Type, Selector) of
        [SelType] ->
            State#state{
              type = SelType,
              checked_query = [{selector, Selector, SelType}| Lst]};
        [] ->
            case ?Lib:prop_type(Type, Selector) of
                [PropType] ->
                    State#state{
                      type = PropType,
                      checked_query = [{property, Selector, PropType} | Lst]};
                [] ->
                    throw(?LocalError(illegal_selector, [Type, Selector]))
            end
    end;

check({Action, {seq, Seq}, {mult, Mult}},
      #state{type=Type, checked_query=Lst, variables=Variables}=State) -> 
    ?Check(Mult > 0, ?LocalError(mult_error, [])),
    case lists:foldl(fun check/2, State#state{checked_query = []}, Seq) of
        #state{type = Type, variables = Variables, checked_query = QSLst} ->
            Body = lists:reverse(QSLst),
            State#state{checked_query = [{Action, Body, Mult} | Lst]};
        #state{type = Type, variables = MoreVariables, checked_query = QSLst} = NewState
          when Action == closure ->
            NewlyBoundVariables = variables_got_bound(NewState, State),
            Body = lists:reverse(QSLst),
            State#state{
              checked_query = [{closure_seq, Body, Mult, NewlyBoundVariables} | Lst],
              variables = MoreVariables};                       
        #state{type = Type, variables = MoreVariables, checked_query = QSLst}->
            Body = lists:reverse(QSLst),
            State#state{checked_query = [{Action, Body, Mult} | Lst],
                        variables = MoreVariables};
        #state{type = BadType} ->
            throw(?LocalError(type_mismatch, [Action, {Seq, BadType}, Type]))
    end;

check({filter, Filter}, #state{checked_query = Lst} = State) ->
    {Checked_Query, NewState} = check_filter(State, Filter),
    case has_variable_got_bound(NewState, State) of
        true ->
            FilterElement = {filter_with_variable_to_bind,
                             Checked_Query,
                             variables_got_bound(NewState, State)},
            State#state{checked_query = [FilterElement | Lst],
                        variables = NewState#state.variables};
        false ->
            State#state{checked_query = [{filter, Checked_Query} | Lst]}
    end;

check({statistics, Stat}, #state{type=PropType}=State) ->
    Statistics =
        case Stat of
            {quoted, S} -> S;
            S           -> S
        end,
    case PropType of
        any ->
            State#state{type=int, checked_query = [{statistics, Statistics}]};
        int ->
            State#state{type=int, checked_query = [{statistics, Statistics}]};
        _ ->
            throw(?LocalError(statistics_error, []))
    end;

%todo: filterekben help?
check({help, HelpType}, #state{type=Type}=State) ->
    State#state{type=help, checked_query={help,
        case HelpType of
            initial_selectors -> {initial_selectors, []};
            queries           -> {selectors, Type};
            statistics        -> {statistics, []};
            filters           -> {properties, Type}
        end}
    }.


%% @private
%% @spec check_filter(Type::atom(), Filter::atom()|tuple()) -> atom()|tuple()
check_filter(State,'true') ->
    {'true', State};

check_filter(State, 'false') ->
    {'false', State};

check_filter(State, {'or', Filter1, Filter2}) ->
    {CheckedF1, NewState1} = check_filter(State, Filter1),
    {CheckedF2, NewState2} = check_filter(NewState1, Filter2),
    {{'or', CheckedF1, CheckedF2}, NewState2};

check_filter(State, {'and', Filter1, Filter2}) ->
    {CheckedF1, NewState1} = check_filter(State, Filter1),
    {CheckedF2, NewState2} = check_filter(NewState1, Filter2),
    {{'and', CheckedF1, CheckedF2}, NewState2};

check_filter(State, {'not', Filter}) ->
    {CheckedFilter, NewState} = check_filter(State, Filter),
    {{'not', CheckedFilter}, NewState};

check_filter(#state{type=Type} = State, {set_op, {SetOp, Q1, Q2}=Filter}) ->
    Check_filter_setop =
    fun(F, Variables) -> 
        case F of
            {seq, Q} ->
                StartState = State#state{checked_query = [],
                                         variables = Variables},
                Seq = lists:foldl(fun check/2, StartState, Q),
                Query = lists:reverse(Seq#state.checked_query),
                {Seq#state.type, {seq, Query}, Seq#state.variables};
            {cons, _} -> 
                {any, F, Variables};
            [{initial_selector, _} | _] ->
                StartState = State#state{checked_query = [],
                                         variables = Variables},
                Seq = lists:foldl(fun check/2, StartState, F),
                Query = lists:reverse(Seq#state.checked_query),
                {Seq#state.type, {'query', Query}, Seq#state.variables};
            Q when is_list(Q) ->
                StartState = State#state{checked_query = [],
                                         variables = Variables},
                Seq = lists:foldl(fun check/2, StartState, Q),
                Query = lists:reverse(Seq#state.checked_query),
                {Seq#state.type, {seq, Query}, Seq#state.variables};
            Property when is_atom(Property) ->
                PropType = case ?Lib:prop_type(Type, Property) of
                               [T] -> T;
                               _ -> null
                           end,
                {PropType, Property, Variables};
            {variable, Variable} ->
                case lookup_variable(Variable, State) of
                    none ->
                        throw(?LocalError(unbound_variable, Variable));
                    {VarType, _} ->
                        {VarType, F, Variables}     
                end
        end
    end,
    Variables = State#state.variables,
    {Type1, Q1Q, MaybeNewVariables1} = Check_filter_setop(Q1, Variables),
    {Type2, Q2Q, MaybeNewVariables2} = Check_filter_setop(Q2, MaybeNewVariables1),

    ?Check(merged_type(Type1, Type2) /= null,
           ?LocalError(type_mismatch, [{filter, Filter}, Type2, Type1])),
    ?Check(MaybeNewVariables2 == State#state.variables,
           ?LocalError(unsupported_feature, "set operand with variable binding")),

    {QueryElement, NewState} =
        case has_variable_got_bound(MaybeNewVariables2, Variables) of
            true ->
                {set_op_with_variable,
                 State#state{variables = MaybeNewVariables2}};
            false ->
                {set_op, State}
        end,                                   
    {{QueryElement, {SetOp, Q1Q, Q2Q}}, NewState};

check_filter(State, {'in', P1, P2})->
    check_filter(State, {set_op, {all_in, P1, P2}});

check_filter(State, {seq, Seq}) ->
    St = lists:foldl(fun check/2, 
                     State#state{checked_query = []},
                     Seq),
    CheckedQuery = lists:reverse(St#state.checked_query),
    case has_variable_got_bound(St, State) of
        true ->
            {{seq_with_variable, CheckedQuery}, St};
        false ->
            {{seq, CheckedQuery}, St}
    end;

check_filter(#state{type = Type} = State, {CompOp, Op1, Op2} = FullExpr) ->
    AnalOf = fun(Op) ->
        case Op of
            {variable, VarName} ->
                case lookup_variable(VarName, State) of
                    {VarType,_} -> {variable, VarType, VarName};
                    _ -> {unbound_variable, undefined, VarName}
                end;
            {quoted, _} -> {const, atom, Op};
            _ when is_tuple(Op) -> {compop, bool, Op};
            _ when is_atom(Op) ->
                case ?Lib:prop_type(Type, Op) of
                    [PropType] -> {property, PropType, Op};
                    _ -> {const, atom, Op}
                end;
            _ -> {const, hd(type_of(Op)), Op}
        end
    end,

    {Class1, Type1, Print1} = AnalOf(Op1),
    {Class2, Type2, Print2} = AnalOf(Op2),

    In = fun(Elem, List) ->
            ordsets:is_element(Elem, ordsets:from_list(List)) end,

    ?Check( %Check for bad types like "any"
        In(Type1, [atom, bool, int, string]) orelse Class1 == unbound_variable,
        ?LocalError(bad_type, [Type1, Print1])),

    ?Check( %Check for bad types like "any"
        In(Type2, [atom, bool, int, string]) orelse Class2 == unbound_variable,
        ?LocalError(bad_type, [Type2, Print2])),

    ?Check( %...[3=2], ...[non_prop=non_prop], ...['atom' /= non_prop]
        Class1 /= const orelse Class2 /= const,
        ?LocalError(no_property_in_comparison, [])),

    ?Check( %mods[Var = non_prop]..., mods[Var <= prop]...
        Class1 /= unbound_variable orelse
            (Class2 == property andalso CompOp == '=='),
        ?LocalError(unbound_variable, Print1)),

    ?Check( %mods[non_prop = Var]..., mods[prop <= Var]...
        Class2 /= unbound_variable orelse
            (Class1 == property andalso CompOp == '=='),
        ?LocalError(unbound_variable, Print2)),

    ?Check( %mods[Var = name].funs[Var = arity], mods[Var = name].funs[Var = 3]
        Class1 /= variable orelse Type1 == Type2,
        ?LocalError(type_mismatch, [FullExpr, {Print1, Type1}, Type2])),

    ?Check( %mods[Var = name].funs[arity = Var], mods[Var = name].funs[3 = Var]
        Class2 /= variable orelse Type1 == Type2,
        ?LocalError(type_mismatch, [FullExpr, {Print2, Type2}, Type1])),

    ?Check( %[(name='name') = arity]
        Class1 /= compop orelse Type2 == bool,
        ?LocalError(type_mismatch, [FullExpr, {Print2, Type2}, bool])),

    ?Check( %[arity = (name='name')]
        Class2 /= compop orelse Type1 == bool,
        ?LocalError(type_mismatch, [FullExpr, {Print1, Type1}, bool])),

    ProcOf = fun({Op, Class, _, _}, S, {_, _, OtherType, _}) ->
        case {Class, Op} of
            {unbound_variable, {variable, VarName}} ->
                {Op, S#state{variables = add_variable(VarName, OtherType, S)}};
            {compop, _} ->
                check_filter(S, Op);
            _ ->
                {Op, S}
        end
    end,

    {Expr1, State1} = ProcOf({Op1, Class1, Type1, Print1}, State,
                             {Op2, Class2, Type2, Print2}),

    {Expr2, State2} = ProcOf({Op2, Class2, Type2, Print2}, State1,
                             {Op1, Class1, Type1, Print1}),

    {{CompOp, Expr1, Expr2}, State2};

% check_filter(_State, {_CompOp, {quoted, _}, {quoted, _}}) ->
%     throw(?LocalError(no_property_in_comparison, []));

% check_filter(State, {CompOp, {quoted, _} = F1, F2}) ->
%     check_filter(State, {CompOp, F2, F1});

% check_filter(#state{type=Type}=State, {CompOp, F1, {quoted, _} = F2} = Expr) ->
%     case F1 of
%         {variable, Var} ->
%             case lookup_variable(Var, State) of
%                 {_,_} -> {Expr, State};
%                 %none when CompOp == '=='->
%                 _ -> throw(?LocalError(unbound_variable, Var))
%             end;
%         _ when is_tuple(F1) -> 
%         {CheckedFilter, NewState} = check_filter(State, F1),
%         {{CompOp, CheckedFilter, F2}, NewState};
%         _ when is_atom(F1)  ->
%             PropType1 = ?Lib:prop_type(Type, F1),
%             ?Check(ordsets:is_element(
%                 PropType1,
%                 [[atom], [bool], [int], [string]]
%             ),
%                    ?LocalError(no_property_in_comparison, [])),
%             {Expr, State};
%         _ ->
%             throw(?LocalError(no_property_in_comparison, []))
%     end;

% check_filter(State, {_CompOp, {variable, Variable1}, {variable, Variable2}}=Expr)->
%     VariableInfo1 = lookup_variable(Variable1, State),
%     VariableInfo2 = lookup_variable(Variable2, State),

%     case {VariableInfo1, VariableInfo2} of
%       {{Type, _}, {Type, _}} ->
%           {Expr, State};
%       {{Type1, _}, {Type2, _}} ->
%           throw(?LocalError(type_mismatch,
%                             [Expr, {Variable1, Type1}, Type2]));
%       {none, _} ->
%           throw(?LocalError(unbound_variable, Variable1));
%       {_, none} ->
%           throw(?LocalError(unbound_variable, Variable2))
%     end;

% check_filter(#state{type=Type} = State,
%            {CompOp, F1, {variable, Variable}} = Expression) ->
%     TypeOfProperty = ?Lib:prop_type(Type, F1),
%     [TypeOfLiteral] = type_of(F1),
%     NewState = 
%       case lookup_variable(Variable, State) of
%           {VariableType, _} when [VariableType] == TypeOfProperty ->
%               State;    
%           {VariableType, _} when TypeOfProperty /= [] ->
%               throw(?LocalError(type_mismatch, 
%                                 [Expression,
%                                  {Variable, VariableType},
%                                  hd(TypeOfProperty)]));
%           {VariableType, _} when VariableType == TypeOfLiteral ->
%               State;
%           {VariableType, _} ->
%               throw(?LocalError(type_mismatch,
%                                 [Expression,
%                                  {Variable, VariableType},
%                                  TypeOfLiteral]));
%           none when (CompOp == '==') andalso (TypeOfProperty /= []) ->
%               State#state{
%                 variables = add_variable(Variable, hd(TypeOfProperty), State)};
%           none when (CompOp == '==') ->
%               throw(?LocalError(no_property_in_comparison, []));
%           none when (CompOp /= '==') ->
%               throw(?LocalError(unbound_variable, Variable))
%       end,
%     {Expression, NewState};

% check_filter(State, {CompOp, F1 = {variable, _}, F2}) ->
%     check_filter(State, {CompOp, F2, F1});

% check_filter(State, {CompOp, F1, F2}) when is_tuple(F1) andalso is_tuple(F2) ->
%     {CheckedF1, NewState1} = check_filter(State, F1),
%     {CheckedF2, NewState2} = check_filter(NewState1, F2),
%     {{CompOp, CheckedF1, CheckedF2}, NewState2};

% check_filter(#state{type=Type} = State, {CompOp, F1, F2}) when is_tuple(F1) ->
%     ?Check(?Lib:prop_type(Type, F2) == [bool] orelse is_boolean(F2),
%            ?LocalError(type_mismatch, [{filter,{CompOp, F1, F2}}, F2, bool])),
%     {CheckedF1, NewState} = check_filter(State, F1),
%     {{CompOp, CheckedF1, F2}, NewState};

% check_filter(State, {CompOp, F1, F2}) when is_tuple(F2) ->
%     {{CompOp, CheckedF2, F1}, NewState} =
%     check_filter(State, {CompOp, F2, F1}),
%     {{CompOp, F1, CheckedF2}, NewState};

% check_filter(#state{type=Type}=State, {CompOp, F1, F2})
%   when
%       is_atom(F1) andalso is_atom(F2)->
    
%     PropType1 = ?Lib:prop_type(Type, F1),
%     PropType2 = ?Lib:prop_type(Type, F2),
%     ?Check(PropType1 /= [] orelse PropType2 /= [],
%            ?LocalError(no_property_in_comparison, [])),

%     Type1 = case PropType1 of [] -> atom; [H] -> H end,
%     Type2 = case PropType2 of [] -> atom; [Hd] -> Hd end,

%     ?Check(merged_type(Type1, Type2) /= null,
%             ?LocalError(type_mismatch, [{filter, {CompOp, F1, F2}},
%                                                   {F1, Type1}, Type2])),
%     {{CompOp, F1, F2}, State};

% check_filter(#state{type=Type} = State, {CompOp, F1, F2}) when is_atom(F1) ->
%     PropType = ?Lib:prop_type(Type, F1),
%     ?Check(PropType /= [],
%            ?LocalError(no_property_in_comparison, [])),

%     ?Check(merged_type(hd(PropType), hd(type_of(F2))) /= null,
%            ?LocalError(type_mismatch,
%                        [{filter, {CompOp, F1, F2}}, F2, hd(PropType)])),
%     {{CompOp, F1, F2}, State};

% check_filter(State, {CompOp, F1, F2}) when is_atom(F2) ->
%     {{CompOp, CheckedF2, CheckedF1}, State} = 
%     check_filter(State, {CompOp, F2, F1}),
%     {{CompOp, CheckedF1, CheckedF2}, State};

% check_filter(_Type, {_CompOp, _F1, _F2}) ->
%     throw(?LocalError(no_property_in_comparison, []));

check_filter(#state{type=Type} = State, Filter) ->
    PropType = ?Lib:prop_type(Type, Filter),
    case PropType of
        [bool] ->
            {Filter, State};
        [] ->
            throw(?LocalError(illegal_property, [Type, Filter]));
        _ ->
            throw(?LocalError(type_mismatch,
                              [{filter,Filter}, {Filter, hd(PropType)}, bool]))
    end.

type_of(E) when is_integer(E) -> [int];
type_of(true) -> [bool];
type_of(false) -> [bool];
type_of(E) when is_atom(E) -> [atom];
type_of([H|_]) when is_integer(H) -> [string];
type_of(E) when is_list(E) -> [list];
type_of(_) -> [any].
%%% ============================================================================
%%% Processing of queries

process_set_op_operand(State, Q) ->
    case Q of
        [{sem_query, SQ}] ->
            {QuerySeq, QuerySeqLastPart} = split_semantic_query(SQ),
            fun(_, SomeState) ->
                    StartState = #state{params = State#state.params,
                                        variables = SomeState#state.variables},
                    process_query(StartState, QuerySeq, QuerySeqLastPart)
            end;
        _ ->
            fun (Entities, SomeState) -> 
                    process_query(
                      #state{
                         res = Entities,
                         type = State#state.type,
                         params = State#state.params,
                         variables = SomeState#state.variables},
                      Q,
                      [])
            end
    end.

process_setop({Setop, Q1, Q2}, State = #state{entitynumber = EntNumber,
                                              groupby_place = GbPlace})
  when EntNumber < GbPlace ->
    Proc1 = process_set_op_operand(State, Q1),
    Proc2 = process_set_op_operand(State, Q2),

    Merge = fun (Q1State, Q2States) ->
                    lists:map(fun(Q2State) ->
                                      Merged = merge_results(Setop, Q1State,
                                                             Q2State),
                                      Q2State#state{res = Merged}
                              end,
                              Q2States)
            end,
    Entities = State#state.res,
    ProcessedQ1 = Proc1(Entities, State),
    ProcessedQ2 = lists:map(fun (Q1State) ->
                                    Proc2(Entities, Q1State)
                            end,
                            ProcessedQ1),
    Merged = lists:append(lists:zipwith(Merge, ProcessedQ1, ProcessedQ2)),
    lists:map(fun (Processed) ->
                      grouping(Processed, State)
              end,
              Merged);

process_setop({Setop, Q1, Q2}, State) ->
    Proc1 = process_set_op_operand(State, Q1),
    Proc2 = process_set_op_operand(State, Q2),

    NeedsGrouping = State#state.entitynumber == State#state.groupby_place,
    IsEmpty = is_empty_state(State),
    {Groups, GroupbyRes, GroupbyType} =
        if IsEmpty ->
                {[ [] ], [ [] ], State#state.groupby_type};
           NeedsGrouping andalso IsEmpty ->
                {[ [] ], [ [] ], State#state.type};
           NeedsGrouping ->
                {[[Entity] || Entity <- State#state.res],
                 State#state.res,
                 State#state.type};
           true ->
                {State#state.res,
                 State#state.groupby_res,
                 State#state.groupby_type}
        end,

    Merge = fun (Q1State, Q2States) ->
                    lists:map(fun(Q2State) ->
                                      Merged = merge_results(Setop, Q1State,
                                                             Q2State),
                                      Action = case Q2State#state.action of
                                                   property_query ->
                                                       composite_property_query;
                                                   OtherAction ->
                                                       OtherAction
                                               end,
                                      Q2State#state{action = Action,
                                                    res = [Merged]}
                              end,
                              Q2States)
            end,
    lists:append(
      lists:zipwith(
        fun (Group, GroupingEntity) ->
                ProcessedQ1 = Proc1(Group, State),
                ProcessedQ2 = lists:map(fun (Q1State) ->
                                                Proc2(Group, Q1State)
                                        end,
                                        ProcessedQ1),
                Merged = lists:append(
                           lists:zipwith(Merge, ProcessedQ1, ProcessedQ2)),
                FirstQ1 = hd(ProcessedQ1),
                FirstQ2 = hd(hd(ProcessedQ2)),
                GroupingState = State#state{res = [GroupingEntity],
                                            type = GroupbyType,
                                            groupby_res = [GroupingEntity],
                                            groupby_type = GroupbyType},
                if FirstQ1#state.type /= FirstQ2#state.type ->
                        lists:map(fun (Processed) ->
                                          property_to_string(
                                            grouping(Processed, GroupingState))
                                  end,
                                  Merged);
                   true ->
                        lists:map(fun (Processed) ->
                                          grouping(Processed, GroupingState)
                                  end,
                                  Merged)
                end           
        end,
        Groups,
        GroupbyRes)).

rpc_yield(Key) ->
    ?MISC:yield(Key).

append_parallel_states(State = #state{parallel_key = Keys}) ->
    
    ParallelStates = lists:flatmap(fun rpc_yield/1, Keys),
    [State#state{parallel_key = []} | ParallelStates].

process({initial_selector, {set_op, {SetOp, Q1, Q2}}}, State) ->
    Params = State#state.params,
    {GroupbyPlace1, GroupbyPlace2} = case proplists:get_value(groupby, Params) of
                                         default ->
                                             {entitynumber_increment(Q1),
                                              entitynumber_increment(Q2)};
                                         undefined ->
                                             {undefined, undefined};
                                         GroupbyPlace ->
                                             {GroupbyPlace, GroupbyPlace}
                                     end,

    StartState1 = State#state{groupby_place = GroupbyPlace1},

    {QuerySeq1, QuerySeqLastPart1} = split_semantic_query(Q1),
    {QuerySeq2, QuerySeqLastPart2} = split_semantic_query(Q2),

    Query1 = QuerySeq1 ++ QuerySeqLastPart1,
    Query2 = QuerySeq2 ++ QuerySeqLastPart2,
    
    ProcessedQ1 = process_query(StartState1, Query1, []),
    ProcessQ2 = 
        fun (Q1State) ->
                StartState2 = State#state{
                                groupby_place = GroupbyPlace2,
                                variables = Q1State#state.variables},
                ProcessedQ2 = process_query(StartState2, Query2, []),
                lists:map(fun (Q2State) ->
                                  {Res1, Res2} = property_to_string(Q1State,
                                                                    Q2State),
                                  MergedResult = merge_states(SetOp, Res1, Res2),
                                  filter_empty_groups(MergedResult)
                          end,
                          ProcessedQ2)
        end,
    StatesOfSetop = lists:flatmap(ProcessQ2, ProcessedQ1),
    case StatesOfSetop of
        [Singleton] ->
            Singleton;
        [Head | Tail] ->
            RemainingQuery = State#state.semquery,
            RemainingQueryLastPart = State#state.semquery_last,
            Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                                 [Tail, RemainingQuery, RemainingQueryLastPart]),
            Head#state{parallel_key = [Key]}
    end;

process({initial_selector, {quoted, InitialSelector}}, State) ->
    process({initial_selector, InitialSelector}, State);

process({initial_selector, InitialSelector}, #state{params = Params} = State) ->
    {NodeType, NodeList} =
        case proplists:get_value(node_list, Params) of
            undefined ->
                case InitialSelector of
                    undef -> {none, []};
                    _ ->
                        ?Lib:init_sel([{ask_missing,false} |Params],
                                      InitialSelector)
                end;
            [] ->
                {none, []};
            [HNode|TNodes] = Nodes ->
                Type = proplists:get_value(node_type, Params,
                                           ?Lib:node_type(HNode)),

                NT = [{Node, ?Lib:node_type(Node)}|| Node <- TNodes],
                Diffs = lists:filter(fun({_N, NType}) -> NType/=Type end, NT),
                case Diffs of
                    [] ->
                        {Type, Nodes};
                    _ ->
                        Params = ["initial state", hd(Diffs), Type],
                        throw(?LocalError(type_mismatch, Params))
                end
        end,
    State#state{type=NodeType, res=ordsets:from_list(NodeList)};

process({set_op, Setop}, State) when is_record(State, state) ->
    States = process_setop(Setop, State),
    case States of
        [Singleton] ->
            Singleton;
        [Head | Tail] ->
            RemainingQuery = State#state.semquery,
            RemainingQueryLastPart = State#state.semquery_last,
            Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                                 [Tail, RemainingQuery, RemainingQueryLastPart]),
            Keys = [Key | State#state.parallel_key],
            Head#state{parallel_key = Keys}
    end;

process({selector, Selector, SelType}, St=#state{type = Type,
                                                 res = Entities, 
                                                 entitynumber=EntNum,
                                                 groupby_place=GbyPlace}) ->
    [Fun] = ?Lib:sel_fun(Type, Selector),
    Sort = fun lists:usort/1,
    PFun = case (Entities == []) orelse (not is_list(hd(Entities))) of
        true ->
            fun(E)->Sort(lists:flatten(Fun(E))) end;
        _ ->
            fun(E)->Sort(lists:flatten([Fun(F) || F <- E])) end
    end,
    Res = pmap({?MODULE, pmap_helper}, [PFun], Entities),
    Result = St#state{
               action = selection,
               type = SelType,
               res = case EntNum >= GbyPlace of
                         true -> Res;
                         false -> ordsets:union(Res)
                     end},
    grouping(Result, St);        

process({iteration, _QSLst, 0}, State=#state{entitynumber=EntNum}) ->
    State#state{entitynumber = EntNum + 1};

process({iteration, QSLst, Mult}, State=#state{entitynumber=EntNum}) ->
    EntityNumber =
        if
            State#state.groupby_place > EntNum ->
                Increment = entitynumber_increment(QSLst),
                EntNum - Increment;
            true ->
                EntNum
        end,
    SubQueryState = State#state{entitynumber = EntityNumber},
    NewState = process_closure_or_iteration_body(SubQueryState, QSLst),
    Iteration = {iteration, QSLst, Mult - 1},
    process(Iteration, NewState);

process({closure, QSLst, Mult}, #state{res=Ents,
                                       entitynumber=EntNum,
                                       groupby_place=GbyPlace}=St)->
    ?Check(length(ets:all())  < ?ets_limit,
           ?LocalError(ets_limit_reached, [])),
    Tab = ets:new(store,
                  [public, bag, {write_concurrency, true}, {keypos, 2}]),
    ProcessParallel =
        fun(Entity, N)->
                ?MISC:async_call(node(), ?MODULE, closure_worker,
                               [St#state{res=Entity}, QSLst, Mult, Tab, N]) 
        end,
    case EntNum >= GbyPlace of 
        true->
            Entities = case EntNum of
                           GbyPlace -> [[E]||E<-Ents];
                           _ -> Ents
                       end,
            Length = lists:foldl(
                       fun(Group, Counter)->
                               [ets:insert(Tab, {Counter, Entity})
                                || Entity <- Group],
                               Counter + 1 end,
                       1,
                       Entities) - 1,        
            {_, KeyList} =
                lists:foldl(           
                  fun (Entity, {Counter, KeyList})->
                          {Counter + 1,
                           [ProcessParallel(Entity, Counter) | KeyList]} end,
                  {1, []},
                  Entities),
            lists:foreach(fun rpc_yield/1, KeyList),
            Res = [ets:select(Tab, [{{N, '$2'}, [], ['$2']}]) ||
                      N <- lists:seq(1, Length)],
            GroupbyRes = St#state.groupby_res;
        false ->
            [ets:insert(Tab, {0, E}) || E <- Ents],
            EntityLists = group_entities(Ents),
            KeyList = [ProcessParallel(E, 0) || E <- EntityLists],
            lists:foreach(fun rpc_yield/1, KeyList),
            Res = ets:select(Tab, ets:fun2ms(fun({0, X}) -> X end)),
            GroupbyRes = Res
    end,
    Result = St#state{action = selection, res = Res, groupby_res = GroupbyRes},
    ets:delete(Tab),
    grouping(Result, St);

process({closure_seq, QSLst, Mult, VariablesToBind},
        State = #state{entitynumber = EntNumber, groupby_place = GbPlace,
                       closure_res = undefined})
  when EntNumber < GbPlace ->
    BaseSet = gb_sets:from_list(State#state.res),
    SetofVariableBinding = gb_sets:empty(),
    StartState = State#state{closure_res = BaseSet, parallel_key = []},
    {ResultSet, ResultBaseSet, ResultState} = 
        closure_worker_seq(StartState, QSLst, Mult, SetofVariableBinding,
                           BaseSet),
    RemainingQuery = tl(State#state.semquery),
    RemainingQueryLastPart = State#state.semquery_last, 
    StateWithoutNewVariableBindings = 
        State#state{
          res = gb_sets:to_list(ResultBaseSet),
          entitynumber = EntNumber + 1,
          closure_res = undefined,
          parallel_key = []},
    MarkedState =
        mark_undefined_as_valueless(VariablesToBind,
                                    StateWithoutNewVariableBindings),
    Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                         [[MarkedState],
                          RemainingQuery,
                          RemainingQueryLastPart]),
    Keys = [Key | State#state.parallel_key ++ ResultState#state.parallel_key],
    EntitiesWithVariableBinding = gb_sets:to_list(ResultSet),
    ResultState#state{res = EntitiesWithVariableBinding,
                      closure_res = undefined,
                      entitynumber = EntNumber + 1,
                      parallel_key = Keys};

process({closure_seq, QSLst, Mult, _}, 
        State = #state{entitynumber = EntNumber, groupby_place = GbPlace})
  when EntNumber < GbPlace ->
    ClosureEntities = State#state.closure_res,
    {NewEntities, SetofVariableBinding, BaseSet} = 
        filter_closure_result(State#state.res, gb_sets:empty(), ClosureEntities),
    StartState =
        State#state{res = NewEntities, closure_res = undefined,
                    parallel_key = []},
    {ResultSet, _ResultBaseSet, ResultState} = 
        closure_worker_seq(StartState, QSLst, Mult, SetofVariableBinding,
                           BaseSet),
    EntitiesWithVariableBinding = gb_sets:to_list(ResultSet),
    ResultState#state{res = EntitiesWithVariableBinding,
                      closure_res = undefined,
                      entitynumber = EntNumber + 1};

process({closure_seq, QSLst, Mult, VariablesToBind},
        State = #state{closure_res = undefined}) ->
    #state{res = Entities,
           groupby_place = GbPlace,
           entitynumber = EntNumber,
           type = Type,
           groupby_type = GbType} = State,
    {GroupingEntities, Groups, GroupbyType} = 
        if EntNumber == GbPlace ->
                {Entities, [[Entity] || Entity <- Entities], Type};
           true ->
                {State#state.groupby_res, Entities, GbType}
        end,
    Store = fun(GroupingEntity, Group, Dict)->
                    dict:store(GroupingEntity, 
                               {gb_sets:empty(), gb_sets:from_list(Group)},
                               Dict) end,
    GroupsWithoutVariableBinding = foldl2(Store,
                                          dict:new(),
                                          GroupingEntities,
                                          Groups),
    StartState = State#state{closure_res = GroupsWithoutVariableBinding,
                             parallel_key = []},
    {ResultSets, ResultState} = closure_worker_seq(StartState, QSLst, Mult,
                                                   GroupsWithoutVariableBinding),
    GroupWithoutBind =
        fun (GroupingEntity) -> 
                {_, GroupWithoutBind} = dict:fetch(GroupingEntity, ResultSets),
                gb_sets:to_list(GroupWithoutBind) end,
    RemainingQuery = tl(State#state.semquery),
    RemainingQueryLastPart = State#state.semquery_last,
    GroupByRes = dict:fetch_keys(ResultSets),
    StateWithoutNewVariableBindings = 
        State#state{
          res = lists:map(GroupWithoutBind, GroupByRes),
          groupby_res = GroupByRes,
          groupby_type = GroupbyType,
          entitynumber = EntNumber + 1,
          closure_res = undefined,
          parallel_key = []},
    MarkedState =
        mark_undefined_as_valueless(VariablesToBind,
                                    StateWithoutNewVariableBindings),
    Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                         [[MarkedState],
                          RemainingQuery,
                          RemainingQueryLastPart]),
    Keys = [Key | State#state.parallel_key ++ ResultState#state.parallel_key],
    GroupWithBoundVariable = 
        fun (GroupingEntity) ->
                {GroupWithBind, _} = dict:fetch(GroupingEntity, ResultSets),
                gb_sets:to_list(GroupWithBind) end,
    ActualGroupByRes = ResultState#state.groupby_res,
    State#state{
      res = lists:map(GroupWithBoundVariable, ActualGroupByRes),
      groupby_res = ActualGroupByRes,
      groupby_type = GroupbyType,
      entitynumber = EntNumber + 1,
      closure_res = undefined,
      variables = ResultState#state.variables,
      parallel_key = Keys};

process({closure_seq, QSLst, Mult, _}, State) ->
    #state{res = [Group],
           groupby_res = [GroupingEntity],
           closure_res = ClosureEntities,
           entitynumber = EntNumber} = State,    
    {GroupWithVariableBinding, 
     GroupWithoutVariableBinding} = dict:fetch(GroupingEntity,
                                               ClosureEntities),
    {ReallyNewEntities, 
     GroupWithVariableBinding_New,
     GroupWithoutVariableBinding_New} = 
        filter_closure_result(Group, 
                              GroupWithVariableBinding,
                              GroupWithoutVariableBinding),
    NewDict = dict:from_list([{GroupingEntity,
                               {GroupWithVariableBinding_New,
                                GroupWithoutVariableBinding_New}}]),
    Increment = entitynumber_increment(QSLst),
    OriginalEntityNumber = EntNumber-Increment,
    StartState  = State#state{
                    res = [ReallyNewEntities],
                    entitynumber = OriginalEntityNumber,
                    closure_res = undefined,
                    parallel_key = []},
    {ResultSets, ResultState} = 
        closure_worker_seq(StartState, QSLst, Mult, NewDict),
    GroupWithBoundVariable = 
        fun (SomeGroupingEntity) ->
                {GroupWithBind, _} = dict:fetch(SomeGroupingEntity, ResultSets),
                gb_sets:to_list(GroupWithBind) end,
    ActualGroupByRes = ResultState#state.groupby_res,
    IncrementedEntityNumber = OriginalEntityNumber + 1,
    State#state{
      res = lists:map(GroupWithBoundVariable, ActualGroupByRes),
      groupby_res = ActualGroupByRes,
      groupby_type = StartState#state.groupby_type,
      entitynumber = IncrementedEntityNumber,
      closure_res = undefined,
      variables = ResultState#state.variables};

process({filter, Filter},
        St = #state{res = Res, entitynumber = EntNum, groupby_place = GbyPlace}) ->
    NonGrouped = to_nongrouped_state(St),
    St#state{
      res = case GbyPlace < EntNum of 
                true ->
                    pmap({?MODULE, pmap_helper},
                         [fun(Ent) -> 
                                  res(filter(Filter, NonGrouped#state{res = Ent}))
                          end],
                         Res);
                false ->
                    res(filter(Filter, St))
            end};

process({filter_with_variable_to_bind, _, _} = FilterElement, State) ->
    process_filter_with_variable_to_bind(FilterElement, State);

process(QueryElement, State) ->
    process_last(QueryElement, State).
           
group_entities(Entities) ->
    NumberOfData = length(Entities),
    Divide =
        case erlang:system_info(logical_processors_available) of
            unknown ->
                (NumberOfData div 10) + 1;
            N ->
                (NumberOfData div (N*10)) + 1
        end,
    ?MISC:slice_list(Divide, Entities).

%% @private
closure_worker(_State, _QSLst, 0, _Tab, _Group) ->
    ok;
closure_worker(#state{res=[]}, _QSLst, _Mult, _Tab, _Group) ->
    ok;
closure_worker(State, QSLst, Mult, Tab, Group) ->
    StateToProc = State#state{
        entitynumber = -entitynumber_increment(QSLst),
        groupby_type = [],
        groupby_res = []},
    #state{res=NewEntities} = 
        process_query_seq(StateToProc, QSLst),
    ReallyNewEntities =
        lists:filter(fun(Entity) -> ets:match(Tab, {Group, Entity}) == [] end,
                     NewEntities),
    lists:foreach(fun (Entity) -> ets:insert(Tab, {Group, Entity}) end,
                  ReallyNewEntities),
    NewMult = case Mult of infinite = I -> I; M -> M-1 end,
    NewState = State#state{res=ReallyNewEntities},
    closure_worker(NewState, QSLst, NewMult, Tab, Group).


closure_worker_seq(State, _QSLst, 0, SetWithVariableBinding, BaseSet) ->
    {SetWithVariableBinding, BaseSet, State};

closure_worker_seq(State=#state{entitynumber=EntNumber, groupby_place=GbPlace},
                   QSLst, Mult, SetWithVariableBinding, BaseSet)
  when EntNumber < GbPlace ->
    case is_empty_state(State) of
        true ->
            {SetWithVariableBinding, BaseSet, State};
        false ->
            Increment = entitynumber_increment(QSLst),
            StartState = State#state{entitynumber = EntNumber - Increment},
            SubQueryState = process_closure_or_iteration_body(StartState, QSLst),
            #state{res = NewEntities, 
                   variables = Variables, 
                   parallel_key = Keys} = SubQueryState,
            {ReallyNewEntities, 
             EntitiesWithVariableBinding,
             EntitiesWithoutVariableBinding} = 
                filter_closure_result(NewEntities, SetWithVariableBinding, BaseSet),
            NewMult = case Mult of infinite = I -> I; M -> M - 1 end,
            NewState = State#state{res = ReallyNewEntities, 
                                   variables = Variables,
                                   parallel_key = Keys},
            closure_worker_seq(NewState, QSLst, NewMult, 
                               EntitiesWithVariableBinding,
                               EntitiesWithoutVariableBinding)
    end.

closure_worker_seq(State, _QSLst, 0, Sets) ->
    {Sets, State};

closure_worker_seq(State, QSLst, Mult, Sets) ->
    SubQueryState = process_closure_or_iteration_body(State, QSLst),
    case is_empty_state(SubQueryState) of
        true ->
            {Sets, State};
        false ->
            #state{res = [Group],
                   groupby_res = [GroupingEntity]} = SubQueryState,
            {SetofActualGroup, BaseSet} = dict:fetch(GroupingEntity, Sets),
            {ReallyNewEntities, 
             EntitiesWithVariableBinding,
             EntitiesWithoutVariableBinding} = 
                filter_closure_result(Group, SetofActualGroup, BaseSet),
            NewSets = dict:store(GroupingEntity, 
                                 {EntitiesWithVariableBinding,
                                  EntitiesWithoutVariableBinding},
                                 Sets),
            NewMult = case Mult of infinite = I -> I; M -> M - 1 end,        
            NewState = SubQueryState#state{
                         res = [ReallyNewEntities],
                         entitynumber = State#state.entitynumber,
                         closure_res = undefined},
            closure_worker_seq(NewState, QSLst, NewMult, NewSets)
    end.

grouping(NewState, #state{groupby_place=GroupBy, entitynumber=EntNum}=PrevState) ->
    Res =
        if
            EntNum == GroupBy ->
                NewState#state{
                  groupby_type = PrevState#state.type,
                  groupby_res = PrevState#state.res,
                  entitynumber = EntNum+1,
                  groupby_place = GroupBy,
                  params = PrevState#state.params,
                  semquery = PrevState#state.semquery,
                  semquery_last = PrevState#state.semquery_last};
            true ->
                NewState#state{
                  groupby_type = PrevState#state.groupby_type,
                  groupby_res = PrevState#state.groupby_res,
                  entitynumber = EntNum+1,
                  groupby_place = GroupBy,
                  params = PrevState#state.params,
                  semquery = PrevState#state.semquery,
                  semquery_last = PrevState#state.semquery_last}
        end,
    case Res#state.groupby_res of
        [] -> filter_empty_results(Res);
        _ -> filter_empty_groups(Res)
    end.

process_last({closure_seq, QSLst, Mult, _}, State)->
    process_last({closure, QSLst, Mult}, State);

process_last({selector, _, Type}, #state{res=[]}=State)->
    Res = State#state{
            action=selection,
            type=Type},
    grouping(Res, State);

process_last({property, Property, Type}, #state{res=[]}=State) ->
    Res = State#state{
            action=property_query,
            type=Type,
            name=Property},
    grouping(Res, State);

process_last(QueryElement, #state{res=[]}=State) 
  when (element(1, QueryElement) /= iteration) andalso
       (element(1, QueryElement) /= closure) andalso
       (element(1, QueryElement) /= set_op) ->
    State#state{groupby_res=[]};

process_last({set_op, SetOp}, State) when is_record(State, state) ->
    States = process_setop(SetOp, State),
    case States of
        [Singleton] ->
            Singleton;
        [Head | Tail] ->
            RemainingQuery = State#state.semquery,
            RemainingQueryLastPart = State#state.semquery_last,
            Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                                 [Tail, RemainingQuery, RemainingQueryLastPart]),
            Keys = [Key | State#state.parallel_key],
            Head#state{parallel_key = Keys}
    end;

process_last({selector, Selector, SelType}, St=#state{type=Type, res=Entities}) ->
    [Fun] = ?Lib:sel_fun(Type, Selector),
    Sort = fun lists:usort/1,
    PFun = case Entities == [] orelse (not is_list(hd(Entities))) of
        true ->
            fun(E)->Sort(lists:flatten(Fun(E))) end;
        _ ->
            fun(E)->Sort(lists:flatten([Fun(F) || F <- E])) end
    end,
    Res = pmap({?MODULE, pmap_helper}, [PFun], Entities),
    NewState = St#state{
            action = selection,
            type = SelType,
            res = Res},
    grouping(NewState, St);

process_last({property, Prop, PropType}, St=#state{type=Type}) ->
    [Fun] = ?Lib:prop_fun(Type, Prop),
    Sort = fun lists:usort/1,
    Res = case St#state.groupby_type of
              [] ->
                  St#state{
                    action = property_query,
                    type = PropType,
                    name = Prop,
                    res = pmap({?MODULE, pmap_helper}, [Fun], St#state.res)};
              _ ->
                  St#state{
                    action = composite_property_query,
                    type = PropType,
                    name = Prop,
                    res = pmap({?MODULE, pmap_helper},
                               [fun(Group)->Sort(lists:map(Fun, Group)) end],
                               St#state.res)}
          end,
    grouping(Res, St);

process_last({variable, Variable}, St=#state{variables=BoundVariables}) ->
    Value = case proplists:lookup(Variable, BoundVariables) of
                {_, BoundValue} ->
                    [BoundValue];
                none ->
                    []
            end,
    Res = St#state{
            action = property_query,
            type = name,  %TODO
            name = Variable,
            res = case St#state.groupby_type of 
                      [] ->
                          lists:duplicate(length(St#state.res), Value);
                      _ ->
                          lists:duplicate(length(St#state.groupby_res), Value)
                  end},

    grouping(Res, St);

process_last({variable_match, _CompOp, Variable}, St) ->
    #state{res=Entities, groupby_type=GbType, variables=BoundVariables} = St,
    case proplists:lookup(Variable, BoundVariables) of
        none ->
            St;
        {_, BoundValue} when GbType == [] ->
            FilteredRes =
                lists:filter(fun (Entity) -> BoundValue == Entity end,
                             Entities),
            St#state{res=FilteredRes};
        {_, BoundValue} ->
            FilterOneGroup =
                fun (Group) ->
                        lists:filter(fun (Entity) -> BoundValue == Entity end,
                                     Group)
                end,
            FilteredGroups = lists:map(FilterOneGroup, Entities),
            St#state{res=FilteredGroups}
    end;

process_last({filter, Filt}, #state{action=iteration, res=Res}=St) ->
    NewIncomplete = lists:filter(
                      fun(Chain) ->
                              State = filter(Filt, St#state{res=[hd(Chain)]}),
                              State#state.res /= [] end,
                      Res#chains.incomplete),    
    St#state{res = Res#chains{incomplete = NewIncomplete}};

process_last({filter, Filt}, #state{action=closure, res=Res}=St) ->
    NewComplete = filter_chain(St, Filt, Res#chains.complete),
    NewIncomplete = filter_chain(St, Filt, Res#chains.incomplete),
    NewRecursive = filter_chain(St, Filt, Res#chains.recursive),

    NewChains = Res#chains{complete   = NewComplete,
                           incomplete = NewIncomplete,
                           recursive  = NewRecursive},

    St#state{res = NewChains};

process_last({filter, Filter}, State = #state{groupby_res = []}) ->
    filter(Filter, State);

process_last({filter, Filter}, State = #state{res = Groups}) ->
    NonGrouped = to_nongrouped_state(State),
    State#state{
      res = pmap({?MODULE, pmap_helper},
                 [fun(Group) ->
                          res(filter(Filter,
                                     NonGrouped#state{res = Group}))
                  end],
                 Groups)};

process_last({filter_with_variable_to_bind, _, _} = FilterElement, State) ->
    process_filter_with_variable_to_bind(FilterElement, State);

process_last({Action, QSLst, Mult}, #state{type=Type, res=Res}=State) ->
    InitialChains =
        case State#state.action of
            selection -> #chains{incomplete=[[Entity | State#state.chain]||
                        Entity <- lists:flatten(Res)]};
            Action -> Res
        end,
    Result = 
    State#state{
      action = Action,
      type = Type, 
      res = chains(State#state{action=Action}, InitialChains, QSLst, Mult)},
    grouping(Result, State);

% todo: preproc
process_last({statistics, Stat}, #state{res=PropValues}) ->
    NonNum = lists:filter(fun(Val) -> not is_number(Val) end, PropValues),

    ?Check(NonNum == [],
           ?LocalError(type_mismatch, [{statistics,Stat}, hd(NonNum), number])),

    Fun = ?Lib:stat_fun(Stat),
    #state{action = statistics, type = Stat, res = Fun(PropValues)}.

%%% ===================o=========================================================
%%% Helper functions
pmap({Module, Func}, ExtraArgs, List) ->
    % rpc:pmap({Module, Func}, ExtraArgs, List). %slower than ?MISC:parallelise
     ?MISC:parallelise(List, Module, Func,
                       fun(Entities)-> [Entities|ExtraArgs] end, false).

pmap_helper(Entities, Fun) when is_list(Entities) ->
     [Fun(Entity) || Entity <- Entities];
pmap_helper(Entity, Fun)->
     pmap_helper([Entity], Fun).

chains_worker(#state{chain_keys=Keys}, [], _QSLst, _Mult, _Tab) ->
    rethrow_badrpc(lists:map(fun rpc_yield/1, Keys));
chains_worker(#state{chain_keys=Keys}, Chains, _QSLst, 0, Tab) ->
    ets:insert(Tab, [{incomplete, Chain} || Chain <- Chains]),
    rethrow_badrpc(lists:map(fun rpc_yield/1, Keys));
chains_worker(St, Chains, QSLst, Mult, Tab) when is_record(St, state)->
    StartState = St#state{action = selection, 
                          entitynumber = - entitynumber_increment(QSLst),
                          groupby_type = []},
    NewChains =
        lists:flatmap(
          fun([H|_] = Chain) ->
                  #state{res = Res} =
                      process_closure_or_iteration_body(
                        StartState#state{res = [H]},
                        QSLst),
                  next_chain(St#state{res = Res}, Chain)
          end,
          Chains),
    Incomplete = insert_chains(Tab, NewChains),
    NewMult = case Mult of infinite -> infinite; Mult -> Mult-1 end,
    chains_worker(St, Incomplete, QSLst, NewMult, Tab).

chains_worker_first(#state{chain_keys=Keys}, [], _QSLst, _Mult, _Tab) ->
    rethrow_badrpc(lists:map(fun rpc_yield/1, Keys));
chains_worker_first(#state{action=Action}=St,
            Chains, QSLst, Mult, Tab)->
    StartState = St#state{action = selection, 
                          entitynumber = - entitynumber_increment(QSLst),
                          groupby_type = [],
                          parallel_key = []},
    {NewChains, States} =
        lists:unzip(
          lists:map(
            fun([H|_] = Chain) ->
                    NewState = #state{res=Res} =             
                        process_closure_or_iteration_body(
                          StartState#state{
                            res = [H],
                            chain = Chain}, 
                          QSLst),
                    {next_chain(St#state{res = Res}, Chain), NewState}
            end,
            Chains)),
    Incompletes =
        lists:map(fun(Chains_) -> insert_chains(Tab, Chains_) end, NewChains),
    NewMult = case Mult of infinite -> infinite; Mult -> Mult - 1 end,
    VariableBindingKeys =
        lists:flatmap(fun(State) -> State#state.parallel_key end, States),
    case has_variable_got_bound(States, St) of
        false ->            
            chains_worker(St, lists:append(Incompletes), QSLst, NewMult, Tab);
        true ->
            ProcessParallel =
                fun(State, Chain)->
                        ?MISC:async_call(node(), ?MODULE, chains_worker,
                                       [State#state{action = Action}, Chain,
                                        QSLst, NewMult, Tab])
                end,
            MoreKeys = lists:zipwith(ProcessParallel,
                                     tl(States),
                                     tl(Incompletes)),
            CurrentProcessState =
                (hd(States))#state{
                  action = Action,
                  chain_keys = VariableBindingKeys ++ MoreKeys,
                  parallel_key = St#state.parallel_key},
            CurrentProcessChains = hd(Incompletes),
            chains_worker(CurrentProcessState, CurrentProcessChains,
                          QSLst, NewMult, Tab)
    end.        

insert_chains(Table, NewChains)->
    SplitFun =
        fun({complete, _} = C, {Incomp, Comp, Rec}) ->
                {Incomp, [C | Comp], Rec};
           ({recursive, _} = R, {Incomp, Comp, Rec}) ->
                {Incomp, Comp, [R | Rec]};
           (List, {Incomp, Comp, Rec}) ->
                {[List | Incomp], Comp, Rec}
        end,
    {Incomplete, Completed, Recursive} =
        lists:foldl(SplitFun, {[], [], []}, NewChains),
    ets:insert(Table, Completed ++ Recursive),
    Incomplete.

chains(_State, #chains{incomplete = []} = Chains, _QSLst, _Mult) ->
    Chains;
chains(State=#state{chains_table = undefined},
       #chains{incomplete=Chains} = ChainsRec, QSLst, Mult) ->
    ?Check(length(ets:all()) < ?ets_limit,
           ?LocalError(ets_limit_reached, [])),
    Table = ets:new(store,
                    [public, bag, {write_concurrency, true}]),
    StartState = State#state{chains_table = Table},
    ChainGroups = group_entities(Chains),
    Keys = [?MISC:async_call(node(), ?MODULE, chains_worker_first,
                           [StartState, Ch, QSLst, Mult, Table])
            || Ch <- ChainGroups],
    Results = lists:map(fun rpc_yield/1, Keys),
    try
        rethrow_badrpc(Results),
        Recursive =
            ets:select(Table, ets:fun2ms(fun({recursive, Chain}) -> Chain end)),
        Completed =
            ets:select(Table, ets:fun2ms(fun({complete, Chain}) -> Chain end)),
        Incomplete =
            ets:select(Table, ets:fun2ms(fun({incomplete, Chain}) -> Chain end)),
        ChainsRec#chains{complete = Completed ++ ChainsRec#chains.complete,
                         incomplete = Incomplete,
                         recursive = Recursive ++ ChainsRec#chains.recursive}
    after
        ets:delete(Table)
    end;

chains(State=#state{chains_table=Table},
       #chains{incomplete=Chains}, QSLst, Mult) ->
    ChainGroups = group_entities(Chains),
    Keys = [?MISC:async_call(node(), ?MODULE, chains_worker,
                           [State, Ch, QSLst, Mult, Table])
            || Ch <- ChainGroups],
    rethrow_badrpc(lists:map(fun rpc_yield/1, Keys)).

next_chain(#state{action = iteration, res = Result}, Chain) ->
    lists:foldl(fun(Entity, Acc) -> [[Entity| Chain]|Acc] end, [], Result);
next_chain(#state{action = closure, res = []}, Chain) ->
    [{complete, Chain}];
next_chain(#state{action = closure, res = Result}, Chain) ->
    lists:foldl(
      fun(Entity, Acc) ->
              case lists:member(Entity, Chain) of
                  true -> [{recursive, [Entity|Chain]}|Acc];
                  _    -> [[Entity| Chain]|Acc]
              end
      end,
      [],
      Result).

filter_chain(State, Filter, Chain) ->
    ResFun = fun(List)-> res(filter(Filter, State#state{res=List})) end,
    NewChainWithEmpties = lists:map(ResFun, Chain),
    lists:filter( fun(List) -> List /= [] end, NewChainWithEmpties).

%%% ============================================================================
%%% Filters

%% TODO: entity listakat rendezni filter elott!!!!
%% @private
%% @spec filter(Filter::term(), #state{}) -> ordset()
filter(_Filter, #state{res = []} = State) -> State;

filter('true', State) -> State;

filter('false', State) -> State#state{res = []};

filter({'not', Filter}, #state{res = Entities} = State) ->
    NewState = filter(Filter, State),
    NewState#state{res = ordsets:subtract(Entities, NewState#state.res)};

filter({'or', Filter1, Filter2}, State) ->
    NewStateF1 = filter(Filter1, State),
    StatesForFilter2 = combine_variable_values(State,
                                               NewStateF1#state.variables),
    StatesOfFilter2 = lists:map(fun (StateForFilter2) ->
                                        filter(Filter2, StateForFilter2) end,
                                StatesForFilter2),
    case not_empty_states([NewStateF1 | StatesOfFilter2]) of
        [] ->
            State#state{res = []};
        NotEmptyStates ->
            merge_states(NotEmptyStates)
    end;

filter({'and', Filter1, Filter2}, State) ->
    NewStateF1 = filter(Filter1, State),
    StatesForFilter2 = combine_variable_values(NewStateF1),
    StatesOfFilter2 = lists:map(fun (StateForFilter2) ->
                                        filter(Filter2, StateForFilter2) end,
                                StatesForFilter2),
    case not_empty_states(StatesOfFilter2) of
        [] ->
            State#state{res = []};
        NotEmptyStates ->
            merge_states(NotEmptyStates)
    end;

filter({set_op_with_variable, {SetOp, Q1, Q2}}, State) ->
    Get_val = get_setop_value(State),
    Op1 = Get_val(Q1),
    Op2 = Get_val(Q2),
    [Entity] = State#state.res,
    Filter =
        case SetOp of
            any_in ->
                fun (Op1State, Op2State) ->
                        not(ordsets:is_disjoint(res(Op1State), res(Op2State)))
                end;
            all_in ->
                fun (Op1State, Op2State) ->
                        ordsets:is_subset(res(Op1State), res(Op2State))
                end
        end,
    FilterStates = fun (Op1State, Op2States) ->
                           lists:filter(fun (Op2State) ->
                                                Filter(Op1State, Op2State) end,
                                        Op2States) 
                   end,
    ProcessedOp1 = Op1(Entity, State),
    ProcessedOp2 = lists:map(fun (Op1State) ->
                                     Op2(Entity, Op1State)
                             end,
                             ProcessedOp1),
    FilteredStates = lists:append(
                       lists:zipwith(FilterStates, ProcessedOp1, ProcessedOp2)),
    case FilteredStates of
        [] ->
            State#state{res = []};
        [_ | _] ->
            Variables = collect_variable_values(FilteredStates),
            State#state{variables = Variables}
    end;

filter({set_op, {SetOp, Q1, Q2}}, State) ->
    Get_val = get_setop_value(State),
    Op1 = Get_val(Q1),
    Op2 = Get_val(Q2),
    Filter =
        case SetOp of
            any_in ->
                fun (Op1State, Op2State) ->
                        not(ordsets:is_disjoint(res(Op1State), res(Op2State)))
                end;
            all_in ->
                fun (Op1State, Op2State) ->
                        ordsets:is_subset(res(Op1State), res(Op2State))
                end
        end,
    FilteredEntities =
        ordsets:filter(
          fun (Entity) ->
                  [ProcessedOp1] = Op1(Entity, State),
                  [ProcessedOp2] = Op2(Entity, State),
                  Filter(ProcessedOp1, ProcessedOp2)
          end,
          State#state.res),
    State#state{res = FilteredEntities};
    
filter({seq_with_variable, QuerySeq}, #state{res = Entity} = State) ->
    FilteredState = 
        process_full_query(State#state{res = Entity,
                                       entitynumber = -length(QuerySeq)},
                           QuerySeq,
                           []),
    case FilteredState#state.res of
        [] ->
            State#state{res = []};
        _ ->
            State#state{variables = FilteredState#state.variables}
    end;

filter({seq, QuerySeq}, #state{res = Entities} = State) ->
    FilteredEntities = 
        ordsets:filter(
          fun(Entity) ->
                  St = process_full_query(
                         State#state{
                           res = [Entity],
                           entitynumber = -length(QuerySeq)},
                         QuerySeq,
                         []),
                  St#state.res /= []
          end,
          Entities),
    State#state{res = FilteredEntities};

% filter({CompOp, {variable, Variable1}, {variable, Variable2}}, State) ->
%     {value, Value1} = lookup_variable_value(Variable1, State),
%     {value, Value2} = lookup_variable_value(Variable2, State),
%     filter(compare(CompOp, Value1, Value2), State);

% filter({CompOp, PropertyOrLiteral, {variable, Variable}},
%        #state{type = Type} = State) ->
%     VariableValue = lookup_variable_value(Variable, State),
%     MaybePropFun = ?Lib:prop_fun(Type, PropertyOrLiteral),
%     case {VariableValue, MaybePropFun} of
%       {undefined, [PropFun]} ->
%           case State#state.res of
%               [Entity] ->
%                   State#state{
%                     variables = bind_variable(Variable, PropFun(Entity), State)};
%               _ ->
%                   filter(false, State)
%           end;
%       {undefined, []} ->
%           filter(false, State);
%       {{value, BoundValue}, [_]} ->           
%           filter({CompOp, BoundValue, PropertyOrLiteral}, State);
%       {{value, BoundValue}, []} ->
%           filter(compare(CompOp, BoundValue, PropertyOrLiteral), State)
%     end;

%% Comparison works on atom, int and string.
%% todo: regexp match -> if a string doesn't match
filter({CompOp, Filt1, Filt2}, #state{type=EntityType,res=Entities} = State) ->
    ParamFunG = fun(Param, PrevParam) ->
        case Param of
            {quoted, B} -> %[name = 'Var']
                fun(_) -> B end;
            {variable, Var} ->
                case lookup_variable_value(Var, State) of
                    {value, Value} -> fun(_) -> Value end;
                    undefined -> {variable, Var};
                    valueless -> {valueless_variable}
                end;
            _ when is_tuple(Param) -> %[(name=run) = (arity=3)]
                Res = res(filter(Param, State)),
                fun(_) -> Res end;
            PrevParam -> %[name=name]
                fun(_) -> Param end;
            _ when is_atom(Param) ->
                case ?Lib:prop_fun(EntityType, Param) of
                    [Fun] -> Fun;
                    _ -> fun(_) -> Param end
                end;
            _ -> fun(_) -> Param end
        end
    end,

    LFun = ParamFunG(Filt1, []),
    RFun = ParamFunG(Filt2, Filt1),
    
    IsProperty = fun (Operand) ->
                         is_atom(Operand) andalso
                             ?Lib:prop_fun(EntityType, Operand) /= [] end,
    Filt1IsProperty = IsProperty(Filt1),
    Filt2IsProperty = IsProperty(Filt2),

    case {LFun, RFun, Entities, CompOp} of
        {{variable, Var}, _, [Entity], '=='} when Filt2IsProperty ->
            bind_variable(Var, RFun(Entity), State);
        {_, {variable, Var}, [Entity], '=='} when Filt1IsProperty ->
            bind_variable(Var, LFun(Entity), State);
        {_, _, _, _} when is_tuple(LFun) orelse is_tuple(RFun) ->
            filter(false, State);
        _ ->
            DoesTypeMatter = (CompOp == '=:=') orelse (CompOp == '=/='),
            FilteredEntities = 
            ordsets:filter(
              fun(Entity) ->
                      CompL = LFun(Entity),
                      CompR = RFun(Entity),
                      HasSameType = type_of(CompL) == type_of(CompR),
                      if HasSameType orelse DoesTypeMatter ->
                              compare(CompOp, CompL, CompR);
                         true ->
                              compare(CompOp, to_string(CompL), to_string(CompR))
                      end
              end,
              Entities),
            State#state{res = FilteredEntities}
    end;

filter(Filter, #state{type = EntityType, res = Entities} = State) ->
    State#state{res = ordsets:filter(prop_fun(EntityType, Filter), Entities)}.

get_setop_value(State) ->
    ToOrdset = fun(Result = #state{res = Entities}) ->
                       Result#state{res = ordsets:from_list(Entities)} end,
    fun({cons, L}) ->
            fun(_, SomeState) -> 
                    [SomeState#state{res = ordsets:from_list(all_to_string(L))}]
            end;
       ({seq, S}) ->
            fun(E, SomeState) ->
                    Processed = process_query(
                                  #state{
                                     res = [E],
                                     type = State#state.type,
                                     params = State#state.params,
                                     variables = SomeState#state.variables
                                    },
                                  S,
                                  []),
                    lists:map(fun (Result) ->
                                      property_to_string(ToOrdset(Result)) end, 
                              Processed)
            end;
       ({'query', SemQuery}) ->
            fun(_, SomeState) -> 
                    StartState = #state{
                      params = State#state.params,
                      variables = SomeState#state.variables},
                    Processed = process_query(StartState, SemQuery, []),
                    lists:map(fun (Result) ->
                                      property_to_string(ToOrdset(Result)) end,
                              Processed)
            end;
       (Property) when is_atom(Property) ->
            PropFun = ?Lib:prop_fun(State#state.type, Property),
            case PropFun of
                [] ->
                    fun(_, SomeState) ->
                            [SomeState#state{res = [to_string(Property)]}]
                    end;
                [Fun] ->
                    fun(E, SomeState) ->
                            [SomeState#state{res = [to_string(Fun(E))]}]
                    end
            end;
       ({variable, Var}) ->
            Val = case lookup_variable(Var, State) of
                      {_, {value, Value}} -> [to_string(Value)];
                      _ -> []
                  end,
            fun(_, SomeState) -> 
                    [SomeState#state{res = Val}]
            end
    end.

%%% ============================================================================
%%% Helper functions

compare(like, CompL, CompR) ->
    Distance =
        if
            is_list(CompL) andalso is_list(CompR) ->
                refusr_strm:getDistance(CompL, CompR);
            is_atom(CompL) andalso is_atom(CompR) ->
                refusr_strm:getDistance(atom_to_list(CompL),
                                        atom_to_list(CompR));
            true -> false
        end,

    case Distance of
         {lev, 0} -> true;
         {lev, 1} -> true;
         {lev, 2} ->
            Length = if is_atom(CompL) -> length(atom_to_list(CompL));
                        true           -> length(CompL)
                     end,
            case Length > 5 of true -> true; _ -> false end;
        _ -> false
    end;

compare('~', CompL, CompR) when is_atom(CompR)->
    compare('~', CompL, atom_to_list(CompR));
compare('~', CompL, CompR) when is_atom(CompL)->
    compare('~', atom_to_list(CompL), CompR);
compare('~', CompL, {quoted, A}) when is_atom(A)->
    compare('~', CompL, atom_to_list(A));
compare('~', {quoted, A}, CompR) when is_atom(A)->
    compare('~', atom_to_list(A), CompR);
compare('~', CompL, CompR) ->
    RegExp = case re:compile(CompR) of
        {ok, MP} -> MP;
        {error, _} -> throw(?LocalErr0r(bad_regexp))
    end,
    case re:run(CompL, RegExp) of
        {match, _} -> true;
        nomatch -> false
    end;


compare(CompOp, {quoted, A}, CompR) ->
    erlang:CompOp(A, CompR);

compare(CompOp, CompL, {quoted, A}) ->
    erlang:CompOp(CompL, A);

compare(CompOp, CompL, CompR) ->
    erlang:CompOp(CompL, CompR).

prop_fun(EntityType, Filter) ->
    case ?Lib:prop_fun(EntityType, Filter) of
        [] -> throw(?LocalError(illegal_property, [EntityType, Filter]));
        [Fun]   -> Fun
    end.

%% @spec format_nodes(Nodes::[entity()], Positions::atom()) -> string()
%%       Positions = none|scalar|linecol
%% @doc Returns a textual representation for a list of nodes.
format_nodes(Nodes, Position) -> ?Format:nodes(Nodes, Position).

process_parallel([First | _] = States) ->
    RemainingQuery = First#state.semquery,
    RemainingQueryLastPart = First#state.semquery_last,
    process_parallel(States, RemainingQuery, RemainingQueryLastPart).

process_parallel(States, Query, LastQuery) ->
    Keys = [?MISC:async_call(node(), ?MODULE, process_query,
                           [State, Query, LastQuery]) || State <- States],
    lists:flatmap(fun rpc_yield/1, Keys).

entitynumber_increment(Query)->
    IncrementingElements =
        lists:filter(fun({initial_selector, _}) ->
                             false;
                        ({filter, _}) ->
                             false;
                        ({filter_with_variable_to_bind, _, _}) ->
                             false;
                        ({variable_match, _, _}) ->
                             false;
                        (_) ->
                             true end,
                     Query),
    length(IncrementingElements).

lookup_variable(Variable, State) ->
    case orddict:find(Variable, State#state.variables) of
        {ok, TypeAndValue} ->
            TypeAndValue;
        error ->
            none
    end.

lookup_variable_value(Variable, State) ->
    {_, Value} = orddict:fetch(Variable, State#state.variables),
    Value.

add_variable(Variable, Type, State) ->
    orddict:store(Variable, {Type, undefined}, State#state.variables).

bind_variable(Variable, Value, State) ->
    Update = fun ({Type, undefined}) ->
                     {Type, {value, Value}} end,
    State#state{
      variables = orddict:update(Variable, Update, State#state.variables)}.

has_variable_got_bound(States, OldState) when is_list(States) andalso
                                              is_record(OldState, state) ->
    EarlierVariableList = OldState#state.variables,
    StatesWithNewVariables = 
        lists:dropwhile(fun (#state{variables = Variables}) ->
                                Variables == EarlierVariableList end,
                        States),
    case StatesWithNewVariables of
        [] ->
            false;
        _ ->
            true
    end;
has_variable_got_bound(NewState, OldState) 
  when is_record(NewState, state) andalso
       is_record(OldState, state)->
    NewState#state.variables /= OldState#state.variables;
has_variable_got_bound(Variables1, Variables2) ->
    Variables1 /= Variables2.

variables_got_bound(NewState, OldState) ->
    AlreadyExistingVariables = bound_variables(OldState),
    MoreVariables = bound_variables(NewState),
    ordsets:subtract(MoreVariables, AlreadyExistingVariables).

bound_variables(State) ->
    orddict:fetch_keys(State#state.variables).

is_variable_bound(Variable, State) ->
    case lookup_variable_value(Variable, State) of
        {value, _} ->
            true;
        valueless ->
            true;
        _ -> 
            false
    end.

are_variables_bound(Variables, State) ->
    lists:all(fun (Variable) -> is_variable_bound(Variable, State) end, Variables).

variable_values_every_combination([]) ->
    [[]];
variable_values_every_combination([{Id, {Type, {value, Values}}} | Tail])
  when is_list(Values) ->
    CombinationsOfTail = variable_values_every_combination(Tail),    
    [ [{Id, {Type, {value, Value}}} | Rest] || Value <- Values,
                                               Rest <- CombinationsOfTail];
variable_values_every_combination([First | Tail]) ->
    CombinationsOfTail = variable_values_every_combination(Tail),
    [ [First | Rest] || Rest <- CombinationsOfTail].

combine_variable_values(State, Variables) ->
    VariableValueCombinations = variable_values_every_combination(Variables),
    lists:map(fun (VariableValues) ->
                      State#state{variables = VariableValues} end,
              VariableValueCombinations).

combine_variable_values(State) ->
    combine_variable_values(State, State#state.variables).

mark_undefined_as_valueless(Identifiers, States) when is_list(States) ->
    lists:map(fun (State) ->
                      mark_undefined_as_valueless(Identifiers, State) end,
              States);
mark_undefined_as_valueless(Identifiers, State) ->
    Variables = State#state.variables,
    Update = fun ({Type, undefined}) ->
                     {Type, valueless};
                 (StoredInfo) ->
                     StoredInfo end,
    NewVariables = 
        lists:foldl(fun (Identifier, SomeVariables) ->
                            orddict:update(Identifier, Update, SomeVariables)
                    end,
                    Variables,
                    Identifiers),
    State#state{variables = NewVariables}.

rethrow_badrpc({badrpc, _} = Exception) ->
    error(Exception);
rethrow_badrpc(Results) when is_list(Results)->
    lists:foreach(fun rethrow_badrpc/1, Results);
rethrow_badrpc(_) ->
    ok.

filter_closure_result(NewEntities, EntitiesWithVariableBinding,
                      EntitiesWithoutVariableBinding) ->
    SetofEntities = gb_sets:from_list(NewEntities),
    PossiblyNewEntities = 
        gb_sets:difference(SetofEntities, EntitiesWithoutVariableBinding),
    ReallyNewEntities = 
        gb_sets:difference(PossiblyNewEntities, EntitiesWithVariableBinding),
    NewEntitiesWithVariableBinding = 
        gb_sets:union(SetofEntities, EntitiesWithVariableBinding),
    NewEntitiesWithoutVariableBinding = 
        gb_sets:difference(EntitiesWithoutVariableBinding, ReallyNewEntities),
    {gb_sets:to_list(ReallyNewEntities), 
     NewEntitiesWithVariableBinding,
     NewEntitiesWithoutVariableBinding}.

process_filter_with_variable_to_bind(FilterElement, State) ->
    {_, Filter, VariablesToBind} = FilterElement,
    case are_variables_bound(VariablesToBind, State) of
        true ->
            process({filter, Filter}, State);
        false ->
            StateToRunFilterOn = State#state{parallel_key = []},
            StashedKeys = State#state.parallel_key,
            FilteredStates = not_empty_states(
                               filter_entities_separately(Filter,
                                                          StateToRunFilterOn)),
            MarkedStates = mark_undefined_as_valueless(VariablesToBind,
                                                       FilteredStates),
            CombinedStates = lists:flatmap(fun combine_variable_values/1,
                                           MarkedStates),
            case CombinedStates of
                [] ->
                    EmptyState = State#state{res = [], groupby_res = []},
                    mark_undefined_as_valueless(VariablesToBind, EmptyState);
                [NewState] ->
                    NewState#state{parallel_key = StashedKeys};
                [FirstState | States] ->
                    Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                                         [States]),
                    FirstState#state{parallel_key = [Key | StashedKeys]}
            end
    end.

filter_entities_separately(Filter, State = #state{res = [H | _]})
  when is_list(H) ->
    lists:append(
      lists:zipwith(fun (GroupingEntity, Group) ->
                            filter_group_separately(GroupingEntity, Group,
                                                    Filter, State)
                    end,
                    State#state.groupby_res,
                    State#state.res));
    
filter_entities_separately(Filter, State) ->
    pmap({?MODULE, pmap_helper},
         [fun(Entity) -> 
                  filter(Filter, State#state{res = [Entity]})
          end],
         State#state.res).

filter_group_separately(GroupingEntity, Group, Filter, State) ->
    ToGroupedState = fun (#state{res = Entity, variables = Variables}) ->
                             State#state{res = [Entity], 
                                         groupby_res = [GroupingEntity],
                                         variables = Variables} end,
    StateOfGroup = 
        to_nongrouped_state(State#state{res = Group}),
    FilteredStates = 
        filter_entities_separately(Filter, StateOfGroup),
    lists:map(ToGroupedState, FilteredStates).

res(State)->
    State#state.res.
