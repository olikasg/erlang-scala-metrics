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

%%% ============================================================================
%%% Module information

%%% @doc
%%%  Completes unfinished semantic queries. This module is intended to be
%%%  served as a backend for various user interfaces. Thus the output is
%%%  human readable.
%%%
%%%  It uses an extended semantic query grammar to control the amount of
%%%  possible cases. The module provides a validator for the original grammar.
%%%

%%% == Implementation status ==
%%% This feature is not fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>
-module(refusr_ac).

-include("user.hrl").
-include("sq_lib.hrl").

-vsn("$Rev: 12359 $ ").


-export([run/1]).
-export([run/2]).
-export([validate_query/1]).

%%%
%% @private
%% @doc
%% Gives the possible endings of the given semantic query. It does not
%% run the query, thus overpredicts the possible postfixes.
%% @end
%%
%% @spec run(atom() | string()) ->
%%                                [{string() , string()} |
%%                                 {'type'   , ('int' | 'atom' | 'string')}]
run(Query) ->
    run(Query, simple).

run(Query, Format) when is_atom(Query) ->
    run(atom_to_list(Query), Format);
run(QueryStr, Format) ->
    Query = refusr_sq_lib:replace_special_chars(QueryStr),
    {Base, Last} = case Query of
        [] -> {initial_selector, ''};
        _ ->
            case refusr_sq_lexer:string(Query) of
                {ok, Tokens, _} ->
                    case refusr_ac_parser:parse(Tokens) of
                        {ok, Result} ->?d(Result),
                            sup(main, Result);
                        E ->
                            throw(E)
                    end;
                E ->
                    throw(E)
            end
    end,%?d({Base, Last}),
    format(spaces(complete(Base, Last), lists:last(Query)), Format).

%% @doc
%%  Computes the last entity on which the last item is invoked,
%%  and returns this last element.
%%  Both last entity and element  massively depend on the structure of the query.
%% @end
%%
%% Second argument is the last element of List.
%% List is NOT reversed.
sup(main, List) ->
    {Base, Last} = sup([], List),
    {Base, [Last, {setop, operator}]};

sup([], List) when is_list(List) ->
    sup(lists:sublist(List, length(List)-1), lists:last(List));

sup(_List, {complete, {initial_selector, Selector}}) ->
    {initial_selector, [Selector, {null, complete}]};

sup(List, {complete,   {selector, _Sel} = Selector}) ->
    LastEntity = last_entity(List),
    {LastEntity, [Selector, {{LastEntity, Selector}, complete}]};

sup(List, {incomplete, {'query', '.'}}) ->
    {last_entity(List), {selector, ''}};

sup(List, {complete, {set_op, {_, Seq1, Seq2}}}) ->
    LastType = ordsets:intersection(last_type(Seq1), last_type(Seq2)),
    {Base, Last} = sup([], Seq1 ++ List ++ Seq2),
    {Base, [Last, {{type, LastType}, complete} ]};

sup(List, {incomplete, {set_op, {Op, Seq1, Seq2}}})
    when Op==union; Op==intersect; Op==minus ->
        {Base, Last} = sup([], Seq1 ++ List ++ Seq2),
        {Base, [Last, {setop, unclosed}]};

sup(_List, {incomplete, {set_op, {Op, _}}})
    when Op==union; Op==intersect; Op==minus ->
        {needs, {space, {setop, [secondop, {initial_selector, ''}]} }};

sup(_List, {incomplete, {set_op, '('}}) ->
    {null, [complete, {initial_selector, ''}]};

% ---- help
sup(List, {complete, {help, queries}}) ->
    sup(List, {incomplete,{'query','.'}});
sup(List, {complete,{help,statistics}}) ->
    sup(List, {incomplete,{statistics,':'}});
sup(List, {complete,{help,filters}}) ->
    sup(List, {incomplete,{filter,'['}});
sup(_List, {complete,{help,initial_selectors}}) ->
    {initial_selector, ''};

% ---- iteration/closure
sup(_List, {complete, {I, _, _}}) when I==iteration; I==closure ->
    {null, complete};
sup(List, {incomplete, {closure, '('}}) ->
    {iteration, sup(List, {incomplete,{'query','.'}})};
sup(_List, {incomplete, {closure, ')'}}) ->
    {closure, ending};
sup(List, {incomplete, {iteration, '{'}}) ->
    {iteration, sup(List, {incomplete,{'query','.'}})};
sup(_List, {incomplete, {iteration, '}'}}) ->
    {iteration, ending};
sup(List, {incomplete, {I, SubList}}) when I==iteration; I==closure  ->
    {Base, Last} = sup([], List++SubList),
    {Base, [Last, {I, unclosed}]};

% ---- statistics
sup(List, {_, {statistics,Stat}}) ->
    LastEntity = last_entity(List),
    {LastEntity, {statistics, case Stat of ':' -> ''; _ -> Stat end}};

% ---- filter conditions
sup(List, {_, {Seq, Query}}) when Seq=='query'; Seq==seq ->
    sup([], List++Query);

sup(List, {in, Property}) when is_atom(Property) ->
    LastEntity = last_entity(List),
    {LastEntity, {filter, Property}};

sup(List, {in, {_, {cons, _}}=Cons}) ->
    sup(List, Cons);

sup(List, {in, Query}) ->
    {needs, {space, sup([], List++Query)}};

sup(_List, {complete, {cons, _}}) ->
    {cons, complete};

sup(_List, {incomplete, {cons, L}}) ->
    case L of
        '|' -> {cons, '|'};
        A when is_integer(A) -> {interval, unclosed};
        _ -> {cons, unclosed}
    end;

sup(List, {complete, {in, _P1, P2}}) ->
    sup(List, {in, P2});

sup(_List, {incomplete, {Op, _P1}}) when Op==in; Op=='and'; Op=='or' ->
    {needs, {space, {Op, operand}}};

sup(_List, {incomplete, {set_op, {Op, _P1}}}) when Op==any_in; Op==all_in ->
    {Op, operand};

sup(List, {complete, {set_op, {Op, _P1, P2}}}) when Op==any_in; Op==all_in ->
    sup(List, {in, P2});

sup(_List, {incomplete,{'not'}}) ->
    {filter, 'not'};

sup(List, {complete,{'not', Property}}) ->
    sup(List, Property);

sup(List, Property) when is_atom(Property) ->
    LastEntity = last_entity(List),
    {'and', {
        {LastEntity, {complete_filter, Property}},
        {needs, {space, {filter, 'comparator'}}}
    } };

sup(_List, {variable, _}) ->
    {needs, {space, {filter, 'comparator'}}};

sup(List, {incomplete,{C, {variable, _}}})
    when C == 'comparator' ; C == 'like' ->
        LastEntity = last_entity(List),
        {needs, {space, 
        {'and', {
            {LastEntity, properties},
            {vars, used_variables(List)}
        } }
        } };

sup(List, {incomplete,{C, Property}})
    when C == 'comparator' ; C == 'like' ->
        LastEntity = last_entity(List),
        {'and', {
        {'or', {
            {LastEntity, [{filter, Property},
                            {needs, {space, {LastEntity, {comparator, Property}} } }]},
            {needs, {space, {LastEntity, {property, type(Property)}} } }
        } },
        {needs, {space, {vars, used_variables(List)}}}
        } };

sup(List, {complete,{_, _, Property}}) ->
    LastEntity = last_entity(List),
    {LastEntity, {filter, Property}};

% ---- filter
sup(List, {complete, {filter, _}}) ->
    case sup([], List) of %necessary for differentiating int properties for statistics suggestion
        {_, [_, {{_, {selector, _}} = Last, complete}]} -> %Previous element was a selector
            {Last, complete};
        {{_, {selector, _}} = Last, complete} -> %Previous element was a filter
            {Last, complete};
        _ ->
            {null, complete}
    end;

sup(List, {incomplete, {filter, '['} = Filter}) ->
    LastEntity = last_entity(List),
    {'and', {
        {LastEntity, Filter},
        {vars, used_variables(List)}
    } };

sup(_List, {incomplete, {filter, '[]'}}) -> 
    {null, complete}; 

sup(List, {incomplete, {filter, Cond}}) ->
    {Base, Last} = sup(List, Cond),
    {Base, [Last, {needs, [{space, {filter, separator}}, {filter, unclosed}]}]}.

type(Value) ->
    if
        is_list(Value) -> string;
        Value == true orelse Value == false -> bool;
        is_atom(Value) -> atom;
        is_integer(Value) -> int;
        true -> other
    end.

type_to_string(Type) ->
    case Type of
        string -> "\"\"";
        atom -> "''";
        int -> "42";
        bool -> "true";
        _ -> ""
    end.

%used_variables(List) ->
%    ["A"].

used_variables(E) when is_list(E) ->
    lists:append(lists:map(fun used_variables/1, E));

used_variables(E) when is_tuple(E) ->
    case E of
        {variable, [C|_]=Var} when is_integer(C) -> [Var];
        _ -> used_variables(tuple_to_list(E))
    end;

used_variables(_) ->
    [].

%% @doc
%%  Finds the last selector that determines the entity of the following.
%%  his last selector should be a complete one. If it is an iteration or
%%  closure than we should recursively find the last and most inner one.
%% @end
last_selector([]) -> 
    {initial_selector, '@def'}; 
last_selector(List) when is_list(List) ->
    NewList = lists:sublist(List, length(List)-1),
    case lists:last(List) of
        {_, {initial_selector, _}=I} ->
            I;
        {complete, {selector, _}=I} ->
            {last_entity(NewList), I};
        {_, {set_op, I}} when is_tuple(I) ->
            last_selector(NewList ++ element(2, I));
        {_, {statistics, _}} ->
            [];
        {_, {filter, _}} ->
            last_selector(NewList);
        {_,{iteration,_,_}} ->
            last_selector(NewList);
        {_,{closure,_,_}} ->
            last_selector(NewList);
        _ ->
            {initial_selector, '@def'}
    end.

last_entity(E) ->
    sel_entity(last_selector(E)).

last_type([]) ->
    [any];
last_type(List) when is_list(List) ->
    NewList = lists:sublist(List, length(List)-1),
    case lists:last(List) of
        {_, {filter, _}} ->
            last_type(NewList);
        {complete, {selector, Sel}} ->
            case prop_type(last_entity(NewList), Sel) of
                [_] = Type -> Type;
                _ -> last_entity(List)
            end;
        _ ->
            last_entity(List)
    end.

%% @doc
%%  Selects the entity from refusr_sq_lib:entities().
%%  First selects the entity records than searching for the given
%%  selector in the selectors list of the record.
%%  The result should be a one element list.
%% @end
sel_entity([]) ->
    [];
sel_entity({initial_selector, Selector}) ->
    [T || {S, _, T,_} <- get_initial_selectors(undef), S==Selector];
sel_entity({[any], {selector, Selector}}) ->
    lists:usort(lists:flatten([ [S#selector.type
                                 || S <- E#entity.selectors,
                                    lists:member(Selector, S#selector.name)
                                ]
                                || E <- refusr_sq_lib:entities()
                              ]));
sel_entity({[Entity], {selector, Selector}}) ->
    refusr_sq_lib:sel_type(Entity, Selector);

sel_entity({List, Sel}) -> 
    lists:usort( 
        lists:flatten( 
            [sel_entity({[E], Sel}) || E <- List])).

prop_type([any], PropertyName) ->
    lists:usort(lists:flatten(
        [ [Property#property.type ||
            Property <- Entity#entity.properties,
            lists:member(PropertyName, Property#property.name)]
            || Entity <- refusr_sq_lib:entities()
        ]));

prop_type([Entity], PropertyName) ->
    refusr_sq_lib:prop_type(Entity, PropertyName);

prop_type(Types, PropName) ->
    lists:usort( 
        lists:flatten( 
            [prop_type([E], PropName) || E <- Types])).

%% @doc
%%  Returns the possible postfixes of the query in a tuple format.
%%  In case of Format is 'simple':
%%     First element contains the original prefix, the second element
%%     contains the possible ending.
%%  In case of Format is 'detailed':
%%     First element contains the original prefix, the second element
%%     contains the possible ending, the third element contains
%%     the details in {Name, Names, Type, Desc} format.

%% @end

complete('and', {{Base, Last}, {OtherBase, OtherLast}}) ->
?d({Last, OtherLast}),
?d({complete(Base, Last), complete(OtherBase, OtherLast)}),
    complete(Base, Last) ++ complete(OtherBase, OtherLast);

complete('or', {{Base, Last}, {OtherBase, OtherLast}}) ->
    case complete(Base, Last) of
        [] -> complete(OtherBase, OtherLast);
        Res -> Res
    end;

complete(needs, {space, {Base, Last}}) ->
    lists:keymap(
        fun([]) -> [];
            ([32|Text]) -> [32|Text];
            (Text) -> " " ++ Text
        end, 2, complete(Base, Last));

complete(Base, [Last, {OtherBase, OtherLast}]) ->
    Res = complete(Base, Last),
    case lists:keymember([], 2, Res) orelse OtherBase==initial_selector of 
        true -> 
            filter_same(lists:keydelete([], 2, Res) ++
            complete(OtherBase, OtherLast));
        _ -> Res
    end;

complete(initial_selector, Prefix) ->
    All = get_initial_selectors(initial_selector),
    filter_and_split(Prefix, All);

complete([], {selector, Prefix}) ->
    [{"."++atom_to_list(Prefix), [], {'.', ['.'], invalid,
        "The previous selector was a property-selector."
        " No valid sequence available."}}];

complete(LastEntity, {selector, Prefix}) ->
    Select = case Prefix of '' -> unique; _ -> all end,
    All = get_selectors(LastEntity, Select) ++
          get_filters(LastEntity, Select),
    Unsorted = filter_and_split(Prefix, All),
    lists:ukeysort(2, Unsorted);

complete(iteration, {Base, Last}) ->
    lists:filter(
        fun({_, _, {_, _, Type, _}}) ->
            type_to_string(Type) == []
        end, complete(Base, Last));

complete(closure, ending) ->
    [{"", "+", {'+', ['+'], symbol,
        "Closure - infinite iteration"}},
    {"", "2", {'2', ['1', '2', '3', '...'], 'int',
        "Multiplicity of the closure sequence."}}];

complete(iteration, ending) ->
    [{"", "2", {'2', ['1', '2', '3', '...'], 'int',
        "Multiplicity of the iteration sequence."}}];

complete(setop, operator) ->
    [{"", " U", {'U', ['U', 'union'],
        set_operator, "The union of the two queries or sequences."}},
    {"", " I", {'I', ['I', 'intersect', 'intersection'],
        set_operator, "The intersection of the two queries or sequences."}},
    {"", " --", {'--', ['--', 'minus', 'except'],
        set_operator, "The difference of the two queries or sequences."}},
    {"", [], {null, [null], null , ""}}];

complete(setop, secondop) ->
    [{"", "(", {'(', ['('], symbol, "Set_op"}},
    {"", [], {null, [null], null, ""}}];

complete(setop, seq) ->
    [{"", ".", {'.', ['.'], symbol, "Query seq"}}];

complete(Elem, complete) when Elem==cons; Elem==comp ->
    [{"", [], {null, [null], null, ""}}];

complete(Last, complete) ->
    Stat = case Last of
        {Type, {selector, Sel}} when is_list(Type) ->
            case prop_type(Type, Sel) of
                [int] -> [{"", ":", {':', [':'], symbol, "Statistic"}}];
                _ -> []
            end;
        {type, [int]} ->
            [{"", ":", {':', [':'], symbol, "Statistic"}}];
        _ -> []
    end,

    {Seq, SetOp} =
    case Last of
        {type, [T]} when T==int; T==atom; T==bool; T==string ->
            {[], []};
        _ ->
            try
                    {[_|_]=T, {selector, S}} = Last,
                    [_] = prop_type(T, S),
                    {[], []}
            catch
                _:_ ->
                    {[{"", ".", {'.', ['.'], symbol, "Query seq"}}],
                    [{"", "(", {'(', ['('], symbol, "Set_op"}}]}
            end
    end,

    Seq ++
    Stat ++
    [{"", "[", {'[', ['['], symbol, "Filter"}}] ++
    SetOp ++
    [{"", [], {null, [null], null , ""}}];

complete(cons, '|') ->
    [{"", "int|string|atom", {'', ['42', 'atom', '\"string\"'], '(int|string|atom)',
        "List. Elements type can be of int, string or atom."}},
    {"", "int .. int", {'', ['1..1', '1..2', '1..3', '..'], 'int .. int',
        "Interval"}}];

complete(LastEntity, {filter, '['}) ->
    complete(LastEntity, properties) ++
    [{"", ".", {'.', ['.'], symbol, "Query seq"}},
    {"", "\"\"", {constant, [constant], 'atom|bool|string|int',
        "A constant value."}}];

complete(LastEntity, {comparator, PropName}) ->
    case prop_type(LastEntity, PropName) of
        [RetEnt] ->
            [{"", type_to_string(RetEnt), {RetEnt, [RetEnt],
                RetEnt, atom_to_list(RetEnt)++" type constant."}}
            %,{"", "Var", {'Var', ['Var', 'Variable'],
            %    'variable',
            %    "Properties can be assigned to variables for usage later in the query"}}
            ] ++
            begin
                All = get_filters(LastEntity, unique),
                TypeMatchedAndFiltered =
                    lists:map(
                        fun({_, P}) -> P end,
                        lists:keysort(1, [{element(3, Prop) /= RetEnt, Prop} || Prop <- All,
                                    not lists:member(PropName, element(2, Prop))])),
                filter_and_split('', TypeMatchedAndFiltered)
            end;
        [] ->
            complete(LastEntity, {filter, ''})
    end;

complete(vars, List) when is_list(List) ->
    lists:map(
        fun(Var) ->
            {"", Var, {list_to_atom(Var), [list_to_atom(Var)], 'variable',
                "Variable with bound property value."}}
        end, List);

complete(LastEntity, {property, Type}) ->
    All = get_filters(LastEntity, unique),
    TypeMatched =
        lists:map(
            fun({_, P}) -> P end,
            lists:keysort(1,  [{element(3, Prop) /= Type, Prop} || Prop <- All])),
    filter_and_split('', TypeMatched);

complete(LastEntity, properties) ->
    Properties = get_filters(LastEntity, unique),
    filter_and_split('', Properties);

complete(filter, 'comparator') ->
    [{"", "=", {'=', ['=', '==', '=:='],
        'property|filter_op|variable = property|filter_op|variable',
        "True when the operands are equal."}},
    {"", "/=", {'/=', ['/=', '=/='],
        'property|filter_op|variable /= property|filter_op|variable',
        "True when the operands are not equal."}},
    {"", "<", {'<', ['<', '>', '>=', '=<'],
        'property|filter_op|variable < property|filter_op|variable',
        "True when the inequality operator is true for the given operands."}},
    {"", "~", {'~', ['~'],
        'property|filter_op|variable ~ property|filter_op|variable',
        "True if the second operand can be matched to the first."}},
    {"", "like", {'like', ['like'],
        'property|filter_op|variable like property|filter_op|variable',
        "True if the property or the first query_seq is not empty and"
        " is a subset of the other operand."}},
    {"", "in", {'in', ['in'],
        'property|query_seq|list in query_seq|sem_query|list|property',
        "True if the property or the first query_seq is not empty and"
        " is a subset of the other operand."}}];

complete(filter, separator) ->
    [{"", "orelse", {'orelse', ['orelse', 'or', ';'],
        'property|query_seq|filter_op orelse property|query_seq|filter_op',
        "True if any operand is true."}},
    {"", "andalso", {'andalso', ['andalso', ','],
        'property|query_seq|filter_op andalso property|query_seq|filter_op',
        "True if both operands are true."}},
    {"", "in", {'in', ['in'],
        'property|query_seq|list in query_seq|sem_query|list|property',
        "True if the property or the first query_seq is not empty and"
        " is a subset of the other operand."}},
    {"", "all_in", {'all_in', ['all_in'],
        'query_seq|list|sem_query all_in query_seq|list|sem_query',
        "True if the first operand is a subset of the second one."}},
    {"", "any_in", {'any_in', ['any_in'],
        'query_seq|list|sem_query all_in query_seq|list|sem_query',
        "True if at least one element of the first operand is also"
        " a member of the second operand."}},
    {"", [], {a, [a], a , ""}}];

complete(_Op, operand) ->
    complete(initial_selector, '') ++ 
    [{"", "|", {'|', ['|'], symbol, "List or interval starting pipe."}},
    {"", ".", {'.', ['.'], symbol, "Query seq"}}];

complete(filter, 'not') ->
    [{"", "(", {'(', ['('], symbol,
        "Filter conditions in parentheses."}},
    {"", "\"\"", {constant, [constant], 'atom|string|int|bool',
        "A constant value."}}];

complete(LastEntity, {filter, Prefix}) when is_atom(Prefix) ->
    All = get_filters(LastEntity, all),
    filter_and_split(Prefix, All);

complete(LastEntity, {complete_filter, Prefix}) when is_atom(Prefix) ->
    lists:filter(
        fun ({_, [], {_, _, Type, _}}) -> Type == bool;
            (_) -> true
        end, complete(LastEntity, {filter, Prefix}) );

complete(LastEntity, {statistics, Prefix}) ->
    All = get_stats(LastEntity),
    filter_and_split(Prefix, All);

complete(Part, unclosed) ->
    case Part of
        filter -> [{"", "]", {']', [']'], symbol, "Filter ending."}}];
        iteration -> [{"", "}", {'}', ['}'], symbol, "Iteration ending."}}];
        cons -> [{"", "|", {'|', ['|'], symbol, "List or interval ending."}}];
        interval -> [{"", [], {'', [''], int, "Interval end."}}];
        _ -> [{"", ")", {')', [')'], symbol, "Closing parentheses."}}]
    end;

complete(_, _) ->
    [].

filter_same([H|T]) ->
    case lists:member(H,T) of
        true -> filter_same(T);
        false -> [H]++filter_same(T)
    end;
filter_same([]) -> [].

filter_and_split(PrefixAtom, Tuples) ->
    Prefix = atom_to_list(PrefixAtom),
    [{Prefix, lists:sublist(atom_to_list(Name),
                                    length(Prefix) + 1,
                                    length(atom_to_list(Name))), Details}
     || {Name, _Names, _Type, _Desc}=Details <- Tuples,
        lists:prefix(Prefix, atom_to_list(Name))].

spaces(X, 32) ->
    lists:keymap(
        fun([32|Text]) -> Text;
            (Text) -> Text
        end,
    2, X);
spaces(X, _) ->
    X.

format(X, simple) ->
    [{element(1,T),element(2,T)} || T<-X];
format(X, detailed) ->
    X.

%% @private
%% @doc
%% Returns the selectors of a given entity
%% @end

get_selectors(any) ->
    lists:append([E#entity.selectors || E<-refusr_sq_lib:entities()]);

get_selectors(Entity) ->
    (refusr_sq_lib:entity(Entity))#entity.selectors.

get_selectors(Entities, Select) ->
    Records = ordsets:from_list(lists:append([get_selectors(E) || E<-Entities])),
    Tuples = [  case Select of
                    all -> [{N, Names, T, D} || N <- Names];
                    unique -> {hd(Names), Names, T, D}
                end
              || #selector{name = Names, type = T, desc = D} <- Records],
    lists:ukeysort(1, lists:flatten(Tuples)).

%% @private
%% @doc
%% Returns the properties of a given entity
%% @end

get_filters(any) ->
    lists:append([E#entity.properties || E<-refusr_sq_lib:entities()]);

get_filters(Entity) ->
    (refusr_sq_lib:entity(Entity))#entity.properties.

get_filters(Entities, Select) ->
    Records = ordsets:from_list(lists:append([get_filters(E) || E<-Entities])),
    Tuples = [  case Select of
                    all -> [{N, Names, T, D} || N <- Names];
                    unique -> {hd(Names), Names, T, D}
                end
              || #property{name = Names, type = T, desc = D} <- Records],
    lists:ukeysort(1, lists:flatten(Tuples)).

%% @private
%% @doc
%% Returns the statistics of a given entity
%% @end

get_stats(Entities) ->
    Records = get_stats(Entities, record),
    Tuples = [[{N, Names, statistics, D} || N <- Names]
              || #statistics{name = Names, desc = D} <- Records],
    lists:ukeysort(1, lists:flatten(Tuples)).

get_stats(_Entities, record) ->
    refusr_sq_lib:statistics().

get_initial_selectors(Entities) ->
    Records = get_initial_selectors(Entities, record),
    Tuples = [[{N, Names, T, D} || N <- Names]
              || #initial_selector{name = Names, type = T, desc = D} <- Records],
    lists:reverse(lists:ukeysort(1, lists:flatten(Tuples))).

get_initial_selectors(_, record) ->
    refusr_sq_lib:initial_selectors().

%%% =========================================================================
%% @doc
%% Validate a maybe-incomplete query whether it is complete.
%% tested with:
%% "mods.(funs[not (name==(.called[name==\"alam\"]))])3.calls.calls.{called_by}3[name=korte]"
%% @end


validate_query(ParsedList) ->
    try
        {ok, find_incomplete(ParsedList)}
    catch
        Incomplete ->
            {error, io_lib:format("The query element: ~p is incomplete!",
                [element(1, Incomplete)])}
    end.

find_incomplete(L) when is_list(L) ->
    [find_incomplete(E) || E<-L];

find_incomplete(E) when is_tuple(E) ->
    case element(1,E) of
        incomplete -> throw(element(2,E));
        complete -> find_incomplete(element(2,E));
        _ -> list_to_tuple(find_incomplete(tuple_to_list(E)))
    end;

find_incomplete(E) ->
    E.

