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

%%% @doc A duplicated code search algorithm based on suffix tree.

%%% @author First Zsofia Arvay <zsofia.arvay@gmail.com>
%%% @author Second Szabo Bence <sztadi@caesar.elte.hu>

-module(refusr_clone_identifierl_suffix_tree).
-vsn("$Rev: 11297 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

%%nif function
-export([search_dupcode/4]).
%%search functions
-export([default_options/0, get_clones/0, get_clones/1]).
%%utility functions
-export([const_var_diff/2, is_node_type/2, is_string/1,
          get_token_list/1, is_diff_length/1, minimal_common_ancestor_with_children/1]).
% @private
-export([process_trimmed_clones/1, trim_search_result_map/1,
         filter_clones_by_length/2]).

%%------------------------------------------------------------------------------
-define(LEXLINKS, [flex,clex,elex]).
-define(DUPFORMTYPES, [macro,record,module,export,include,import]).
-define(DUPCODE, dupcode).
-define(HELPER, helper).

-on_load(load_nifs/0).

%%% ============================================================================
%%% Interface

default_options() ->
    [{minlen, 32},
     {minnum, 2},
     {overlap, 0},
     {files,[]}].

get_clones()->
    get_clones(default_options()).


get_clones(Options) when is_list(Options) ->
    search_initial_clones(Options).

%%------------------------------------------------------------------------------
%% completed functions
%%------------------------------------------------------------------------------

%%----------------------------------------------------------
%% get_all_files/0 - get all files from database
%%----------------------------------------------------------
get_all_files() ->
    try
        [?File:path(File) || File <- ?Query:exec(?File:all())]  
    catch
        _ -> []
    end.

extend_all_files() ->
    DBFiles = get_all_files(),
    [fun(DBFile)->
            [File] = ?Query:exec(?Graph:root(),?File:find(DBFile)),
              {DBFile, ?MISC:file_hash(File)}
        end(DBFile1) || DBFile1 <- DBFiles].


%%---------------------------------------------------------
%% is_string/1 - checks whether a list is a string
%%---------------------------------------------------------
is_string([])-> true;
is_string([X|T])->
    is_integer(X) andalso
    X>=0 andalso
    X=<255 andalso
    is_string(T);
is_string(_)-> false.

%%---------------------------------------------------------
%% get_valid_files/1
%%---------------------------------------------------------
get_file_by_atom(Modname)->
    Files =
        ?Query:exec(?Graph:root(),
                    ?Query:seq([?Mod:find(Modname),
                                ?Mod:file()])),
    case Files of
        [] -> io:format("Warning: ~p ignored (not present in the database).~n",
                        [Modname]),
              [];
        [FileNode] -> [{?File:path(FileNode),?MISC:file_hash(FileNode)}]
    end.

get_file_by_string(PathOrRegexp)->
    DBFiles = get_all_files(),
    case lists:member(PathOrRegexp, DBFiles) of
        true ->
            [FileNode] = ?Query:exec(?Graph:root(),?File:find(PathOrRegexp)),
            [{PathOrRegexp, ?MISC:file_hash(FileNode)}];
        _ ->
            Mods =
            case catch refusr_fb_regexp:re([{type, list},
                                            {regexp, [PathOrRegexp]}]) of
                [{_,Result}] when is_list(Result) ->
                    lists:usort(Result);
                _ -> []
            end,
            FileNodes =
                lists:flatten([?Query:exec(Mod, ?Mod:file())||Mod<-Mods]),
            Files = lists:flatmap(
                fun(FileNode)->
                    Path = ?File:path(FileNode),
                        case lists:member(Path, DBFiles) of
                            false -> [];
                            true -> [{Path, ?MISC:file_hash(FileNode)}]
                        end
                end, FileNodes),
            case Files of
                [] -> io:format("Warning: ~s ignored (not present in the database).~n",
                                [PathOrRegexp]);
                _ -> ok
            end,
            Files
    end.

get_valid_files([], Res)->
    Res;
get_valid_files([DAFHead|DAFTail], Res)->
    Files =
        case is_string(DAFHead) of
            true ->
                get_file_by_string(DAFHead);
            false when is_atom(DAFHead) ->
                get_file_by_atom(DAFHead);
            _ ->
                io:format("Warning: ~p ignored (neither string nor atom).~n",
                          [DAFHead]),
                []
        end,
    get_valid_files(DAFTail, lists:umerge(Res, Files)).

%%----------------------------------------------------------
%% get_file_tokens/1 - get tokens of the files from the graph
%%----------------------------------------------------------
get_file_tokens(VAllFiles)->
    FileNodes =
        lists:flatmap(
            fun(Path)-> ?Query:exec(?File:find(Path)) end,
            VAllFiles),

    ets:insert(?HELPER,{files,FileNodes}),
    _TokenNodes =
        lists:flatmap(
            fun(Node)-> ?Syn:leaves(Node) end,
            FileNodes).

%%----------------------------------------------------------
%%all token type substituted with one letter
%%----------------------------------------------------------
process_tokens(Tokens)->
    ?Alphabet:to_alphabet0(?Alphabet:default_alphabet(), Tokens, []).

%%----------------------------------------------------------
%%load nif
%%----------------------------------------------------------
load_nifs() ->
    Nif = filename:join(code:lib_dir(referl_user, priv), "suffix_tree"),
    case erlang:load_nif(Nif, 0) of
        {error, Rnle} -> throw(Rnle);
        ok -> ok
    end.

%%----------------------------------------------------------
%% get tokens from all tokens by first and last index
%%----------------------------------------------------------
get_tokens_by_first_last_idx(Tokens,First,Last) ->
    lists:sublist(Tokens,First,Last-First+1).

%%----------------------------------------------------------
%% gets two node minimal common ancestor and its next child
%%----------------------------------------------------------
minimal_common_ancestor_with_children(TokenList)->
    Left = ?Syn:root_path(hd(TokenList),left),
    Right = ?Syn:root_path(lists:last(TokenList),right),
    case ?MISC:common_prefix(Left,Right) of
        [] -> {{root, ?Graph:root()},{hd(Left),hd(Right)}};
        Prefix ->
            %%TODO
            PLen =
                lists:min([length(Prefix),
                           length(Left)-1,
                           length(Right)-1]),
            {lists:last(Prefix),
             {lists:nth(PLen+1,Left),lists:nth(PLen+1,Right)}}
    end.

%%----------------------------------------------------------
%% gets a node all syntactical children between two one
%%----------------------------------------------------------
all_nonlexical_children(Node,First,Last)->
    Children = ?Syn:children(Node),
    ChildrenInterval =
        ?MISC:separate_interval(Children,
                                First,
                                Last),
    SynChildren =
        lists:filter(
            fun({CLink,_})->
                not lists:member(CLink,?LEXLINKS)
            end,ChildrenInterval),
    SynChildren.

%%----------------------------------------------------------
%% returns with the tokenlist of a not fully node
%%----------------------------------------------------------
get_edge_node_leaves(EdgeNode,TokenList)->
    {Link,Node} = EdgeNode,
    Leaves = ?Syn:leaves(Node),
    {Link,Node,?MISC:intersect(Leaves,TokenList)}.

%%----------------------------------------------------------
%% returns with the longest node of a list
%%----------------------------------------------------------
max_length_node_1([],MaxNode,_) -> [MaxNode];
%% left or right edge of the children list (not full node)
max_length_node_1([Head|Tail],MaxNode,MaxLen) when tuple_size(Head) == 3 ->
    {_,Node,List} = Head,
    if
        length(List) > MaxLen ->
            max_length_node_1(Tail,Node,length(List));
        true -> max_length_node_1(Tail,MaxNode,MaxLen)
    end;
max_length_node_1([Head|Tail],MaxNode,MaxLen) when tuple_size(Head) == 2->
    {_,Node} = Head,
    Leaves = ?Syn:leaves(Node),
    if
        length(Leaves) > MaxLen ->
            max_length_node_1(Tail,Node,length(Leaves));
        true -> max_length_node_1(Tail,MaxNode,MaxLen)
    end.
max_length_node(Ets, LLeaves,RLeaves,AllNodes)->
    _MaxChild =
        case length(AllNodes) of
            0 -> [];
            1 ->
                [{_,Node}] = AllNodes,
                trim_token_list_up_down(Ets, ?Syn:leaves(Node));
            2 -> max_length_node_1([LLeaves,RLeaves],nil,0);
            Len -> max_length_node_1([LLeaves]
                                   ++lists:sublist(AllNodes,2,Len-2)
                                   ++[RLeaves],nil,0)
        end.

% get_real_file(File)->
%     ?d({File}),
%     case ?Query:exec(File,?File:included()) of
%         [File] -> File;
%         List -> ?d({List}), hd(lists:delete(File,List))
%     end.
%%----------------------------------------------------------
%% returns with one syntactical unit corresponding to the
%% tokenlist by trimming
%%----------------------------------------------------------
trim_root_up_down(Ets, {{_,LFiChild},{_,RFiChild}},{LFoChild,RFoChild},TokenList)->
    AllFiles = ets:lookup_element(?HELPER, files, 2),
    Files = ?MISC:separate_interval(AllFiles, LFiChild, RFiChild),
    AllForms =
        lists:flatmap(
            fun(Node)->
                ?Syn:children(Node)
            end,Files),
    Forms = ?MISC:separate_interval(AllForms,LFoChild,RFoChild),
    process_forms(Ets, Forms,LFoChild,RFoChild,TokenList).
%%----------------------------------------------------------
trim_file_up_down(Ets, File,LChild,RChild,TokenList)->
    Forms = ?MISC:separate_interval(?Syn:children(File),
                                    LChild,
                                    RChild),
    process_forms(Ets, Forms,LChild,RChild,TokenList).

process_forms(Ets, Forms,LChild,RChild,TokenList)->
    {_,FormNodes} = lists:unzip(Forms),
    {_,LNode,LList} = LLeaves = get_edge_node_leaves(LChild,TokenList),
    {_,RNode,RList} = RLeaves = get_edge_node_leaves(RChild,TokenList),
    FullLChild =
        case lists:prefix(?Syn:leaves(LNode),TokenList) of
            true -> [LNode];
            _ -> TrimmedLChild = trim_token_list_up_down(Ets, LList),

                 case TrimmedLChild/=[] of
                    true -> case is_node_type(TrimmedLChild,[clause,form]) of
                                true -> TrimmedLChild;
                                _ -> [[{left,TrimmedLChild}]]
                            end;
                    _ -> []
                end
        end,
    FullRChild =
        case lists:suffix(?Syn:leaves(RNode),TokenList) of
            true -> [RNode];
            _ -> TrimmedRChild = trim_token_list_up_down(Ets, RList),
                case TrimmedRChild/=[] of
                  true ->  [[{right,TrimmedRChild}]];
                    _ -> []
                end
        end,

    FilteredForms =
        if
            length(FormNodes)>2 ->
                {greater,filter_forms(FullLChild ++
                lists:sublist(FormNodes,2,length(FormNodes)-2) ++
                FullRChild)};
            length(FormNodes)==2 andalso (length(FullLChild)>0 orelse length(FullRChild)>0) ->
                {equal, filter_forms(FullLChild ++ FullRChild)};
            true -> {nothing, filter_forms(FormNodes)}
        end,
    case FilteredForms of
        {_,[]} -> [];
        {Atom, Result} when Atom==greater orelse Atom==equal -> Result;
        {nothing, [LNode]} -> trim_token_list_up_down(Ets, LList);
        {nothing, [RNode]} -> trim_token_list_up_down(Ets, RList);
        {nothing, _} ->
            MaxChild =
                max_length_node(Ets, LLeaves,RLeaves,Forms),
            case MaxChild of
                [LNode] -> trim_token_list_up_down(Ets, LList);
                [RNode] -> trim_token_list_up_down(Ets, RList)
            end
    end.
filter_forms([]) -> [];
filter_forms(LForms)->
    ExtraClones = lists:filter(fun(K) -> is_list(K) end, LForms),
    Forms = lists:filter(fun(K) -> not is_list(K) end, LForms),
    FilteredForms = lists:filter(
        fun(Form)->
            {_,Type,_} = Form,
            case Type of
                clause -> false;
                _ -> not lists:member(?Form:type(Form),?DUPFORMTYPES)
            end
        end, Forms),
    FilteredForms++ExtraClones.
%%----------------------------------------------------------
%% @doc Trims expressions to tokenlist.
trim_expr_up_down(Ets, Expr, LChild, RChild, TokenList)->
    Drop = drop_expr(Expr),
    case ?Syn:leaves(Expr) == TokenList of
        true when Drop -> [];
        true -> [Expr];
        _ -> trim_expr_up_down_1(Ets, Expr,LChild,RChild,TokenList)
    end.
trim_expr_up_down_1(Ets, Expr,LChild,RChild,TokenList)->
    SynChildren = all_nonlexical_children(Expr,LChild,RChild),
    LLeaves = get_edge_node_leaves(LChild,TokenList),
    RLeaves = get_edge_node_leaves(RChild,TokenList),
    MaxChild =
        max_length_node(Ets, LLeaves,RLeaves,SynChildren),

    {{_,LNode,LList},{_,RNode,RList}} = {LLeaves,RLeaves},
    case MaxChild of
        [LNode] -> trim_token_list_up_down(Ets, LList);
        [RNode] -> trim_token_list_up_down(Ets, RList);
        _ -> MaxChild
    end.

drop_expr({_, Type, _} = Expr) ->
    case Type of
        expr ->
            [{_,Parent}] = ?Syn:parent(Expr),
            {_,ParentType,_} = Parent,
            case (ParentType == expr andalso ?Expr:type(Parent) == application)
                 orelse ?Expr:type(Expr) == list of
                true -> true;
                _ -> false
            end;
        _ -> false
    end.

%%----------------------------------------------------------
%% @doc Main loop for trimming.
trim_token_list_up_down(_, [])-> [];
trim_token_list_up_down(Ets, TokenList)->
    Minlen = ets:lookup_element(?HELPER, minlen, 2),
    {{_,NAncestor},{LChild,RChild}} =
        minimal_common_ancestor_with_children(TokenList),
    {_,AL,AI} = NAncestor,
    {_,{_,LL,LI}} = LChild,
    {_,{_,RL,RI}} = RChild,
    EtsList = ets:lookup(Ets,AL),
    case length(TokenList) >= Minlen andalso
         not lists:member({AL,AI,LL,LI,RL,RI,length(TokenList),TokenList}, EtsList) of
        true ->
              ets:insert(Ets,{AL,AI,LL,LI,RL,RI,length(TokenList),TokenList}),
              Res = trim_token_list_up_down_case(Ets, NAncestor,LChild,RChild,TokenList),
              ets:match_delete(Ets,{AL,AI,LL,LI,RL,RI,length(TokenList),TokenList}),
              Res;
        _ -> ets:match_delete(Ets,{AL,AI,LL,LI,RL,RI,length(TokenList),TokenList}),  
             []
    end.

%% @doc Trims the TokenList based on the type of NAncestor
trim_token_list_up_down_case(Ets, NAncestor,LChild,RChild,TokenList)->
    case ?Graph:class(NAncestor) of
        root ->
            LFoChild = lists:nth(2,?Syn:root_path(hd(TokenList),left)),
            RFoChild =
                lists:nth(2, ?Syn:root_path(lists:last(TokenList),
                                            right)),
            trim_root_up_down(Ets, {LChild,RChild},
                              {LFoChild,RFoChild},
                              TokenList);
        file ->
            trim_file_up_down(Ets, NAncestor,LChild,RChild,TokenList);
        form -> NAncestor0 = hd(?Lib:get_real_form(NAncestor)),
            case ?Form:type(NAncestor) of
                Type when Type == func; Type == lex -> 
                    trim_expr_up_down(Ets, NAncestor0,LChild,RChild,TokenList);
                _ ->  [] 
            end;
        clause ->
            case ?Clause:type(NAncestor) of
                fundef ->
                    trim_fundef_up_down(Ets, NAncestor,LChild,RChild,TokenList);
                pattern ->
                    trim_fundef_up_down(Ets, NAncestor,LChild,RChild,TokenList);
                guard ->
                    trim_fundef_up_down(Ets, NAncestor,LChild,RChild,TokenList);
                _ ->
                    trim_expr_up_down(Ets, NAncestor,LChild,RChild,TokenList)
            end;
        expr ->
            trim_expr_up_down(Ets, NAncestor,LChild,RChild,TokenList);
        _ -> []
    end.
%%----------------------------------------------------------

%% @doc Returns the bodynodes which are completely in the tokenlist
filter_body_nodes([], _, Result) -> Result;
filter_body_nodes([BodyNode|Tail], TokenList, Result) ->
    BodyLeaves = ?Syn:leaves(BodyNode),
    Last = lists:last(BodyLeaves),
    First = hd(BodyLeaves),

    case lists:member(First, TokenList) andalso lists:member(Last, TokenList) of
        true -> filter_body_nodes(Tail, TokenList, Result++[BodyNode]);
        _ -> filter_body_nodes(Tail, TokenList, Result)
    end.


%% @doc Trims a function into top level expressions or
%%      or returns the funcion.
trim_fundef_up_down(Ets, Fundef,LChild,RChild,TokenList)->
    case ?Syn:leaves(Fundef) == TokenList of
        true -> [Fundef];
        _ -> trim_fundef_up_down_1(Ets, Fundef,LChild,RChild,TokenList)
    end.
trim_fundef_up_down_1(Ets, Fundef,LChild,RChild,TokenList)->
    Bodies =
        lists:filter(
            fun({Link,_})->
                Link == body
            end, all_nonlexical_children(Fundef,LChild,RChild)),
    {_,BodyNodes} = lists:unzip(Bodies),
    FilteredBodyNodes = filter_body_nodes(BodyNodes,TokenList,[]),
    {_, LNode} = LChild,
    {_, RNode} = RChild,
    LExtra = case lists:member(LNode, FilteredBodyNodes) of
        true -> [];
        _ -> LLexes = ?MISC:intersect(?Syn:leaves(LNode),TokenList),
             case trim_token_list_up_down(Ets,LLexes) of
                [] -> [];
                LTrim ->  [[{left,LTrim}]]
            end
    end,
    RExtra = case lists:member(RNode, FilteredBodyNodes) of
        true -> [];
        _ -> RLexes = ?MISC:intersect(?Syn:leaves(RNode),TokenList),
             case trim_token_list_up_down(Ets,RLexes) of
                [] -> [];
                RTrim ->  [[{right,RTrim}]]
            end
    end,
    FilteredBodyNodes++LExtra++RExtra.


%%----------------------------------------------------------
%% gets the amount of lengths corresponding to the nodes
%%----------------------------------------------------------
get_nodes_length([],Acc)-> Acc;
get_nodes_length([Node|Tail],Acc)->
    get_nodes_length(Tail,length(?Syn:leaves(Node))+Acc).

%%----------------------------------------------------------
%% filter the clones by clone numbers and clone lengths
%%----------------------------------------------------------
filter_clones_by_length(Cs, Minlen) ->
    [lists:filter(
         fun(C) ->
             get_nodes_length(C,0) >= Minlen
         end, CPs)
     || CPs <- Cs].


filter_clones(Cs, Minlen, Minclones)->
    FilterArgs = fun(Arg)-> [Arg,Minlen] end,
    TCs = ?MISC:parallelise(Cs, ?MODULE, filter_clones_by_length, FilterArgs, false, low),

    lists:filter(
        fun(TCPs)->
            length(TCPs) >= Minclones
        end, TCs).


%%----------------------------------------------------------
%% get parameters from option list
%%----------------------------------------------------------
extract_parameters(Options)->
    Files =
        case proplists:get_value(files, Options) of
            undefined -> extend_all_files();
            [] -> extend_all_files();
            [Head|Tail] when is_atom(Head) orelse is_list(Head) ->
                get_valid_files([Head|Tail],[]);
            _ -> throw(?LocalErr0r(no_list))
        end,
    NoFilesOptions = proplists:delete(files, Options),
    NoFilesOptions ++ [{files, Files}].

%%----------------------------------------------------------
%% gets different constants and variables between two nodes
%%----------------------------------------------------------
const_var_diff(LNodes,RNodes)->
    LList = lists:flatmap(fun(E)->?Syn:leaves(E) end, LNodes),
    RList = lists:flatmap(fun(E)->?Syn:leaves(E) end, RNodes),
    if
        length(LList) == length(RList) ->
            const_var_diff_1(LList,RList,[],[]);
        true -> []
    end.
const_var_diff_1([],[],DConsts,DVars)-> {DConsts,DVars};
const_var_diff_1([LE|LTail],[RE|RTail],DConsts,DVars)->
    {lex,token,#token{type=Type,text=LVal}} = ?Graph:data(LE),
    {lex,token,#token{type=Type,text=RVal}} = ?Graph:data(RE),
    case {Type,{LVal,RVal}} of
        {variable,{LVal,RVal}} when LVal /= RVal ->
            const_var_diff_1(LTail,
                        RTail,
                        DConsts,
                        DVars++
                            [{element(2,hd(?Syn:parent(LE))),
                              element(2,hd(?Syn:parent(RE)))}]);
        {Const,{LVal,RVal}} when (Const == atom orelse
                                 Const == integer orelse
                                 Const == float orelse
                                 Const == string) andalso
                                 LVal /= RVal ->
            const_var_diff_1(LTail,
                        RTail,
                        DConsts++
                            [{element(2,hd(?Syn:parent(LE))),
                              element(2,hd(?Syn:parent(RE)))}],
                        DVars);
        _ ->
            const_var_diff_1(LTail,RTail,DConsts,DVars)
    end.

%%------------------------------------------------------------------------------
%%code detection
%%------------------------------------------------------------------------------
prepare_search(Files)->
    Tokens = get_file_tokens(Files),
    TokenString = process_tokens(Tokens),
    %% Write tokenstring to a file.
    case file:write_file(?TEMPFILE, TokenString) of
        {error, _} -> throw(?LocalErr0r(file_write_error));
        ok -> ok
    end,
    Tokens.
%%----------------------------------------------------------
%%search initial clones
%%[{files,[]},{minlen,integer()},{minnum,integer()},{overlap,integer()}]
%%----------------------------------------------------------    
search_initial_clones(Options)->
    Properties = extract_parameters(Options),
    {UnSortedFiles, _} = lists:unzip(proplists:get_value(files, Properties)),
    Files = lists:sort(UnSortedFiles),

    ets:new(?DUPCODE,[named_table,bag,public]), 
    ets:new(?HELPER,[named_table,set,public,{read_concurrency,true}]),
    Tokens = prepare_search(Files),

    Minlen = proplists:get_value(minlen, Properties),
    Minnum = proplists:get_value(minnum, Properties),
    Overlap = proplists:get_value(overlap, Properties),

    ICs = ?MODULE:search_dupcode(?TEMPFILE,Minlen,Minnum,Overlap),
    ets:insert(?HELPER,{tokens,Tokens}),
    ets:insert(?HELPER,{minlen,Minlen}),
    ParArgs = fun(Arg)-> [Arg] end,
    TICs = ?MISC:parallelise(ICs, ?MODULE, trim_search_result_map, ParArgs, false, low),
    PTICs = ?MISC:parallelise(TICs, ?MODULE, process_trimmed_clones, ParArgs, false, low),
    EPTICs = get_extras(PTICs),    
    A = trim_exprs(EPTICs,[]),

    ets:delete(?DUPCODE),
    ets:delete(?HELPER),
 
    FTICs = filter_clones(A,Minlen,Minnum),
    MergedClones = merge_clone_groups(FTICs, []),
    UnifiedClones = ?MISC:parallelise(MergedClones, ?Lib, to_uni_format, ParArgs, false, low),

    [{analysed_candidates_num, no_data},
     {detected_clones_num, length(UnifiedClones)},
     {detected_clones, UnifiedClones}].


%%% ============================================================================
%%% Trimming clones

trim_search_result_map(ICTokens) ->
    Ets = ets:new(?DUPCODE,[bag,public]),
    TICs = [trim_search_result_ri(Ets, K) || K <- ICTokens],
    ets:delete(Ets),
    TICs.

%% @doc Returns the maximal syntactically right nodes between RangeList.
trim_search_result_ri(Ets, {RangeList,_,_})->
    Tokens = ets:lookup_element(?HELPER, tokens, 2),
    ICTokens =
       [get_tokens_by_first_last_idx(Tokens,Start,End)
        || {Start,End} <- RangeList],
    TICs =
       [ trim_token_list_up_down(Ets, TokenList)|| TokenList <- ICTokens],
    TICs.


%%% ============================================================================
%%% Repair the trimmed functions

process_trimmed_clones(TICs) ->
    RTICs = remove_empty_lists(TICs, []),
    ERTICs = extract_duplicates(RTICs, []),
    UERTICs = unzip_forms(ERTICs),
    [trim_wrong_clones(K) || K <- UERTICs].

%% @doc Removes emplty lists from a List.
remove_empty_lists([], Result) -> Result;
remove_empty_lists([H|T], Result) ->
    H2 = lists:filter(fun(K) -> K /= [] andalso K /= [[]] end, H),
    case H2 /= [] of
        true -> remove_empty_lists(T, Result++[H2]);
        _ -> remove_empty_lists(T, Result)
    end.

%% @doc Determines if the clone contains another one.
has_extra(_, true) -> true;
has_extra([], _) -> false;
has_extra([CloneHead|Tail], _) ->
    Extra = lists:any(fun(K)-> is_list(K) end, CloneHead),
    has_extra(Tail, Extra).


%% @doc Seperates clones from right and left direction.
seperate_left_right_clones(Extra) ->
    LeftExtra = get_clones_from_dir(Extra, left),
    RightExtra = get_clones_from_dir(Extra, right),
    {LeftExtra, RightExtra}.

%% @doc Gets the clones from a given direction.
get_clones_from_dir(Extra, Direction) ->
    [case Angle == Direction of
         true -> Clone;
         _ -> []
     end
     || [{Angle, Clone}] <- Extra].


%% @doc Extends extra clones from Original clone.
extract_extra([], _, _) -> ok;
extract_extra([Head | Tail], Original, Dir) ->
    NoExtra = lists:filter(fun(K) -> not is_list(K) end, Head),
    case NoExtra of
        [] -> extract_extra(Tail, Original, Dir);
        _ ->
            Expr =
                case Dir of
                    left -> lists:last(NoExtra);
                    right -> hd(NoExtra)
                end,
            extract_extra0(Original, Expr)
           
    end.
extract_extra0(Original, Expr) ->
    FlatOrigi = lists:flatten(Original),
    HeadTokens = get_token_list(Expr),
    Extr = lists:filter(fun(K) ->
               get_token_list(K) == HeadTokens
           end, FlatOrigi),
    Exprs = lists:filter(fun(K) ->
                K /= []  andalso is_node_type(K,[expr])
            end, [[Expr]]++[Extr]),
    Trim =
        trim_wrong_clones(Exprs),
    Insert = 
        Trim /= [] andalso lists:all(fun(K)-> 
                         length(hd(Trim)) == length(K) andalso is_node_type(K,[expr])
                        end, Trim),
    case Insert of
       true ->
            ets:insert(?DUPCODE,{Trim});
       _ -> ok
    end.


%% @doc Extract the additional clone group from a single clone group
%%      without empty lists.
get_extra([], Original, Extra) ->
    {LExtra, RExtra} = seperate_left_right_clones(Extra),
    extract_extra(LExtra, Original, left),
    extract_extra(RExtra, Original, right),
    ELRExtra = remove_empty_lists([LExtra] ++ [RExtra], []),
    FullExtra = case extract_duplicates(ELRExtra, []) of
        ELRExtra -> ELRExtra;
        NewExtra -> NewExtra
    end,
    [Original]++FullExtra;
get_extra([CloneHead|Tail], Original, Extra) ->
    CloneElement = lists:filter(fun(K)-> is_list(K) end, CloneHead),
    CleanCloneHead = lists:filter(
                        fun(K) -> not lists:member(K,CloneElement)
                     end, CloneHead),

    case CleanCloneHead /= [] of
        true -> get_extra(Tail, Original++[CleanCloneHead], Extra++CloneElement);
        _ -> get_extra(Tail, Original, Extra++CloneElement)
    end.


%% @doc Extracts the additional clones.
extract_duplicates([], Result) -> Result;
extract_duplicates([Clone|Tail], Result) ->
    CloneWithExtra =
        case has_extra(Clone, false) of
            true ->
                get_extra(Clone, [], []);
            _ -> [Clone]
            end,
    extract_duplicates(Tail, Result++CloneWithExtra).


%% @doc Merges a list of lists to a list
submerge([]) -> [];
submerge([H|T]) ->
    H++submerge(T).

%% @doc Merges the clones which has common element.
merge_clone_groups([], Result) -> Result;
merge_clone_groups([Head|Tail], Result) ->
    Common = lists:filter(fun(K) -> ?MISC:intersect(Head,K) /= [] end, Tail),
    NoCommon = lists:filter(fun(K) -> ?MISC:intersect(Head,K) == [] end, Tail),
    case Common of
        [] -> merge_clone_groups(Tail, Result++[Head]);
        _ -> Clone = submerge(Common),
             ZippedClones = ?MISC:list_cnt(Clone++Head),
             {MergedClones, _} = lists:unzip(ZippedClones),
             merge_clone_groups([MergedClones]++NoCommon, Result)
        end.
    

%% @doc Takes every member of the clone's nth element
takeCurr([], _, Result) -> Result;
takeCurr([[] | Tail], Curr, Result) -> takeCurr(Tail,Curr,Result);
takeCurr([CloneHead | Tail], Curr, Result) ->
    takeCurr(Tail, Curr, Result++[[lists:nth(Curr, CloneHead)]]).


%% @doc Creates new clone groups from clones where a clone
%%      contains more then one form/clause.
unzip_forms([]) -> [];
unzip_forms([[] | Tail]) ->  unzip_forms(Tail); %üres van benne még?
unzip_forms([Clone | Tail]) ->
case lists:all(fun(K) -> is_node_type(K, [form,clause]) end, Clone) of
    true ->
        UnzippedForms =
            case is_diff_length(Clone) of
                true -> unzip_diff_len_form(Clone);
                _ ->
                    unzip_form(Clone, 1, length(hd(Clone)))
            end,
        UnzippedForms ++ unzip_forms(Tail);
    _ -> [Clone] ++ unzip_forms(Tail)
end.

%% @doc Determines if a list contains different length lists.
is_diff_length(List) ->
        HeadLengths = lists:filter(fun(K) -> length(K) == length(hd(List)) end, List),
    length(HeadLengths) /= length(List).

%% @doc Unzips different length formgroups.
unzip_diff_len_form([]) -> [];
unzip_diff_len_form(Clone) ->
    MinForms = get_min_exprs(Clone),
    TrimmedForms = align_to_minexpr(Clone,MinForms),
    UnzippedForms = unzip_form(TrimmedForms, 1, length(hd(TrimmedForms))),
    UnzippedMinForms = unzip_form(MinForms, 1, length(hd(MinForms))),
    NoMinForms = lists:filter(fun(K) -> length(K) /= length(hd(MinForms)) end, Clone),
    UnzippedForms ++ UnzippedMinForms ++ unzip_diff_len_form(NoMinForms).

%% @doc Unzips formgroups into single form groups.
unzip_form(_, Curr, Max) when Curr > Max ->
    [];
unzip_form(Clone, Curr, Max)->
    [takeCurr(Clone, Curr, [])] ++ unzip_form(Clone, Curr+1, Max).


%% @doc Trims again the not correctly trimmed clones
trim_wrong_clones(FTICsE) ->
    Clauses = lists:filter(fun(K) -> is_node_type(K, [clause,form]) end, FTICsE),
    TrimClauses = trim_clauses(Clauses),
    Expressions = lists:filter(fun(K) -> is_node_type(K, [expr]) end, FTICsE),

    case length(TrimClauses) /= length(FTICsE) orelse is_diff_length(FTICsE) of
        true ->
            MinExprs = get_min_exprs(Expressions),
            ClauseBodies = [get_bodies(K) || K <- Clauses],
            Remains = (FTICsE -- TrimClauses) -- MinExprs,
            case Remains of 
                [] -> ok;
                _ -> ets:insert(?DUPCODE, {Remains})
            end,
            case TrimClauses /= [] andalso length(TrimClauses) > 1 of
                true -> ets:insert(?DUPCODE, {TrimClauses});
                _ -> ok
            end,

            TrimmedBodies = align_to_minexpr(Remains++ClauseBodies, MinExprs),
            TrimmedBodies++MinExprs;
        _ -> case TrimClauses of
                 Clauses -> FTICsE;
                 _ -> TrimClauses
             end
    end.


%% @doc Trims the clauses from the forms.
trim_clauses(Clauses) ->
    Forms = lists:filter(fun(K) -> is_node_type(K, [form]) end, Clauses),
    TrueClauses = lists:filter(fun(K) -> is_node_type(K, [clause]) end, Clauses),
    case {Forms,TrueClauses} of
        {[], _} -> Clauses;
        {_, []} -> Clauses;
        _ ->
            FormClauses = lists:map(fun([K]) -> ?Query:exec(K, ?Form:clauses()) end, Forms),
            TrimmedClauses = align_to_minexpr(FormClauses, TrueClauses),
            TrimmedClauses++TrueClauses
    end.
    

%% @doc Trims the first parameter's length to the second parameters.
trim_to_length(_, [], Result) -> Result;
trim_to_length([], _, Result) -> Result;
trim_to_length([CloneHead | CloneTail], [MinExprHead | MinExprTail], Result) ->
    case get_token_list(CloneHead) == get_token_list(MinExprHead) of
        true -> trim_to_length(CloneTail, MinExprTail, Result++[CloneHead]);
        _ -> trim_to_length(CloneTail, [MinExprHead]++MinExprTail, Result)
    end.


%% @doc Trims the clone to the minexpr.
align_to_minexpr([], _) -> [];
align_to_minexpr([Clone | Clones], MinExpr) ->
    TrimmedClone = align_to_minexpr_0(Clone, hd(MinExpr)),
    [TrimmedClone] ++ align_to_minexpr(Clones, MinExpr).

align_to_minexpr_0([], _) -> [];
align_to_minexpr_0(Clone, MinExpr) ->
    LastCloneTokens = get_token_list(lists:last(Clone)),
    LastMinExprTokens = get_token_list(lists:last(MinExpr)),
    case get_token_list(hd(Clone)) == get_token_list(hd(MinExpr)) of
        true when length(Clone) == length(MinExpr) -> 
            Clone;
        true when LastCloneTokens == LastMinExprTokens -> 
            trim_to_length(Clone, MinExpr, []);
        true ->
            LastTrimmed = lists:delete(lists:last(Clone), Clone),
            align_to_minexpr_0(LastTrimmed, MinExpr);
        false ->
            FirstTrimmed = lists:delete(hd(Clone), Clone),
            align_to_minexpr_0(FirstTrimmed, MinExpr)
     end.


%% @doc Returns the bodies of a clause.
get_bodies([{_,Type,_} = Clause]) -> 
    case Type of
        clause -> 
            ?Query:exec(Clause, ?Clause:body());
        form ->
            ?Query:exec(Clause, ?Form:deep_exprs())
    end.


%% @doc Returns the Node's tokenlist using the alphabet.
get_token_list({_, Type, _} = Node) ->
    case Type of
        func -> [];
        _ -> ?Alphabet:to_alphabet0(?Alphabet:default_alphabet(), ?Syn:leaves(Node), [])
    end.

%% @doc Returns the minimal length of sublists from the list
get_min_exprs([]) -> [];
get_min_exprs(Exprs) ->
    ExprLengths = [length(K) || K <- Exprs],
    Minlen = lists:min(ExprLengths),
    lists:filter(fun(K) -> length(K) == Minlen end, Exprs).


%% @doc Determines if the node's type in the GoodTypes list.
is_node_type([], _) -> false;
is_node_type(Nodes, GoodTypes) ->
   {_,Type,_} = case is_list(hd(Nodes)) of
                    true ->  hd(hd(Nodes));
                    _ -> hd(Nodes)
                end,
    lists:member(Type, GoodTypes).

%% @doc Gets the additional clones from the ets table.
get_extras(TICs) ->
    Extras = ets:match(?DUPCODE, {'$1'}),
    ets:match_delete(?DUPCODE, {'$1'}),
    append_extras(TICs, Extras).



%% @doc Groups expressions thats head tokens does not match.
trim_exprs([], Res) -> Res;
trim_exprs([H|T], Res) ->
    Group =
        case is_node_type(hd(H), [expr]) of
            true ->
                case need_trim(H) of
                    true -> group_exprs(H,[]);
                  _ -> [H]
                end;
            _ -> [H]
        end,
trim_exprs(T, Res++Group).

%% @doc Groups an expression clone
group_exprs([], Result) -> Result;
group_exprs(Clone, Result) ->
    Head = get_token_list(hd(hd(Clone))),
    Group = lists:filter(fun(K)-> Head == get_token_list(hd(K)) end, Clone),
    Rem = Clone -- Group,
    group_exprs(Rem, Result++[Group]).

%% @doc Determines if grouping needed.
need_trim(H) ->
    Head = get_token_list(hd(hd(H))),
    lists:any(fun(K) -> Head /= get_token_list(hd(K)) end, H).

%% @doc Appdens the extras to the result.
append_extras(TICs, []) -> TICs;
append_extras(TICs, [Head | Tail]) ->
    append_extras(TICs ++ Head, Tail ).

search_dupcode(_Files, _Minlen, _Minclones, _Overlap) ->
    "Not loaded!".
