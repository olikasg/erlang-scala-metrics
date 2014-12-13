Expect 40.

Nonterminals semantic_query query_seq s_query_seq query filter qs qss setop_query_seq setop_qss
             exp100 exp200 exp300 exp400 exp_max
             maybe_query incomplete_query complete_query
             maybe_filter incomplete_filter complete_filter
             maybe_list incomplete_list complete_list item items interv
             maybe_iteration incomplete_iteration complete_iteration
             maybe_closure incomplete_closure complete_closure
	         maybe_stat incomplete_stat complete_stat  .

Terminals    '.' '[' ']' '{' '}' '(' ')' '+' ':' '?' '|'
             atom int string variable comparator set_op filter_set_op
             'not' 'comma' 'or' 'like' 'in' 'interval'.

Rootsymbol   semantic_query.


semantic_query -> atom              : [{complete, {initial_selector, element(3, '$1')}}].
semantic_query -> atom query_seq    : [{complete, {initial_selector, element(3, '$1')}}| '$2'].
semantic_query -> query_seq         : '$1'.
semantic_query -> '?'               : [{complete, {help, initial_selectors}}].
semantic_query -> setop_query_seq   : ['$1'].

s_query_seq -> maybe_filter                         : '$1'.
s_query_seq -> complete_query query_seq             : ['$1' | '$2'].
s_query_seq -> maybe_query                          : ['$1'].
s_query_seq -> maybe_stat                           : ['$1'].

query_seq -> s_query_seq : '$1'.
query_seq -> '(' setop_query_seq ')'                : ['$2'].
query_seq -> '(' setop_query_seq ')' query_seq      : ['$2' | '$4'].
query_seq -> '(' setop_query_seq                    : [{incomplete, element(2, '$2')}].
query_seq -> '('                                    : [{incomplete, {set_op, '('}}].
query_seq -> '(' query_seq ')'                      : '$2'.
query_seq -> '(' query_seq ')' query_seq            : '$2' ++ '$4'.
%query_seq -> '(' query_seq                          : '$2'.

setop_query_seq -> query_seq set_op semantic_query              : {complete, {set_op,{element(3, '$2'), '$1', '$3'}}}.
setop_query_seq -> atom query_seq set_op semantic_query         : {complete, {set_op,{element(3, '$3'), [{complete, {initial_selector, element(3, '$1')}} | '$2'], '$4'}}}.
setop_query_seq -> atom set_op semantic_query                   : {complete, {set_op,{element(3, '$2'), [{complete, {initial_selector, element(3, '$1')}}], '$3'}}}.
setop_query_seq -> query_seq set_op                             : {incomplete, {set_op,{element(3, '$2'), '$1'}}}.
setop_query_seq -> atom query_seq set_op                        : {incomplete, {set_op,{element(3, '$3'), [{complete, {initial_selector, element(3, '$1')}} | '$2']}}}.
setop_query_seq -> atom set_op                                  : {incomplete, {set_op,{element(3, '$2'), [{complete, {initial_selector, element(3, '$1')}}]}}}.
%%setop_query_seq -> query_seq :{incomplete}

s_query_seq -> '[' '?' ']'              : [{complete, {help, filters}}].
s_query_seq -> '.' '?'                  : [{complete, {help, queries}}].

maybe_query -> complete_query         : '$1'.
maybe_query -> incomplete_query       : '$1'.

complete_query -> '.' query           : '$2'.

incomplete_query -> '.'               : {incomplete, {'query', '.'}}.

query -> atom                   : {complete, {selector, element(3, '$1')}}.
query -> maybe_iteration        : '$1'.
query -> maybe_closure          : '$1'.

maybe_stat -> complete_stat	    : '$1'.
maybe_stat -> incomplete_stat	: '$1'.

complete_stat -> ':' atom	: {complete, {statistics, element(3, '$2')}}.
complete_stat -> ':' '?'	: {complete, {help, statistics}}.

incomplete_stat -> ':'		: {incomplete, {statistics, ':'}}.

maybe_filter -> complete_filter                 : '$1'.
maybe_filter -> incomplete_filter               : '$1'.

complete_filter -> '[' filter ']' query_seq     : [{complete, {filter, '$2'}} | '$4'].
complete_filter -> '[' filter ']'               : [{complete, {filter, '$2'}}].

incomplete_filter -> '['                        : [{incomplete, {filter, '['}}].
incomplete_filter -> '[' filter                 : [{incomplete, {filter, '$2'}}].
incomplete_filter -> '[' ']'                    : [{incomplete, {filter, '[]'}}].
incomplete_filter -> '[' ']' query_seq          : [{incomplete, {filter, '[]'}} | '$3'].


maybe_iteration -> complete_iteration   : '$1'.
maybe_iteration -> incomplete_iteration : '$1'.

complete_iteration -> '{' qss '}' int   : {complete, {iteration, {seq, '$2'}, {mult, element(3, '$4')}}}.

incomplete_iteration -> '{'             : {incomplete, {iteration, element(1, '$1')}}.
incomplete_iteration -> '{' qss         : {incomplete, {iteration, '$2'}}.
incomplete_iteration -> '{' qss '}'     : {incomplete, {iteration, element(1, '$3')}}.

maybe_closure -> complete_closure       : '$1'.
maybe_closure -> incomplete_closure     : '$1'.

complete_closure -> '(' qss ')' int     : {complete, {closure, {seq, '$2'}, {mult, element(3, '$4')}}}.
complete_closure -> '(' qss ')' '+'     : {complete, {closure, {seq, '$2'}, {mult, infinite}}}.

incomplete_closure -> '('               : {incomplete, {closure, element(1, '$1')}}.
incomplete_closure -> '(' qss           : {incomplete, {closure, '$2'}}.
incomplete_closure -> '(' qss ')'       : {incomplete, {closure, element(1, '$3')}}.

qss -> setop_qss                        : ['$1'].
qss -> '(' setop_qss ')'                : ['$2'].
%%qss -> '(' setop_qss                    : [{incomplete, '$2'}].
%%qss -> '('                              : [{incomplete, '$1'}].
qss -> '(' setop_qss ')' query_seq      : ['$2' | '$4'].
qss -> qs                               : '$1'.

setop_qss -> qss set_op qss                   : {complete, {set_op,{element(3, '$2'), '$1', '$3'}}}.
setop_qss -> qss set_op                       : {incomplete, {set_op,{element(3, '$2'), '$1'}}}.

qs -> s_query_seq     : '$1'.
qs -> query query_seq : ['$1'| '$2'].
qs -> query           : ['$1'].

filter  -> exp100                   : '$1'.

%%Disjunction
exp100  -> exp200 'or' exp100       : {complete,   {'or', '$1', '$3'}}.
exp100  -> exp200 'or'              : {incomplete, {'or', '$1'}}.
exp100  -> exp200                   : '$1'.
%%Conjunction
exp200  -> exp300 'comma' exp200    : {complete,   {'and', '$1', '$3'}}.
exp200  -> exp300 'comma'           : {incomplete, {'and', '$1'}}.
exp200  -> exp300                   : '$1'.
%%Comparison
exp300  -> exp400 comparator exp400 : {complete,   {element(3, '$2'), '$1', '$3'}}.
exp300  -> exp400 comparator        : {incomplete, {comparator, '$1'}}.
exp300  -> exp400 'like' exp400     : {complete,   {'like', '$1', '$3'}}.
exp300  -> exp400 'like'            : {incomplete, {'like', '$1'}}.

exp300 -> exp_max 'in' maybe_list    : {complete,{'in', '$1', '$3'}}.
exp300 -> exp_max 'in' semantic_query: {complete,{'in', '$1', '$3'}}.
exp300 -> complete_list 'in' exp_max : {complete,{'in', '$3', '$1'}}.
exp300 -> complete_list 'in'         : {incomplete,{'in','$1'}}.
exp300 -> exp_max 'in'               : {incomplete, {'in', '$1'}}.

exp300 -> complete_list filter_set_op query_seq  : {complete,{set_op,{element(3,'$2'), '$1', {complete, {seq, '$3'}}}}}.
exp300 -> atom query_seq filter_set_op query_seq : {complete,{set_op,{element(3,'$3'), [{complete, {initial_selector, element(3, '$1')}}| '$2'], {complete, {seq, '$4'}}}}}.
exp300 -> atom filter_set_op query_seq           : {complete,{set_op,{element(3,'$2'), [{complete, {initial_selector, element(3, '$1')}}], {complete, {seq, '$3'}}}}}.
exp300 -> query_seq filter_set_op semantic_query : {complete,{set_op,{element(3,'$2'), {complete, {seq, '$1'}}, '$3'}}}.
exp300 -> query_seq filter_set_op maybe_list     : {complete,{set_op,{element(3,'$2'), {complete, {seq, '$1'}}, '$3'}}}.

exp300 -> query_seq filter_set_op                : {incomplete,{set_op,{element(3,'$2'), {complete, {seq, '$1'}}}}}.
exp300 -> complete_list filter_set_op            : {incomplete,{set_op,{element(3,'$2'), '$1'}}}.
exp300 -> atom query_seq filter_set_op           : {incomplete,{set_op,{element(3,'$3'), [{complete, {initial_selector, element(3, '$1')}}| '$2']}}}.
exp300 -> atom filter_set_op                     : {incomplete,{set_op,{element(3,'$2'), [{complete, {initial_selector, element(3, '$1')}}]}}}.

exp300  -> exp400                    : '$1'.
%%Negation
exp400  -> 'not' exp_max            : {complete,   {'not', '$2'}}.
exp400  -> 'not'                    : {incomplete, {'not'}}.
exp400  -> exp_max                  : '$1'.
%%Parenthesis
exp_max -> '(' exp100 ')'           : '$2'.
%%Embedded queries
exp_max -> query_seq                : {complete, {seq, '$1'}}.
%%Simple values
exp_max -> atom                     : element(3, '$1').
exp_max -> int                      : element(3, '$1').
exp_max -> string                   : element(3, '$1').
exp_max -> variable		    : {variable, element(3, '$1')}.

maybe_list -> complete_list         : '$1'.
maybe_list -> incomplete_list       : '$1'.

incomplete_list -> '|'              : {incomplete, {cons, '|'}}.
incomplete_list -> '|' items        : {incomplete, {cons,['$2']}}.
incomplete_list -> '|' interv       : {incomplete, element(2, '$2')}.

complete_list -> '|''|'             : {complete,{cons, []}}.
complete_list -> '|' items '|'      : {complete, {cons, '$2'}}.
complete_list -> '|' interv '|'     : '$2'.

items -> item                       : ['$1'].                                
items -> item 'comma' items         : ['$1'] ++ '$3'.
interv -> int 'interval' int :
    {complete,{cons,
        if
            element(3, '$1') > element(3, '$3') -> [];
            true -> lists:seq(element(3, '$1'), element(3, '$3'))
        end
    }}.
interv -> int 'interval'            : {incomplete,{cons, element(3, '$1')}}.
item -> atom                        : element(3, '$1').                    
item -> int                         : element(3, '$1').
item -> string                      : element(3,'$1').
