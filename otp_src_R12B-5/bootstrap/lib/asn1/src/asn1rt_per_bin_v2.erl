%%<copyright>
%% <year>2007-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
-module(asn1rt_per_bin_v2).

%% encoding / decoding of PER aligned

-include("asn1_records.hrl").

-export([dec_fixup/3, cindex/3, list_to_record/2]).
-export([setchoiceext/1, setext/1, fixoptionals/3, fixextensions/2, 
	 getext/1, getextension/2, skipextensions/3, getbit/1, getchoice/3 ]).
-export([getoptionals/2, getoptionals2/2, 
	 set_choice/3, encode_integer/2, encode_integer/3  ]).
-export([decode_integer/2, decode_integer/3, encode_small_number/1, 
	 decode_boolean/1, encode_length/2, decode_length/1, decode_length/2,
	 encode_small_length/1, decode_small_length/1,
	 decode_compact_bit_string/3]).
-export([decode_enumerated/3, 
	 encode_bit_string/3, decode_bit_string/3  ]).
-export([encode_octet_string/2, decode_octet_string/2,
	 encode_null/1, decode_null/1,
	 encode_object_identifier/1, decode_object_identifier/1,
	 complete/1]).


-export([encode_open_type/2, decode_open_type/2]).

-export([encode_GeneralString/2, decode_GeneralString/2,
	 encode_GraphicString/2, decode_GraphicString/2,
	 encode_TeletexString/2, decode_TeletexString/2,
	 encode_VideotexString/2, decode_VideotexString/2,
	 encode_ObjectDescriptor/2, decode_ObjectDescriptor/1,
	 encode_UTF8String/1,decode_UTF8String/1
	]).

-export([decode_constrained_number/2,
	 decode_constrained_number/3,
	 decode_unconstrained_number/1,
	 decode_semi_constrained_number/2,
	 encode_unconstrained_number/1,
	 decode_constrained_number/4,
	 encode_octet_string/3,
	 decode_octet_string/3,
	 encode_known_multiplier_string/5,
	 decode_known_multiplier_string/5,
	 getoctets/2, getbits/2
	]).


-export([eint_positive/1]).
-export([pre_complete_bits/2]).

-define('16K',16384).
-define('32K',32768).
-define('64K',65536).


dec_fixup(Terms,Cnames,RemBytes) ->
    dec_fixup(Terms,Cnames,RemBytes,[]).

dec_fixup([novalue|T],[_Hc|Tc],RemBytes,Acc) ->
    dec_fixup(T,Tc,RemBytes,Acc);
dec_fixup([{_Name,novalue}|T],[_Hc|Tc],RemBytes,Acc) ->
    dec_fixup(T,Tc,RemBytes,Acc);
dec_fixup([H|T],[Hc|Tc],RemBytes,Acc) ->
    dec_fixup(T,Tc,RemBytes,[{Hc,H}|Acc]);
dec_fixup([],_Cnames,RemBytes,Acc) ->
    {lists:reverse(Acc),RemBytes}.

cindex(Ix,Val,Cname) ->
    case element(Ix,Val) of
	{Cname,Val2} -> Val2;
	X -> X
    end.

%% converts a list to a record if necessary
list_to_record(_,Tuple) when tuple(Tuple) ->
    Tuple;
list_to_record(Name,List) when list(List) ->
    list_to_tuple([Name|List]).

%%--------------------------------------------------------
%% setchoiceext(InRootSet) -> [{bit,X}]
%% X  is set to  1 when InRootSet==false
%% X  is set to  0 when InRootSet==true
%%
setchoiceext(true) ->
    [0];
setchoiceext(false) ->
    [1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% setext(true|false) ->  CompleteList
%%

setext(false) ->
    <<0:1>>;
setext(true) ->
    <<1:1>>.

fixoptionals(OptList,_OptLength,Val) when tuple(Val) ->
    {Val,<< <<B:1>> || B <- fixoptionals(OptList,Val,[])>>};

fixoptionals([],_,Acc) ->
    %% Optbits
    lists:reverse(Acc);
fixoptionals([Pos|Ot],Val,Acc) ->
    case element(Pos,Val) of
	asn1_NOVALUE -> fixoptionals(Ot,Val,[0|Acc]);
	asn1_DEFAULT -> fixoptionals(Ot,Val,[0|Acc]);
	_ -> fixoptionals(Ot,Val,[1|Acc])
    end.


getext(Bytes) when is_bitstring(Bytes) ->
    getbit(Bytes).

getextension(0, Bytes) ->
    {<<>>,Bytes};
getextension(1, Bytes) ->
    {Len,Bytes2} = decode_small_length(Bytes),
    getbits_as_binary(Len,Bytes2).% {Bin,Bytes3}.

fixextensions({ext,ExtPos,ExtNum},Val) ->
    case fixextensions(ExtPos,ExtNum+ExtPos,Val,0) of
	0 -> [];
	ExtBits ->
	    [encode_small_length(ExtNum),pre_complete_bits(ExtNum,ExtBits)]
    end.

fixextensions(Pos,MaxPos,_,Acc) when Pos >= MaxPos ->
    Acc;
fixextensions(Pos,ExtPos,Val,Acc) ->
    Bit = case catch(element(Pos+1,Val)) of
	      asn1_NOVALUE ->
		  0;
	      asn1_NOEXTVALUE ->
		  0;
	      {'EXIT',_} ->
		  0;
	      _ ->
		  1
	  end,
    fixextensions(Pos+1,ExtPos,Val,(Acc bsl 1)+Bit).

skipextensions(Bytes,Nr,ExtensionBitstr) when is_bitstring(ExtensionBitstr) -> 
    Prev = Nr - 1,
    case ExtensionBitstr of
	<<_:Prev,1:1,_/bitstring>> ->
	    {_,Bytes2} = decode_open_type(Bytes,[]),
 	    skipextensions(Bytes2, Nr+1, ExtensionBitstr);
	<<_:Prev,0:1,_/bitstring>> ->
	    skipextensions(Bytes, Nr+1, ExtensionBitstr);
	_ ->
	    Bytes
    end.
	

getchoice(Bytes,1,0) -> % only 1 alternative is not encoded
    {0,Bytes};
getchoice(Bytes,_,1) ->
    decode_small_number(Bytes);
getchoice(Bytes,NumChoices,0) ->
    decode_constrained_number(Bytes,{0,NumChoices-1}).

%% old version kept for backward compatibility with generates from R7B01
getoptionals(Bytes,NumOpt) ->
    getbits_as_binary(NumOpt,Bytes).

%% new version used in generates from r8b_patch/3 and later
getoptionals2(Bytes,NumOpt) ->
    {_,_} = getbits(Bytes,NumOpt).


%% getbits_as_binary(Num,Bytes) -> {Bin,Rest}
%% Num = integer(),
%% Bytes = bitstring(),
%% Bin = bitstring(),
%% Rest = bitstring()
getbits_as_binary(Num,Bytes) when is_bitstring(Bytes) ->
    <<BS:Num/bitstring,Rest/bitstring>> = Bytes,
    {BS,Rest}.
    
getbits_as_list(Num,Bytes) when is_bitstring(Bytes) ->
    <<BitStr:Num/bitstring,Rest/bitstring>> = Bytes,
    {[ B || <<B:1>> <= BitStr],Rest}.


getbit(Buffer) ->
    <<B:1,Rest/bitstring>> = Buffer,
    {B,Rest}.


getbits(Buffer,Num) when is_bitstring(Buffer) ->
    <<Bs:Num,Rest/bitstring>> = Buffer,
    {Bs,Rest}.



align(Bin) when is_binary(Bin) ->
    Bin;
align(BitStr) when is_bitstring(BitStr) ->
    AlignBits = bitsize(BitStr) rem 8,
    <<_:AlignBits,Rest/binary>> = BitStr,
    Rest.
    

%% First align buffer, then pick the first Num octets.
%% Returns octets as an integer with bit significance as in buffer.
getoctets(Buffer,Num) when is_binary(Buffer) ->
    <<Val:Num/integer-unit:8,RestBin/binary>> = Buffer,
    {Val,RestBin};
getoctets(Buffer,Num) when is_bitstring(Buffer) ->
    AlignBits = bitsize(Buffer) rem 8,
    <<_:AlignBits,Val:Num/integer-unit:8,RestBin/binary>> = Buffer,
    {Val,RestBin}.


%% First align buffer, then pick the first Num octets.
%% Returns octets as a binary
getoctets_as_bin(Bin,Num) when is_binary(Bin) ->
    <<Octets:Num/binary,RestBin/binary>> = Bin,
    {Octets,RestBin};
getoctets_as_bin(Bin,Num) when is_bitstring(Bin) ->
    AlignBits = bitsize(Bin) rem 8,
    <<_:AlignBits,Val:Num/binary,RestBin/binary>> = Bin,
    {Val,RestBin}.
    

%% same as above but returns octets as a List
getoctets_as_list(Buffer,Num) ->
    {Bin,Buffer2} = getoctets_as_bin(Buffer,Num),
    {binary_to_list(Bin),Buffer2}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set_choice(Alt,Choices,Altnum) -> ListofBitSettings
%% Alt = atom()
%% Altnum = integer() | {integer(),integer()}% number of alternatives
%% Choices = [atom()] | {[atom()],[atom()]}
%% When Choices is a tuple the first list is the Rootset and the
%% second is the Extensions and then Altnum must also be a tuple with the
%% lengths of the 2 lists 
%%
set_choice(Alt,{L1,L2},{Len1,_Len2}) ->
    case set_choice_tag(Alt,L1) of
	N when integer(N), Len1 > 1 ->
	    [<<0:1>>, % the value is in the root set
	     encode_constrained_number({0,Len1-1},N)];
	N when integer(N) ->
	    <<0:1>>; % no encoding if only 0 or 1 alternative
	false ->
	    [<<1:1>>, % extension value
	     case set_choice_tag(Alt,L2) of
		 N2 when integer(N2) ->
		     encode_small_number(N2);
		 false ->
		     unknown_choice_alt
	     end]
    end;
set_choice(Alt,L,Len) ->
    case set_choice_tag(Alt,L) of
	N when integer(N), Len > 1 ->
	    encode_constrained_number({0,Len-1},N);
	N when integer(N) ->
	    []; % no encoding if only 0 or 1 alternative
	false ->
	    [unknown_choice_alt]
    end.

set_choice_tag(Alt,Choices) ->
    set_choice_tag(Alt,Choices,0).

set_choice_tag(Alt,[Alt|_Rest],Tag) ->
    Tag;
set_choice_tag(Alt,[_H|Rest],Tag) ->
    set_choice_tag(Alt,Rest,Tag+1);
set_choice_tag(_Alt,[],_Tag) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_fragmented_XXX; decode of values encoded fragmented according
%% to ITU-T X.691 clause 10.9.3.8. The unit (XXX) is either bits, octets,
%% characters or number of components (in a choice,sequence or similar).
%% Buffer is a buffer binary().
%% C is the constrained length.
%% If the buffer is not aligned, this function does that.
decode_fragmented_bits(Buffer,C) when is_binary(Buffer) ->
    decode_fragmented_bits(Buffer,C,[]);
decode_fragmented_bits(Buffer,C) when is_bitstring(Buffer) ->
    AlignBits = bitsize(Buffer) rem 8,
    <<_:AlignBits,Rest/binary>> = Buffer,
    decode_fragmented_bits(Rest,C,[]).

decode_fragmented_bits(<<3:2,Len:6,Bin/binary>>,C,Acc) ->
    {Value,Bin2} = split_binary(Bin, Len * ?'16K'), % Len = 1 | 2 | 3 | 4
    decode_fragmented_bits(Bin2,C,[Value|Acc]);
decode_fragmented_bits(<<0:1,0:7,Bin/binary>>,C,Acc) ->
    BinBits = erlang:list_to_bitstr(lists:reverse(Acc)),
    case C of
	Int when integer(Int),C == bitsize(BinBits) ->
	    {BinBits,Bin};
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinBits}}})
    end;
decode_fragmented_bits(<<0:1,Len:7,Bin/binary>>,C,Acc) ->
    <<Value:Len/bitstring,Rest/bitstring>> = Bin,
    BinBits = erlang:list_to_bitstr([Value|Acc]),
    case C of
	Int when integer(Int),C == bitsize(BinBits) ->
	    {BinBits,Rest};
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinBits}}})
    end.


decode_fragmented_octets(Bin,C) ->
    decode_fragmented_octets(Bin,C,[]).

decode_fragmented_octets(<<3:2,Len:6,Bin/binary>>,C,Acc) ->
    {Value,Bin2} = split_binary(Bin,Len * ?'16K'),
    decode_fragmented_octets(Bin2,C,[Value|Acc]);
decode_fragmented_octets(<<0:1,0:7,Bin/binary>>,C,Acc) ->
    Octets = list_to_binary(lists:reverse(Acc)),
    case C of
	Int when integer(Int), C == size(Octets) ->
	    {Octets,Bin};
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,Octets}}})
    end;
decode_fragmented_octets(<<0:1,Len:7,Bin/binary>>,C,Acc) ->
    <<Value:Len/binary-unit:8,Bin2/binary>> = Bin,
    BinOctets = list_to_binary(lists:reverse([Value|Acc])),
    case C of
	Int when integer(Int),size(BinOctets) == Int ->
	    {BinOctets,Bin2};
	Int when integer(Int) ->
	    exit({error,{asn1,{illegal_value,C,BinOctets}}})
    end.


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Constraint, Value) -> CompleteList
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary
%% Contraint = not used in this version
%%
encode_open_type(_Constraint, Val) when list(Val) ->
    Bin = list_to_binary(Val),
    case size(Bin) of 
	Size when Size>255 ->
	    [encode_length(undefined,Size),align,Bin];
	Size ->
	    [encode_length(undefined,Size),align,Bin]
    end;
encode_open_type(_Constraint, Val) when binary(Val) ->
    case size(Val) of
	Size when Size>255 ->
	    [encode_length(undefined,size(Val)),align,Val]; % octets implies align
	Size ->
	    [encode_length(undefined,Size),align,Val]
    end.
%% the binary_to_list is not optimal but compatible with the current solution

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Buffer,Constraint) -> Value
%% Constraint is not used in this version
%% Buffer = [byte] with PER encoded data 
%% Value = [byte] with decoded data (which must be decoded again as some type)
%%
decode_open_type(Bytes, _Constraint) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_bin(Bytes2,Len).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_integer(Constraint,Value,NamedNumberList) -> CompleteList
%% encode_integer(Constraint,Value) -> CompleteList
%% encode_integer(Constraint,{Name,Value}) -> CompleteList
%% 
%%
encode_integer(C,V,NamedNumberList) when atom(V) ->
    case lists:keysearch(V,1,NamedNumberList) of
	{value,{_,NewV}} -> 
	    encode_integer(C,NewV);
	_ -> 
	    exit({error,{asn1,{namednumber,V}}})
    end;
encode_integer(C,V,_NamedNumberList) when integer(V) ->
    encode_integer(C,V);
encode_integer(C,{Name,V},NamedNumberList) when atom(Name) ->
    encode_integer(C,V,NamedNumberList).

encode_integer(C,{Name,Val}) when atom(Name) ->
    encode_integer(C,Val);

encode_integer([{Rc,_Ec}],Val) when tuple(Rc) -> % XXX when is this invoked? First argument most often a list,...Ok this is the extension case...but it doesn't work.
    case (catch encode_integer([Rc],Val)) of
	{'EXIT',{error,{asn1,_}}} ->
	    [<<1:1>>,encode_unconstrained_number(Val)];
	Encoded ->
	    [<<0:1>>,Encoded]
    end;

encode_integer([],Val) ->
    encode_unconstrained_number(Val);
%% The constraint is the effective constraint, and in this case is a number
encode_integer([{'SingleValue',V}],V) ->
    [];
encode_integer([{'ValueRange',VR={Lb,Ub},Range,PreEnc}],Val) when Val >= Lb,
						Ub >= Val ->
    %% this case when NamedNumberList
    encode_constrained_number(VR,Range,PreEnc,Val);
encode_integer([{'ValueRange',{Lb,'MAX'}}],Val) ->
    encode_semi_constrained_number(Lb,Val);
encode_integer([{'ValueRange',{'MIN',_}}],Val) ->
    encode_unconstrained_number(Val);
encode_integer([{'ValueRange',VR={_Lb,_Ub}}],Val) ->
    encode_constrained_number(VR,Val);
encode_integer(_,Val) ->
    exit({error,{asn1,{illegal_value,Val}}}).

    

decode_integer(Buffer,Range,NamedNumberList) ->
    {Val,Buffer2} = decode_integer(Buffer,Range),
    case lists:keysearch(Val,2,NamedNumberList) of
	{value,{NewVal,_}} -> {NewVal,Buffer2};
	_ -> {Val,Buffer2}
    end.

decode_integer(Buffer,[{Rc,_Ec}]) when tuple(Rc) ->
    {Ext,Buffer2} = getext(Buffer),
    case Ext of
	0 -> decode_integer(Buffer2,[Rc]);
	1 -> decode_unconstrained_number(Buffer2)
    end;
decode_integer(Buffer,undefined) ->
    decode_unconstrained_number(Buffer);
decode_integer(Buffer,C) ->
    case get_constraint(C,'SingleValue') of
	V when integer(V) ->
	    {V,Buffer};
	_ ->
	    decode_integer1(Buffer,C)
    end.

decode_integer1(Buffer,C) ->
    case VR = get_constraint(C,'ValueRange') of
	no ->
	    decode_unconstrained_number(Buffer);
	{Lb, 'MAX'} ->
	    decode_semi_constrained_number(Buffer,Lb);
	{_Lb,_Ub} ->
	    decode_constrained_number(Buffer,VR)
    end.

%% X.691:10.6 Encoding of a normally small non-negative whole number
%% Use this for encoding of CHOICE index if there is an extension marker in 
%% the CHOICE
encode_small_number({Name,Val}) when atom(Name) ->
    encode_small_number(Val);
encode_small_number(Val) when Val =< 63 ->
    <<Val:7>>; % same as above but more efficient
encode_small_number(Val) ->
    [<<1:1>>,encode_semi_constrained_number(0,Val)].

decode_small_number(Bytes) ->
    {Bit,Bytes2} = getbit(Bytes),
    case Bit of
	0 -> 
	    getbits(Bytes2,6);
	1 ->
	    decode_semi_constrained_number(Bytes2,0)
    end.

%% X.691:10.7 Encoding of a semi-constrained whole number
%% might be an optimization encode_semi_constrained_number(0,Val) ->
encode_semi_constrained_number(C,{Name,Val}) when atom(Name) ->
    encode_semi_constrained_number(C,Val);
encode_semi_constrained_number({Lb,'MAX'},Val) ->
    encode_semi_constrained_number(Lb,Val);
encode_semi_constrained_number(Lb,Val) ->
    Val2 = Val - Lb,
    Bin = eint_bin_positive(Val2),
    Size = size(Bin),
    if 
	Size < 128 ->
	    [align,<<Size>>,Bin];
	Size < 16384 ->
	    [align,<<2:2,Size:14>>,Bin];
	true ->
	    [encode_length(undefined,Size),Bin]
    end.

decode_semi_constrained_number(Bytes,{Lb,_}) ->
    decode_semi_constrained_number(Bytes,Lb);
decode_semi_constrained_number(Bytes,Lb) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {V,Bytes3} = getoctets(Bytes2,Len),
    {V+Lb,Bytes3}.

%% Val is in range
encode_constrained_number({Lb,_Ub},_Range,{bits,N},Val) ->
    Val2 = Val-Lb,
    <<Val2:N>>;
encode_constrained_number({Lb,_Ub},_Range,{octets,N},Val) when N < 256->
    %% N is 8 or 16 (1 or 2 octets)
    Val2 = Val-Lb,
    [align,<<Val2:(8*N)>>];
encode_constrained_number({Lb,_Ub},_Range,{octets,N},Val) -> % N>255
    %% N is 8 or 16 (1 or 2 octets)
    Val2 = Val-Lb,
    [align,<<Val2:(8*N)>>];
encode_constrained_number({Lb,_Ub},Range,_,Val) ->
    Val2 = Val-Lb,
    if
	Range =< 16#1000000  -> % max 3 octets
	    Bin = eint_bin_positive(Val2),
	    S = size(Bin),
	    %% optimize encode_length -> encode_constrained_number
	    [<<(S-1):2>>,align,Bin]; 
	Range =< 16#100000000  -> % max 4 octets
	    Bin = eint_bin_positive(Val2),
	    S = size(Bin),
	    %% optimize encode_length -> encode_constrained_number
	    [<<(S-1):2>>,align,Bin];
	Range =< 16#10000000000  -> % max 5 octets
	    Bin = eint_bin_positive(Val2),
	    S = size(Bin),
	    %% optimize encode_length -> encode_constrained_number
	    [<<(S-1):3>>,align,Bin];
	true  ->
	    exit({not_supported,{integer_range,Range}})
    end.

encode_constrained_number(Range,{Name,Val}) when atom(Name) ->
    encode_constrained_number(Range,Val);
encode_constrained_number({Lb,Ub},Val) when Val >= Lb, Ub >= Val -> 
    Range = Ub - Lb + 1,
    Val2 = Val - Lb,
    if 
	Range  == 2 ->
	    <<Val2:1>>;
	Range  =< 4 -> 
	    <<Val2:2>>;
	Range  =< 8 ->
	    <<Val2:3>>;
	Range  =< 16 ->
	    <<Val2:4>>;
	Range  =< 32 ->
	    <<Val2:5>>;
	Range  =< 64 ->
	    <<Val2:6>>;
	Range  =< 128 ->
	    <<Val2:7>>;
	Range  =< 255 ->
	    <<Val2>>;
	Range  =< 256 -> %% octet alined
	    [align,<<Val2>>];
	Range  =< 65536 ->
	    [align,<<Val2:16>>];
	Range =< 16#1000000  ->
	    Bin = eint_bin_positive(Val2),
	    S = size(Bin),
	    [<<(S-1):2>>,align,Bin];
	Range =< 16#100000000  ->
	    Bin = eint_bin_positive(Val2),
	    S = size(Bin),
	    [<<(S-1):2>>,align,Bin];
	Range =< 16#10000000000  ->
	    Octs = eint_bin_positive(Val2),
	    Len = size(Octs),
	    <<(Len-1):3,0:5,Octs/binary>>;
	true  ->
	    exit({not_supported,{integer_range,Range}})
    end;
encode_constrained_number({_,_},Val) -> 
    exit({error,{asn1,{illegal_value,Val}}}).

decode_constrained_number(Buffer,VR={Lb,Ub}) ->
    Range = Ub - Lb + 1,
    decode_constrained_number(Buffer,VR,Range).

decode_constrained_number(Buffer,{Lb,_Ub},_Range,{bits,N}) ->
    {Val,Remain} = getbits(Buffer,N),
    {Val+Lb,Remain};
decode_constrained_number(Buffer,{Lb,_Ub},_Range,{octets,N}) ->
    {Val,Remain} = getoctets(Buffer,N),
    {Val+Lb,Remain}.

decode_constrained_number(Buffer,{Lb,_Ub},Range) ->
						%    Val2 = Val - Lb,
    {Val,Remain} = 
	if 
	    Range  == 2 ->
		getbits(Buffer,1);
	    Range  =< 4 -> 
		getbits(Buffer,2);
	    Range  =< 8 ->
		getbits(Buffer,3);
	    Range  =< 16 ->
		getbits(Buffer,4);
	    Range  =< 32 ->
		getbits(Buffer,5);
	    Range  =< 64 ->
		getbits(Buffer,6);
	    Range  =< 128 ->
		getbits(Buffer,7);
	    Range  =< 255 ->
		getbits(Buffer,8);
	    Range  =< 256 ->
		getoctets(Buffer,1);
	    Range  =< 65536 ->
		getoctets(Buffer,2);
	    Range =< 16#1000000  ->
		{Len,Bytes2} = decode_length(Buffer,{1,3}),
		{Octs,Bytes3} = getoctets_as_bin(Bytes2,Len),
		{dec_pos_integer(Octs),Bytes3};
	    Range =< 16#100000000  ->
		{Len,Bytes2} = decode_length(Buffer,{1,4}),
		{Octs,Bytes3} = getoctets_as_bin(Bytes2,Len),
		{dec_pos_integer(Octs),Bytes3};
	    Range =< 16#10000000000  ->
		{Len,Bytes2} = decode_length(Buffer,{1,5}),
		{Octs,Bytes3} = getoctets_as_bin(Bytes2,Len),
		{dec_pos_integer(Octs),Bytes3};
	    true  ->
		exit({not_supported,{integer_range,Range}})
	end,
    {Val+Lb,Remain}.

%% X.691:10.8 Encoding of an unconstrained whole number

encode_unconstrained_number(Val) when Val >= 0 ->
    Oct = eint_bin_2Cs(Val),
    Len = size(Oct),
    if 
	Len < 128 ->
	    %% equiv with encode_length(undefined,Len) but faster
	    [align,<<Len>>,Oct];
	Len < 16384 ->
	    %% equiv with encode_length(undefined,Len) but faster
	    [align,<<2:2,Len:14>>,Oct];
	true ->
	    [align,encode_length(undefined,Len),<<Len:16>>,Oct]
    end;
encode_unconstrained_number(Val) -> % negative
    Oct = enint(Val,[]),
    Len = size(Oct),
    if 
	Len < 128 ->
	    %% equiv with encode_length(undefined,Len) but faster
	    [align,<<Len>>,Oct];
	Len < 16384 ->
	    %% equiv with encode_length(undefined,Len) but faster
	    [<<2:2,Len:14>>,align,Oct];
	true ->
	    [align,encode_length(undefined,Len),Oct]
    end.


%% used for positive Values which don't need a sign bit
%% returns a list
eint_positive(Val) ->
    case eint(Val,[]) of
	[0,B1|T] ->
	    [B1|T];
	T -> 
	    T
    end.


eint(0, [B|Acc]) when B < 128 ->
    [B|Acc];
eint(N, Acc) ->
    eint(N bsr 8, [N band 16#ff| Acc]).

enint(-1, [B1|T]) when B1 > 127 ->
    list_to_binary([B1|T]);
enint(N, Acc) ->
    enint(N bsr 8, [N band 16#ff|Acc]).

eint_bin_2Cs(Int) ->
    case eint_bin_positive(Int) of
	Bin = <<B,_/binary>> when B > 16#7f ->
	    <<0,Bin/binary>>;
	Bin -> Bin
    end.
	    
%% returns the integer as a binary
eint_bin_positive(Val) when Val < 16#100  ->
    <<Val>>;
eint_bin_positive(Val) when Val < 16#10000 ->
    <<Val:16>>;
eint_bin_positive(Val) when Val < 16#1000000 ->
    <<Val:24>>;
eint_bin_positive(Val) when Val < 16#100000000 ->
    <<Val:32>>;
eint_bin_positive(Val) ->
    list_to_binary([eint_bin_positive2(Val bsr 32)|<<Val:32>>]).
eint_bin_positive2(Val) when Val < 16#100  ->
    <<Val>>;
eint_bin_positive2(Val) when Val < 16#10000 ->
    <<Val:16>>;
eint_bin_positive2(Val) when Val < 16#1000000 ->
    <<Val:24>>;
eint_bin_positive2(Val) when Val < 16#100000000 ->
    <<Val:32>>;
eint_bin_positive2(Val) ->
    [eint_bin_positive2(Val bsr 32)|<<Val:32>>].




decode_unconstrained_number(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {Ints,Bytes3} = getoctets_as_bin(Bytes2,Len),
    {dec_integer(Ints),Bytes3}.


dec_pos_integer(Ints) ->
    decpint(Ints).
dec_integer(Bin = <<0:1,_:7,_/binary>>) ->  
    decpint(Bin);
dec_integer(<<_:1,B:7,BitStr/bitstring>>) ->
    Size = bitsize(BitStr),
    <<I:Size>> = BitStr,
    (-128 + B) bsl bitsize(BitStr) bor I.

    
    
decpint(Bin) ->
    Size = bitsize(Bin),
    <<Int:Size>> = Bin,
    Int.

%% X.691:10.9 Encoding of a length determinant
%%encode_small_length(undefined,Len) -> % null means no UpperBound
%%    encode_small_number(Len).

%% X.691:10.9.3.5 
%% X.691:10.9.3.7
encode_length(undefined,Len) -> % un-constrained
    if 
	Len < 128 ->
	    [align,<<Len>>];
	Len < 16384 ->
	    [align,<<2:2,Len:14>>];
	true  -> % should be able to endode length >= 16384 i.e. fragmented length
	    exit({error,{asn1,{encode_length,{nyi,above_16k}}}})
    end;

encode_length({0,'MAX'},Len) ->
    encode_length(undefined,Len);
encode_length(Vr={Lb,Ub},Len) when Ub =< 65535 ,Lb >= 0 -> % constrained
    encode_constrained_number(Vr,Len);
encode_length({Lb,_Ub},Len) when integer(Lb), Lb >= 0 -> % Ub > 65535
    encode_length(undefined,Len);
encode_length({Vr={Lb,Ub},Ext},Len) 
  when Ub =< 65535 ,Lb >= 0,Len=<Ub, is_list(Ext) -> 
    %% constrained extensible 
    [<<0:1>>,encode_constrained_number(Vr,Len)];
encode_length({{Lb,_},Ext},Len) when is_list(Ext) -> 
    [<<1:1>>,encode_semi_constrained_number(Lb,Len)];
encode_length(SingleValue,_Len) when integer(SingleValue) ->
    [].

%% X.691 10.9.3.4 (only used for length of bitmap that prefixes extension 
%% additions in a sequence or set
encode_small_length(Len) when Len =< 64 ->
    <<(Len-1):7>>;
encode_small_length(Len) ->
    [<<1:1>>,encode_length(undefined,Len)].


decode_small_length(Buffer) ->
    case getbit(Buffer) of
	{0,Remain} -> 
	    {Bits,Remain2} = getbits(Remain,6),
	    {Bits+1,Remain2};
	{1,Remain} -> 
	    decode_length(Remain,undefined)
    end.

decode_length(Buffer) ->
    decode_length(Buffer,undefined).

decode_length(Buffer,undefined)  -> % un-constrained
    case align(Buffer) of
	<<0:1,Oct:7,Rest/binary>> ->
	    {Oct,Rest};
	<<2:2,Val:14,Rest/binary>> ->
	    {Val,Rest};
	<<3:2,_Val:14,_Rest/binary>> ->
	    %% this case should be fixed
	    exit({error,{asn1,{decode_length,{nyi,above_16k}}}})
    end;

decode_length(Buffer,{Lb,Ub}) when Ub =< 65535 ,Lb >= 0 -> % constrained
    decode_constrained_number(Buffer,{Lb,Ub});
decode_length(Buffer,{Lb,_Ub}) when integer(Lb), Lb >= 0 -> % Ub > 65535
    decode_length(Buffer,undefined);
decode_length(Buffer,{{Lb,Ub},Ext}) when is_list(Ext) -> 
    case getbit(Buffer) of
	{0,Buffer2} ->
	    decode_length(Buffer2, {Lb,Ub})
    end;


%When does this case occur with {_,_Lb,Ub} ??
% X.691:10.9.3.5 
decode_length(Bin,{_,_Lb,_Ub}) -> % Unconstrained or large Ub NOTE! this case does not cover case when Ub > 65535
    case Bin of
	<<0:1,Val:7,Rest/bitstring>> -> 
	    {Val,Rest};
	_ ->
	    case align(Bin) of
		<<2:2,Val:14,Rest/binary>> -> 
		    {Val,Rest};
		<<3:2,_:14,_Rest/binary>> -> 
		    exit({error,{asn1,{decode_length,{nyi,length_above_64K}}}})
	    end
    end;
decode_length(Buffer,SingleValue) when integer(SingleValue) ->
    {SingleValue,Buffer}.


						% X.691:11
decode_boolean(Buffer) -> %when record(Buffer,buffer)
    case getbit(Buffer) of
	{1,Remain} -> {true,Remain};
	{0,Remain} -> {false,Remain}
    end.


%% ENUMERATED with extension marker
decode_enumerated(Buffer,C,{Ntup1,Ntup2}) when tuple(Ntup1), tuple(Ntup2) ->
    {Ext,Buffer2} = getext(Buffer),
    case Ext of
	0 -> % not an extension value
	    {Val,Buffer3} = decode_integer(Buffer2,C),
	    case catch (element(Val+1,Ntup1)) of
		NewVal when atom(NewVal) -> {NewVal,Buffer3};
		_Error -> exit({error,{asn1,{decode_enumerated,{Val,[Ntup1,Ntup2]}}}})
	    end;
	1 -> % this an extension value
	    {Val,Buffer3} = decode_small_number(Buffer2),
	    case catch (element(Val+1,Ntup2)) of
		NewVal when atom(NewVal) -> {NewVal,Buffer3};
		_ -> {{asn1_enum,Val},Buffer3}
	    end
    end;

decode_enumerated(Buffer,C,NamedNumberTup) when tuple(NamedNumberTup) ->
    {Val,Buffer2} = decode_integer(Buffer,C),
    case catch (element(Val+1,NamedNumberTup)) of
	NewVal when atom(NewVal) -> {NewVal,Buffer2};
	_Error -> exit({error,{asn1,{decode_enumerated,{Val,NamedNumberTup}}}})
    end.

%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Bitstring value, ITU_T X.690 Chapter 8.5
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% encode bitstring value
%%===============================================================================



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bitstring NamedBitList
%% Val can be  of:
%% - [identifiers] where only named identifers are set to one, 
%%   the Constraint must then have some information of the 
%%   bitlength.
%% - [list of ones and zeroes] all bits 
%% - integer value representing the bitlist
%% C is constraint Len, only valid when identifiers


%% when the value is a list of {Unused,BinBits}, where 
%% Unused = integer(),
%% BinBits = binary().

encode_bit_string(C,Bin={Unused,BinBits},NamedBitList) when integer(Unused),
							    binary(BinBits) ->
    encode_bin_bit_string(C,Bin,NamedBitList);

%% when the value is a list of named bits

encode_bit_string(C, LoNB=[FirstVal | _RestVal], NamedBitList) when atom(FirstVal) ->
    ToSetPos = get_all_bitposes(LoNB, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);% consider the constraint

encode_bit_string(C, BL=[{bit,_} | _RestVal], NamedBitList) ->
    ToSetPos = get_all_bitposes(BL, NamedBitList, []),
    BitList = make_and_set_list(ToSetPos,0),
    encode_bit_string(C,BitList,NamedBitList);

%% when the value is a list of ones and zeroes
encode_bit_string(Int, BitListValue, _) 
  when list(BitListValue),integer(Int),Int =< 16 ->
    %% The type is constrained by a single value size constraint
    bit_list2bitstr(Int,BitListValue);
encode_bit_string(Int, BitListValue, _) 
  when list(BitListValue),integer(Int) ->
    %% The type is constrained by a single value size constraint
    [align,bit_list2bitstr(Int,BitListValue)];
encode_bit_string(no, BitListValue,[]) 
  when list(BitListValue) ->
    Len = length(BitListValue),
    [align,encode_length(undefined,Len),bit_list2bitstr(Len,BitListValue)];
encode_bit_string(C, BitListValue,[]) 
  when list(BitListValue) ->
    Len = length(BitListValue),
    [encode_length(C,Len),align,bit_list2bitstr(Len,BitListValue)];
encode_bit_string(no, BitListValue,_NamedBitList) 
  when list(BitListValue) ->
    %% this case with an unconstrained BIT STRING can be made more efficient
    %% if the complete driver can take a special code so the length field
    %% is encoded there.
    NewBitLVal = lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,
					    lists:reverse(BitListValue))),
    Len = length(NewBitLVal),
    [encode_length(undefined,Len),align,bit_list2bitstr(Len,NewBitLVal)];
encode_bit_string(C,BitListValue,_NamedBitList) 
  when list(BitListValue) ->% C = {_,'MAX'}
    NewBitStr = bitstr_trailing_zeros(BitListValue,C),
    [encode_length(C,bitsize(NewBitStr)),
     align,NewBitStr]; 


%% when the value is an integer
encode_bit_string(C, IntegerVal, NamedBitList) when integer(IntegerVal)->
    BitList = int_to_bitlist(IntegerVal),
    encode_bit_string(C,BitList,NamedBitList);

%% when the value is a tuple
encode_bit_string(C,{Name,Val}, NamedBitList) when atom(Name) ->
    encode_bit_string(C,Val,NamedBitList).

bit_list2bitstr(Len,BitListValue) ->
    case length(BitListValue) of
	Len ->
	    << <<B:1>> ||B <- BitListValue>>;
	L when L > Len -> % truncate
	    << << <<B:1>> ||B <- BitListValue>> :Len/bitstring>>;
	L -> % Len > L -> pad
	    << << <<B:1>> ||B <- BitListValue>>/bitstring ,0:(Len-L)>>
	end.

adjust_trailing_zeros(Len,Bin) when Len == size(Bin) ->
    Bin;
adjust_trailing_zeros(Len,Bin) when Len > bitsize(Bin) ->
    <<Bin/bitstring,0:(Len-bitsize(Bin))>>;
adjust_trailing_zeros(Len,Bin) ->
    <<Bin:Len/bitstring>>.

bitstr_trailing_zeros(BitList,C) when integer(C) ->
    bitstr_trailing_zeros1(BitList,C,C);
bitstr_trailing_zeros(BitList,{Lb,Ub}) when integer(Lb) ->
    bitstr_trailing_zeros1(BitList,Lb,Ub);
bitstr_trailing_zeros(BitList,{{Lb,Ub},_}) when integer(Lb) ->
    bitstr_trailing_zeros1(BitList,Lb,Ub);
bitstr_trailing_zeros(BitList,_) ->
    bit_list2bitstr(length(BitList),BitList).

bitstr_trailing_zeros1(BitList,Lb,Ub) ->
    case length(BitList) of
	Lb -> bit_list2bitstr(Lb,BitList);
	B when B<Lb -> bit_list2bitstr(Lb,BitList);
	D -> F = fun(L,LB,LB,_,_)->bit_list2bitstr(LB,lists:reverse(L));
		    ([0|R],L1,LB,UB,Fun)->Fun(R,L1-1,LB,UB,Fun);
		    (L,L1,_,UB,_)when L1 =< UB -> 
			 bit_list2bitstr(L1,lists:reverse(L));
		    (_,_L1,_,_,_) ->exit({error,{list_length_BIT_STRING,
						 BitList}}) end,
	     F(lists:reverse(BitList),D,Lb,Ub,F)
    end.

%% encode_bin_bit_string/3, when value is a tuple of Unused and BinBits.
%% Unused = integer(),i.e. number unused bits in least sign. byte of
%% BinBits = binary().
encode_bin_bit_string(C,{_,BinBits},_NamedBitList)
  when integer(C),C=<16 ->
    adjust_trailing_zeros(C,BinBits);
encode_bin_bit_string(C,{_Unused,BinBits},_NamedBitList)
  when integer(C) ->
    [align,adjust_trailing_zeros(C,BinBits)];
encode_bin_bit_string(C,UnusedAndBin={_,_},NamedBitList) ->
    %% removes all trailing bits if NamedBitList is not empty
    BitStr = remove_trailing_bin(NamedBitList,UnusedAndBin),
    case C of
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    [encode_length({Lb,Ub},bitsize(BitStr)),align,BitStr];
	no ->
	    [encode_length(undefined,bitsize(BitStr)),align,BitStr];
	Sc -> 
	    [encode_length(Sc,bitsize(BitStr)),align,BitStr]
    end.

remove_trailing_bin([], {Unused,Bin}) ->
    BS = bitsize(Bin)-Unused,
    <<BitStr:BS/bitstring,_:Unused>> = Bin,
    BitStr;
remove_trailing_bin(_NamedNumberList,{_Unused,<<>>}) ->
    <<>>;
remove_trailing_bin(NamedNumberList, {_Unused,Bin}) ->
    Size = size(Bin)-1,
    <<Bfront:Size/binary, LastByte:8>> = Bin,

    %% clear the Unused bits to be sure
    Unused1 = trailingZeroesInNibble(LastByte band 15),
    Unused2 = 
	case Unused1 of 
	    4 ->
		4 + trailingZeroesInNibble(LastByte bsr 4);
	    _ -> Unused1
	end,
    case Unused2 of
	8 ->
	    remove_trailing_bin(NamedNumberList,{0,Bfront});
	_ ->
	    BS = bitsize(Bin) - Unused2,
	    <<BitStr:BS/bitstring,_:Unused2>> = Bin,
	    BitStr
    end.


trailingZeroesInNibble(0) ->
    4;
trailingZeroesInNibble(1) ->
    0;
trailingZeroesInNibble(2) ->
    1;
trailingZeroesInNibble(3) ->
    0;
trailingZeroesInNibble(4) ->
    2;
trailingZeroesInNibble(5) ->
    0;
trailingZeroesInNibble(6) ->
    1;
trailingZeroesInNibble(7) ->
    0;
trailingZeroesInNibble(8) ->
    3;
trailingZeroesInNibble(9) ->
    0;
trailingZeroesInNibble(10) ->
    1;
trailingZeroesInNibble(11) ->
    0;
trailingZeroesInNibble(12) -> %#1100
    2;
trailingZeroesInNibble(13) ->
    0;
trailingZeroesInNibble(14) ->
    1;
trailingZeroesInNibble(15) ->
    0.

%%%%%%%%%%%%%%%
%% The result is presented as a list of named bits (if possible)
%% else as a tuple {Unused,Bits}. Unused is the number of unused
%% bits, least significant bits in the last byte of Bits. Bits is
%% the BIT STRING represented as a binary.
%% 
decode_compact_bit_string(Buffer, C, NamedNumberList) ->
    case get_constraint(C,'SizeConstraint') of
	0 -> % fixed length
	    {{8,0},Buffer};
	V when integer(V),V=<16 -> %fixed length 16 bits or less
	    compact_bit_string(Buffer,V,NamedNumberList);
	V when integer(V),V=<65536 -> %fixed length > 16 bits
	    Bytes2 = align(Buffer),
	    compact_bit_string(Bytes2,V,NamedNumberList);
	V when integer(V) -> % V > 65536 => fragmented value
	    {BitStr,Buffer2} = decode_fragmented_bits(Buffer,V),
	    case bitsize(BitStr) band 7 of
		0 -> {{0,BitStr},Buffer2};
		N -> {{8-N,<<BitStr/bitstring,0:(8-N)>>},Buffer2}
	    end;
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    %% This case may demand decoding of fragmented length/value
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    Bytes3 = align(Bytes2),
	    compact_bit_string(Bytes3,Len,NamedNumberList);
	no ->
	    %% This case may demand decoding of fragmented length/value
	    {Len,Bytes2} = decode_length(Buffer,undefined),
	    Bytes3 = align(Bytes2),
	    compact_bit_string(Bytes3,Len,NamedNumberList);
	Sc ->
	    {Len,Bytes2} = decode_length(Buffer,Sc),
	    Bytes3 = align(Bytes2),
	    compact_bit_string(Bytes3,Len,NamedNumberList)
    end.


%%%%%%%%%%%%%%%
%% The result is presented as a list of named bits (if possible)
%% else as a list of 0 and 1.
%% 
decode_bit_string(Buffer, C, NamedNumberList) ->
    case get_constraint(C,'SizeConstraint') of
	{Lb,Ub} when integer(Lb),integer(Ub) ->
	    {Len,Bytes2} = decode_length(Buffer,{Lb,Ub}),
	    Bytes3 = align(Bytes2),
	    bit_list_or_named(Bytes3,Len,NamedNumberList);
	no ->
	    {Len,Bytes2} = decode_length(Buffer,undefined),
	    Bytes3 = align(Bytes2),
	    bit_list_or_named(Bytes3,Len,NamedNumberList);
	0 -> % fixed length
	    {[],Buffer}; % nothing to encode
	V when integer(V),V=<16 -> % fixed length 16 bits or less
	    bit_list_or_named(Buffer,V,NamedNumberList);
	V when integer(V),V=<65536 ->
	    Bytes2 = align(Buffer),
	    bit_list_or_named(Bytes2,V,NamedNumberList);
	V when integer(V) ->
	    Bytes2 = align(Buffer),
	    {BinBits,_Bytes3} = decode_fragmented_bits(Bytes2,V),
	    bit_list_or_named(BinBits,V,NamedNumberList);
	Sc -> % extension marker
	    {Len,Bytes2} = decode_length(Buffer,Sc),
	    Bytes3 = align(Bytes2),
	    bit_list_or_named(Bytes3,Len,NamedNumberList)
    end.


%% if no named bits are declared we will return a
%% {Unused,Bits}. Unused = integer(),
%% Bits = binary().
compact_bit_string(Buffer,Len,[]) ->
    {BitStr,Rest} = getbits_as_binary(Len,Buffer), % {{Unused,BinBits},NewBuffer}
    PadLen = (8 - (bitsize(BitStr) rem 8)) rem 8,
    {{PadLen,<<BitStr/bitstring,0:PadLen>>},Rest};
compact_bit_string(Buffer,Len,NamedNumberList) ->
    bit_list_or_named(Buffer,Len,NamedNumberList).


%% if no named bits are declared we will return a
%% BitList = [0 | 1]

bit_list_or_named(Buffer,Len,[]) ->
    getbits_as_list(Len,Buffer);

%% if there are named bits declared we will return a named
%% BitList where the names are atoms and unnamed bits represented
%% as {bit,Pos}
%% BitList = [atom() | {bit,Pos}]
%% Pos = integer()

bit_list_or_named(Buffer,Len,NamedNumberList) ->
    {BitList,Rest} = getbits_as_list(Len,Buffer),
    {bit_list_or_named1(0,BitList,NamedNumberList,[]), Rest}.

bit_list_or_named1(Pos,[0|Bt],Names,Acc) ->
    bit_list_or_named1(Pos+1,Bt,Names,Acc);
bit_list_or_named1(Pos,[1|Bt],Names,Acc) ->
    case lists:keysearch(Pos,2,Names) of
	{value,{Name,_}} ->
	    bit_list_or_named1(Pos+1,Bt,Names,[Name|Acc]);
	_  -> 
	    bit_list_or_named1(Pos+1,Bt,Names,[{bit,Pos}|Acc])
    end;
bit_list_or_named1(_Pos,[],_Names,Acc) ->
    lists:reverse(Acc).



%%%%%%%%%%%%%%%
%% 

int_to_bitlist(Int) when integer(Int), Int > 0 ->
    [Int band 1 | int_to_bitlist(Int bsr 1)];
int_to_bitlist(0) ->
    [].


%%%%%%%%%%%%%%%%%%
%% get_all_bitposes([list of named bits to set], named_bit_db, []) ->
%%   [sorted_list_of_bitpositions_to_set]

get_all_bitposes([{bit,ValPos}|Rest], NamedBitList, Ack) ->
    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack ]);

get_all_bitposes([Val | Rest], NamedBitList, Ack) ->
    case lists:keysearch(Val, 1, NamedBitList) of
	{value, {_ValName, ValPos}} ->
	    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]);
	_ ->
	    exit({error,{asn1, {bitstring_namedbit, Val}}})
    end;
get_all_bitposes([], _NamedBitList, Ack) ->
    lists:sort(Ack).

%%%%%%%%%%%%%%%%%%
%% make_and_set_list([list of positions to set to 1])->
%% returns list with all in SetPos set.
%% in positioning in list the first element is 0, the second 1 etc.., but
%% 

make_and_set_list([XPos|SetPos], XPos) ->
    [1 | make_and_set_list(SetPos, XPos + 1)];
make_and_set_list([Pos|SetPos], XPos) ->
    [0 | make_and_set_list([Pos | SetPos], XPos + 1)];
make_and_set_list([], _) ->
    [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% X.691:16
%% encode_octet_string(Constraint,ExtensionMarker,Val)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_octet_string(C,Val) ->
    encode_octet_string(C,false,Val).

encode_octet_string(C,Bool,{_Name,Val}) ->
    encode_octet_string(C,Bool,Val);
encode_octet_string(_C,true,_Val) -> 
    exit({error,{asn1,{'not_supported',extensionmarker}}});
encode_octet_string(SZ={_,_},false,Val) ->
    [encode_length(SZ,length(Val)),align,list_to_binary(Val)];
encode_octet_string(SZ,false,Val) when list(SZ) ->
    [encode_length({hd(SZ),lists:max(SZ)},length(Val)),align,list_to_binary(Val)];
encode_octet_string(no,false,Val) ->
    [encode_length(undefined,length(Val)),align,list_to_binary(Val)];
encode_octet_string(C,_,_) ->
    exit({error,{not_implemented,C}}).


decode_octet_string(Bytes,Range) ->
    decode_octet_string(Bytes,Range,false).

decode_octet_string(<<B1,Bytes/bitstring>>,1,false) ->
%%    {B1,Bytes2} = getbits(Bytes,8),
    {[B1],Bytes};
decode_octet_string(<<B1,B2,Bytes/bitstring>>,2,false) ->
%%    {Bs,Bytes2}= getbits(Bytes,16),
%%    {binary_to_list(<<Bs:16>>),Bytes2};
    {[B1,B2],Bytes};
decode_octet_string(Bytes,Sv,false) when integer(Sv),Sv=<65535 ->
    %%    Bytes2 = align(Bytes),
    %% getoctets_as_list aligns buffer before it picks octets
    getoctets_as_list(Bytes,Sv);
decode_octet_string(Bytes,Sv,false) when integer(Sv) ->
    Bytes2 = align(Bytes),
    decode_fragmented_octets(Bytes2,Sv);
decode_octet_string(Bytes,{Lb,Ub},false) ->
    {Len,Bytes2} = decode_length(Bytes,{Lb,Ub}),
%%    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes2,Len);
decode_octet_string(Bytes,Sv,false) when list(Sv) ->
    {Len,Bytes2} = decode_length(Bytes,{hd(Sv),lists:max(Sv)}),
%%    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes2,Len);
decode_octet_string(Bytes,no,false) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
%%    Bytes3 = align(Bytes2),
    getoctets_as_list(Bytes2,Len).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Restricted char string types 
%% (NumericString, PrintableString,VisibleString,IA5String,BMPString,UniversalString)
%% X.691:26 and X.680:34-36
%%encode_restricted_string(aligned,'BMPString',Constraints,Extension,Val)


encode_restricted_string(aligned,{Name,Val}) when atom(Name) ->
    encode_restricted_string(aligned,Val);

encode_restricted_string(aligned,Val) when list(Val)->
    [encode_length(undefined,length(Val)),list_to_binary(Val)].


encode_known_multiplier_string(StringType,SizeC,NumBits,CharOutTab,{Name,Val}) when atom(Name) ->
    encode_known_multiplier_string(StringType,SizeC,NumBits,CharOutTab,Val);
encode_known_multiplier_string(_StringType,SizeC,NumBits,CharOutTab,Val) ->
    Result = chars_encode2(Val,NumBits,CharOutTab),
    case SizeC of
	Ub when integer(Ub), Ub*NumBits =< 16  ->
	    Result;
	Ub when integer(Ub),Ub =<65535 -> % fixed length
	    [align,Result];
	{Ub,Lb} ->
	    [encode_length({Ub,Lb},length(Val)),align,Result];
	no  ->
	    [encode_length(undefined,length(Val)),align,Result]
    end.

decode_restricted_string(Bytes,aligned) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    getoctets_as_list(Bytes2,Len).

decode_known_multiplier_string(StringType,SizeC,NumBits,CharInTab,Bytes) ->
    case SizeC of
	Ub when integer(Ub), Ub*NumBits =< 16  ->
	    chars_decode(Bytes,NumBits,StringType,CharInTab,Ub);
	Ub when integer(Ub),Ub =<65535 -> % fixed length
	    Bytes1 = align(Bytes),
	    chars_decode(Bytes1,NumBits,StringType,CharInTab,Ub);
	Vl when list(Vl) ->
	    {Len,Bytes1} = decode_length(Bytes,{hd(Vl),lists:max(Vl)}),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,CharInTab,Len);
	no  ->
	    {Len,Bytes1} = decode_length(Bytes,undefined),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,CharInTab,Len);
	{Lb,Ub}->
	    {Len,Bytes1} = decode_length(Bytes,{Lb,Ub}),
	    Bytes2 = align(Bytes1),
	    chars_decode(Bytes2,NumBits,StringType,CharInTab,Len)
    end.

encode_GeneralString(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_GeneralString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).

encode_GraphicString(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_GraphicString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).

encode_ObjectDescriptor(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_ObjectDescriptor(Bytes) ->
    decode_restricted_string(Bytes,aligned).

encode_TeletexString(_C,Val) -> % equivalent with T61String
    encode_restricted_string(aligned,Val).
decode_TeletexString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).

encode_VideotexString(_C,Val) ->
    encode_restricted_string(aligned,Val).
decode_VideotexString(Bytes,_C) ->
    decode_restricted_string(Bytes,aligned).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getBMPChars(Bytes,Len) ->{BMPcharList,RemainingBytes}
%%
getBMPChars(<<T/binary>>, 0, Acc) ->
    {lists:reverse(Acc),T};
getBMPChars(<<0,O2,Bytes1/bitstring>>, Len, Acc) ->
    getBMPChars(Bytes1,Len-1,[O2|Acc]);
getBMPChars(<<O1,O2,Bytes1/bitstring>>, Len, Acc) ->
    getBMPChars(Bytes1,Len-1,[{0,0,O1,O2}|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% chars_encode(C,StringType,Value) -> ValueList
%%
%% encodes chars according to the per rules taking the constraint PermittedAlphabet 
%% into account.
%% This function does only encode the value part and NOT the length

chars_encode2(Chars,NumBits,T1) ->
    Enc =  fun(Ch,{Min,Max,notab}) when Ch =< Max, Ch >= Min ->
		   Ch - Min;
	      (Ch,{Min,Max,Tab}) when Ch =< Max, Ch >= Min ->
		   exit_if_false(Ch,element(Ch-Min+1,Tab));
	      ({A,B,C,D},{Min,_Max,notab}) ->
		   ((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min;
	      (Ch = {A,B,C,D},{Min,_Max,Tab}) ->
		   exit_if_false(Ch,element(((((((A bsl 8)+B) bsl 8)+C) bsl 8)+D)-Min,Tab));
	      (Ch,_) ->
		   exit({error,{asn1,{illegal_char_value,Ch}}})
	   end,
    << <<(Enc(C,T1)):NumBits>> ||C <- Chars>>.


exit_if_false(V,false)->
    exit({error,{asn1,{"illegal value according to Permitted alphabet constraint",V}}});
exit_if_false(_,V) ->V.

pre_complete_bits(NumBits,Val) when NumBits =< 8 ->
    <<Val:NumBits>>;
pre_complete_bits(NumBits,Val) when NumBits =< 16 ->
    <<Val:NumBits>>;
pre_complete_bits(NumBits,Val) when NumBits =< 2040 -> % 255 * 8
    <<Val:NumBits>>.


chars_decode(Bytes,_,'BMPString',_,Len) ->
    getBMPChars(Bytes,Len,[]);
chars_decode(Bytes,NumBits,_StringType,CharInTab,Len) ->	
    chars_decode2(Bytes,CharInTab,NumBits,Len).


chars_decode2(Bytes,CharInTab,NumBits,Len) ->
    chars_decode2(Bytes,CharInTab,NumBits,Len,[]).

chars_decode2(Bytes,_CharInTab,_NumBits,0,Acc) ->
    {lists:reverse(Acc),Bytes};
chars_decode2(Bytes,{Min,Max,notab},NumBits,Len,Acc) when NumBits > 8 ->
    {Char,Bytes2} = getbits(Bytes,NumBits),
    Result = 
	if
	    Char < 256 -> Char;
	    true ->
		list_to_tuple(binary_to_list(<<Char:32>>))
	end,
    chars_decode2(Bytes2,{Min,Max,notab},NumBits,Len -1,[Result|Acc]);
chars_decode2(Bytes,{Min,Max,notab},NumBits,Len,Acc) ->
    {Char,Bytes2} = getbits(Bytes,NumBits),
    chars_decode2(Bytes2,{Min,Max,notab},NumBits,Len -1,[Char+Min|Acc]);

%% BMPString and UniversalString with PermittedAlphabet is currently not supported
chars_decode2(Bytes,{Min,Max,CharInTab},NumBits,Len,Acc) ->
    {Char,Bytes2} = getbits(Bytes,NumBits),
    chars_decode2(Bytes2,{Min,Max,CharInTab},NumBits,Len -1,[element(Char+1,CharInTab)|Acc]).


						% X.691:17 
encode_null(_Val) -> []. % encodes to nothing
%encode_null({Name,Val}) when atom(Name) ->
%    encode_null(Val).

decode_null(Bytes) ->
    {'NULL',Bytes}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_UTF8String(Val) -> CompleteList
%% Val -> <<utf8encoded binary>>
%% CompleteList -> [apropriate codes and values for driver complete]
%%
encode_UTF8String(Val) when binary(Val) ->
    [encode_length(undefined,size(Val)),align,Val];
encode_UTF8String(Val) ->
    encode_UTF8String(list_to_binary(Val)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_UTF8String(Bytes) -> {Utf8Binary,RemainingBytes}
%% Utf8Binary -> <<utf8 encoded binary>>
%% RemainingBytes -> <<buffer>>
decode_UTF8String(Bytes) -> 
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {_Bin,_Bytes3} = getoctets_as_bin(Bytes2,Len).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_object_identifier(Val) -> CompleteList
%% encode_object_identifier({Name,Val}) -> CompleteList
%% Val -> {Int1,Int2,...,IntN} % N >= 2
%% Name -> atom()
%% Int1 -> integer(0..2)
%% Int2 -> integer(0..39) when Int1 (0..1) else integer()
%% Int3-N -> integer()
%% CompleteList -> [{bits,8,Val}|{octets,Ol}|align|...]
%%
encode_object_identifier({Name,Val}) when atom(Name) ->
    encode_object_identifier(Val);
encode_object_identifier(Val) ->
    OctetList = e_object_identifier(Val),
    Octets = list_to_binary(OctetList), % performs a flatten at the same time
    [encode_length(undefined,size(Octets)),align,Octets].


%% This code is copied from asn1_encode.erl (BER) and corrected and modified 

e_object_identifier({'OBJECT IDENTIFIER',V}) ->
    e_object_identifier(V);
e_object_identifier({Cname,V}) when atom(Cname),tuple(V) ->
    e_object_identifier(tuple_to_list(V));
e_object_identifier({Cname,V}) when atom(Cname),list(V) ->
    e_object_identifier(V);
e_object_identifier(V) when tuple(V) ->
    e_object_identifier(tuple_to_list(V));

%% E1 = 0|1|2 and (E2 < 40 when E1 = 0|1) 
e_object_identifier([E1,E2|Tail]) when E1 >= 0, E1 < 2, E2 < 40 ; E1==2 ->
    Head = 40*E1 + E2,  % weird
    e_object_elements([Head|Tail],[]);
e_object_identifier(Oid=[_,_|_Tail]) ->
    exit({error,{asn1,{'illegal_value',Oid}}}).

e_object_elements([],Acc) ->
    lists:reverse(Acc);
e_object_elements([H|T],Acc) ->
    e_object_elements(T,[e_object_element(H)|Acc]).

e_object_element(Num) when Num < 128 ->
    Num;
%% must be changed to handle more than 2 octets
e_object_element(Num) ->  %% when Num < ???
    Left = ((Num band 2#11111110000000) bsr 7) bor 2#10000000,
    Right = Num band 2#1111111 ,
    [Left,Right].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_object_identifier(Bytes) -> {ObjId,RemainingBytes}
%% ObjId -> {integer(),integer(),...} % at least 2 integers
%% RemainingBytes -> [integer()] when integer() (0..255)
decode_object_identifier(Bytes) ->
    {Len,Bytes2} = decode_length(Bytes,undefined),
    {Octs,Bytes3} = getoctets_as_list(Bytes2,Len),
    [First|Rest] = dec_subidentifiers(Octs,0,[]),
    Idlist = if
		 First < 40 ->
		     [0,First|Rest];
		 First < 80 ->
		     [1,First - 40|Rest];
		 true ->
		     [2,First - 80|Rest]
	     end,
    {list_to_tuple(Idlist),Bytes3}.

dec_subidentifiers([H|T],Av,Al) when H >=16#80 ->
    dec_subidentifiers(T,(Av bsl 7) + (H band 16#7F),Al);
dec_subidentifiers([H|T],Av,Al) ->
    dec_subidentifiers(T,0,[(Av bsl 7) + H |Al]);
dec_subidentifiers([],_Av,Al) ->
    lists:reverse(Al).

get_constraint([{Key,V}],Key) ->
    V;
get_constraint([],_) ->
    no;
get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	    no;
	{value,{_,V}} -> 
	    V
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% complete(InList) -> ByteList
%% Takes a coded list with bits and bytes and converts it to a list of bytes
%% Should be applied as the last step at encode of a complete ASN.1 type
%%

%%  complete(L) ->
%%      case complete1(L) of
%%  	{[],0} ->
%%  	    <<0>>;
%%  	{Acc,0} ->
%%  	    erlang:list_to_bitstr(Acc);
%%  	{Acc,PadLen}  ->
%%  	    erlang:list_to_bitstr([Acc|[<<0:PadLen>>]])
%%      end.


%% % this function builds the ugly form of lists [E1|E2] to avoid having to reverse it at the end.
%% % this is done because it is efficient and that the result always will be sent on a port or
%% % converted by means of list_to_binary/1
%% complete1(InList) when is_list(InList) ->
%%     complete1(InList,[],0);
%% complete1(InList) when is_binary(InList) ->
%%     {[InList],0};
%% complete1(InList) when is_bitstr(InList) ->
%%     PadLen = 8 - (bitsize(InList) rem 8),
%%     {[<<InList/bitstring,0:PadLen>>],0}.

%% complete1([],Acc,PadLen) ->
%%     {Acc,PadLen};
%% complete1([H|T],Acc,PadLen) when is_list(H) ->
%%     {NewAcc,PadLen2} = complete1(H,Acc,PadLen),
%%     complete1(T,NewAcc,PadLen2);

%% complete1([Bin|T],Acc,PadLen) when is_binary(Bin) ->
%%     complete1(T,[Acc,Bin],PadLen);

%% complete1([Bin|T],Acc,PadLen) when is_bitstr(Bin) ->
%%     complete1(T,[Acc,Bin],(8-(((8 - PadLen) + bitsize(Bin)) rem 8)) rem 8);

%% complete1([align|T],Acc,0) ->
%%     complete1(T,Acc,0);
%% complete1([align|T],Acc,PadLen) ->
%%     complete1(T,[Acc,<<0:PadLen>>],0).


complete(InList) when is_list(InList) ->
    case complete1(InList, <<>>) of
	<<>> ->
	    <<0>>;
	Res ->
	    case bitsize(Res) band 7 of
		0 -> Res;
		Bits -> <<Res/bitstring,0:(8-Bits)>>
	end
    end;
complete(InList) when is_binary(InList) ->
    InList;
complete(InList) when is_bitstr(InList) ->
    PadLen = 8 - (bitsize(InList) band 7),
    <<InList/bitstring,0:PadLen>>.

complete1([align|T], Acc) ->
    case bitsize(Acc) band 7 of
	0 -> complete1(T, Acc);
	Bits -> complete1(T, <<Acc/bitstring,0:(8-Bits)>>)
    end;
complete1([H|T], Acc) when is_list(H) ->
    NewAcc = complete1(H, Acc),
    complete1(T, NewAcc);
complete1([Bin|T], Acc) ->
    complete1(T, <<Acc/bitstring,Bin/bitstring>>);
complete1([], Acc) -> Acc.
