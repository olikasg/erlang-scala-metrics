%%<copyright>
%% <year>2004-2007</year>
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

-define(verify_format_version(VFV_Ver1,VFV_Ver2),
	fun(VFV_V,VFV_V) ->
		ok;
	   (VFV_V1,VFV_V2) when list(VFV_V1), list(VFV_V2) ->
		Toks1 = string:tokens(VFV_V1, [$.]),
		[Major1|_] = case (catch [list_to_integer(I) || I <- Toks1]) of
				Nums when list(Nums) ->
				    Nums;
				_ ->
				    {error, {invalid_version_format, VFV_V1}}
			    end,
		Toks2 = string:tokens(VFV_V2, [$.]),
		case (catch [list_to_integer(I) || I <- Toks2]) of
		    [Major1|_] ->
			ok;
		    [_Major2|_] ->
			{error, wrong_version};
		    _ ->
			{error, {invalid_version_format, VFV_V2}}
		end;
	   (VFV_V1,VFV_V2) ->
		{error, {invalid_format, VFV_V1, VFV_V2}}
	end(VFV_Ver1,VFV_Ver2)).

-define(read_mib(RM_FN),
	RM_Bin = case file:read_file(RM_FN) of
		     {ok, RM_B} ->
			 RM_B;
		     RM_Error ->
			 throw(RM_Error)
	       end,
	RM_Mib = case (catch binary_to_term(RM_Bin)) of
		     RM_M when record(RM_M, mib) ->
			 RM_M;
		     _ ->
			 throw({error, bad_mib_format})
		 end,
	#mib{mib_format_version = RM_V1} = #mib{},
	case RM_Mib of
	    #mib{mib_format_version = RM_V2,
		 misc               = RM_X} when integer(RM_X) ->
		case (catch ?verify_format_version(RM_V1, RM_V2)) of
		    ok ->
			{ok, RM_Mib#mib{misc = []}};
		    _ ->
			throw({error, {wrong_mib_format_version_tag, RM_V2}})
		end;
	    #mib{mib_format_version = RM_V2} ->
		case (catch ?verify_format_version(RM_V1, RM_V2)) of
		    ok ->
			{ok, RM_Mib#mib{misc = []}};
		    _ ->
			throw({error, {wrong_mib_format_version_tag, RM_V2}})
		end
	end).
