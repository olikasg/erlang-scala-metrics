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

%%% @author Viktoria Fordos <v@fviktoria.hu>

%% -*- mode: nitrogen -*-

-module (images).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 9568 $ ").


main() -> 
	case wf:q(image) of
		undefined-> ok;
		Name-> case filename:extension(Name) of
				   ".svg"->wf:content_type("image/svg+xml");
				   ".dot"->wf:content_type("application/octet-stream");
				   _ -> ok
			   end
	end,
    binary_to_list(image_data()).

event(_) -> ok.

image_data() -> 
	case wf:q(image) of
		undefined -> <<>>;
		Name -> case file:read_file(Name) of
					{ok, Binary} -> Binary;
					{error,_}-> <<>>
				end
	end.
