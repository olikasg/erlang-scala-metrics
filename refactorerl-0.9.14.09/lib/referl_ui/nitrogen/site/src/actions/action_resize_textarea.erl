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

-module (action_resize_textarea).
-compile (export_all).
-include_lib ("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 9568 $ ").


render_action(_Record = #resize_textarea{targetId=TargetId, maxWidth=MaxWidth}) ->
	TargetIdStr=io_lib:format("~p",[TargetId]),
	MaxWidthStr=io_lib:format("~p",[MaxWidth]),
    "var ta = obj('"++TargetIdStr++"')
	var lines = ta.value.split(String.fromCharCode(10));
    var width = "++MaxWidthStr++";
    var height = 1;
    for (var i = 0; i < lines.length; i++) {
        var linelength = lines[i].length;
        if (linelength >= width ) {
            height += Math.abs(Math.ceil(linelength / width));
        }
    }
    height += lines.length;
	ta.cols= width;
    ta.rows = height;".
