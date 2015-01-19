%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% ------------------------------------------------------------
%% A simple demo showing a ball
%% bouncing in a window.
%% ------------------------------------------------------------

-module(ball).

-export([start/0,init/0]).

start() ->
    spawn(ball,init,[]).

init() ->
    I= gs:start(),
    W= gs:window(I,[{title,"Ball"},{width,300},{height,300},{map,true}]),
    C= gs:canvas(W,[{width,300},{height,300},{bg,yellow}]),
    gs:button(W,[{label, {text,"Quit Demo"}},{x,100}]),
    Ball = gs:oval(C,[{coords,[{0,0},{50,50}]},{fill,red}]),
    ball(Ball,0,0,5.5,4.1).

ball(Ball,X,Y,DX,DY) ->
    {NX,NDX} = cc(X,DX),
    {NY,NDY} = cc(Y,DY),
    gs:config(Ball,{move,{DX,DY}}),    
    receive
	{gs,_,click,_,_} -> exit(normal);
	{gs,_,destroy,_,_} -> exit(normal)
    after 20 ->
	    true
    end,
    ball(Ball,NX,NY,NDX,NDY).

cc(X,DX) ->
    if 
	DX>0 ->
	    if 
		X=<250 ->
		    {X+DX,DX};
		x>250 ->
		    {X-DX,-DX}
	    end;
	DX<0 ->
	    if
		X>=0 ->
		    {X+DX,DX};
		X<0 ->
		    {X-DX,-DX}
	    end
    end.

%% ------------------------------------------------------------
