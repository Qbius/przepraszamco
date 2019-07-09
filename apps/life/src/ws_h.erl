-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(ws_state, {cells}).

init(Req, _State) ->
	{cowboy_websocket, Req, #ws_state{cells = []}}.

websocket_init(State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{ok, State}.

websocket_handle({text, Msg}, State) ->
	case jiffy:decode(Msg, [return_maps]) of
	    #{<<"width">> := _Width, <<"height">> := _Height, <<"cells">> := Cells} ->
	        Result = conway:conway(Cells),
		io:format("~p~n", [Result]),
		{reply, {text, jiffy:encode(#{<<"cells">> => Result})}, State#ws_state{ cells = Result }};
	    #{<<"get">> := <<"next">>} ->
		Result = conway:conway(State#ws_state.cells),
		{reply, {text, case Result =:= State#ws_state.cells of
		    false -> jiffy:encode(#{<<"cells">> => Result});
                    true -> <<"Stagnation">>
                end}, State#ws_state{ cells = Result }};
	    _ ->
	        {reply, {text, <<"hm">>}, State}
        end;
websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State};
websocket_info(_Info, State) ->
        {ok, State}.
