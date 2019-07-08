-module(life_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
	    {'_', [
	    	{"/", cowboy_static, {priv_file, life, "index.html"}},
		{"/life", cowboy_static, {priv_file, life, "files/grid.html"}},
		{"/websocket", ws_h, []},
		{"/files/[...]", cowboy_static, {priv_dir, life, "files"}},
		{"/images/[...]", cowboy_static, {priv_dir, life, "images"}}
	    ]}
	]),
	{ok, _} = cowboy:start_clear(my_http_listener,
	    [{port, 8080}],
	    #{env => #{dispatch => Dispatch}}
	),
	life_sup:start_link().

stop(_State) ->
	ok.
