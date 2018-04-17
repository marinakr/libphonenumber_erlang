-module(libphonenumber_erlang_app).
-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

start() ->
	start(normal, []).

start(_Type, _Args) ->
	libphonenumber_erlang_sup:start_link().

stop(_State) ->
	ok.
