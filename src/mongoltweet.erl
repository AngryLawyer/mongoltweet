-module(mongoltweet).
-export([start/0, stop/0]).

start() ->
    application:start(mongoltweet).

stop() ->
    application:stop(mongoltweet).
