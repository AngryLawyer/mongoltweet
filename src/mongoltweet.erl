-module(mongoltweet).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(lhttpc),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    application:start(mongoltweet).

stop() ->
    Res = application:stop(mongoltweet),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(lhttpc),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(inets),
    Res.
