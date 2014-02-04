%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(translate_resource).
-export([init/1, to_html/2, charsets_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Formatted = to_json(unpack_tweet(worker:get_unsolved_tweet())),
    {unicode:characters_to_binary(lists:flatten(Formatted)), ReqData, State}.

charsets_provided(ReqData, State) ->
    {[{"utf8", fun convert/1}], ReqData, State}.

unpack_tweet({tweet, _, Author, Time, Tweet, _, _}) ->
    {Author, Time, Tweet};
unpack_tweet(undefined) ->
    {"NONE", 0, "NO TWEETS FOUND"}.

convert(Body) ->
    Body.


to_json({Author, Time, Tweet}) ->
    io_lib:format(
"{"
"   'author': '~ts',"
"   'time': ~i,"
"   'tweet': '~ts'"
"}", [Author, Time, Tweet]).
