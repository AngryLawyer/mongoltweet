%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(translate_resource).
-export([init/1, to_html/2, charsets_provided/2, convert/1]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {Author, _Time, Tweet} = unpack_tweet(worker:get_unsolved_tweet()),
    Encoded_tweet = unicode:characters_to_binary(Tweet),
    Formatted = io_lib:format("<html><body><p>~ts</p><p>~ts</p></body></html>", [Author, Encoded_tweet]), %FIXME: Needs to produce a binary
    {Encoded_tweet, ReqData, State}.

charsets_provided(ReqData, State) ->
    {[{"utf8", fun convert/1}], ReqData, State}.

unpack_tweet({tweet, _, Author, Time, Tweet, _, _}) ->
    {Author, Time, Tweet};
unpack_tweet(undefined) ->
    {"NONE", 0, "NO TWEETS FOUND"}.

convert(Body) ->
    Body.
