%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(translate_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {Author, _Time, Tweet} = unpack_tweet(worker:get_unsolved_tweet()),
    %{io_lib:format("<html><body><p>~ts</p><p>~ts</p></body></html>", [Author, <<Tweet/utf8>>]), ReqData, State}.
    {encode_uri_rfc3986:encode(Tweet), ReqData, State}.

unpack_tweet({tweet, _, Author, Time, Tweet, _, _}) ->
    {Author, Time, Tweet};
unpack_tweet(undefined) ->
    {"NONE", 0, "NO TWEETS FOUND"}.
