%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(translate_resource).
-export([init/1, to_json/2, charsets_provided/2, content_types_provided/2, process_post/2, allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Params) -> {ok, Params}.

allowed_methods(ReqData, State) ->
    Methods = case proplists:get_value(posted, State) of
        true -> ['POST', 'HEAD'];
        _ -> ['GET', 'HEAD']
    end,
    {Methods, ReqData, State}.

process_post(ReqData, State) ->
    Info = wrq:path_info(ReqData),
    Query = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    Result = worker:solve_tweet(proplists:get_value(author, Info), list_to_integer(proplists:get_value(timestamp, Info)), proplists:get_value("translation", Query)),
    Body = io_lib:format("{\"result\": \"~p\"}", [Result]),
    {true, wrq:append_to_response_body(Body, ReqData), State}.

to_json(ReqData, State) ->
    Formatted = to_json(unpack_tweet(worker:get_unsolved_tweet())),
    {unicode:characters_to_binary(lists:flatten(Formatted)), ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

charsets_provided(ReqData, State) ->
    {[{"utf8", fun convert/1}], ReqData, State}.

unpack_tweet({tweet, _, Author, Time, Tweet, Translation, _}) ->
    {Author, Time, Tweet, Translation};
unpack_tweet(undefined) ->
    {"NONE", 0, "NO TWEETS FOUND", "NO TWEETS FOUND"}.

convert(Body) ->
    Body.


to_json({Author, Time, Tweet, Translation}) ->
    io_lib:format(
"{"
"   \"author\": \"~ts\","
"   \"time\": \"~p\","
"   \"tweet\": \"~ts\","
"   \"translation\": \"~ts\""
"}", [Author, Time, Tweet, Translation]).
