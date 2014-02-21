-module(history_resource).
-export([init/1, to_html/2, charsets_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("records.hrl").

init(Params) -> {ok, Params}.


to_html(ReqData, State) ->
    Tweets = tweets(),
    {ok, Content} = history_dtl:render([
        {path, wrq:path(ReqData)},
        {tweets, Tweets}
    ]),
    {unicode:characters_to_binary(Content), ReqData, State}.

charsets_provided(ReqData, State) ->
    {[{"utf8", fun convert/1}], ReqData, State}.

%% Private functions

convert(Body) ->
    Body.

tweets() ->
    tweets_to_proplist(worker:get_solved_tweets(10)).

tweets_to_proplist(Tweets) ->
    tweets_to_proplist(Tweets, []).

tweets_to_proplist([], Out) ->
    lists:reverse(Out);
tweets_to_proplist([Head | Rest], Out) ->
    tweets_to_proplist(Rest, [tweet_to_proplist(Head) | Out]).

tweet_to_proplist(#tweet{user_name_and_timestamp=_, user_name=User_name, timestamp=Timestamp, mongolian=Mongolian, english=English, solved=Solved}) ->
    [{user_name, User_name}, {timestamp, Timestamp}, {mongolian, Mongolian}, {english, English}, {solved, Solved}].
