-module(tweetgrabber).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("~p~n", [Args]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

encode_consumer_key(Consumer_key, Consumer_secret) ->
    base64:encode_to_string(pair_consumer_key(Consumer_key, Consumer_secret)).

pair_consumer_key(Consumer_key, Consumer_secret) ->
    string:join([Consumer_key, Consumer_secret], ":").

build_oauth_details(Consumer_key, Access_token) ->
    [
        {oauth_consumer_key, Consumer_key},
        {oauth_nonce, erlang:timestamp()}, %FIXME: Should be unix time
        {oauth_signature_method, "HMAC-SHA1"},
        {oath_token, Access_token},
        {oath_timestamp, erlang_timestamp()},
        {oath_version, "1.0"}
    ].
% http://stackoverflow.com/questions/12916539/simplest-php-example-for-retrieving-user-timeline-with-twitter-api-version-1-1
