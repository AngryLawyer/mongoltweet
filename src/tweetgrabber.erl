-module(tweetgrabber).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-compile(export_all).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile([export_all]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).

test_fetch() ->
    gen_server:call(?MODULE, test).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(test, _From, State) ->
    Consumer_key = proplists:get_value(consumer_key, State),
    Access_token = proplists:get_value(access_token, State),
    Consumer_secret = proplists:get_value(consumer_secret, State),
    {reply, request(Consumer_key, Access_token, Consumer_secret), State};
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

request(Consumer_key, Access_token, Consumer_secret) ->
    Oauth_params = build_oauth_details(Consumer_key, Access_token),
    Base_string = build_base_string("https://api-twitter.com/1.1/statuses/user_timeline.json", "GET", Oauth_params),
    Composite_key = build_composite_key(Consumer_secret, Access_token),
    Signature = build_oauth_signature(Base_string, Composite_key),
    Oauth_params_extended = add_oauth_signature(Oauth_params, Signature),
    Oauth_params_extended.

build_oauth_details(Consumer_key, Access_token) ->
    Time = lists:nth(1, io_lib:format("~w", [unix_time()])),
    [
        {oauth_consumer_key, Consumer_key},
        {oauth_nonce, Time},
        {oauth_signature_method, "HMAC-SHA1"},
        {oath_token, Access_token},
        {oath_timestamp, Time},
        {oath_version, "1.0"}
    ].

build_base_string(Uri, Method, Params) ->
    Start = Method ++ "&" ++ Uri ++ "&",
    Start ++ string:join([lists:flatten(io_lib:format("~w=~s", [Key, Value])) || {Key, Value} <- Params], "&").

build_composite_key(Consumer_secret, Access_token) ->
    string:join([Consumer_secret, Access_token], ":").

build_oauth_signature(Base_info, Composite_key) ->
    base64:encode_to_string(crypto:sha_mac(Base_info, Composite_key)).

add_oauth_signature(Details, Oauth_signature) ->
    [{oauth_signature, Oauth_signature} | Details].

build_authorization_header(Oauth_details) ->
    "Authorization: OAuth " ++ string:join([lists:flatten(io_lib:format("~p=\"~p\"", [Key, Value])) || {Key, Value} <- Oauth_details], ", ").

unix_time() ->
    {Megasecs, Secs, _Microsecs} = os:timestamp(),
    Megasecs * 1000000 + Secs.

% http://stackoverflow.com/questions/12916539/simplest-php-example-for-retrieving-user-timeline-with-twitter-api-version-1-1
