-module(tweetgrabber).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    Access_token_secret = proplists:get_value(access_token_secret, State),
    {reply, request(Consumer_key, Access_token, Consumer_secret, Access_token_secret), State};
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

request(Consumer_key, Access_token, Consumer_secret, Access_token_secret) ->
    Url = "https://api.twitter.com/1.1/statuses/user_timeline.json",
    Headers = build_all_headers(Url, Consumer_key, Access_token, Consumer_secret, Access_token_secret),
    lhttpc:request(Url,get,[Headers],infinity). 

build_all_headers(Url, Consumer_key, Access_token, Consumer_secret, Access_token_secret) -> 
    Oauth_params = build_oauth_details(Consumer_key, Access_token, unix_time()),
    Base_string = build_base_string(Url, "GET", Oauth_params),
    Composite_key = build_composite_key(Consumer_secret, Access_token_secret),
    Signature = build_oauth_signature(Base_string, Composite_key),
    Oauth_params_extended = add_oauth_signature(Oauth_params, Signature),
    build_authorization_header(Oauth_params_extended).

build_oauth_details(Consumer_key, Access_token, Timestamp) ->
    Time_string = lists:nth(1, io_lib:format("~w", [Timestamp])),
    [
        {oauth_consumer_key, Consumer_key},
        {oauth_nonce, Time_string},
        {oauth_signature_method, "HMAC-SHA1"},
        {oauth_token, Access_token},
        {oauth_timestamp, Time_string},
        {oauth_version, "1.0"}
    ].

build_base_string(Uri, Method, Params) ->
    Start = Method ++ "&" ++ Uri ++ "&",
    Start ++ string:join([lists:flatten(io_lib:format("~w=~s", [Key, Value])) || {Key, Value} <- Params], "&").

build_composite_key(Consumer_secret, Access_token_secret) ->
    string:join([Consumer_secret, Access_token_secret], ":").

build_oauth_signature(Base_info, Composite_key) ->
    base64:encode_to_string(crypto:sha_mac(Base_info, Composite_key)).

add_oauth_signature(Details, Oauth_signature) ->
    [{oauth_signature, Oauth_signature} | Details].

build_authorization_header(Oauth_details) ->
    {"Authorization", "OAuth " ++ string:join([lists:flatten(io_lib:format("~s=\"~s\"", [Key, Value])) || {Key, Value} <- Oauth_details], ", ")}.

unix_time() ->
    {Megasecs, Secs, _Microsecs} = os:timestamp(),
    Megasecs * 1000000 + Secs.

% http://stackoverflow.com/a/12939923

-ifdef(TEST).

build_oauth_details_test() ->
    Consumer_key = "12345",
    Access_token = "67890",
    Timestamp = 100,
    Details = build_oauth_details(Consumer_key, Access_token, Timestamp),
    % Now actually test
    ?assertEqual(proplists:get_value(oauth_consumer_key, Details), Consumer_key),
    ?assertEqual(proplists:get_value(oauth_nonce, Details), "100"),
    ?assertEqual(proplists:get_value(oauth_signature_method, Details), "HMAC-SHA1"),
    ?assertEqual(proplists:get_value(oauth_token, Details), Access_token),
    ?assertEqual(proplists:get_value(oauth_timestamp, Details), "100"),
    ?assertEqual(proplists:get_value(oauth_version, Details), "1.0").

build_base_string_test() ->
    Consumer_key = "12345",
    Access_token = "67890",
    Timestamp = 100,
    Details = build_oauth_details(Consumer_key, Access_token, Timestamp),

    String = build_base_string("https://api.twitter.com/1.1/statuses/user_timeline.json", "GET", Details),
    ?assertEqual(String, "GET&https%3A%2F%2Fapi.twitter.com%2F1.1%2Fstatuses%2Fuser_timeline.json&oauth_consumer_key%3D12345%26oauth_nonce%3D100%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D100%26oauth_token%3D67890%26oauth_version%3D1.0").

build_composite_key_test() ->
    Consumer_secret = "abcde",
    Access_token_secret = "lololo",
    String = build_composite_key(Consumer_secret, Access_token_secret),
    ?assertEqual(String, "abcde&lolol").

build_oauth_signature_test() ->
    Signature = build_oauth_signature("GET&https%3A%2F%2Fapi.twitter.com%2F1.1%2Fstatuses%2Fuser_timeline.json&oauth_consumer_key%3D12345%26oauth_nonce%3D100%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D100%26oauth_token%3D67890%26oauth_version%3D1.0","abcde&lolol"),
    ?assertEqual(Signature, "VcaKB8r06MhJ7+FHbT/b7jSHy6U=").

-endif.
