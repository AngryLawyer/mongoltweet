-module(tweetgrabber).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, fetch/1, fetch/2]).

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

fetch(Screen_name) ->
    gen_server:call(?MODULE, {fetch, Screen_name}).

fetch(Screen_name, Since_tweet) ->
    gen_server:call(?MODULE, {fetch, Screen_name, Since_tweet}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({fetch, Screen_name}, _From, State) ->
    {Consumer_key, Access_token, Consumer_secret, Access_token_secret} = unpack_permissions(State),
    Data = request(Consumer_key, Access_token, Consumer_secret, Access_token_secret, Screen_name),
    Munged = [shrink_tweet(X) || X <- Data],
    {reply, Munged, State};
handle_call({fetch, Screen_name, Since_tweet}, _From, State) ->
    {Consumer_key, Access_token, Consumer_secret, Access_token_secret} = unpack_permissions(State),
    Data = request(Consumer_key, Access_token, Consumer_secret, Access_token_secret, Screen_name, Since_tweet),
    Munged = [shrink_tweet(X) || X <- Data],
    {reply, Munged, State};
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

request(Consumer_key, Access_token, Consumer_secret, Access_token_secret, Screen_name) ->
    request_with_params(Consumer_key, Access_token, Consumer_secret, Access_token_secret, [{screen_name, Screen_name}]).

request(Consumer_key, Access_token, Consumer_secret, Access_token_secret, Screen_name, Since_tweet) ->
    request_with_params(Consumer_key, Access_token, Consumer_secret, Access_token_secret, [{screen_name, Screen_name}, {since_id, Since_tweet}]).

request_with_params(Consumer_key, Access_token, Consumer_secret, Access_token_secret, Params) -> 
    Url = "https://api.twitter.com/1.1/statuses/user_timeline.json",
    Consumer = {Consumer_key, Consumer_secret, hmac_sha1},
    {ok, Response} = oauth:get(Url, Params, Consumer, Access_token, Access_token_secret),
    get_response_data(Response).

unpack_permissions(State) -> 
    {proplists:get_value(consumer_key, State), proplists:get_value(access_token, State), proplists:get_value(consumer_secret, State), proplists:get_value(access_token_secret, State)}.

get_response_data({_, _, Data}) ->
    mochijson2:decode(Data).

shrink_tweet({struct, Proplist}) ->
    {proplists:get_value(<<"id">>, Proplist), proplists:get_value(<<"text">>, Proplist)}.

-ifdef(TEST).

-endif.
