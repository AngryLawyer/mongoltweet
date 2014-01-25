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
    Consumer = {Consumer_key, Consumer_secret, hmac_sha1},
    {ok, Response} = oauth:get(Url, [], Consumer, Access_token, Access_token_secret),
    Response.

-ifdef(TEST).

-endif.
