
-module(mongoltweet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Args = [[
        {consumer_key, application:get_env(consumer_key)},
        {consumer_secret, application:get_env(consumer_secret)},
        {access_token, application:get_env(access_token)},
        {access_token_secret, application:get_env(access_token_secret)}
    ]],
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(tweetgrabber, worker, Args)
    ]}}.
