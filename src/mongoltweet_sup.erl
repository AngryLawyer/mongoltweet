
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
        {consumer_key, get_setting(consumer_key, undefined)},
        {consumer_secret, get_setting(consumer_secret, undefined)},
        {access_token, get_setting(access_token, undefined)},
        {access_token_secret, get_setting(access_token_secret, undefined)}
    ]],
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(ibrowse, worker, []),
        ?CHILD(tweetgrabber, worker, Args)
    ]}}.

get_setting(Name, Default) ->
    case application:get_env(Name) of
        {ok, Value} -> Value;
        _ -> Default
    end.
