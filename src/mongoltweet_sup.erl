
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
    Twitter_args = [[
        {consumer_key, get_setting(consumer_key, undefined)},
        {consumer_secret, get_setting(consumer_secret, undefined)},
        {access_token, get_setting(access_token, undefined)},
        {access_token_secret, get_setting(access_token_secret, undefined)}
    ]],
    Translate_args = [[
        {translate_key, get_setting(translate_key, undefined)}
    ]],
    User_args = [[
        {twitter_accounts, get_setting(twitter_accounts, [])}
    ]],
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(translate, worker, Translate_args),
        ?CHILD(tweetgrabber, worker, Twitter_args),
        ?CHILD(database, worker, []),
        ?CHILD(worker, worker, User_args),
        webmachine_conf()
    ]}}.

get_setting(Name, Default) ->
    case application:get_env(Name) of
        {ok, Value} -> Value;
        _ -> Default
    end.

webmachine_conf() ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, App} = application:get_application(?MODULE),
    {ok, Dispatch} = file:consult(filename:join([priv_dir(App),
                                                 "dispatch.conf"])),
    Port = case os:getenv("WEBMACHINE_PORT") of
            false -> 8000;
            AnyPort -> AnyPort
          end,
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Web.

%%
%% @doc return the priv dir
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.
