-module(worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, update_tweets/0, get_unsolved_tweet/0]).

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

update_tweets() ->
    gen_server:call(?MODULE, update_tweets).

get_unsolved_tweet() ->
    gen_server:call(?MODULE, get_unsolved_tweet).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(update_tweets, _From, State) ->
    % Find all of our users from State
    Accounts = proplists:get_value(twitter_accounts, State),
    % Get the users out of the database
    Account_tuples = get_accounts(Accounts),
    % Get the latest tweets from each
    Tweets = fetch_tweets(Account_tuples),
    % Stash them
    Stashed = stash_tweets(Tweets),
    Updated_users = update_users(Tweets),
    % Update our latest tweets field
    {reply, [Account_tuples, Stashed, Updated_users], State};
handle_call(get_unsolved_tweet, _From, State) ->
    Tweet = get_unsolved_tweet_inner(),
    Translated = translate(Tweet),
    {reply, Translated, State};
handle_call(solve_tweet, _From, State) ->
    {reply, ok, State};
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

get_accounts(Accounts) ->
    %Get the users out of the database
    Account_grabber = fun(Account) ->
        case database:get_user(Account) of
            undefined -> {user, Account, 1};
            Object -> Object
        end
    end,
    lists:map(Account_grabber, Accounts).

store_accounts(Account_tuples) ->
    Account_storer = fun({user, Account, Timestamp}) ->
        database:insert_user(Account, Timestamp)
    end,
    lists:map(Account_storer, Account_tuples).

store_tweets(Tweets) ->
    Tweet_storer = fun({User_name, Timestamp, Mongolian}) ->
        database:insert_tweet(User_name, Timestamp, Mongolian)
    end,
    lists:map(Tweet_storer, Tweets).

fetch_tweets(Account_tuples) ->
    Tweet_grabber = fun({user, Account, Timestamp}) ->
        {Account, filter_tweets(tweetgrabber:fetch(Account, Timestamp))}
    end,
    lists:map(Tweet_grabber, Account_tuples).

filter_tweets(Tweets_list) ->
    Tweet_filterer = fun({_, Tweet}) ->
        translate:is_mongolian(Tweet)
    end,
    lists:filter(Tweet_filterer, Tweets_list).

stash_tweets(Tweets_list) ->
    lists:map(fun({User_name, Tweets}) ->
        lists:map(fun({Timestamp, Tweet}) ->
            database:insert_tweet(User_name, Timestamp, Tweet)
        end, Tweets)
    end, Tweets_list).

update_users(Tweets_list) ->
    lists:map(fun({User_name, Tweets}) ->
        case Tweets of
            [] -> ok;
            _ -> database:insert_user(User_name, get_latest_timestamp(Tweets))
        end
    end, Tweets_list).

get_latest_timestamp(Tweets) ->
    lists:foldl(fun({Timestamp, _}, Last_timestamp) ->
        if
            Timestamp > Last_timestamp -> Timestamp;
            true -> Last_timestamp
        end
    end, 1, Tweets).

get_unsolved_tweet_inner() ->
    Tweets = database:search_tweets_by_not_solved(),
    case Tweets of
        [] -> undefined;
        [Head | _] -> Head
    end.

translate({tweet, Identifier, User_name, Timestamp, Mongolian, undefined, Complete}) -> 
    English = translate:translate(Mongolian),
    database:insert_tweet(User_name, Timestamp, Mongolian, English, Complete),
    {tweet, Identifier, User_name, Timestamp, Mongolian, English, Complete};
translate(Tweet) -> 
    Tweet.
