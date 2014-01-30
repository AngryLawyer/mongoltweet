-module(database).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("stdlib/include/qlc.hrl").

-record(user, {user_name, last_tweet}).
-record(tweet, {user_name_and_timestamp, user_name, timestamp, mongolian, english, solved}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, insert_user/2, get_user/1, insert_tweet/3, insert_tweet/4, insert_tweet/5, search_tweets_by_name/1, search_tweets_by_not_translated/0, search_tweets_by_not_solved/0, get_tweet/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert_user(User_name, Last_tweet) ->
    gen_server:call(?MODULE, {insert_user, User_name, Last_tweet}).

get_user(User_name) ->
    gen_server:call(?MODULE, {get_user, User_name}).

insert_tweet(User_name, Timestamp, Mongolian) ->
    insert_tweet(User_name, Timestamp, Mongolian, undefined).

insert_tweet(User_name, Timestamp, Mongolian, English) ->
    insert_tweet(User_name, Timestamp, Mongolian, English, false).

insert_tweet(User_name, Timestamp, Mongolian, English, Solved) ->
    gen_server:call(?MODULE, {insert_tweet, User_name, Timestamp, Mongolian, English, Solved}).

search_tweets_by_name(User_name) ->
    gen_server:call(?MODULE, {search_tweets_by_name, User_name}).

search_tweets_by_not_translated() ->
    gen_server:call(?MODULE, search_tweets_by_not_translated).

search_tweets_by_not_solved() ->
    gen_server:call(?MODULE, search_tweets_by_not_solved).

get_tweet(User_name, Timestamp) ->
    gen_server:call(?MODULE, {get_tweet, User_name, Timestamp}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    initialize_mnesia(),
    {ok, Args}.

handle_call({insert_user, User_name, Last_tweet}, _From, State) ->
    {reply, internal_insert_user(User_name, Last_tweet), State};
handle_call({get_user, User_name}, _From, State) ->
    Users = internal_retrieve_users(User_name),
    User = case Users of
        [] -> undefined;
        [Head | _Rest] -> Head
    end,
    {reply, User, State};
handle_call({insert_tweet, User_name, Timestamp, Mongolian, English, Solved}, _From, State) ->
    {reply, internal_insert_tweet(User_name, Timestamp, Mongolian, English, Solved), State};
handle_call({search_tweets_by_name, User_name}, _From, State) ->
    {reply, internal_search_tweets_by_name(User_name), State};
handle_call(search_tweets_by_not_translated, _From, State) ->
    {reply, internal_search_tweets_by_not_translated(), State};
handle_call(search_tweets_by_not_solved, _From, State) ->
    {reply, internal_search_tweets_by_not_solved(), State};
handle_call({get_tweet, User_name, Timestamp}, _From, State) ->
    Tweet = case internal_retrieve_tweet(User_name, Timestamp) of
        [] -> undefined;
        [Head | _Rest] -> Head
    end,
    {reply, Tweet, State};
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

initialize_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user, [{disc_copies, [node()]}, {attributes, record_info(fields, user)}]),
    mnesia:create_table(tweet, [{disc_copies, [node()]}, {attributes, record_info(fields, tweet)}]),
    ok.

internal_insert_user(User_name, Last_tweet) ->
    Record = #user{user_name = User_name, last_tweet = Last_tweet},
    internal_insert(Record).

internal_insert_tweet(User_name, Timestamp, Mongolian, English, Solved) ->
    Record = #tweet{user_name_and_timestamp = {User_name, Timestamp}, user_name = User_name, timestamp = Timestamp, mongolian = Mongolian, english = English, solved = Solved},
    internal_insert(Record).

internal_insert(Record) -> 
    F = fun() ->
		mnesia:write(Record)
	end,
    mnesia:transaction(F).

internal_retrieve_users(User_name) ->
    internal_retrieve(user, User_name).

internal_retrieve_tweet(User_name, Timestamp) ->
    internal_retrieve(tweet, {User_name, Timestamp}).

internal_retrieve(Table, Key) ->
    F = fun() ->
		mnesia:read({Table, Key})
	end,
    {atomic, Data} = mnesia:transaction(F),
    Data.

internal_search_tweets_by_name(User_name) ->
    internal_search_tweets(User_name, '_', '_', '_', '_').

internal_search_tweets_by_not_translated() ->
    internal_search_tweets('_', '_', '_', undefined, '_').

internal_search_tweets_by_not_solved() ->
    internal_search_tweets('_', '_', '_', '_', false).

internal_search_tweets(User_name, Timestamp, Mongolian, English, Solved) ->
    F = fun() ->
		mnesia:match_object(#tweet{user_name_and_timestamp = '_', user_name = User_name, timestamp = Timestamp, mongolian = Mongolian, english = English, solved = Solved})
	end,
    {atomic, Data} = mnesia:transaction(F),
    Data.
