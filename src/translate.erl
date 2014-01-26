-module(translate).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, translate/1]).

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

translate(String) ->
    gen_server:call(?MODULE, {translate, String}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({translate, String}, _From, State) ->
    {reply, do_translate(String, proplists:get_value(translate_key, State)), State};
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

do_translate(String, Key) ->
    Params = build_params(String, Key),
    Url = build_get_params("https://www.googleapis.com/language/translate/v2/", Params),
    {ok, Result} = lhttpc:request(Url, get, [], infinity),
    get_response_data(Result).

build_params(String, Key) ->
    [
        {q, String},
        {key, Key},
        {source, mn},
        {target, en}
    ].

build_get_params(Url, Params) ->
    oauth:uri(Url, Params).

get_response_data({_, _, Data}) ->
    mochijson2:decode(Data),
    {struct, Decoded} = mochijson2:decode(Data),
    {struct, Data_prop} = proplists:get_value(<<"data">>, Decoded),
    {struct, Translations} = lists:nth(1, proplists:get_value(<<"translations">>, Data_prop)),
    proplists:get_value(<<"translatedText">>, Translations).

-ifdef(TEST).

get_response_data_test() ->
    Input = {blah, blah, <<"{\n \"data\": {\n  \"translations\": [\n   {\n    \"translatedText\": \"YAY\"\n   }\n  ]\n }\n}\n">>},
    ?assertEqual(<<"YAY">>, get_response_data(Input)).

-endif.
