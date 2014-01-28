-module(translate).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, translate/1, is_mongolian/1]).

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

is_mongolian(String) ->
    gen_server:call(?MODULE, {is_mongolian, String}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({translate, String}, _From, State) ->
    {reply, do_translate(String, proplists:get_value(translate_key, State)), State};
handle_call({is_mongolian, String}, _From, State) ->
    {reply, check_is_mongolian(String), State};
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
        {source, "mn"},
        {target, "en"}
    ].

build_get_params(Url, Params) ->
    Url ++ "?" ++ munge_params(Params).

munge_params(Params) ->
    munge_params(Params, []).

munge_params([], Acc) ->
    Acc;
munge_params([{Key, Value} | Rest], []) ->
    munge_params(Rest, atom_to_list(Key) ++ "=" ++ encode_uri_rfc3986:encode(Value));
munge_params([{Key, Value} | Rest], Acc) ->
    munge_params(Rest, atom_to_list(Key) ++ "=" ++ encode_uri_rfc3986:encode(Value) ++ "&" ++ Acc).

get_response_data({_, _, Data}) ->
    mochijson2:decode(Data),
    {struct, Decoded} = mochijson2:decode(Data),
    {struct, Data_prop} = proplists:get_value(<<"data">>, Decoded),
    {struct, Translations} = lists:nth(1, proplists:get_value(<<"translations">>, Data_prop)),
    lists:nth(1, io_lib:format("~ts", [proplists:get_value(<<"translatedText">>, Translations)])).

check_is_mongolian(String) ->
    lists:any(fun(Char) -> Char >= 1024 andalso Char =< 2377 end, String).

-ifdef(TEST).

get_response_data_test() ->
    Input = {blah, blah, <<"{\n \"data\": {\n  \"translations\": [\n   {\n    \"translatedText\": \"YAY\"\n   }\n  ]\n }\n}\n">>},
    ?assertEqual("YAY", get_response_data(Input)).

build_get_params_test() ->
    String = "I say " ++ [1089,1072,1081,1085],
    Params = build_get_params("lol", [{q, String}]),
    ?assertEqual("lol?q=I+say+%d1%81%d0%b0%d0%b9%d0%bd", Params).

check_is_mongolian_test() ->
    ?assertEqual(check_is_mongolian([1089,1072,1081,1085]), true),
    ?assertEqual(check_is_mongolian("sain"), false).

-endif.
