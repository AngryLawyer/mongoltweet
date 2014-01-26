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
    Unicode_url = binary:list_to_bin(Url),
    Binary_params = params_to_binary(Params),
    <<Unicode_url/binary, "?", Binary_params/binary>>.

params_to_binary(Params) ->
    params_to_binary(Params, <<>>).

params_to_binary([], Acc) ->
    Acc;
params_to_binary([{Key, Value} | Rest], Acc) ->
    New = <<Key/binary, "=", Value/binary>>,
    params_to_binary(Rest, <<New/binary, "&", Acc/binary>>).

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

build_get_params_test() ->
    String = <<64,117,110,100,97,114,49,48,48,53,32,208,176,
                               208,178,209,129,209,130,209,128,208,176,208,
                               187,208,184,32,209,131,209,133,209,141,209,128,
                               208,184,208,185,208,189,32,208,188,208,176,209,
                               133>>,
    Params = build_get_params("lol", [{q, String}]),
    ?assertEqual(<<108, 111, 108, 63, 113, 61, 64,117,110,100,97,114,49,48,48,53,32,208,176,
                               208,178,209,129,209,130,209,128,208,176,208,
                               187,208,184,32,209,131,209,133,209,141,209,128,
                               208,184,208,185,208,189,32,208,188,208,176,209,
                               133>>, Params).

-endif.
