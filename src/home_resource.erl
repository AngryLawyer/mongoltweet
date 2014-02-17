%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(home_resource).
-export([init/1, to_html/2, charsets_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Params) -> {ok, Params}.


to_html(ReqData, State) ->
    {ok, Content} = home_dtl:render([]),
    {Content, ReqData, State}.

charsets_provided(ReqData, State) ->
    {[{"utf8", fun convert/1}], ReqData, State}.

convert(Body) ->
    Body.
