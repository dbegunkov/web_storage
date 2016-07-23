%% -*- coding: utf-8 -*-
-module(web_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(TIMEOUT, 3000).

init(_Transport, Req, []) ->
	{ok, Req, no_state}.

handle(Req, State) ->
	{Path, _} = cowboy_req:path(Req),
	{PL, Req1} = cowboy_req:qs_vals(Req),
    lager:debug("cow params ~p",[PL]),
    {ok, Req2} = try check_params_from_request(Path, PL, []) of
                     LParam ->
                         Res = gen_server:call(web_storage_wrk,{Path, LParam}, ?TIMEOUT),
                         {Code, Reply} = catch_reply(Res),
                         {ok, Req3} = cowboy_req:reply(Code, [], Reply, Req1),
                         {ok, Req3}
                 catch
                     _:Error ->
                         cowboy_req:reply(400, [], Error, Req1)
                 end,
	{ok, Req2, State}.

check_params_from_request(_Path, [], Acc) -> Acc;
check_params_from_request(<<"/put">>, [{<<"key">>, Key},{<<"value">>, Value},{<<"ttl">>, TTL}|T], Acc) ->
    check_params_from_request(<<"/put">>, T, Acc ++ [{key, Key},{value, Value},{ttl, TTL}]);
check_params_from_request(<<"/put">>, [{<<"key">>, Key},{<<"value">>, Value}|T], Acc) ->
    check_params_from_request(<<"/put">>, T, Acc ++ [{key, Key},{value, Value},{ttl, undefined}]);
check_params_from_request(<<"/put">>, [H | _T], _Acc) ->
    lager:debug("Error data H~p",[H]),
    throw(<<"Bad params! Use: /put?key=1&value=2&ttl=100">>);
check_params_from_request(<<"/get">>, [{<<"key">>, Key}|T], Acc) ->
    check_params_from_request(<<"/get">>, T, Acc ++ [{key, Key}]);
check_params_from_request(<<"/get">>, _H, _Acc) ->
    throw(<<"Bad params! Use: /get?key=1">>);
check_params_from_request(_Path, _Any, _Acc) ->
    throw(<<"Bad method! Use: /put or /get">>).


terminate(_Reason, _Req, _State) ->
	ok.


%%INTERNAL

catch_reply({ok, Data}) ->
        {200, Data};
catch_reply({error, Reason}) when is_binary(Reason) ->
        {400, Reason};
catch_reply({error, _Reason}) ->
        {400, <<"error operation">>}.
