%% -*- coding: utf-8 -*-
-module(web_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(TIMEOUT, 3000).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Path, _} = cowboy_req:path(Req),
	{PL, Req1} = cowboy_req:qs_vals(Req),
    Key = proplists:get_value(<<"key">>, PL),
    Value = proplists:get_value(<<"value">>, PL),
    TTL = proplists:get_value(<<"ttl">>, PL),
    Req2 = parse_request(Path, Key, Value, TTL, Req1),
	{ok, Req2, State}.

parse_request(_, Key, _, _, Req) when Key == undefined ->
	cowboy_req:reply(400, [], <<"Missing key parameter.">>, Req);
parse_request(<<"put">>, _, Value, _, Req) when Value == undefined ->
	cowboy_req:reply(400, [], <<"Missing value parameter.">>, Req);
parse_request(Method, _Key, _Value, _TTL, Req) when Method /= <<"pit">>, Method /= <<"get">> ->
	cowboy_req:reply(400, [], <<"Error method.">>, Req);
parse_request(Method, Key, Value, TTL, Req)  ->
    Repl = gen_server:call(web_storage_wrk, {Method, Key, Value, TTL}, ?TIMEOUT),
	cowboy_req:reply(200, [], Repl, Req).

terminate(_Reason, _Req, _State) ->
	ok.


%%INTERNAL
