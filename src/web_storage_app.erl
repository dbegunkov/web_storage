-module(web_storage_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
	[ok = application:start(App)
	 || App <- [syntax_tools, compiler, goldrush, lager,
	            crypto, asn1, public_key, ssl, ranch, cowlib, cowboy, web_storage]],
	ok.

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/put", web_handler, []},
			{"/get", web_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 10, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
    web_storage_sup:start_link().

stop(_State) ->
    ok.
