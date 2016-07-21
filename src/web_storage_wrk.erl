-module(web_storage_wrk).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    self ! init_dets,
    {ok, Args}.

handle_call({<<"put">>, Key, Value, TTL}, _From, State) ->
    {reply, put_value(Key, Value, TTL), State};
handle_call({<<"get">>, Key, _, _}, _From, State) ->
    {reply, get_value(Key), State};
handle_call(Request, _From, State) ->
    lager:errort("Error call ~p",[Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_dets, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:error("Error info ~p",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


put_value(_Key, _Value, _TTL) ->
    ok.

get_value(_Key) ->
    ok.
