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

-record(state, {ref :: reference()}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    self() ! init_dets,
    lager:debug("I'm start ~p",[self()]),
    {ok, #state{ref = undefined}}.

handle_call({<<"/put">>, Key, Value, TTL}, _From, #state{ref = Ref} = State) ->
    {reply, put_value(Key, Value, TTL, Ref), State};
handle_call({<<"/get">>, Key, _, _}, _From, #state{ref = Ref} = State) ->
    {reply, get_value(Key, Ref), State};
handle_call(Request, _From, State) ->
    lager:errort("Error call ~p",[Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_dets, State) ->
    NewRef = case dets:open_file('mydata.file',[]) of
        {ok, Ref} -> Ref;
        {error, Reason} -> lager:error("eror open table ~p",[Reason]),
                           undefined
    end,
    {noreply, State#state{ref = NewRef}};
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


put_value(Key, Value, TTL, Ref) ->
    case dets:insert(Ref, {Key, {Value, TTL}}) of
        {error, Reason} -> lager:error("error insert data ~p",[Reason]);
        ok -> {ok, <<"OK">>}
    end.

get_value(Key, Ref) ->
    case dets:lookup(Ref, Key) of
        {error, Reason} ->
            lager:error("error lookup ~p",[Reason]),
            {error, Reason};
        [] ->
            {error, empty};
        [Data] ->
            {Key, {Value, _TTL}} = Data,
            {ok, Value}
    end.
