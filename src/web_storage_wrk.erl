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
-define(CLEAN_TIMEOUT, 60000).
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
    erlang:send_after(?CLEAN_TIMEOUT, self(), clean),
    lager:debug("I'm start ~p",[self()]),
    {ok, #state{ref = undefined}}.

handle_call({<<"/put">>, ListData}, _From, #state{ref = Ref} = State) ->
    Res = try put_values(ListData, Ref) of
              Data -> Data
          catch
              _:Error -> {error, Error}
          end,
        {reply, {ok, Res}, State};
handle_call({<<"/get">>, ListData}, _From, #state{ref = Ref} = State) ->
    Res = try get_values(ListData, Ref, []) of
              Data -> Data
          catch
              _:Error -> {error, Error}
          end,
        {reply, {ok, Res}, State};
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
handle_info(clean, #state{ref = Ref} =  State) when Ref == undefined ->
    {noreply, State};
handle_info(clean, #state{ref = Ref} =  State)  ->
    clean_tab(Ref),
    erlang:send_after(?CLEAN_TIMEOUT, self(), clean),
    {noreply, State};
handle_info(Info, State) ->
    lager:error("Error info ~p",[Info]),
    {noreply, State}.

terminate(_Reason, #state{ref = Ref}) when Ref =/= undefined ->
    dets:close(Ref),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

clean_tab(Ref) ->
    remove_old(dets:first(Ref), Ref).

remove_old('$end_of_table', _Ref) ->
    ok;
remove_old(DKey, Ref) ->
    case dets:lookup(Ref, DKey) of
        {error, Reason} ->
            lager:debug("Erorr lookup dets ~p",[Reason]);
        [] ->
            ok;
        [Data] ->
            {Key, {Value, TTL}} = Data,
            case check_ttl(Value, TTL) of
                {ok, _} -> ok;
                {error, _} ->
                    lager:debug("deleted old record ~p",[Key]),
                    dets:delete(Ref, Key)
            end
    end,
    remove_old(dets:next(Ref, DKey), Ref).

put_values([], _Ref) -> <<"ok">>;
put_values([{key, Key}, {value, Value}, {ttl, TTL}| T], Ref) ->
     ok = put_value(Key,Value, TTL, Ref),
     put_values(T, Ref).

put_value(Key, Value, TTL, Ref) ->
    case dets:insert(Ref, {Key, {Value, get_time(TTL)}}) of
        {error, Reason} -> lager:error("error insert data ~p",[Reason]),
                           throw(<<"Error">>);
        ok -> ok
    end.

get_values([], _Ref, Acc) ->
    Acc;
get_values([{key, Key} | T], Ref, Acc) ->
    SubTotal = case get_value(Key, Ref) of
        {ok, Value} ->
                       <<Key/binary,"=",Value/binary,13,10>>;
        {error, Error} when Error == <<"Empty">>; Error == <<"Outdated">> ->
                       <<Key/binary,"=error, reason="/utf8,Error/binary>>
                           end,
    get_values(T, Ref, Acc ++[SubTotal]).

get_value(Key, Ref) ->
    case dets:lookup(Ref, Key) of
        {error, Reason} ->
            lager:error("error lookup ~p",[Reason]),
            throw(<<"Unknown error">>);
        [] ->
            {error, <<"Empty">>};
        [Data] ->
            {Key, {Value, TTL}} = Data,
            check_ttl(Value, TTL)
    end.

check_ttl(Value, undefined) ->
    {ok, Value};
check_ttl(Value, TTL) ->
    case int_timestamp() of
        Now when Now > TTL ->
            {error, <<"Outdated">>};
        _  -> {ok, Value}
    end.

get_time(undefined) ->
    undefined;
get_time(TTL) when is_binary(TTL)->
    int_timestamp() + binary_to_integer(TTL).

int_timestamp() ->
    {Mega, Secs, _Micro} = os:timestamp(),
    Res = Mega * 1000 * 1000 + Secs,
    Res.
