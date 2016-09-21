-module(bertconf_bert_loader).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("bertconf.hrl").
-record(state, {watches, changes=[], old_tables=[], reloader=undefined}).

%%% PUBLIC INTERFACE %%%
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%%% GEN_SERVER CALLBACKS %%%
init([]) ->
    process_flag(trap_exit, true),
    ?MODULE = ets:new(?MODULE, [named_table, set, public, {keypos, #tab.name}, {read_concurrency, true}]),
    {Old, Changes} = reload_bert_sync([]),
    {ok,
     #state{watches=[begin
                         {ok,Pid} = dirwatch:start(self(), P, reload_delay()),
                         Pid
                     end || P <- find_dirs()],
            changes=Changes,
            old_tables=Old},
     hibernate}.

handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({dirwatch, _Pid, changed}, S=#state{changes=Chg, reloader=undefined}) ->
    {noreply, S#state{reloader = reload_bert_async(Chg)}, hibernate};
handle_info({dirwatch, _Pid, changed}, S=#state{reloader=R}) ->
    %% Trying to reload before previous reload is finished
    R ! needs_reload,
    {noreply, S, hibernate};
handle_info({reloaded, OldTables, NewChanges}, S=#state{old_tables=Old}) ->
    delete_tables(Old),
    {noreply, S#state{changes=NewChanges, old_tables=OldTables, reloader=undefined}, hibernate};
handle_info({'EXIT', Pid, Reason}, S=#state{reloader=Pid}) when Reason /= normal->
    error_logger:error_msg("Bertconf crashed while reloading bert files"),
    {noreply, S#state{reloader=undefined}, hibernate};
handle_info(_Event, State) ->
    {noreply, State, hibernate}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{watches=Ws}) ->
    [dirwatch:stop(W) || W <- Ws],
    ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

reload_bert_async(Chg) ->
    spawn_opt(fun () -> reload_bert(Chg) end, [link, {min_heap_size, 8000000}]).

reload_bert_sync(Chg) ->
    reload_bert_async(Chg),
    receive
        {reloaded, OldTables, NewChanges} ->
            {OldTables, NewChanges}
    end.

reload_bert(OldChanges) ->
    Dirs = find_dirs(),
    Changes = [bertconf_lib:inspect(Dir, OldChanges) || Dir <- Dirs],
    Files = lists:append([Changed || {Changed, _Refs} <- Changes]),
    Terms = [Terms || {ok, Terms} <- [reload_bert_file(File) || File <- Files]],
    NewTables = store(merge(lists:sort(lists:flatten(Terms)))),
    OldTables = update_table_index(NewTables),
    NewChanges = lists:append([Refs || {_, Refs} <- Changes]),

    %% There's a race condition that can occur if we're doing multiple
    %% nested reload (the reload process receives a needs_reload message).
    %%
    %% The issue is that bertconf:read first reads the current table
    %% for a namespace then looksup a key in that table which means
    %% that if the table is deleted between these two calls, the
    %% second lookup with trigger an error.
    %%
    %% This code tries to avoid that by introducing a grace period
    %% where old tables are kept around until the next reload which
    %% would normally happen on the configure periodic timer. That
    %% being said, if one table takes too long to load and we end up
    %% executing multiple nested reloads (has been known to happen)
    %% then no grace periods are introduced and we may end up deleting
    %% the table that is currently being read.
    ?MODULE ! {reloaded, OldTables, NewChanges},

    %% This MUST be tail-recursive otherwise we could end up with
    %% multiple versions of large bertconf tables loaded in memory
    %% which can easily trigger out-of-memory errors.
    case reload_more() of
        reload -> reload_bert(Changes);
        stop -> ok
    end.

reload_more() ->
    reload_more(stop).

%% dequeue all the pending messages to avoid doing a bunch of
%% pointless reloads (has been observed in prod).
reload_more(Result) ->
    receive
        needs_reload -> reload_more(reload)
    after
        0 -> Result
    end.



reload_bert_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            case bertconf_lib:decode(Bin) of
                {ok, Terms} -> {ok, Terms};
                _ ->
                    error_logger:error_msg("Bertconf failed to decode ~p", [File]),
                    {error, bertconf_failed_decode}
            end;
        {error, Err} ->
            error_logger:error_msg("Bertconf failed to read ~p with ~p", [File, Err]),
            {error, Err}
    end.


reload_delay() ->
    case application:get_env(bertconf, delay) of
        undefined -> 5000;
        {ok, Val} -> Val
    end.

find_dirs() ->
    case application:get_env(bertconf, dir) of
        undefined -> [];
        {ok, Val} -> Val
    end.

merge([]) -> [];
merge([Table]) -> [Table];
merge([{NameSpace, ListA}, {NameSpace, ListB} | Terms]) -> % identical NS.
    merge([{NameSpace, ListA++ListB} | Terms]);
merge([Table | Terms]) -> % different NS
    [Table | merge(Terms)].

store([]) -> [];
store([[]|Tables]) -> % some file not respecting the format got skipped
    store(Tables);
store([{NameSpace, Records} | Tables]) ->
    Tid = ets:new(NameSpace, [set, public, {read_concurrency, true}, {heir, whereis(?MODULE), undefined}]),
    ets:insert(Tid, Records),
    [#tab{name=NameSpace, id=Tid} | store(Tables)].

%% We should delete old tables, but it might be safer to keep them
%% in case of programmer mistake. We're more likely to die of such a
%% mistake than we are to go out of memory because of config files.
%% However, tables that were successfully replaced should mean that
%% the old version of the table is gone.
%% The ordering of operations is important to avoid having a window
%% where some config values are unavailable.
update_table_index(NewTables) ->
    OldTables = [Tid || #tab{name=Name} <- NewTables,
                        [#tab{id=Tid}] <- [ets:lookup(?MODULE, Name)]],
    ets:insert(?MODULE, NewTables),
    %% Don't delete, keep an old generation of tables:
    %% [Current | Old | ToDelete]
    %% to avoid blackouts during changes
    OldTables.

delete_tables(OldTables) ->
    [ets:delete(Tid) || Tid <- OldTables],
    ok.
