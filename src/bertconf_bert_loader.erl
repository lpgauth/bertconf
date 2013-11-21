-module(bertconf_bert_loader).
-include("bertconf.hrl").

-export([
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {
    fnotify_ref,
    old_tables = []
}).

%% public
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    TableOpts = [
        named_table,
        public,
        {keypos, #tab.name},
        {read_concurrency, true},
        set
    ],
    ?MODULE = ets:new(?MODULE, TableOpts),

    Filenames = bertconf_lib:find_bert_files(watch_dir()),
    [reload(Filename) || Filename <- Filenames],

    {ok, FnotifyRef} = fnotify:watch(watch_dir(), [create, modify]),

    {ok, #state {
        fnotify_ref = FnotifyRef
    }}.

handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({fevent, FnotifyRef, [Event], [Path], File}, #state {
        fnotify_ref = FnotifyRef,
        old_tables = OldTables
    } = State) when Event =:= create; Event =:= write ->

    Filename = filename:join(Path, File),
    case ".bert" =:= filename:extension(Filename) of
        true ->
            NewTables = reload(Filename),
            ok = delete_tables(OldTables),
            {noreply, State#state {
                old_tables = NewTables
            }};
        false ->
            {noreply, State}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state {fnotify_ref = FnotifyRef}) ->
    fnotify:unwatch(FnotifyRef),
    ok.

%% private
delete_tables(OldTables) ->
    [ets:delete(Tid) || Tid <- OldTables],
    ok.

merge([]) -> [];
merge([Table]) -> [Table];
merge([{NameSpace, ListA}, {NameSpace, ListB} | Terms]) ->
    merge([{NameSpace, ListA ++ ListB} | Terms]);
merge([Table | Terms]) ->
    [Table | merge(Terms)].

reload(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, Terms} = bertconf_lib:decode(Bin),
    NewTables = store(merge(lists:sort(lists:flatten(Terms)))),
    update_table_index(NewTables).

store([]) -> [];
store([[] | Tables]) ->
    store(Tables);
store([{NameSpace, Records} | Tables]) ->
    TableOpts = [
        public,
        set,
        {read_concurrency, true}
    ],
    Tid = ets:new(NameSpace, TableOpts),
    ets:insert(Tid, Records),
    [#tab {name = NameSpace, id = Tid} | store(Tables)].

update_table_index(NewTables) ->
    OldTables = [Tid || #tab{name = Name} <- NewTables,
                         [#tab{id = Tid}] <- [ets:lookup(?MODULE, Name)]],
    ets:insert(?MODULE, NewTables),
    OldTables.

watch_dir() ->
    case application:get_env(bertconf, watch_dir) of
        undefined -> [];
        {ok, Val} -> Val
    end.
