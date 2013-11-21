-module(bertconf).
-include("bertconf.hrl").

-export([
    all/1,
    read/2,
    start/0,
    version/1,
    version/2
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
-spec all(namespace()) -> list().
all(NameSpace) ->
    loop_all({table(NameSpace), '_', ?MATCH_LIMIT}).

-spec read(namespace(), Key::term()) -> undefined | {ok, term()}.
read(NameSpace, Key) ->
    case ets:lookup(table(NameSpace), Key) of
        [{_Key, Value}] -> {ok, Value};
        [] -> undefined
    end.

start() ->
    ok = application:start(fnotify),
    ok = application:start(bertconf).

-spec version(namespace()) -> version().
version(NameSpace) -> {vsn, table(NameSpace)}.

-spec version(namespace(), version()) -> current | old.
version(NameSpace, {vsn, Version}) ->
    case {table(NameSpace), Version} of
        {X, X} -> current;
        _ -> old
    end.

%% application callbacks
start(normal, _) ->
    bertconf_sup:start_link().

stop(_) -> ok.

%% private
loop_all('$end_of_table') ->
    [];
loop_all({Match, Continuation}) ->
    [Match | loop_all(ets:match_object(Continuation))];
loop_all({Tid, Pattern, Limit}) ->
    lists:append(loop_all(ets:match_object(Tid, Pattern, Limit))).

table(NameSpace) ->
    [#tab{id = Tid}] = ets:lookup(?TABLE, NameSpace),
    Tid.
