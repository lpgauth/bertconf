-module(bertconf).
-behaviour(application).
-export([start/2, stop/1]).
-export([read/2, all/1, version/1, version/2]).
-define(TABLE, bertconf_bert_loader).
-define(MATCH_LIMIT, 500).
-include("bertconf.hrl").

-type namespace() :: term().
-opaque version() :: {vsn,term()}.
-export_types([namespace/0, version/0]).

%%% APPLICATION CALLBACKS %%%
start(normal, _) ->
    bertconf_sup:start_link().

stop(_) -> ok.


%%% PUBLIC INTERFACE %%%
-spec read(namespace(), Key::term()) -> undefined | {ok, term()}.
read(NameSpace, Key) ->
    case table(NameSpace) of
        undefined -> error(bertconf_undefined_namespace);
        Table ->
            case ets:lookup(Table, Key) of
                [{_Key,Val}] -> {ok, Val};
                [] -> undefined
            end
    end.

-spec all(namespace()) -> undefined | list().
all(NameSpace) ->
    case table(NameSpace) of
        undefined -> undefined;
        Table ->
            loop_all({Table, '_', ?MATCH_LIMIT})
    end.

%% The version is the table id, which should be swapped on
%% any update. This is a very scary thing to use, but it works
%% as long as we use it as an opaque data type.
-spec version(namespace()) -> version().
version(NameSpace) ->
    case table(NameSpace) of
        undefined -> undefined;
        Table -> {vsn, Table}
    end.

-spec version(namespace(), version()) -> current | old.
version(NameSpace, {vsn, Version}) ->
    case table(NameSpace) of
        undefined -> undefined;
        Version -> current;
        _ -> old
    end.

%%% PRIVATE
table(NameSpace) ->
    case ets:lookup(?TABLE, NameSpace) of
        [#tab{id=Tid}] -> Tid;
        _ -> undefined
    end.

loop_all('$end_of_table') ->
    [];
loop_all({Match, Continuation}) ->
    [Match | loop_all(ets:match_object(Continuation))];
loop_all({Tid, Pat, Limit}) ->
    lists:append(loop_all(ets:match_object(Tid, Pat, Limit))).
