-module(bertconf_sup).

-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

-define(CHILD(Name, Mod), {Name, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).

%% supervisor callbacks
start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 5000}, [
        ?CHILD(bertconf_serv, bertconf_bert_loader)
    ]}}.
