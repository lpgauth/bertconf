-define(TABLE, bertconf_bert_loader).
-define(MATCH_LIMIT, 500).

-record(tab, {name, id}).

-type namespace() :: term().
-type version() :: {vsn, term()}.

-export_types([
    namespace/0,
    version/0
]).

