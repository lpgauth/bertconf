-module(bertconf_lib).

-export([
    decode/1,
    find_bert_files/1
]).

%% public
decode(Bin) ->
    try validate(binary_to_term(Bin)) of
        Terms ->
            {ok, Terms}
    catch
        throw:Reason ->
            {error, Reason}
    end.

find_bert_files(Directory) ->
    {ok, Files} = file:list_dir(Directory),
    [filename:join(Directory, Name) || Name <- Files,
        ".bert" =:= filename:extension(Name)].

%% private
validate(List = [_ | _]) ->
    case validate_namespaces(List) andalso validate_keyval(List) of
        true -> List;
        false -> throw(bad_config_format)
    end;
validate(_) -> throw(bad_config_format).

validate_namespaces([]) -> true;
validate_namespaces([{Namespace, _Value} | Rest]) when is_atom(Namespace) ->
    validate_namespaces(Rest);
validate_namespaces(_) -> false.

validate_keyval([]) -> true;
validate_keyval([{_, List = [_ | _]} | Rest]) ->
    validate_keyval1(List) andalso validate_keyval(Rest);
validate_keyval(_) -> false.

validate_keyval1([]) -> true;
validate_keyval1([{_Key, _Vvalue} | Rest]) -> validate_keyval1(Rest);
validate_keyval1(_) -> false.
