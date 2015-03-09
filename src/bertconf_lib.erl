-module(bertconf_lib).
-export([decode/1, find_bert_files/1, inspect/1, inspect/2]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUBLIC INTERFACE %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% Decodes a binary term and validates its format to make sure
%% that it fits the [{Namespace::atom(), [{Key, Val}]}] format
decode(Bin) ->
    try validate(binary_to_term(Bin)) of
        Terms -> {ok, Terms}
    catch
        _Error:Reason -> {error, Reason}
    end.

%% accepts a directory and returns the bert files in there, with the
%% file name relative to the directory received.
find_bert_files(Directory) ->
    {ok, Files} = file:list_dir(Directory),
    [filename:join(Directory, Name) || Name <- Files,
        ".bert" =:= filename:extension(Name)].

%% Finds bert files that changed in a given directory. Works with a list of
%% hashes based on the content of all files.
inspect(Dir) -> inspect(Dir, []).

inspect(Dir, Refs) ->
    Files = find_bert_files(Dir),
    NewRefs = [{File, MD5} || {ok, File, MD5} <- [inspect_file(File) || File <- Files]],
    DiffFiles = diff(NewRefs, Refs),
    {DiffFiles, NewRefs}.

inspect_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, File, crypto:hash(md5, Bin)};
        {error, Err} ->
            error_logger:error_msg("Bertconf failed to read ~p with ~p", [File, Err]),
            {error, Err}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
%% The accepted format is [{Namespace::atom(), [{Key, Val}]}]
validate(L = [_|_]) ->
    case validate_namespaces(L) andalso validate_keyval(L) of
        true -> L;
        false -> error(bad_config_format)
    end;
validate(_) -> error(bad_config_format).

validate_namespaces([]) -> true;
validate_namespaces([{Ns,_Val}|Rest]) when is_atom(Ns) ->
    validate_namespaces(Rest);
validate_namespaces(_) -> false.

validate_keyval([]) -> true;
validate_keyval([{_,L} | Rest]) when is_list(L) ->
    validate_keyval1(L) andalso validate_keyval(Rest);
validate_keyval(_) -> false.

validate_keyval1([]) -> true;
validate_keyval1([{_K,_V} | Rest]) -> validate_keyval1(Rest);
validate_keyval1(_) -> false.

%diff(NewRefs, Refs) ->
diff([], _Refs) -> [];
diff([{File, Ref} | Rest], Refs) ->
    case lists:keyfind(Ref, 2, Refs) of
        false -> [File | diff(Rest, Refs)];
        _ -> diff(Rest, Refs)
    end.
