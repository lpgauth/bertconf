-module(bertconf_lib).
-export([decode/1, find_bert_files/1, inspect/1, inspect/2]).

-include_lib("kernel/include/file.hrl").        % for #file_info

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


-spec change_time_of(file:name_all()) -> file:date_time() | undefined.

change_time_of(File) ->
    case file:read_file_info(File, [raw, {time,posix}]) of
        {ok, #file_info{mtime = M}} -> M;
        _ -> undefined
    end.


%% Finds bert files that changed in a given directory. Works with a list of
%% hashes based on the content of all files.
inspect(Dir) -> inspect(Dir, []).

inspect(Dir, Refs) ->
    Files = find_bert_files(Dir),
    NewRefs = [{File, change_time_of(File)} || File <- Files],
    DiffFiles = diff(NewRefs, Refs),
    {DiffFiles, NewRefs}.

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
diff([{_File, undefined} | Rest], Refs) -> diff(Rest, Refs);
diff([{File, Ref} | Rest], Refs) ->
    case lists:keyfind(File, 1, Refs) of
        {File, OldRef} when OldRef == Ref -> diff(Rest, Refs);
        _ -> [File | diff(Rest, Refs)]
    end.
