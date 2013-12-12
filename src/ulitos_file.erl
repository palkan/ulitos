-module(ulitos_file).
-author("palkan").

-export([recursively_list_dir/1,
  recursively_list_dir/2,
  recursively_del_dir/1,
  dir_traversal/2,
  dir_traversal/3]).


% @type name() = string() | atom() | binary().
-type name() :: string() | atom() | binary().




%% API

% @spec (Dir::name()) -> {ok, [string()]} | {error, atom()}
% @equiv recursively_list_dir(Dir, false)
%
% @doc Lists all the files in a directory and recursively in all its
% sub directories. Returns {ok, Paths} if successful. Otherwise,
% it returns {error, Reason}. Paths is a list of Paths of all the
% files and directories in the input directory's subtree. The paths are not
% sorted in any order.

-spec recursively_list_dir(Dir::name()) ->
  {ok, [string()]} | {error, atom()}.

recursively_list_dir(Dir) ->
  recursively_list_dir(Dir, false). % default value of FilesOnly is false




% @spec (Dir::name(), FilesOnly::boolean()) -> {ok, [string()]} |
%                                                   {error, atom()}
%
% @doc Lists all the files in a directory and recursively in all its
% sub directories. Returns {ok, Paths} if successful. Otherwise,
% it returns {error, Reason}. If FilesOnly is false, Paths is a list of paths
% of all the files <b>and directories</b> in the input directory's subtree.
% If FilesOnly is true, Paths is a list of paths of only the files in the
% input directory's subtree. The paths are not sorted in any order.

-spec recursively_list_dir(Dir::name(), FilesOnly::boolean()) ->
  {ok, [string()]} | {error, atom()}.

recursively_list_dir(Dir, FilesOnly) ->
  case filelib:is_file(Dir) of
    true ->
      case filelib:is_dir(Dir) of
        true -> {ok, dir_traversal([Dir], fun(Path,Acc) -> [Path|Acc] end, FilesOnly)};
        false -> {error, enotdir}
      end;
    false -> {error, enoent}
  end.


% @spec (Dir::name()) -> ok | {error, atom()}
%
% @doc Delete all files and directories within Dir (and Dir itself).
% Return {error, Reason} if Dir doesn't exist or is not directory.

-spec recursively_del_dir(Dir::name()) ->
  ok | {error, atom()}.


recursively_del_dir(Dir) ->
  case recursively_list_dir(Dir) of
    {ok,List} -> del_paths(List);
    Error -> Error
  end.


% @spec (Dir::name(),Fun::function()) -> {ok, Result::list()} | {error, atom()}
% @equiv dir_traversal(Dir,Fun,false)
%
% @doc Traverse through directory from top to bottom and execute Fun on each path.
%% Fun must have signature fun(Path::string(), Acc:list()) -> list().

-spec dir_traversal(Dir::name(),Fun::function()) ->
{ok,Result::list()} | {error, atom()}.

dir_traversal(Dir,Fun) -> dir_traversal(Dir,Fun,false).

% @spec (Dir::name(),Fun::function(),FilesOnly::boolean()) -> {ok, Result::list()} | {error, atom()}
% @equiv dir_traversal(Dir,Fun,false)
%
% @doc Traverse through directory from top to bottom and execute Fun on each path.
% If FilesOnly is true, then affects only files.
% Fun must have signature fun(Path::string(), Acc:list()) -> list().

-spec dir_traversal(Dir::name(),Fun::function(),FilesOnly::boolean()) ->
  {ok,Result::list()} | {error, atom()}.

dir_traversal(Dir,Fun,FilesOnly) ->
  dir_traversal(Dir,Fun,FilesOnly,[]).

%% Internal

dir_traversal([], _Fun, _FilesOnly, Acc) -> Acc;
dir_traversal([Path|Paths], Fun, FilesOnly, Acc) ->
  dir_traversal(Paths, Fun, FilesOnly,
    case filelib:is_dir(Path) of
      false -> Fun(Path,Acc);
      true ->
        {ok, Listing} = file:list_dir(Path),
        SubPaths = [filename:join(Path, Name) || Name <- Listing],
        dir_traversal(SubPaths, Fun, FilesOnly,
          case FilesOnly of
            true -> Acc;
            false -> Fun(Path,Acc)
          end)
    end).


del_paths([]) -> ok;

del_paths([Path|Paths]) ->
  case filelib:is_dir(Path) of
    true -> file:del_dir(Path);
    false -> file:delete(Path)
  end,
  del_paths(Paths).




%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

non_existing_file_returns_error_test() ->
  ?assertEqual({error, enoent},
    recursively_list_dir("UnUSuAalfIlEnaMe")),
  ok.

non_directory_input_returns_error_test() ->
  file:write_file("f1.test", <<"temp test file">>),
  ?assertEqual({error, enotdir},
    recursively_list_dir("f1.test")),
  file:delete("f1.test"),
  ok.

list_test_() ->
  {"List dir tests",
    {foreach,
      fun list_setup/0,
      fun list_cleanup/1,
      [
        fun list_simple_t/1,
        fun list_nofiles_t/1,
        fun list_filesonly_t/1
      ]

    }
  }.


del_test_() ->
  {"delete dir tests",
    {foreach,
      fun delete_setup/0,
      fun list_cleanup/1,
      [
        fun del_no_dir_t/1,
        fun del_simple_t/1
      ]

    }
  }.


list_simple_t(_) ->
  filelib:ensure_dir("a/b/c/"),
  file:write_file("a/b/f.test", <<"temp test file">>),
  [?_assertEqual({ok, ["a/b/f.test","a/b/c","a/b","a"]},
    recursively_list_dir("a"))].

list_nofiles_t(_) ->
  filelib:ensure_dir("a/b/c/"),
  [?_assertEqual({ok, ["a/b/c", "a/b", "a"]},recursively_list_dir("a"))].

list_filesonly_t(_) ->
  filelib:ensure_dir("a/b/f.test"),
  file:write_file("a/b/f.test", <<"hello">>),
  [?_assertEqual({ok, ["a/b/f.test"]},
    recursively_list_dir("a", true))].

list_setup() ->
  ok.

list_cleanup(_) ->
  file:delete("f1.test"),
  file:delete("a/b/f.test"),
  file:del_dir("a/b/c"),
  file:del_dir("a/b"),
  file:del_dir("a"),
  ok.

del_no_dir_t(_) ->
  [
    ?_assertEqual({error,enoent},recursively_del_dir("UnUSuAalfIlEnaMe")),
    ?_assertEqual({error,enotdir},recursively_del_dir("a/b/f.test"))
  ].

del_simple_t(_) ->
   [
    ?_assertEqual(ok,recursively_del_dir("a")),
    ?_assertNot(filelib:is_file("a/b"))
   ].


delete_setup() ->
  filelib:ensure_dir("a/b/c/"),
  file:write_file("a/b/f.test", <<"temp test file">>),
  ok.

-endif.
