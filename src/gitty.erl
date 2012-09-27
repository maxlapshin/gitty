-module(gitty).

-export([show/2, list/2]).
-export([fixture/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").




show(Git, Path) when is_list(Path) ->
  show(Git, list_to_binary(Path));

show(Git, Path) when is_list(Git) ->
  show(git_repo:init(Git), Path);

show(Git, Path) ->
  case re:run(Path, ":") of
    {match, _} -> show_file_by_path(Git, Path);
    nomatch when size(Path) == 40 -> git_repo:read_object(Git, Path);
    _ -> {error, enoent}
  end.

show_file_by_path(Git, RawPath) ->
  {ok, Git1, Tree, Path} = prepare_path(git_repo:init(Git), RawPath),

  {ok, Git2, Type, Blob} = case Path of
    <<>> -> 
      {ok, Git1, tree, Tree} ;
    _ ->
      Parts = binary:split(Path, <<"/">>, [global]),
      lookup(Git1, Parts, Tree)
  end,

  {ok, Git2, Type, Blob}.



list(Git, Path) when is_list(Path) ->
  list(Git, list_to_binary(Path));

list(Git, Path) ->
  case show(Git, Path) of
    {ok, Git1, tree, Tree} ->
      Dir = case binary:split(Path, <<":">>) of
        [D] -> D;
        [_Branch, D] -> D
      end,
      list_tree(Git1, Dir, Tree);
    {ok, _, _, _} ->
      {error, enotdir};
    {error, Error} ->
      {error, Error}
  end.


list_tree(Git, Path, [{Name,Mode,SHA1}|Tree]) ->
  FullName = case Path of
    <<>> -> Name;
    _ -> <<Path/binary, "/", Name/binary>>
  end,
  case (catch git_repo:read_object(Git, SHA1)) of
    {ok, Git1, blob, _Blob} ->
      {ok, Git2, Content} = list_tree(Git1,Path,Tree),
      {ok, Git2, [{FullName, Mode, SHA1}|Content]};
    {ok, Git1, tree, InnerTree} ->
      {ok, Git2, Content1} = list_tree(Git1, FullName, InnerTree),
      {ok, Git3, Content2} = list_tree(Git2,Path,Tree),
      {ok, Git3, Content1 ++ Content2};
    {unimplemented,Error} ->
      ?D({unimplemented,Error, Path, Name, SHA1}),
      list_tree(Git, Path, Tree)
  end;

list_tree(Git, _, []) ->
  {ok, Git, []}.



prepare_path(Git, Path) when is_list(Path) ->
  prepare_path(Git, list_to_binary(Path));

prepare_path(Git, Path) when is_binary(Path) ->
  {ok, Git1, Refs} = git_repo:refs(Git),
  {SHA1, RealPath} = case binary:split(Path, <<":">>) of
    [Branch, Path_] when size(Branch) == 40 ->
      {Branch, Path_};
    [Branch, Path_] ->
      {proplists:get_value(Branch, Refs), Path_};
    [Path_] ->
      {proplists:get_value(<<"master">>, Refs), Path_}
  end,
  {ok, Git2, commit, Head} = git_repo:read_object(Git1, SHA1),
  {ok, Git3, tree, Tree} = git_repo:read_object(Git2, proplists:get_value(tree, Head)),
  {ok, Git3, Tree, RealPath}.




lookup(Git, [Part|Parts], Tree) ->
  case lists:keyfind(Part, 1, Tree) of
    {Part,_,SHA1} ->
      case git_repo:read_object(Git, SHA1) of
        {ok, Git1, Type, Blob} when length(Parts) == 0 ->
          {ok, Git1, Type, Blob};
        {ok, Git1, tree, Tree1} when length(Parts) > 0 ->
          lookup(Git1, Parts, Tree1);
        {ok, _, _, _} ->
          {error, eisdir};
        {error, Error} ->
          {error, Error}
      end;
    false ->
      {error, enoent}
  end.





test_fixture_dir() ->
  code:lib_dir(gitty, test).

fixture(Name) ->
  filename:join(test_fixture_dir(), Name).


double_check(Fixture, SHA1) ->
  Reply1 = show(fixture(Fixture), SHA1),
  ?assertMatch({ok, _, blob, _}, Reply1),
  {ok, _, blob, Blob1} = Reply1,
  Cmd = binary_to_list(iolist_to_binary(["git --bare --git-dir " ,fixture(Fixture), " show ", SHA1])),
  Blob2 = iolist_to_binary(os:cmd(Cmd)),
  ?assertEqual(Blob2, Blob1).

show_test() ->
  double_check("dot_git", "eeccc934cad8bb74624ed388988fe79c26e6900d"), % in raw file
  double_check("dot_git", "6fc18f69e9b74eafb4a58a6fcbd218adc0d80c36"), % blob in pack
  double_check("dot_git", "d8c6431e0a82b6b1bd4db339ee536f8bd4099c8f"), % ofs_delta in pack
  % double_check("dot_git", "6fc18f69e9b74eafb4a58a6fcbd218adc0d8bbaa"), % unexistent
  ok.

list_test() ->
  ?assertMatch({ok, _, [
    {<<"README">>, <<"100644">>,<<"cd0d7186badd8fcfbfbdf35a0b9f2c8aaf465e77">>},
    {<<"src/script.sh">>, <<"100644">>,<<"5172f17ee080fb2e922d2b44c031aa41845694ee">>}
  ]}, list(fixture("small_git"), "master:")),
  ?assertMatch({ok, _, [
    {<<"src/script.sh">>, <<"100644">>,<<"5172f17ee080fb2e922d2b44c031aa41845694ee">>}
  ]}, list(fixture("small_git"), "master:src")),
  ok.

read1_test() ->
  ?assertMatch({ok, _, blob, <<"80f136f\n">>}, show(fixture("dot_git"), "nonpack:test/fixtures/rev_parse")).



