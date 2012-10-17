-module(git_repo).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/1, path/1, read_object/2]).
-export([refs/1]).
-export([commit_files/4]).

-export([put_raw_object/3]).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(OFS_DELTA, 6).
-define(REF_DELTA, 7).


-record(git, {
  path,
  refs,
  indexes = []
}).

-record(index, {
  name,
  objects = []
}).

init(Path) when is_list(Path) ->
  #git{path = Path};

init(#git{} = Git) ->
  Git.

path(#git{path = Path}) -> Path.



refs(#git{refs = Refs} = Git) when is_list(Refs) ->
  {ok, Git, Refs};

refs(#git{path = Repo, refs = undefined} = Git) ->
  DirectRefs = lists:flatmap(fun(Path) ->
    case read_file(Path) of
      <<SHA1:40/binary>> -> [{list_to_binary(filename:basename(Path)), SHA1}];
      _ -> []
    end
  end, filelib:wildcard(filename:join([Repo, "refs/heads/*"]))),
  PackedRefs = case file:read_file(filename:join(Repo, "packed-refs")) of
    {ok, Packs} ->
      lists:flatmap(fun(Row) ->
        case re:run(Row, "^([\\da-fA-F]{40}) refs/heads/([\\w/]+)$", [{capture,all_but_first,binary}]) of
          {match, [SHA1, Ref]} -> [{Ref, SHA1}];
          nomatch -> []
        end
      end, binary:split(Packs, <<"\n">>, [global]));
    {error, _} ->
      []
  end,
  Refs = lists:sort(DirectRefs ++ PackedRefs),
  {ok, Git#git{refs = Refs}, Refs}.



read_file(Path) ->
  {ok,Bin} = file:read_file(Path),
  binary:replace(Bin, <<"\n">>, <<>>).




read_object(Git, SHA1hex) when length(SHA1hex) == 40 ->
  read_object(Git, list_to_binary(SHA1hex));

read_object(Git, SHA1) when is_binary(SHA1)->
  {ok, Git1, Type, Content} = try read_raw_object(Git, SHA1)
  catch
    Class:Error -> erlang:raise(Class, {gitty_error, SHA1, Error}, erlang:get_stacktrace())
  end,
  C = case Type of
    tree -> parse_tree(Content);
    commit -> parse_commit(Content);
    blob -> Content;
    _ -> Content
  end,
  {ok, Git1, Type, C}.


unhex(<<>>) -> <<>>;
unhex(<<"\n">>) -> <<>>;
unhex(<<H1,H2,Hex/binary>>) -> <<(list_to_integer([H1,H2], 16)), (unhex(Hex))/binary>>.

hex(Binary) when is_binary(Binary) ->
  iolist_to_binary([string:to_lower(lists:flatten(io_lib:format("~2.16.0B", [H]))) || <<H>> <= Binary]).



parse_tree(<<>>) ->
  [];
parse_tree(Content) ->
  [ModeName, <<SHA1:20/binary, Rest/binary>>] = binary:split(Content, <<0>>),
  [Mode, Name] = binary:split(ModeName, <<" ">>),
  [{Name,Mode, hex(SHA1)}|parse_tree(Rest)].


parse_commit(Commit) ->
  [Header, Message] = binary:split(Commit, <<"\n\n">>),
  Content = [{message,Message}|parse_commit_headers(binary:split(Header, <<"\n">>))],
  Content.

parse_commit_headers([<<"tree ", TreeHex/binary>>|Headers]) ->
  [{tree,TreeHex}|parse_commit_headers(Headers)];
parse_commit_headers([<<"parent ", Parent/binary>>|Headers]) ->
  [{parent,Parent}|parse_commit_headers(Headers)];
parse_commit_headers([<<"author ", Author/binary>>|Headers]) ->
  [{author,Author}|parse_commit_headers(Headers)];
parse_commit_headers([<<"commiter ", Commiter/binary>>|Headers]) ->
  [{commiter,Commiter}|parse_commit_headers(Headers)];
parse_commit_headers([_|Headers]) ->
  parse_commit_headers(Headers);
parse_commit_headers([]) ->
  [].



read_raw_object(#git{path = Repo} = Git, <<Prefix:2/binary, Postfix/binary>> = SHA1hex) when size(SHA1hex) == 40 ->
  DirectPath = filename:join([Repo, "objects", Prefix, Postfix]),
  case file:read_file(DirectPath) of
    % git-ruby/internal/loose.rb#legacy_loose_object?
    {ok, <<16#78, W2, _/binary>> = Zip} when (16#7800 bor W2) rem 31 == 0 -> 
      [Header, Content] = binary:split(unzip(Zip), <<0>>),
      [Type, Size] = binary:split(Header, <<" ">>),
      unpack_size(Size) == size(Content) orelse erlang:error({invalid_size, unpack_size(Size), size(Content)}),
      {ok, Git, unpack_type(Type), Content};
    % git-ruby/internal/loose.rb#unpack_object_header_gently
    % {ok, <<0:1, _Type:3, _Size1:4, Zip/binary>>} -> unzip(Zip);
    % {ok, <<1:1, _Type:3, _Size1:4, 0:1, _Size2:7, Zip/binary>>} -> unzip(Zip);
    % {ok, <<1:1, _Type:3, _Size1:4, 1:1, _Size2:7, 1:1, _Size3:7, Zip/binary>>} -> unzip(Zip);

    {error, enoent} ->
      read_packed_object(Git, SHA1hex)
  end.



read_packed_object(#git{path = Repo} = Git, SHA1hex) -> 
  Indexes = [filename:basename(Path, ".idx") || Path <- filelib:wildcard(filename:join(Repo, "objects/pack/*.idx"))],
  case lookup_via_index(Git, Indexes, SHA1hex) of
    {ok, Git1, Type, Content} ->
      {ok, Git1, Type, Content};
    {error, Git1, Error} ->
      {error, Git1, Error}
  end.

load_index(#git{path = Repo, indexes = Indexes} = Git, IndexName) ->
  case lists:keyfind(IndexName, #index.name, Indexes) of
    false ->
      {ok, I} = file:open(filename:join([Repo, "objects/pack", IndexName++".idx"]), [raw,binary,read]),
      {ok, <<Sig:4/binary, Ver:32>>} = file:read(I, 8),
      Version = case Sig of
        <<8#377, "tOc">> when Ver == 2 -> 2;
        _ -> 1
      end,
      GlobalOffset = case Version of 1 -> 0; 2 -> 8 end,
      FanOutCount = 256,
      IdxOffsetSize = 4,
      {ok, BinOffsets} = file:pread(I, GlobalOffset, FanOutCount*IdxOffsetSize),
      IndexOffset = GlobalOffset + FanOutCount*IdxOffsetSize,
      Offsets = [0] ++ [Offset || <<Offset:32>> <= BinOffsets],
      EntryCount = lists:last(Offsets),
      Entries = case Version of
        1 -> 
          {ok, ShaOnes} = file:pread(I, IndexOffset, 24*EntryCount),
          [{hex(SHA), Offset} || <<Offset:32, SHA:20/binary>> <= ShaOnes];
        2 ->
          {ok, ShaOnes} = file:pread(I, IndexOffset, 20*EntryCount),
          {ok, Offsets1} = file:pread(I, IndexOffset + 24*EntryCount, 4*EntryCount),
          lists:zip([hex(SHA) || <<SHA:20/binary>> <= ShaOnes], [Offset || <<Offset:32>> <= Offsets1])
      end,
      file:close(I),
      Index = #index{name = IndexName, objects = Entries},
      {Index, Git#git{indexes = [Index|Indexes]}};
    #index{} = Index ->
      {Index, Git}
  end.




lookup_via_index(Git, [], _) ->
  {error, Git, enoent};

lookup_via_index(#git{path = Repo} = Git, [IndexFile|Indexes], SHA1) ->
  {#index{objects = Objects} = Index, Git1} = load_index(Git, IndexFile),
  case proplists:get_value(SHA1, Objects) of
    undefined ->
      lookup_via_index(Git1, Indexes, SHA1);
    Offset ->
      {ok, P} = file:open(filename:join([Repo,"objects/pack", IndexFile++".pack"]), [binary,raw,read]),
      {ok, Type, Content} = unpack_object(P, Offset, Index),

      file:close(P),
      {ok, Git1, Type, Content}
  end.


unpack_object(P, Offset, Index) ->
  ReadAhead = 4096,
  {ok, HeaderSize, TypeInt, Size, Bin} = case file:pread(P, Offset, ReadAhead) of
    {ok, <<0:1, T:3, Size1:4, Bin_/binary>>} ->
      {ok, 1, T, Size1, Bin_};
    {ok, <<1:1, T:3, Size1:4, 0:1, Size2:7, Bin_/binary>>} ->
      {ok, 2, T, (Size2 bsl 4) bor Size1, Bin_};
    {ok, <<1:1, T:3, Size1:4, 1:1, Size2:7, 0:1, Size3:7, Bin_/binary>>} ->
      {ok, 3, T, (Size3 bsl 11) bor (Size2 bsl 4) bor Size1, Bin_};
    {ok, <<1:1, T:3, Size1:4, 1:1, Size2:7, 1:1, Size3:7, 0:1, Size4:7, Bin_/binary>>} ->
      {ok, 4, T, (Size4 bsl 18) bor (Size3 bsl 11) bor (Size2 bsl 4) bor Size1, Bin_}
  end,
  Type1 = unpack_type(TypeInt),

  {ok, Type, Content} = if Type1 == ofs_delta orelse Type1 ==ref_delta ->
    {ok, Type_, C} = read_delta_from_file(P, Offset, Offset + HeaderSize, Type1, Size, Index),
    {ok, Type_, C};
  true ->
    C = read_zip_from_file(P, Offset+ReadAhead, Size, Bin),
    {ok, Type1, C}
  end,
  {ok, Type, Content}.


read_zip_from_file(F, Offset, Size, Zip) ->
  Z = zlib:open(),
  try read_zip_from_file0(F, Offset, Size, Zip, Z)
  after
    zlib:inflateEnd(Z),
    zlib:close(Z)
  end.

read_zip_from_file0(F, Offset, Size, Zip, Z) ->
  zlib:inflateInit(Z),
  Bin = iolist_to_binary(zlib:inflate(Z, Zip)),
  case Bin of
    <<Reply:Size/binary, _/binary>> -> Reply;
    _ -> read_zip_from_file(F, Offset, Size - size(Bin), Z, Bin)
  end.

read_zip_from_file(F, Offset, Size, Z, Acc) ->
  {ok, Zip} = file:pread(F, Offset, 4096),
  Bin = iolist_to_binary(zlib:inflate(Z, Zip)),
  case Bin of
    <<Reply:Size/binary, _/binary>> -> <<Acc/binary, Reply/binary>>;
    _ -> read_zip_from_file(F, Offset + 4096, Size - size(Bin), Z, <<Acc/binary, Bin/binary>>)
  end.


read_delta_from_file(F, OrigOffset, Offset, DeltaType, Size, #index{objects = Objects} = Index) ->
  {Shift, BackOffset, Bin} = case file:pread(F, Offset, 1024) of
    {ok, <<SHA1:20/binary, Bin_/binary>>} when DeltaType == ref_delta ->
      {20, OrigOffset - proplists:get_value(hex(SHA1), Objects), Bin_};
    {ok, <<0:1, Base1:7,Bin_/binary>>} -> 
      {1, Base1, Bin_};
    {ok, <<1:1, Base1:7, 0:1, Base2:7, Bin_/binary>>} ->
      {2, ((Base1 + 1) bsl 7) bor Base2, Bin_};
    {ok, <<1:1, Base1:7, 1:1, Base2:7, 0:1, Base3:7, Bin_/binary>>} ->
      {3, ((Base1 + 1) bsl 14) bor ((Base2 + 1) bsl 7) bor Base3, Bin_};
    {ok, <<1:1, Base1:7, 1:1, Base2:7, 1:1, Base3:7, 0:1, Base4:7, Bin_/binary>>} ->
      {4, ((Base1 + 1) bsl 21) bor ((Base2 + 1) bsl 14) bor ((Base3 + 1) bsl 7) bor Base4, Bin_}
  end,
  BaseOffset = OrigOffset - BackOffset,
  {ok, Type, Base} = unpack_object(F, BaseOffset, Index),

  Delta = read_zip_from_file(F, Offset + Shift + size(Bin), Size, Bin),

  {ok, Type, patch_delta(Base, Delta)}.


patch_delta(Base, Delta) ->
  {SrcSize, Delta1} = var_int(Delta),
  {DestSize, Delta2} = var_int(Delta1),
  SrcSize == size(Base) orelse throw({broken_delta,SrcSize,DestSize}),
  Patched = apply_patch(Delta2, Base),
  iolist_to_binary(Patched).

apply_patch(<<0:1, Count:7, Data:Count/binary, Delta/binary>>, Base) ->
  [Data|apply_patch(Delta, Base)];

apply_patch(<<1:1, SizeFlag:3, OffsetFlag:4,Delta/binary>>, Base) ->
  {Offset, Delta1} = read_flagged_int(OffsetFlag, Delta, 0),
  {Size, Delta2} = read_flagged_int(SizeFlag, Delta1, 0),

  <<_:Offset/binary, Data:Size/binary, _/binary>> = Base,
  [Data|apply_patch(Delta2, Base)];

apply_patch(<<>>, _) ->
  [].

read_flagged_int(0, Delta, _Shift) -> {0, Delta};
read_flagged_int(Flag, <<I, Delta/binary>>, Shift) when Flag band 1 == 1 ->
  {Int, Delta1} = read_flagged_int(Flag bsr 1, Delta, Shift + 8),
  {Int bor (I bsl Shift), Delta1};
read_flagged_int(Flag, Delta, Shift) when Flag band 1 == 0 ->
  read_flagged_int(Flag bsr 1, Delta, Shift + 8).
  

  % throw({unimplemented,ofs_delta}).


var_int(<<0:1, I1:7, Rest/binary>>) -> {I1, Rest};
var_int(<<1:1, I1:7, 0:1, I2:7, Rest/binary>>) -> {(I2 bsl 7 ) bor I1, Rest};
var_int(<<1:1, I1:7, 1:1, I2:7, 0:1, I3:7, Rest/binary>>) -> {(I3 bsl 14) bor (I2 bsl 7 ) bor I1, Rest}.

unpack_type(1) -> commit;
unpack_type(2) -> tree;
unpack_type(3) -> blob;
unpack_type(4) -> tag;
unpack_type(6) -> ofs_delta;
unpack_type(7) -> ref_delta;

unpack_type(<<"blob">>) -> blob;
unpack_type(<<"commit">>) -> commit;
unpack_type(<<"tree">>) -> tree;
unpack_type(<<"tag">>) -> tag.

unpack_size(Bin) -> list_to_integer(binary_to_list(Bin)).


put_raw_object(Git, Type, Content) when 
  (Type == commit orelse Type == tree orelse Type == blob orelse Type == tag) ->
  #git{path = Repo} = Git1 = init(Git),
  Header = io_lib:format("~s ~B", [Type, iolist_size(Content)]),
  Store = [Header, 0, Content],
  SHA1 = <<Prefix:2/binary, Postfix/binary>> = hex(crypto:sha(Store)),
  Path = filename:join([Repo, "objects", Prefix, Postfix]),
  filelib:ensure_dir(Path),
  file:write_file(Path, zlib:compress(Store)),
  {ok, Git1, SHA1}.


write_tree(Tree) ->
  iolist_to_binary([<<Mode/binary, " ", Name/binary, 0, (unhex(SHA1))/binary>> || {Name, Mode, SHA1} <- Tree]).



add_to_index(Git1, Path, Mode, BlobSha) when is_list(Path) ->
  add_to_index(Git1, list_to_binary(Path), Mode, BlobSha);

add_to_index(Git1, Path1, Mode, BlobSha) ->
  {ok, Git2, Type, _} = read_raw_object(Git1, BlobSha),
  {ok, Git3, Refs} = refs(Git2),
  Type == blob orelse throw({cant_add_non_blob,BlobSha,Type}),
  {ParentCommit, RealPath} = case binary:split(Path1, <<":">>) of
    [Branch, Path_] when size(Branch) == 40 ->
      {Branch, Path_};
    [Branch, Path_] ->
      {proplists:get_value(Branch, Refs), Path_};
    [Path_] ->
      {proplists:get_value(<<"master">>, Refs), Path_}
  end,
  {ok, Git4, commit, OldHead} = read_object(Git3, ParentCommit),
  OldTreeSha = proplists:get_value(tree, OldHead),
  add_to_tree(Git4, OldTreeSha, RealPath, Mode, BlobSha).

add_to_tree(Git1, OldTreeSha, Path, Mode, BlobSha) when is_list(Path) ->
  add_to_tree(Git1, OldTreeSha, list_to_binary(Path), Mode, BlobSha);

add_to_tree(Git1, OldTreeSha, Path, Mode, BlobSha) when is_list(Mode) ->
  add_to_tree(Git1, OldTreeSha, Path, list_to_binary(Mode), BlobSha);

add_to_tree(Git1, OldTreeSha, Path, Mode, BlobSha) when is_list(BlobSha) ->
  add_to_tree(Git1, OldTreeSha, Path, Mode, list_to_binary(BlobSha));

add_to_tree(Git1, OldTreeSha, Path, Mode, BlobSha) ->
  Parts = binary:split(Path, <<"/">>, [global]),
  {ok, Git2, NewTreeSha} = walk_down_tree(Git1, OldTreeSha, Parts, Mode, BlobSha),
  {ok, Git2, NewTreeSha}.


walk_tree(Git, undefined) ->
  {ok, Git, []};
walk_tree(Git, SHA1) ->
  {ok, Git2, tree, Tree} = read_object(Git, SHA1), {ok, Git2, Tree}.


walk_down_tree(Git1, OldTreeSha, [Path], Mode, BlobSha) ->
  {ok, Git2, OldTree} = walk_tree(Git1, OldTreeSha),
  NewTree = lists:keystore(Path, 1, OldTree, {Path, Mode, BlobSha}),
  {ok, Git3, NewTreeSha} = put_raw_object(Git2, tree, write_tree(NewTree)),
  {ok, Git3, NewTreeSha};

walk_down_tree(Git1, OldTreeSha, [Path|Parts], Mode, BlobSha) ->
  {ok, Git2, OldTree} = walk_tree(Git1, OldTreeSha),
  NextSha = case lists:keyfind(Path, 1, OldTree) of
    {Path, <<"40000">>, NextSha_} ->
      NextSha_;
    {Path, BadMode, _} ->
      throw({cant_add,Path, BadMode, Parts});
    false ->
      undefined
  end,
  {ok, Git3, NewSha} = walk_down_tree(Git2, NextSha, Parts, Mode, BlobSha),
  NewTree = lists:keystore(Path, 1, OldTree, {Path, <<"40000">>, NewSha}),
  {ok, Git4, NewTreeSha} = put_raw_object(Git3, tree, write_tree(NewTree)),
  {ok, Git4, NewTreeSha}.



to_b(List) when is_list(List) -> list_to_binary(List);
to_b(Binary) when is_binary(Binary) -> Binary;
to_b(undefined) -> undefined.


put_raw_commit(Git1, Parent, TreeSha, Options) ->
  Message = to_b(proplists:get_value(message, Options, "default commit message")),
  Author = to_b(proplists:get_value(author, Options, "Gitty <gitty@maxidoors.ru>")),
  Commiter = to_b(proplists:get_value(commiter, Options, Author)),
  {Mega, Sec, _} = erlang:now(),
  UTC = list_to_binary(integer_to_list(Mega*1000000 + Sec)),
  Content = <<"tree ", TreeSha/binary, "\n",
    "parent ", Parent/binary, "\n",
    "author ", Author/binary, " ", UTC/binary, " +0000\n",
    "commiter ", Commiter/binary, " ", UTC/binary, " +0000\n",
    "\n",
    Message/binary>>,
  {ok, Git2, CommitSha} = put_raw_object(Git1, commit, Content),
  {ok, Git2, CommitSha}.



commit_files(Git1, Head_, Files, Options) ->
  Head = to_b(Head_),
  {ok, Git2, Refs} = refs(init(Git1)),
  {Head, ParentCommit} = lists:keyfind(Head, 1, Refs),

  {ok, Git3, commit, OldHead} = read_object(Git2, ParentCommit),
  OldTreeSha = proplists:get_value(tree, OldHead),

  {ok, Git4, NewTreeSha} = lists:foldl(fun({Path,Mode,Content}, {ok, Git1_, OldTree}) ->
    {ok, Git1_1, BlobSha} = put_raw_object(Git1_, blob, Content),
    add_to_tree(Git1_1, OldTree, Path, Mode, BlobSha)
  end, {ok, Git3,OldTreeSha}, Files),

  {ok, #git{refs = Refs1, path = Path} = Git5, CommitSha} = put_raw_commit(Git4, ParentCommit, NewTreeSha, Options),
  Refs2 = lists:keystore(Head, 1, Refs1, {Head, CommitSha}),
  Git6 = Git5#git{refs = Refs2},
  PackedRefs = filename:join([Path, "packed-refs"]),
  case file:read_file(PackedRefs) of
    {ok, Bin} ->
      Lines1 = binary:split(Bin, <<"\n">>, [global]),
      Lines2 = replace_packed_ref(Head, CommitSha, Lines1),
      file:write_file(PackedRefs, [[L,"\n"] || L <- Lines2]);
    {error, enoent} ->      
      RefPath = filename:join([Path, "refs/heads", Head]),
      filelib:ensure_dir(RefPath),
      file:write_file(RefPath, CommitSha)
  end,
  {ok, Git6}.

replace_packed_ref(Head, SHA1, [<<_OldSHA:40/binary, " refs/heads/", Head/binary>>|Lines]) ->
  [<<SHA1:40/binary, " refs/heads/", Head/binary>>|Lines];
replace_packed_ref(Head, SHA1, []) ->
  [<<SHA1:40/binary, " refs/heads/", Head/binary>>];
replace_packed_ref(Head, SHA1, [<<>>]) ->
  replace_packed_ref(Head, SHA1, []);
replace_packed_ref(Head, SHA1, [Line|Lines]) ->
  [Line|replace_packed_ref(Head, SHA1, Lines)].



unzip(Zip) ->
  % Z = zlib:open(),
  % Raw = zlib:inflate(Z, Zip),
  % zlib:close(Z).
  zlib:uncompress(Zip).


hex_test() ->
  ?assertEqual(<<"0102030405060708090a0b0c0d0e0f1011121314">>, hex(<<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20>>)).

unhex_test() ->
  ?assertEqual(<<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20>>, unhex(<<"0102030405060708090a0b0c0d0e0f1011121314">>)).


fixture(Path) ->
  init(gitty:fixture(Path) ).

refs_test() ->
  ?assertMatch({ok, _, [
    {<<"master">>, <<"ca8a30f5a7f0f163bbe3b6f0abf18a6c83b0687a">>},
    {<<"nonpack">>, <<"ca8a30f5a7f0f163bbe3b6f0abf18a6c83b0687a">>},
    {<<"test/chacon">>, <<"ca8a30f5a7f0f163bbe3b6f0abf18a6c83b0687a">>},
    {<<"test/master">>, <<"2d3acf90f35989df8f262dc50beadc4ee3ae1560">>},
    {<<"testing">>, <<"2d3acf90f35989df8f262dc50beadc4ee3ae1560">>}
  ]}, refs(fixture("dot_git"))),
  ?assertMatch({ok, _, [{<<"master">>, <<"b68b3f9327206f81dd9bd1c4347bacaad05bc09f">>}]}, refs(fixture("small_git"))),
  ?assertMatch({ok, _, [{<<"master">>, <<"6a65f190f18b3e8b5e64daab193a944ba7897a62">>}]}, refs(fixture("v2_git"))).


read_raw_object1_test() ->
  ?assertMatch({ok, _, blob, _}, read_raw_object(init(gitty:fixture("erlydoc_git")), <<"3869e362cd6e2def2fa6d1164551765b1cca7aef">>)).


read_raw_object2_test() ->
  ?assertMatch({ok, _, blob, _}, read_raw_object(init(gitty:fixture("erlydoc_git")), <<"8652f8a1f65788ddb713f01bc282e23ecfcb6026">>)).



put_raw_object_test_() ->
  {foreach,
  fun() ->
    Tempdir = gitty:fixture("temp_git"),
    os:cmd("rm -rf "++ Tempdir),
    os:cmd("cp -rf "++gitty:fixture("dot_git")++" "++Tempdir),
    Tempdir
  end,
  fun(Tempdir) ->
    os:cmd("rm -rf "++ Tempdir)
  end,
  [
     {with, [fun test_put_raw_object/1]}
    ,{with, [fun test_make_tree/1]}
    ,{with, [fun test_make_new_dir/1]}
    ,{with, [fun test_raw_commit_files/1]}
    ,{with, [fun test_commit_files/1]}
  ]
  }.

test_put_raw_object(Tempdir) ->
  ?assertMatch({ok, _, <<"e7a891c5b186c734bc98fd2af8946417b80d4f0c">>}, put_raw_object(Tempdir, blob, "simple blob")),
  ?assertMatch({ok, _}, file:read_file_info(Tempdir++"/objects/e7/a891c5b186c734bc98fd2af8946417b80d4f0c")),
  ok.


test_make_tree(Tempdir) ->
  {ok, Git1, BlobSha} = put_raw_object(Tempdir, blob, "simple blob"),
  Result = add_to_index(Git1, "master:lib/grit/git-ruby/file.txt", "100644", BlobSha),
  ?assertMatch({ok, _, _}, Result),
  {ok, Git2, SHA1} = Result,
  ?assertMatch({ok, _, tree, _}, read_object(Git2, SHA1)),
  {ok, _, tree, Tree} = read_object(Git2, SHA1),
  ?assertMatch({<<"lib">>, <<"40000">>, _}, lists:keyfind(<<"lib">>, 1, Tree)),
  ok.

test_make_new_dir(Tempdir) ->
  {ok, Git1, BlobSha} = put_raw_object(Tempdir, blob, "simple blob"),
  Result = add_to_index(Git1, "master:new1/new2/file.txt", "100644", BlobSha),
  ?assertMatch({ok, _, _}, Result),
  ok.




test_raw_commit_files(Tempdir) ->
  {ok, Git1, BlobSha} = put_raw_object(Tempdir, blob, "simple blob"),
  {ok, Git2, TreeSha1} = add_to_index(Git1, "master:lib/grit/git-ruby/file.txt", "100644", BlobSha),
  {ok, #git{refs = Refs} = Git3, TreeSha2} = add_to_tree(Git2, TreeSha1, "lib/grit/a.txt", "100644", BlobSha),
  Parent = proplists:get_value(<<"master">>, Refs),
  Result = put_raw_commit(Git3, Parent, TreeSha2, [{message,"Test commit"},{author, "author@host"}]),
  ?assertMatch({ok, _, _}, Result),
  ok.

test_commit_files(Tempdir) ->
  ?assertMatch({ok, _}, commit_files(Tempdir, "master", [
    {"lib/grit/git-ruby/file.txt", "100644", "simple blob"},
    {"lib/grit/file5.txt", "100644", "another blob"},
    {"new1/new2/file2.txt", "100644", "new blob"}
  ], [{message, "test commit"},{author,"test@commiter"}])),
  ?assertMatch({ok, _, blob, <<"simple blob">>}, gitty:show(Tempdir, "master:lib/grit/git-ruby/file.txt")),
  ?assertMatch({ok, _, blob, <<"another blob">>}, gitty:show(Tempdir, "master:lib/grit/file5.txt")),
  ?assertMatch({ok, _, blob, <<"new blob">>}, gitty:show(Tempdir, "master:new1/new2/file2.txt")),
  ok.




