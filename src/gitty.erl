-module(gitty).

-export([show/2, list/2]).

-include_lib("eunit/include/eunit.hrl").

-define(D(X), io:format("~p:~p ~240p~n", [?MODULE, ?LINE, X])).
-define(DBG(Fmt,X), io:format("~p:~p "++Fmt++"~n", [?MODULE, ?LINE| X])).


-define(OFS_DELTA, 6).
-define(REF_DELTA, 7).


-record(git, {
  path,
  indexes = []
}).

-record(index, {
  name,
  objects = []
}).

show(Git, RawPath) ->
  {ok, Git1, Tree, Path} = prepare_path(git(Git), RawPath),

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
      list_tree(Git1, Path, Tree);
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
  case (catch read_object(Git, SHA1)) of
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


git(Path) when is_list(Path) -> #git{path = Path};
git(#git{} = Git) -> Git.


prepare_path(Git, Path) when is_list(Path) ->
  prepare_path(Git, list_to_binary(Path));

prepare_path(#git{path = Repo} = Git, Path) when is_binary(Path) ->
  {SHA1, RPath} = case binary:split(Path, <<":">>) of
    [Branch, RealPath] when size(Branch) == 40 ->
      {Branch, RealPath};
    [Branch, RealPath] ->
      {read_file(filename:join([Repo, "refs/heads", Branch])), RealPath};
    [RealPath] ->
      {read_file(filename:join([Repo, "refs/heads/master"])), RealPath}
  end,
  {ok, Git1, commit, Head} = read_object(Git, SHA1),
  {ok, Git2, tree, Tree} = read_object(Git1, proplists:get_value(tree, Head)),
  {ok, Git2, Tree, RPath}.




lookup(Git, [Part|Parts], Tree) ->
  case lists:keyfind(Part, 1, Tree) of
    {Part,_,SHA1} ->
      case read_object(Git, SHA1) of
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



unhex(<<>>) -> <<>>;
unhex(<<"\n">>) -> <<>>;
unhex(<<H1,H2,Hex/binary>>) -> <<(list_to_integer([H1,H2], 16)), (unhex(Hex))/binary>>.

hex(<<>>) -> <<>>;
hex(<<H, Raw/binary>>) ->
  [H1,H2] = string:to_lower(lists:flatten(io_lib:format("~2.16.0B", [H]))),
  <<H1, H2, (hex(Raw))/binary>>.

read_file(Path) ->
  {ok,Bin} = file:read_file(Path),
  binary:replace(Bin, <<"\n">>, <<>>).



read_object(Git, SHA1hex) when length(SHA1hex) == 40 ->
  read_object(Git, list_to_binary(SHA1hex));

read_object(Git, SHA1) when is_binary(SHA1)->
  {ok, Git1, Type, Content} = read_raw_object(Git, SHA1),
  C = case Type of
    tree -> parse_tree(Content);
    commit -> parse_commit(Content);
    blob -> Content;
    _ -> Content
  end,
  {ok, Git1, Type, C}.


parse_tree(<<>>) ->
  [];
parse_tree(Content) ->
  [ModeName, <<SHA1:20/binary, Rest/binary>>] = binary:split(Content, <<0>>),
  [Mode, Name] = binary:split(ModeName, <<" ">>),
  SHA1hex = hex(SHA1),
  [{Name,Mode, hex(SHA1)}|parse_tree(Rest)].


parse_commit(Commit) ->
  [Header, Message] = binary:split(Commit, <<"\n\n">>),
  [<<"tree ",TreeHex/binary>>|_Headers] = binary:split(Header, <<"\n">>),
  [{tree,TreeHex},{message,Message}].


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
          [{hex(SHA), Offset} || <<Offset:32, SHA:20/binary>> <= ShaOnes]
      end,
      file:close(I),
      ?D({add_to_index,IndexName, Indexes}),
      Index = #index{name = IndexName, objects = Entries},
      {Index, Git#git{indexes = [Index|Indexes]}};
    #index{} = Index ->
      {Index, Git}
  end.




lookup_via_index(Git, [], _) ->
  {error, Git, enoent};

lookup_via_index(#git{path = Repo} = Git, [IndexFile|Indexes], SHA1) ->
  {#index{objects = Objects}, Git1} = load_index(Git, IndexFile),
  case proplists:get_value(SHA1, Objects) of
    undefined ->
      lookup_via_index(Git1, Indexes, SHA1);
    Offset ->
      {ok, P} = file:open(filename:join([Repo,"objects/pack", IndexFile++".pack"]), [binary,raw,read]),
      {ok, Type, Content} = unpack_object(P, Offset),

      file:close(P),
      % ?D({pack,SHA1, Type, Content}),
      {ok, Git1, Type, Content}
  end.


unpack_object(P, Offset) ->
  {ok, HeaderSize, TypeInt, Size, Bin} = case file:pread(P, Offset, 4096) of
    {ok, <<0:1, T:3, Size1:4, Bin_/binary>>} ->
      {ok, 1, T, Size1, Bin_};
    {ok, <<1:1, T:3, Size1:4, 0:1, Size2:7, Bin_/binary>>} ->
      {ok, 2, T, (Size2 bsl 4) bor Size1, Bin_};
    {ok, <<1:1, T:3, Size1:4, 1:1, Size2:7, 0:1, Size3:7, Bin_/binary>>} ->
      {ok, 3, T, (Size3 bsl 11) bor (Size2 bsl 4) bor Size1, Bin_}
  end,
  Type1 = unpack_type(TypeInt),
  % ?D({reading,SHA1,Type1,Offset,Size}),

  {ok, Type, Content} = if Type1 == ofs_delta orelse Type1 ==ref_delta ->
    {ok, Type_, C} = read_delta_from_file(P, Offset, Offset + HeaderSize, Type1, Size),
    {ok, Type_, C};
  true ->
    C = read_zip_from_file(P, Offset+4096, Size, Bin),
    {ok, Type1, C}
  end,
  {ok, Type, Content}.


read_zip_from_file(F, Offset, Size, Zip) ->
  Z = zlib:open(),
  zlib:inflateInit(Z),
  Bin = iolist_to_binary(zlib:inflate(Z, Zip)),
  case Bin of
    <<Reply:Size/binary, _/binary>> -> zlib:close(Z), Reply;
    _ -> read_zip_from_file(F, Offset, Size - size(Bin), Z, Bin)
  end.

read_zip_from_file(F, Offset, Size, Z, Acc) ->
  {ok, Zip} = file:pread(F, Offset, 4096),
  Bin = iolist_to_binary(zlib:inflate(Z, Zip)),
  case Bin of
    <<Reply:Size/binary, _/binary>> -> zlib:close(Z), <<Acc/binary, Reply/binary>>;
    _ -> read_zip_from_file(F, Offset + 4096, Size - size(Bin), Z, <<Acc/binary, Bin/binary>>)
  end.


read_delta_from_file(F, OrigOffset, Offset, DeltaType, Size) ->
  {Shift, BackOffset, Bin} = case file:pread(F, Offset, 1024) of
    {ok, <<SHA1:40/binary, Bin_/binary>>} when DeltaType == ref_delta ->
      throw({unimplemented,{ref_delta,SHA1}});
    {ok, <<0:1, Base1:7,Bin_/binary>>} -> 
      {1, Base1, Bin_};
    {ok, <<1:1, Base1:7, 0:1, Base2:7, Bin_/binary>>} ->
      {2, ((Base1 + 1) bsl 7) bor Base2, Bin_};
    {ok, <<1:1, Base1:7, 1:1, Base2:7, 0:1, Base3:7, Bin_/binary>>} ->
      {3, ((Base1 + 1) bsl 7) bor ((Base2 + 1) bsl 7) bor Base3, Bin_}
  end,
  BaseOffset = OrigOffset - BackOffset,
  {ok, Type, Base} = unpack_object(F, BaseOffset),

  Delta = read_zip_from_file(F, Offset + Shift, Size, Bin),

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
  {Int bor (I bsl Shift), Delta1}.
  

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

unzip(Zip) ->
  % ?D({inflate, Zip}),
  % Z = zlib:open(),
  % Raw = zlib:inflate(Z, Zip),
  % zlib:close(Z).
  zlib:uncompress(Zip).



read1_test() ->
  ?assertEqual({ok, <<"80f136f\n">>}, show("test/fixtures/dot_git", "test/fixtures/rev_parse")).
