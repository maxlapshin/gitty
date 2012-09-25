-module(gitty).

-export([show/2, list/2]).

-include_lib("eunit/include/eunit.hrl").

-define(D(X), io:format("~p:~p ~240p~n", [?MODULE, ?LINE, X])).
-define(DBG(Fmt,X), io:format("~p:~p "++Fmt++"~n", [?MODULE, ?LINE| X])).


show(Git, RawPath) ->
  {Tree, Path} = prepare_path(Git, RawPath),
  
  Parts = binary:split(Path, <<"/">>, [global]),

  {ok, Type, Blob} = lookup(Git, Parts, Tree),

  {ok, Type, Blob}.



list(Git, Path) when is_list(Path) ->
  list(Git, list_to_binary(Path));

list(Git, Path) ->
  case show(Git, Path) of
    {ok, tree, Tree} ->
      list_tree(Git, Path, Tree);
    {ok, _, _} ->
      {error, enotdir};
    {error, Error} ->
      {error, Error}
  end.


list_tree(Git, Path, [{Name,Mode,SHA1}|Tree]) ->
  FullName = <<Path/binary, "/", Name/binary>>,
  case read_object(Git, SHA1) of
    {ok, blob, _Blob} ->
      [{FullName, Mode, SHA1}|list_tree(Git,Path,Tree)];
    {ok, tree, InnerTree} ->
      Content = list_tree(Git, FullName, InnerTree),
      Content ++ list_tree(Git,Path,Tree)
  end;

list_tree(_, _, []) ->
  [].



prepare_path(Git, Path) when is_list(Path) ->
  prepare_path(Git, list_to_binary(Path));

prepare_path(Git, Path) when is_binary(Path) ->
  {SHA1, RPath} = case binary:split(Path, <<":">>) of
    [Branch, RealPath] when size(Branch) == 40 ->
      {Branch, RealPath};
    [Branch, RealPath] ->
      {read_file(filename:join([Git, "refs/heads", Branch])), RealPath};
    [RealPath] ->
      {read_file(filename:join([Git, "refs/heads/master"])), RealPath}
  end,
  {ok, commit, Head} = read_object(Git, SHA1),
  {ok, tree, Tree} = read_object(Git, proplists:get_value(tree, Head)),
  {Tree, RPath}.






lookup(Git, [Part|Parts], Tree) ->
  case lists:keyfind(Part, 1, Tree) of
    {Part,_,SHA1} ->
      case read_object(Git, SHA1) of
        {ok, Type, Blob} when length(Parts) == 0 ->
          {ok, Type, Blob};
        {ok, tree, Tree1} when length(Parts) > 0 ->
          lookup(Git, Parts, Tree1);
        {ok, _, _} ->
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
  {ok, Type, Content} = read_raw_object(Git, SHA1),
  C = case Type of
    tree -> parse_tree(Content);
    commit -> parse_commit(Content);
    blob -> Content;
    _ -> Content
  end,
  {ok, Type, C}.


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


read_raw_object(Git, <<Prefix:2/binary, Postfix/binary>> = SHA1hex) when size(SHA1hex) == 40 ->
  DirectPath = filename:join([Git, "objects", Prefix, Postfix]),
  case file:read_file(DirectPath) of
    % git-ruby/internal/loose.rb#legacy_loose_object?
    {ok, <<16#78, W2, _/binary>> = Zip} when (16#7800 bor W2) rem 31 == 0 -> 
      [Header, Content] = binary:split(unzip(Zip), <<0>>),
      [Type, Size] = binary:split(Header, <<" ">>),
      unpack_size(Size) == size(Content) orelse erlang:error({invalid_size, unpack_size(Size), size(Content)}),
      {ok, unpack_type(Type), Content};
    % git-ruby/internal/loose.rb#unpack_object_header_gently
    % {ok, <<0:1, _Type:3, _Size1:4, Zip/binary>>} -> unzip(Zip);
    % {ok, <<1:1, _Type:3, _Size1:4, 0:1, _Size2:7, Zip/binary>>} -> unzip(Zip);
    % {ok, <<1:1, _Type:3, _Size1:4, 1:1, _Size2:7, 1:1, _Size3:7, Zip/binary>>} -> unzip(Zip);

    {error, enoent} ->
      error(enoent)
  end.

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
