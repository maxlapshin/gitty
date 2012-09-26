#!/usr/bin/env escript

-mode(compile).

-define(D(X), io:format("git.erl:~p ~240p~n", [?LINE, X])).
-define(DBG(Fmt,X), io:format("git.erl:~p "++Fmt++"~n", [?LINE| X])).

-define(IS_HEX(A), ((A >= $0 andalso A =< $9) orelse (A >= $A andalso A =< $F) orelse (A >= $a andalso A =< $f)).


test1(0) -> ok;
test1(N) ->
  {ok, blob, Blob} = gitty:show(".git", "src/gitty.app.src"),
  size(Blob) == 228 orelse error({invalid_size, size(Blob)}),
  test1(N - 1).

test2(0) -> ok;
test2(N) ->
  Blob = git:show(".git", "src/gitty.app.src"),
  size(Blob) == 228 orelse error({invalid_size, size(Blob)}),
  test2(N - 1).

main([]) ->
  code:add_pathz("ebin"),

  N = 100,
  {_T1, ok} = timer:tc(fun() -> test1(N) end),
  {_T2, ok} = timer:tc(fun() -> test2(N) end),
  % ?D({T1,T2}),
  % ?D(gitty:list(".git", "src")),
  ?D(gitty:list("test/dot_git", "nonpack:test")),
  ok.

