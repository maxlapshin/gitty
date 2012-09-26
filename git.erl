#!/usr/bin/env escript

-mode(compile).

-define(D(X), io:format("git.erl:~p ~240p~n", [?LINE, X])).
-define(DBG(Fmt,X), io:format("git.erl:~p "++Fmt++"~n", [?LINE| X])).

-define(IS_HEX(A), ((A >= $0 andalso A =< $9) orelse (A >= $A andalso A =< $F) orelse (A >= $a andalso A =< $f)).


test1(0, _Git) -> ok;
test1(N, Git) ->
  {ok, Git1, blob, Blob} = gitty:show(Git, "src/gitty.app.src"),
  size(Blob) == 228 orelse error({invalid_size, size(Blob)}),
  test1(N - 1, Git1).

test2(0) -> ok;
test2(N) ->
  Blob = git:show(".git", "src/gitty.app.src"),
  size(Blob) == 228 orelse error({invalid_size, size(Blob)}),
  test2(N - 1).

main([]) ->
  code:add_pathz("ebin"),

  N = 1000,
  {_T1, ok} = timer:tc(fun() -> test1(N, ".git") end),
  {_T2, ok} = timer:tc(fun() -> test2(N) end),
  % ?D({_T1,_T2}),
  % ?D(gitty:list(".git", "")),
  {ok, _Git, _List} = gitty:list("test/dot_git", "nonpack:test"),
  % ?D(Git),
  % ?D(List),

  
  ok.

