#!/usr/bin/env escript

-mode(compile).

-define(D(X), io:format("git.erl:~p ~240p~n", [?LINE, X])).
-define(DBG(Fmt,X), io:format("git.erl:~p "++Fmt++"~n", [?LINE| X])).

-define(IS_HEX(A), ((A >= $0 andalso A =< $9) orelse (A >= $A andalso A =< $F) orelse (A >= $a andalso A =< $f)).



main([]) ->
  code:add_pathz("ebin"),
  {ok, Blob} = gitty:show(".git", "src/gitty.app.src"),
  ?DBG("~s", [Blob]),
  ok.

