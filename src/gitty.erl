-module(gitty).

-export([show/2]).

-include_lib("eunit/include/eunit.hrl").

show(Git, Path) ->
  ok.



read1_test() ->
  ?assertEqual({ok, <<"80f136f\n">>}, show("test/fixtures/dot_git", "test/fixtures/rev_parse")).
