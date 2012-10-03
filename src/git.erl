-module(git).

-compile(export_all).

git_cmd(Args) ->
  % io:format("GIT: ~s~n", [Cmd]),
  Tag = make_ref(),
  GitPath = os:find_executable("git"),
  {Pid,Mref} = erlang:spawn_monitor(
	  fun() ->
		  process_flag(trap_exit, true),
		  Port = erlang:open_port({spawn_executable, GitPath}, [{args, Args}, binary, exit_status, eof]),
		  Data = iolist_to_binary(get_data(Port)),
		  exit({Tag,Data})
	   end),
  receive
{'DOWN',Mref,_,Pid,{Tag,Result}} ->
    Result;
{'DOWN',Mref,_,Pid,Reason} ->
    exit(Reason)
  end.


git_cmd(Repo, Cmd) ->
  git_cmd(["--git-dir=" ++ Repo | Cmd]).

get_data(Port) ->
  receive
    {Port, eof} -> [];
    {Port, {exit_status, _Status}} -> [];
    {Port, {data, Data}} -> [Data|get_data(Port)];
    Else -> io:format("ZZZ: ~p~n", [Else])
  end.


tree(Repo) ->
  Reply = git_cmd(Repo, ["ls-tree", "--full-tree", "-r", "HEAD"]),
  Rows1 = binary:split(Reply, <<"\n">>, [global]),
  Rows2 = [case re:run(Row, "^\\d+ (\\w+) (\\w+)\t(.+)$", [{capture,all_but_first,list}]) of
    {match, [Type, SHA1, Path]} -> {Type, SHA1, Path};
    nomatch -> undefined
  end || Row <- Rows1],
  Rows3 = [Row || Row <- Rows2, Row =/= undefined],
  Rows3.


show(Repo, Path) ->
  Reply = git_cmd(Repo, ["show", "master:" ++ Path]),
  Reply.


add(Repo, Path) ->
  git_cmd(Repo, ["add", Path]).

commit(Repo, Options) ->
  Cmd = ["commit"] ++ case proplists:get_value(message, Options) of
    undefined -> [];
    Msg -> ["-m", Msg]
  end ++ case proplists:get_value(author, Options) of
    undefined -> [];
    Auth -> ["--author=" ++ Auth]
  end,
  git_cmd(Repo, Cmd).

    

bm() ->
  N = 1000,
  List = lists:seq(1,N),
  {Time, _Res} = timer:tc(fun() -> [show("../doc", "doc/playlist.md") || _ <- List] end),
  Time.

  