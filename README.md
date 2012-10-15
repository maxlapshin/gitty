Gitty
=====


This is a library for reading and editing git repository from erlang.

It is a pure erlang library that implements very small subset of git features, yet it is going to be powerful enough
to be used as a backend for a website, storing its content in git repo.


Usage
-----


    {ok, Git, [{Path,Mode,SHA1}|_] = List} = gitty:list(".git", "master:src").
    {ok, Git, commit, Content} = gitty:show("test/dot_git", "eeccc934cad8bb74624ed388988fe79c26e6900d").
    {ok, Git, List} = gitty:list("/var/www/doc.git", "master:").
    {ok, Git, blob, Content} = gitty:show("/var/www/doc.git", "master:index.html").
    {ok, Git} = gitty:commit_files("/var/www/doc.git", "master", [
      {"doc/en/index.html", "100644", "<html><body>Hi</body</html>"},
      {"doc/de/index.html", "100644", "<html><body>Guten Tag</body</html>"}
    ], [{message, "First commit\n"},{author, "Max Lapshin <max@maxidoors.ru>"}]).


