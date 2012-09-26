Gitty
=====


This is a library for reading and editing git repository from erlang.

It is a pure erlang library that implements very small subset of git features, yet it is going to be powerful enough
to be used as a backend for a website, storing its content in git repo.


Usage
-----


    gitty:list(".git", "src").
    gitty:list("/var/www/doc.git", "").
    gitty:show("/var/www/doc.git", "index.html").

