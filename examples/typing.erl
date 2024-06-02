-module(typing).
-export([do_something/1]).

do_something(atom) ->
    atom_to_list(atom).
