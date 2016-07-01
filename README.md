# erlang-supervision-trees
Code examples for the presentation Erlang Supervision Trees

Following four set of code modules or projects are provided
# Basic supervisors
# Multi-level supervisors forming a supervision tree
# Different supervision strategies and corresponding code samples
# Complete examples

Unit Tests are provided for all the code modules to run and test

Examples are provided which can run and built using make

To compile, run the tests and examples, following steps needs to be followed
$ install erlang
$ rebar compile

Inside each app folder (basic, advanced and examples), run following (NOTE: rebar is not running eunit in sub directories)
$ rebar eunit
$ rebar compile eunit