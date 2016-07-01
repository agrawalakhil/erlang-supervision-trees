advanced
=====

An OTP application to show the multi-level supervision trees and supervision strategies. The advanced application uses basic_sup to form the nested supervision tree, it also uses advanced_worker at the same level as basic_sup. The advanced supervisor registers its process with the name advanced_sup.

The advanced_worker is exactly same as basic_worker but registers its process as advanced_worker as well as logs messages as advanced_worker. Another worker dynamic_worker implements the same functionality but without registering a name for the process which means multiple instances of the dynamic_worker process can be instantiated and therefore the API is changed to take Pid as argument.

Build
-----

    $ rebar compile

Test
----

    $ rebar eunit
    $ rebar eunit tests=one_for_one
    $ rebar eunit tests=one_for_all
    $ rebar eunit tests=rest_for_one
    $ rebar eunit tests=simple_one_for_one