basic
=====

An OTP application showing classic and basic supervisor. Classic supervisor is implemented using linked processes and trap exits without the behavior. Basic supervisor is written using supervisor behaviour provided in Erlang. The basic supervisor registers its process with the name basic_sup.

The basic worker is a simple worker process that provides three functions for alloc (allocating channel), free(free the channel) and all (see the currently allocated channel). The channel is the non-allocated number available in the seq 1-100 and is maintained using allocated and free channel lists. The basic_worker is implemented using gen_server behavior and registers its process with the name basic_worker.

Build
-----

    $ rebar compile


Test
----

    $ rebar eunit
    
