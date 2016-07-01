-module(dynamic_worker_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% gen_server tests
%%--------------------------------------------------------------------
dynamic_worker_test_() ->
    {foreach, fun setup/0, fun cleanup/1, 
     [
      fun(Pid) -> fun() -> server_is_alive(Pid) end end,
      fun(Pid) -> fun() -> server_is_not_registered(Pid) end end,
      fun(Pid) -> fun() -> alloc(Pid) end end,
      fun(Pid) -> fun() -> all(Pid) end end,
      fun(Pid) -> fun() -> free(Pid) end end
     ]}.

setup() ->    
    ?debugMsg("setup dynamic worker"),
    process_flag(trap_exit, true),
    {ok, Pid} = dynamic_worker:start_link(),
    Pid.

server_is_alive(Pid) ->
    ?assertEqual(true, is_process_alive(Pid)).

server_is_not_registered(_Pid) ->
    ?assertEqual(undefined, whereis(dynamic_worker)).

alloc(Pid) ->    
    ?assertEqual(1, dynamic_worker:alloc(Pid)),
    ?assertEqual(2, dynamic_worker:alloc(Pid)).

all(Pid) ->
    dynamic_worker:alloc(Pid), 
    dynamic_worker:alloc(Pid),
    ?assertEqual([2,1], dynamic_worker:all(Pid)).   

free(Pid) ->
    dynamic_worker:alloc(Pid),
    ?assertEqual([1], dynamic_worker:all(Pid)),
    ?assertEqual(ok, dynamic_worker:free(Pid, 1)),
    ?assertEqual([], dynamic_worker:all(Pid)).

cleanup(Pid) ->    
    ?debugMsg("cleanup dynamic worker"),
    exit(Pid, kill),
    %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
