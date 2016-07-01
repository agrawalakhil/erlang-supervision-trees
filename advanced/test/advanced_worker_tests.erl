-module(advanced_worker_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% gen_server tests
%%--------------------------------------------------------------------
advanced_worker_test_() ->
    {foreach, fun setup/0, fun cleanup/1, 
     [
      fun(Pid) -> fun() -> server_is_alive(Pid) end end,
      fun(Pid) -> fun() -> server_is_registered(Pid) end end,
      fun(Pid) -> fun() -> alloc(Pid) end end,
      fun(Pid) -> fun() -> all(Pid) end end,
      fun(Pid) -> fun() -> free(Pid) end end
     ]}.

setup() ->    
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = advanced_worker:start_link(),
    Pid.

server_is_alive(Pid) ->
    ?assertEqual(true, is_process_alive(Pid)).

server_is_registered(Pid) ->
    ?assertEqual(Pid, whereis(advanced_worker)).

alloc(_Pid) ->    
    ?assertEqual(1, advanced_worker:alloc()),
    ?assertEqual(2, advanced_worker:alloc()).

all(_Pid) ->
    advanced_worker:alloc(), 
    advanced_worker:alloc(),
    ?assertEqual([2,1], advanced_worker:all()).   

free(_Pid) ->
    advanced_worker:alloc(),
    ?assertEqual([1], advanced_worker:all()),
    ?assertEqual(ok, advanced_worker:free(1)),
    ?assertEqual([], advanced_worker:all()).

cleanup(Pid) ->    
    ?debugMsg("cleanup"),
    exit(Pid, kill),
    %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
