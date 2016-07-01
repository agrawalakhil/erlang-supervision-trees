-module(basic_worker_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% gen_server tests
%%--------------------------------------------------------------------
basic_worker_test_() ->
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
    {ok, Pid} = basic_worker:start_link(),
    Pid.

server_is_alive(Pid) ->
    ?assertEqual(true, is_process_alive(Pid)).

server_is_registered(Pid) ->
    ?assertEqual(Pid, whereis(basic_worker)).

alloc(_Pid) ->    
    ?assertEqual(1, basic_worker:alloc()),
    ?assertEqual(2, basic_worker:alloc()).

all(_Pid) ->
    basic_worker:alloc(), 
    basic_worker:alloc(),
    ?assertEqual([2,1], basic_worker:all()).   

free(_Pid) ->
    basic_worker:alloc(),
    ?assertEqual([1], basic_worker:all()),
    ?assertEqual(ok, basic_worker:free(1)),
    ?assertEqual([], basic_worker:all()).

cleanup(Pid) ->    
    ?debugMsg("cleanup"),
    exit(Pid, kill),
    %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
