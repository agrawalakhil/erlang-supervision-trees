-module(dynamic_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/1, all/1, free/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->  
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% API Functions
%%====================================================================
alloc(Pid) ->    
    gen_server:call(Pid, alloc).

all(Pid) ->
    gen_server:call(Pid, all).

free(Pid, Ch) ->    
    gen_server:cast(Pid, {free, Ch}).
%%====================================================================
%% Gen Server Functions
%%====================================================================
init(Args) ->
    error_logger:info_msg("[dynamic_worker] starting with pid ~p and args ~p~n", [self(), Args]),
    {ok, channels()}.

handle_call(alloc, _From, Chs) ->    
    {Ch, Chs2} = allocate(Chs),
    {reply, Ch, Chs2};
handle_call(all, _From, Chs = {Allocated, _Free}) -> 
    {reply, Allocated, Chs}.

handle_cast({free, Ch}, Chs) ->    
    Chs2 = freeChannel(Ch, Chs),
    {noreply, Chs2}.

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_message("Exiting process ~p with reason ~p~n", [Pid, Reason]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================
channels() ->
    {_Allocated = [], _Free = lists:seq(1,100)}.

allocate({Allocated, [H|T] = _Free}) ->    
    {H, {[H|Allocated], T}}.

freeChannel(Ch, {Alloc, Free} = Channels) ->    
    case lists:member(Ch, Alloc) of
	true ->	{lists:delete(Ch, Alloc), [Ch|Free]};
	false -> Channels
    end.
