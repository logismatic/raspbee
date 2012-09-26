%% ********************************************************* Module ring
%% @copyright Logismatic GmbH 2012
%% @author	paul.hoesli
%% @doc		Erlang Benchmarks
%%
%% This erlang ring benchmark creates N processes in a ring and
%% sends a message round the ring M times, so that a total of N * M
%% messages get sent. 
%%
%% @version	0.9.1
%% *********************************************************************
-module(ring).

%% ********************************************************* Exports ***
-export([start/2]).

%% ***************************************************** Processing Loop
forward(Next) ->
   Self = self(),
	receive
		{_Any, 0} -> %% The stop message, forward and die.
			Next ! {_Any, 0}; 
			
		{Self, 1} -> %% The last lap, now send stop message
			Next ! {Self, 0}, 
			
			{_, Runtime} = statistics(runtime),
			{_, Clock} = statistics(wall_clock),
			io:format("This requires ~p (~p) milliseconds cpu (clock) time.~n", 
				[Runtime, Clock]),
				
			forward(Next);
			
		{Self, Lap} -> %% Another lap
			Next ! {Self, Lap-1}, 
			forward(Next);
			
		_Any -> %% Just forward
			Next ! _Any, 
			forward(Next)
	end.
	
%% **************************************************** Process Function
%% prc(N) will forward to prc(N-1)
%% prc(1) will forward to the process registered as 'ring' 
%%  this closes the ring
prc(1) -> forward(ring);
prc(N) -> forward(spawn(fun() -> prc(N-1) end)).

%% *************************************************** API Functions ***
%% --------------------------------------------------------- start/2 ---
%% @spec start( nLaps(), nProcs() ) -> string().
%% @type nProcs() = integer() 
%% @type nLaps() = integer() 
%%
%% @doc Creates a ring of NProcs processes and sends initial message.
%%
start(NLaps,NProcs) ->
	register(ring, spawn_link(fun() -> prc(NProcs) end)),
	
	statistics(runtime),
	statistics(wall_clock),
	
	%% Send initial message
	ring ! {whereis(ring), NLaps + 1}, 

	io:format("Runs a message in ~p laps around a ring of ~p processes.~n", 
		[NLaps, NProcs]).

%% ********************************************************* Module ring
