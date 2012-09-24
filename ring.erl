-module(ring).
-export([start/2]).

%% message processing loop
forward(Next) ->
   Self = self(),
	receive
		{_Any, 0} -> %% This is the stop message, forward and die.
			Next ! {_Any, 0}; 
			
		{Self, 1} -> %% This is the last lap
			%% send stop message
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
	
prc(1)  ->
	%% prc(1) will forward to prc(NProcs) (registered as 'ring')
	%% this closes the ring
	forward(ring);
prc(N)  ->
	%% prc(N) will forward to prc(N-1)
	forward(spawn(fun() -> prc(N-1) end)).
		
start(NLaps,NProcs) ->
	%% Create a ring of processes
	register(ring, spawn_link(fun() -> prc(NProcs) end)),
	
	statistics(runtime),
	statistics(wall_clock),
	
	%% Send a start message
	ring ! {whereis(ring), NLaps + 1}, 

	io:format("Runs a message in ~p laps around a ring of ~p processes.~n", 
		[NLaps, NProcs]).


