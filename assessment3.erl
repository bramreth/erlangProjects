-module(assessment3).
-export([forkOnTable/0, thinkingPhilosopher/3, college/0]).

forkOnTable() ->
receive
    {u,Name,RequestingPhilosopher} -> RequestingPhilosopher!true,
	    forkInUse(Name),
	    forkOnTable();  %u - picked up
    stop -> stop
end.

forkInUse(NameOfUser) ->
receive
	{u,_,RequestingPhilosopher} -> RequestingPhilosopher!false,
		io:fwrite("~p's fork is in use~n",[NameOfUser]),
		forkInUse(NameOfUser);
	{d,NameOfUser,RequestingPhilosopher} -> RequestingPhilosopher!true,
		io:fwrite("~p put the fork down~n",[NameOfUser]); % d - put down
	stop -> stop
end.

thinkingPhilosopher(Name,F1,F2) ->
	io:fwrite("~p is thinking~n",[Name]),
	PhilosopherName = Name,
	random:seed(now()),
	timer:sleep(random:uniform(1000)),
	hungryPhilosopher(PhilosopherName,F1,F2).

hungryPhilosopher(Name,F1,F2) ->
	io:fwrite("~p is hungry~n",[Name]),
	PhilosopherName = Name,
	leftForkPhilosopher(PhilosopherName,F1,F2,false).

leftForkPhilosopher(Name,F1,F2,false) ->
	F1!{u,Name,self()},
	receive 
		true -> io:fwrite("~p has left fork~n",[Name]), 
			rightForkPhilosopher(Name,F1,F2,true);
		false -> timer:sleep(500),
			rightForkPhilosopher(Name,F1,F2,false)
	end;
leftForkPhilosopher(Name,F1,F2,true) ->
	F1!{u,Name,self()},
	receive 
		true -> io:fwrite("~p has left fork~n",[Name]),
			eatingPhilosopher(Name,F1,F2);
		false -> timer:sleep(500),
		leftForkPhilosopher(Name,F1,F2,true)
	end.

rightForkPhilosopher(Name,F1,F2,false) ->
	F2!{u,Name,self()},
	receive 
		true -> io:fwrite("~p has right fork~n",[Name]),
			leftForkPhilosopher(Name,F1,F2,true);
		false -> timer:sleep(500),
			leftForkPhilosopher(Name,F1,F2,false)
	end;
rightForkPhilosopher(Name,F1,F2,true) ->
	F2!{u,Name,self()},
	receive 
		true -> io:fwrite("~p has right fork~n",[Name]), 
			eatingPhilosopher(Name,F1,F2);
		false -> timer:sleep(500),
			leftForkPhilosopher(Name,F1,F2,true)
	end.

eatingPhilosopher(Name,F1,F2) ->
	io:fwrite("~p is eating~n",[Name]),
	PhilosopherName = Name,
	random:seed(now()),
	timer:sleep(random:uniform(1000)),
	F1!{d,PhilosopherName,self()},
	receive
			true -> stop
	end,
	F2!{d,PhilosopherName,self()},
	receive
			true -> stop
	end,
	thinkingPhilosopher(PhilosopherName,F1,F2).

college() ->
	ForkA = spawn(?MODULE, forkOnTable, []),
	ForkB = spawn(?MODULE, forkOnTable, []),
	ForkC = spawn(?MODULE, forkOnTable, []),
	ForkD = spawn(?MODULE, forkOnTable, []),
	ForkE = spawn(?MODULE, forkOnTable, []),

	spawn(?MODULE, thinkingPhilosopher, [plato,ForkA,ForkB]),
	spawn(?MODULE, thinkingPhilosopher, [socrates,ForkB,ForkC]),
	spawn(?MODULE, thinkingPhilosopher, [confucius,ForkC,ForkD]),
	spawn(?MODULE, thinkingPhilosopher, [decartes,ForkD,ForkE]),
	spawn(?MODULE, thinkingPhilosopher, [wittgenstein,ForkE,ForkA]).

	%part 2: analysis
	%1. Yes there is a possible deadlock in the system