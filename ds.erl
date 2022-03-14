-module(ds).
-compile(export_all).
set_protocol(Type) ->
    io:format("Hello, I'm bottom level proc ~p, from group [~p]\n", [self(),node()]),
    loop(Type).

start() ->
    % Todo - remote pool?
    % https://stackoverflow.com/questions/4853750/erlang-starting-a-remote-node-programmatically
    N = 2 + rand:uniform(3), % 3 to 5 inc
    io:format("I'm top level proc ~p of group [~p], The other groups are [~p]\n",[self(),node(),nodes()]),
    ChildIds=make_children(N),
    Counts = [[fun(Child) -> Child ! {self(),ds,send_count,[]} end || Child <- L),
    Count=list:sum(ChildIds), % can generalise this to an accumulator for more general fold
    io:format("My count is ~p",[Count])
start(Node) ->
    net_adm:ping(Node), % make contact - it's transitive, can can daisy chain to connect all!
    start().

% TODO: set up topology
% start(Node) ->
    % node connections are transitive, so just need
    % to pass in last one to get a connected topology

make_children(0) -> [];
make_children(N) ->
    [spawn(node(), ds, set_protocol, [count]) | make_children(N-1)].

send_count() ->
    % receive msg, send back a 1

loop(Type) ->
    % how to decide between sending and receiving?
    receive
        {rpc, Pid, M, F, A} ->
            Pid ! {self(), (catch apply(M, F, A))},
            loop()
    end.

% Module, Function, Args
rpc(Pid, M, F, A) ->
    Pid ! {rpc, self(), M, F, A},
    receive
        {Pid, Response} ->
            Response
    end.

whoami() ->
    io:format("Node: [~p], Proc: [~p], Connected to: [~p]\n",[node(),self(),nodes()]).