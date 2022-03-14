-module(ds).
-compile(export_all).
initialise_data() ->
    Data = {1,rand:uniform(5)},
    io:format("Hello, I'm bottom level proc ~p, from group [~p]\n", [self(),node()]),
    respond(Data).

start() ->
    % Todo - remote pool?
    % https://stackoverflow.com/questions/4853750/erlang-starting-a-remote-node-programmatically
    N = 2 + rand:uniform(3), % 3 to 5 inc
    io:format("I'm top level proc ~p of group [~p], The other groups are [~p]\n",[self(),node(),nodes()]),
    ChildIds=make_children(N),
    Counts = [get_value(Child,count) || Child <- ChildIds],
    Numbers = [get_value(Child,number) || Child <- ChildIds],
    % can generalise this to an accumulator for more general fold
    io:format("My total is ~p~n",[lists:sum(Numbers)]),
    io:format("My count is ~p~n",[lists:sum(Counts)]).
start(Node) ->
    net_adm:ping(Node), % make contact - it's transitive, can can daisy chain to connect all!
    start().

get_value(Child,What) ->
    io:format("[~p] Sending request for ~p to ~p~n",[self(),What,Child]),
    Child ! {self(),What},
    receive
        Value ->
            Value
    end.

make_children(0) -> [];
make_children(N) ->
    [spawn(node(), ds, initialise_data, []) | make_children(N-1)].

respond(Data) ->
    {Count, Value} = Data,
    io:format("[~p] Ready to receive requests~n",[self()]),
    % how to decide between sending and receiving?
    receive
        {From, count} ->
            io:format("[~p] Received request for count from ~p~n",[self(),From]),
            From ! Count;
        {From, number} ->
            io:format("[~p] Received request for number from ~p~n",[self(),From]),
            From ! Value
    end,
    respond(Data).

whoami() ->
    io:format("Node: [~p], Proc: [~p], Connected to: [~p]\n",[node(),self(),nodes()]).