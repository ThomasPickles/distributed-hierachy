-module(ds).
-compile(export_all).
initialise_data() ->
    Data = {1,rand:uniform(5)},
    io:format("Hello, I'm bottom level proc ~p, from group [~p]. Ready to loop_child...\n", [self(),node()]),
    loop_child(Data).

initialise_parent() ->
    io:format("I'm top proc ~p in group [~p]\n",[self(),node()]),
    N = 2 + rand:uniform(3), % 3 to 5 inc
    Children=make_children(N),
    loop_parent(Children).

wait()->
    receive
        {done} ->
            io:format("[Main: ~p] Seems that everyone is done\n",[self()])
    end.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response ->
            Response
    end.

init() ->
    io:format("I'm main proc ~p in group [~p], The other groups are [~p]\n",[self(),node(),nodes()]),
    Pid = spawn(node(), ds, initialise_parent, []),
    io:format("Goodbye from main proc. [~p] lives on\n",[Pid]).

init(Node) ->
    net_adm:ping(Node), % make contact - it's transitive, can can daisy chain to connect all!
    init().
    % FIXME: how to send a msg from one parent to the others?
    % [Group ! {Pid,count} || Group <- nodes()].

loop_parent(Children) ->
    receive
        {_,done} ->
            [Child ! {self(),done} || Child <- Children],
            io:format("Goodbye from top too ~p~n",[self()]);
        {GroupId,What} ->
            Values = [get_value(Child,What) || Child <- Children],
            % can generalise this to an accumulator for more general fold
            Total =  lists:sum(Values),
            io:format("Accumulated group value is ~p~n",[Total]),
            GroupId ! Total,
            loop_parent(Children)
    end.

get_value(Child,What) ->
    io:format("Group [~p] top sending request for ~p to ~p~n",[node(),What,Child]),
    Child ! {self(),What},
    receive
        Value ->
            Value
    end.

make_children(0) -> [];
make_children(N) ->
    [spawn(node(), ds, initialise_data, []) | make_children(N-1)].

loop_child(Data) ->
    {Count, Value} = Data,
    % how to decide between sending and receiving?
    receive
        {Top, count} ->
            io:format("[~p] Received request for count from top~n",[self()]),
            Top ! Count,
            loop_child(Data);
        {Top, number} ->
            io:format("[~p] Received request for number from top~n",[self()]),
            Top ! Value,
            loop_child(Data);
        {_, done} ->
            io:format("[~p] Terminating.  Goodbye~n",[self()])
    end.

whoami() ->
    io:format("Node: [~p], Proc: [~p], Connected to: [~p]\n",[node(),self(),nodes()]).