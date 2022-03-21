-module(ds).
-compile(export_all).
start_child() ->
    List = [1,2,3],
    Data = {1,rand:uniform(5),List},
    io:format("Listening on child [~p:~p]...\n", [node(),self()]),
    loop_child(Data).

start_parent() ->
    io:format("Listening on top [~p:~p]...\n", [node(),self()]),
    N = 2 + rand:uniform(3), % 3 to 5 inc
    Children=make_children(N),
    loop_parent(Children).

get_for_system(What) when (What == count) or (What == number) ->
    Groups = [node() | nodes()],
    Values = [req_parent(Group,What) || Group <- Groups],
    Value = lists:sum(Values),
    broadcast(Value,top,no_cascade), % let parents know
    Value;
get_for_system(What) when (What == list) ->
    Groups = [node() | nodes()],
    Lists = [req_parent(Group,list) || Group <- Groups],
    List = lists:append(Lists),
    broadcast(List,top,no_cascade),
    List.

notify_parent(Group, Message,ShouldCascade) ->
    {top,Group} ! {self(), Message,ShouldCascade}.

notify_child(Child, msg, Message) ->
    Child ! {self(), msg, Message}.

% {RegisteredName, NodeName}
% await response
req_parent(Group, Request) ->
    {top,Group} ! {self(), Request},
    receive
        Response ->
            Response
    end.

% send to same group
req_parent(Request) ->
    req_parent(node(),Request).

% private - called by parent
req_child(Child,Request) ->
    Child ! {self(),Request},
    receive
        Response ->
            Response
    end.


broadcast(Message,top,cascade)  ->
    Groups = [node() | nodes()], % sends request to all groups
    [notify_parent(Group,Message,cascade) || Group <- Groups];
broadcast(Message,top,no_cascade)  ->
    Groups = [node() | nodes()], % sends request to all groups
    [notify_parent(Group,Message,no_cascade) || Group <- Groups];
broadcast(Message,Pid,CascadeType) ->
    % if a child, tell parent to do it
    broadcast(Message,top,CascadeType).
broadcast(Message,Pid) ->
    broadcast(Message,top,cascade). % default broadcast is to cascade

init() ->
    io:format("I'm main proc ~p in group [~p], The other groups are [~p]\n",[self(),node(),nodes()]),
    Pid = spawn(node(), ds, start_parent, []),
    io:format("Goodbye from main proc. Top proc [~p] lives on\n",[Pid]),
    register(top,Pid).

init(Node) ->
    net_adm:ping(Node), % make contact - it's transitive, can can daisy chain to connect all!
    init().

loop_parent(Children) ->
    receive
        {From,done} ->
            [Child ! {self(),done} || Child <- Children],
            io:format("Goodbye from top too ~p~n",[self()]),
            From ! 'terminated',
            exit('Finished');
        {Pid,Message,cascade} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]),
            [notify_child(Child,msg,Message) || Child <- Children]; % cascade to children
        {Pid,Message,no_cascade} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]);
        {Pid,list} ->
            Lists = [req_child(Child,list) || Child <- Children],
            Pid ! lists:append(Lists);
        {Pid,count} ->
            Values = [req_child(Child,count) || Child <- Children],
            Pid ! lists:sum(Values) + 1; % don't forget to count myself!
         {Pid,number} ->
            Values = [req_child(Child,number) || Child <- Children],
            Pid ! lists:sum(Values)
    end,
    loop_parent(Children).

make_children(0) -> [];
make_children(N) ->
    [spawn(node(), ds, start_child, []) | make_children(N-1)].

loop_child(Data) ->
    {Count, Value, List} = Data,
    receive
        {Parent, count} ->
            Parent ! Count;
        {Parent, number} ->
            Parent ! Value;
        {Parent, list} ->
            Parent ! List;
        {Parent, msg, Message} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]);
        {_, done} ->
            io:format("[~p] Terminating.  Goodbye~n",[self()]),
            exit('Finished')
    end,
    loop_child(Data).
