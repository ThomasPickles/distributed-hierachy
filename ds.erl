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

get_for_system(What) when is_atom(What) ->
    Groups = [node() | nodes()],
    case What of
        count ->
            Values = [req_parent(Group,count) || Group <- Groups],
            Value = lists:sum(Values);
        number ->
            Values = [req_parent(Group,number) || Group <- Groups],
            Value = lists:sum(Values);
        list ->
            Lists = [req_parent(Group,list) || Group <- Groups],
            Value = lists:append(Lists);
        _ ->
            io:format("Not implemented...~n"),
            exit('done')
    end,
    broadcast(Value,no_cascade). % let parents know

notify_parent(Group, Message,ShouldCascade) ->
    {top,Group} ! {Message,ShouldCascade}.

notify_child(Child, msg, Message) ->
    Child ! {msg, Message}.

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


broadcast(Message,CascadeType) when is_atom(CascadeType) ->
    Groups = [node() | nodes()], % sends request to all groups
    case CascadeType of
        cascade ->
            [notify_parent(Group,Message,cascade) || Group <- Groups];
        no_cascade ->
            [notify_parent(Group,Message,no_cascade) || Group <- Groups];
        _ ->
            io:format("Unknown cascade type.  Exiting...~n")
    end,
    true.  % if a child, tell parent to do it
broadcast(Message) ->
    broadcast(Message,cascade). % default broadcast is to cascade

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
        % notifications
        {Message,cascade} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]),
            [notify_child(Child,msg,Message) || Child <- Children]; % cascade to children
        {Message,no_cascade} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]);
        % requests
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
        % requests
        {Parent, count} ->
            Parent ! Count;
        {Parent, number} ->
            Parent ! Value;
        {Parent, list} ->
            Parent ! List;
        % notifications
        {msg, Message} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]);
        {_, done} ->
            io:format("[~p] Terminating.  Goodbye~n",[self()]),
            exit('Finished')
    end,
    loop_child(Data).
