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

get_consensus_value(Values) ->
    % most common element
    lists:max(Values).
get_consensus_value(Values,weighted) ->
    lists:max(Values).

mutex() ->
    get_for_system(mutex).

get_for_system(What) when is_atom(What) ->
    Groups = [node() | nodes()],
    case What of
        count ->
            Values = [get_for_group(Group,count) || Group <- Groups],
            Value = lists:sum(Values);
        aggregate ->
            Values = [get_for_group(Group,aggregate) || Group <- Groups],
            Value = lists:sum(Values);
        list ->
            Lists = [get_for_group(Group,list) || Group <- Groups],
            Value = lists:append(Lists);
        % both of these are the same
        consensus ->
            Values = [get_for_group(Group,consensus) || Group <- Groups],
            ValueWeight = get_most_frequent(Values,ignore_weights),
            io:format("[~p] Sending value ~p~n",[self(),ValueWeight]),
            {Value,_} = ValueWeight;
        weighted_consensus ->
            Values = [get_for_group(Group,consensus) || Group <- Groups],
            ValueWeight = get_most_frequent(Values),
            io:format("[~p] Sending value ~p~n",[self(),ValueWeight]),
            {Value,_} = ValueWeight;
        mutex ->
            [First|Rest] = Groups,
            QueueParents = Rest++[First], % put to back
            % create the token
            {top,First} ! {tok,[],QueueParents,main_loop},
            Value = true;
        _ ->
            io:format("Not implemented...~n"),
            Value = 0,
            exit('done')
    end,
    broadcast(Value,no_cascade). % let parents know

notify_parent(Group, Message,ShouldCascade) ->
    {top,Group} ! {Message,ShouldCascade}.

notify_child(Child, msg, Message) ->
    Child ! {msg, Message}.

% {RegisteredName, NodeName}
% await response
get_for_group(Group, Request) ->
    {top,Group} ! {self(), Request},
    receive
        Response ->
            Response
    end.

% send to same group
get_for_group(Request) ->
    get_for_group(node(),Request).

% private - called by parent
get_for_process(Child,Request) ->
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

critical_section(Direction,Pid) ->
    io:format("*** Process ~p ~ping critical section ***~n",[Pid,Direction]).

accumulate_by_key(Dict,[H|T],ShouldIgnoreWeights) ->
    case ShouldIgnoreWeights of
        false ->
            {Val,Count} = H;
        true ->
            {Val,_} = H,
            Count = 1 % all candidates count equally
        end,
    io:format("[~p] Adding {~p,~p} to dict~n",[self(),Val,Count]),
    NewDict = dict:update_counter(Val, Count, Dict),
    accumulate_by_key(NewDict, T,ShouldIgnoreWeights);
accumulate_by_key(Dict,[],_) -> Dict.
accumulate_by_key(L,ignore_weights) ->
    accumulate_by_key(dict:new(),L,true).
accumulate_by_key(L) ->
    accumulate_by_key(dict:new(),L,false).

get_most_frequent(L,ignore_weights) when is_list(L) ->
    Dict = accumulate_by_key(L,ignore_weights),
    List = dict:to_list(Dict),
    get_most_frequent(List,{-1,0});
get_most_frequent([H|T],Best) when is_tuple(Best) ->
    {_,BestCount} = Best,
    {_,Count} = H,
    if
        Count > BestCount ->
            get_most_frequent(T,H);
        true ->
            get_most_frequent(T,Best)
    end;
get_most_frequent([],Best) when is_tuple(Best) -> Best.
get_most_frequent(L) when is_list(L) ->
    Dict = accumulate_by_key(L),
    List = dict:to_list(Dict),
    get_most_frequent(List,{-1,0}).

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
        {tok,_,QueueParents,WhichLoop} ->
            case WhichLoop of
                main_loop ->
                    % switch to sub_loop
                    io:format("[~p] Received tok from parent, circulating within group.~n",[self()]),
                    io:format("[~p] Do i want to go in critical section?.~n",[self()]),
                    timer:sleep(1000),
                    QueueGroup = Children++[self()], % append to back of QueueGroup
                    [Next|Rest] = QueueGroup,
                    Next ! {tok,Rest,QueueParents,sub_loop};
                sub_loop ->
                    % switch to main_loop
                    io:format("[~p] Received tok from within group, passing to next parent.~n",[self()]),
                    timer:sleep(1000),
                    [Next|Rest] = QueueParents,
                    {top,Next} ! {tok,[],Rest++[node()],main_loop};
                true ->
                    io:format("shouldn't reach here")
            end;
        % requests
        {Pid,consensus} ->
            Values = [get_for_process(Child,consensus) || Child <- Children],
            {Value,_} = get_most_frequent(Values),
            % send max key
            ValueWeight = {Value,length(Children)},
            io:format("[~p] Sending value ~p~n",[self(),ValueWeight]),
            Pid ! ValueWeight;
        {Pid,list} ->
            Lists = [get_for_process(Child,list) || Child <- Children],
            Pid ! lists:append(Lists);
        {Pid,count} ->
            Values = [get_for_process(Child,count) || Child <- Children],
            Pid ! lists:sum(Values) + 1; % don't forget to count myself!
        {Pid,aggregate} ->
            Values = [get_for_process(Child,number) || Child <- Children],
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
        {Parent, consensus} ->
            Parent ! {self(),1};
        {Parent, list} ->
            Parent ! List;
        % notifications
        {msg, Message} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]);
        {tok,QueueGroup,QueueParents,sub_loop} ->
            io:format("[~p] Got tok.  QueueGroup is ~p~n",[self(),QueueGroup]),
            timer:sleep(1000),
            if Value > 0 ->
                critical_section(enter,self()),
                critical_section(exit,self()),
                NewValue = Value-1;
            true ->
                NewValue = Value
            end,
            [Next|Rest] = QueueGroup,
            Next ! {tok,Rest,QueueParents,sub_loop},
            %% decrement value
            loop_child({Count, NewValue, List});
            % is the loop after this loop going to create a really nasty bug?
        {_, done} ->
            io:format("[~p] Terminating.  Goodbye~n",[self()]),
            exit('Finished')
    end,
    loop_child(Data).
