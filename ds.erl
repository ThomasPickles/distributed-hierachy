-module(ds).
% -compile(export_all).
-export([init/0,init/1,do/1,do/2,do/3,start_top/0,start_bottom/0,broadcast/1,broadcast/2]).
start_bottom() ->
    List = [1,2,3],
    Data = {1,rand:uniform(5),List},
    io:format("Listening on bottom [~p:~p]...\n", [node(),self()]),
    loop_bottom(Data).

start_top() ->
    io:format("Listening on top [~p:~p]...\n", [node(),self()]),
    N = 2 + rand:uniform(3), % 3 to 5 inc
    BottomProcesses=make_bottom(N),
    loop_top(BottomProcesses).

is_valid(Option,Level) ->
    SystemWideOptions = [count,aggregate,list,consensus,weighted_consensus,mutex],
    GroupWideOptions = [create|SystemWideOptions],
    case Level of
        system ->
            lists:member(Option,SystemWideOptions);
        group ->
            lists:member(Option,GroupWideOptions)
    end.
do(What) ->
    % system-wide
    IsValidOption = is_valid(What,system),
    case IsValidOption of
        true ->
            get_for_system(What);
        false ->
            io:format("[~p] Unknown option.  Exiting...",[self()])
    end.
do(What,Group) ->
    % within a group
    IsValidOption = is_valid(What,group),
        case IsValidOption of
        true when What == create ->
            create_bottom(Group);
        true ->
            get_for_group(Group,What);
        false ->
            io:format("[~p] Unknown option.  Exiting...",[self()])
    end.
do(kill,Group,Pid) ->
    kill_bottom(Group,Pid).

get_for_group(Group, What) when is_atom(What) ->
    {top,Group} ! {self(), What},
    receive
        Response ->
            Response
    end.

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
            ValueWeight = get_consensus(Values,ignore_weights),
            io:format("[~p] System-consensus is {Value,Count}=~p~n",[self(),ValueWeight]),
            {Value,_} = ValueWeight;
        weighted_consensus ->
            Values = [get_for_group(Group,consensus) || Group <- Groups],
            ValueWeight = get_consensus(Values),
            io:format("[~p] System-consensus is {Value,Count}= ~p~n",[self(),ValueWeight]),
            {Value,_} = ValueWeight;
        mutex ->
            [First|Rest] = Groups,
            TopQueue = Rest++[First], % put to back
            % create the token
            {top,First} ! {tok,[],TopQueue,top_loop},
            Value = true
    end,
    broadcast(Value,no_cascade). % let tops know

notify_top(Group, CascadeType, Message) when is_atom(CascadeType) ->
    {top,Group} ! {CascadeType, Message}.

notify_bottom(Pid, msg, Message) ->
    Pid ! {msg, Message}.

create_bottom(Group) ->
    {top,Group} ! {create}.
kill_bottom(Group,Pid) ->
    {top,Group} ! {kill,Pid}.


request_from_bottom(Pid,Request) ->
    Pid ! {self(),Request},
    receive
        Response ->
            Response
    end.


broadcast(Message,CascadeType) when is_atom(CascadeType) ->
    Groups = [node() | nodes()], % sends request to all groups
    case CascadeType of
        cascade ->
            [notify_top(Group,cascade,Message) || Group <- Groups];
        no_cascade ->
            [notify_top(Group,no_cascade,Message) || Group <- Groups];
        _ ->
            io:format("Unknown cascade type.  Exiting...~n")
    end,
    true.  % if a bottom, tell top to do it
broadcast(Message) ->
    broadcast(Message,cascade). % default broadcast is to cascade

init() ->
    io:format("I'm main proc ~p in group [~p], The other groups are [~p]\n",[self(),node(),nodes()]),
    Pid = spawn(node(), ds, start_top, []),
    io:format("Goodbye from main proc. Top proc [~p] lives on\n",[Pid]),
    register(top,Pid).

init(Node) ->
    net_adm:ping(Node), % make contact - it's transitive, can can daisy chain to connect all!
    init().


make_bottom(0) -> [];
make_bottom(N) ->
    [spawn(node(), ds, start_bottom, []) | make_bottom(N-1)].


loop_top(BottomProcesses) ->
    receive
        % notifications - no return address
        {create} ->
            Pid = spawn(node(), ds, start_bottom, []),
            io:format("[~p] New process created.  Bottom level processes now~p.~n",[self(),[Pid|BottomProcesses]]),
            loop_top([Pid|BottomProcesses]); % add to processes
        {kill,Pid} ->
            IsMember = lists:member(Pid,BottomProcesses),
            case IsMember of
                false ->
                    io:format("[~p] Couldn't kill ~p.  Not a bottom process~n",[self(),Pid]);
                true ->
                    Pid ! {kill},
                    NewBottomProcesses = lists:filter(fun(X) -> X /= Pid end, BottomProcesses),
                    loop_top(NewBottomProcesses)
            end;
        {cascade,Message} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]),
            [notify_bottom(Pid,msg,Message) || Pid <- BottomProcesses]; % cascade to bottomren
        {no_cascade,Message} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]);
        {tok,_,TopQueue,WhichLoop} ->
            case WhichLoop of
                top_loop ->
                    % switch to sub_loop
                    io:format("[~p] Received tok from top, circulating within group.~n",[self()]),
                    timer:sleep(1000),
                    BottomQueue = BottomProcesses++[self()], % append to back of BottomQueue
                    [Next|Rest] = BottomQueue,
                    Next ! {tok,Rest,TopQueue,sub_loop};
                sub_loop ->
                    % switch to top_loop
                    io:format("[~p] Received tok from within group, passing to next top.~n",[self()]),
                    timer:sleep(1000),
                    [Next|Rest] = TopQueue,
                    {top,Next} ! {tok,[],Rest++[node()],top_loop};
                true ->
                    io:format("shouldn't reach here")
            end;
        % requests
        {Pid,consensus} ->
           Values = [request_from_bottom(Id,consensus) || Id <- BottomProcesses],
           {Value,_} = get_consensus(Values),
           ValueWeight = {Value,length(BottomProcesses)},
           io:format("[~p] Group consensus is {Value,Count}=~p~n",[self(),ValueWeight]),
           Pid ! ValueWeight;
        {Pid,list} ->
           Lists = [request_from_bottom(Id,list) || Id <- BottomProcesses],
           Pid ! lists:append(Lists);
        {Pid,count} ->
           Values = [request_from_bottom(Id,count) || Id <- BottomProcesses],
           Pid ! lists:sum(Values) + 1; % don't forget to count myself!
        {Pid,aggregate} ->
            Values = [request_from_bottom(Id,number) || Id <- BottomProcesses],
            Pid ! lists:sum(Values)
    end,
    loop_top(BottomProcesses).

loop_bottom(Data) ->
    {Count, Value, List} = Data,
    receive
        % requests
        {Parent, count} ->
            Parent ! Count;
        {Parent, number} ->
            Parent ! Value;
        {Parent, consensus} ->
            io:format("[~p] Proposing {Val:~p,Count:1}~n",[self(),Value]),
            Parent ! {Value,1};
        {Parent, list} ->
            Parent ! List;
        % notifications - no return address
        {msg, Message} ->
            io:format("[~p] Got a msg: ~p~n",[self(),Message]);
        {tok,BottomQueue,TopQueue,sub_loop} ->
            timer:sleep(1000),
            if Value > 0 ->
                print_mutex(enter,self(),Value),
                print_mutex(exit,self(),Value-1),
                NewValue = Value-1;
            true ->
                io:format("[~p] Got tok.  No need to enter critical section~n",[self()]),
                NewValue = Value
            end,
            [Next|Rest] = BottomQueue,
            Next ! {tok,Rest,TopQueue,sub_loop},
            %% decrement value
            loop_bottom({Count, NewValue, List});
            % is the loop after this loop going to create a really nasty bug?
        {kill} ->
            io:format("[~p] Terminating.  Goodbye~n",[self()]),
            exit('Finished')
    end,
    loop_bottom(Data).

% Utility functions
print_mutex(Direction,Pid,Times) ->
    io:format("*** Pid ~p ~ping critical section. N=~p ***~n",[Pid,Direction,Times]).

accumulate_by_key(Dict,[H|T],ShouldIgnoreWeights) ->
    case ShouldIgnoreWeights of
        false ->
            {Val,Count} = H;
        true ->
            {Val,_} = H,
            Count = 1 % all candidates count equally
        end,
    % if key if found in dict, add count to its value, if not, initialise it to count
    NewDict = dict:update_counter(Val, Count, Dict),
    accumulate_by_key(NewDict, T,ShouldIgnoreWeights);
accumulate_by_key(Dict,[],_) -> Dict.
accumulate_by_key(L,ignore_weights) ->
    accumulate_by_key(dict:new(),L,true).
accumulate_by_key(L) ->
    accumulate_by_key(dict:new(),L,false).

get_consensus(L,ignore_weights) ->
    Dict = accumulate_by_key(L,ignore_weights),
    List = dict:to_list(Dict),
    get_most_frequent(List,{-1,0}).
get_consensus(L) ->
    Dict = accumulate_by_key(L),
    List = dict:to_list(Dict),
    get_most_frequent(List,{-1,0}).

get_most_frequent([H|T],Best) when is_tuple(Best) ->
    {_,BestCount} = Best,
    {_,Count} = H,
    if
        Count > BestCount ->
            % found a higher count
            get_most_frequent(T,H);
        true ->
            get_most_frequent(T,Best)
    end;
get_most_frequent([],Best) when is_tuple(Best) -> Best.
