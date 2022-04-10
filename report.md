# Hierachical Systems

## Introduction

We consider a fully-connected network of top-level nodes, with each acting as a co-ordinator for a network of bottom-level nodes.

From a high-performance computing (HPC) point of view, the hierarchical system confers a number of benefits.  In effect, it has a tree-like structure and, if the branching factors of the each level are fairly consistent, good load-balancing properties as well as a low diameter while minimising the number of links in the system.  For example, in our two-level system, the maximum distance to pass a message (between bottom-level process in separate groups) is 3 steps: bottom->top->top->bottom.

The hierarchical system confers benefits also from an implementation point of view.  The main one of these is the ability to recurse on the levels.  For example, the aggregation that happens between top-level nodes between groups is identical to the aggregation between the processes within a group.  This leads to very straightforward and reusable algorithms.  The only difference in our case being that, since top-level nodes may represent different group sizes each top process must send an additional count parameter.  For bottom-level (leaf) processes, each count is 1.

### Basic functions

Refer to README.md for basic API.

### Broadcast

When broadcasting a value within the system, the user can choose the depth to cascade down to.  In this case, top-level nodes accept a parameter which determines whether or not to cascade the message down to the bottom-level processes in its group.

## Consensus

The criteria for selecting the consensus value is fairly open.  Common approaches are to pick the highest or lowest id, or the most common value, or perhaps the median value.  However, for consistency, we must select a value from one of the processes; accordingly, operators like the mean or the sum cannot be used.

For our consensus, we choose a value based on the most-frequent in the group.  This is mostly for convenience since, for weighted-consensus, we need some notion of frequency, so we may as well build this directly into the selection criterion.  Note that (as I have interpreted the question), top-level nodes do not propose a value themselves, they only perform the aggregation and broadcast the consensus value to other top-level nodes.

*Consensus Algorithm:*\
Initiator:
```code
consensus():
  values <- {}
  foreach top in tops:
    send(<request>,self,top)
    u, count <- receive(<proposal>,top)
    count = 1 if unweighted else count
    values[u] += count
  v <- keymax(values)
  broadcast(<chosen>,v)
```

Top:
```
receive(<request>,InitId):
  values <- {}
  foreach bottom in group:
    send(<request>,self,bottom)
    u <- receive(<proposal>,bottom)
    values[u] += 1
  v <- keymax(values)
  send(<proposal>,InitId,{v,group.count})
```

Bottom:
```
receive(<req>,Pid):
  send(<proposal>,Pid,value)
```

Notes on implementation:

- Since the aggregation is based on a dictionary-implementation, in the event of a tie, there is no guarantee on any particular ordering.  Accordingly, the consensus value may be unstable between repeated executions.

- A point to recognise is the consensus value may not actually be the most frequent value in the system. Due to the grouped nature of the data, the most frequent of the most frequent,  might not be the most frequent in the system.  This effect is often known as Simpson's paradox.

- A possible extension is to broadcast the consensus value to all processes within the system, and require them to update their values.  I have not implemented this, but it is not a complicated addition, requiring just one additional rule on bottom-level processes.

## Mutex

To implement a mutex, a reasonable question is why not simply use the consensus to determine which process can enter critical section?  We could, for example, change our consensus condition to return the id of the highest id process, and then all processes would have consensus on which process may enter the critical section.  To see why this is not a possible solution, we need to remember the two key properties of a successful mutex implementation:
> **Safety**: at most one process can enter the critical section at a time\
> **Liveness**: every process the wants to enter the critical section will get there in a finite amount of time (starvation-free)

The safety condition ensures we avoid bad behaviour, the second enforces good behaviour.

So what's wrong with using the consensus algorithm to implement the mutex?  Consensus certainly ensures safety, since all processes agree on the one which can enter (if we set our consensus condition to be the max process id, for example).  However, we have no guarantee that any other process will eventually enter the critical section.

What about if we insist that a process that has finished in the critical section must pass the lock to another process?  That would stop a single process monopolising the lock. However, it doesn't avoid the case of two processes constantly passing the lock between themselves, or a clique retaining control of the lock so that some processes remain excluded indefinitely.  To provide the starvation-free guarantee that we want, we need to maintain a priority queue, so that all processes will eventually have their turn in the critical section.

There are a number of possible implemenations.  We restriction the discussion to token-based approaches. In this implementation, only the process holding the token has the right to enter the critical section. The token thus ensures the safety property.

### Implementation

In this hierarchical architecture the ring is a natural choice, since we can link all parents though a main ring, and a parent and its children via sub rings off the main ring.  In effect, the token then circulates around all processes through the main ring, doing a tour around the sub-ring of the group before being passed to the next top in the main ring.
We thus ensure the liveness property because each process sees the token with every pass around the main ring, so that a single process cannot monopolise the lock.

![Fig 1](ring.png)

After a process exits the critical section, it passes on the token through every process in the network before it can next receive it. In order to traverse the network, the token keeps track of two queues, the top-level groups, and the bottom-level processes within its current group.

An improvement to the algorithm is a request-based implementation.  In this case the token could circulate around the ring of top-level nodes; the top level node would then only switch to its sub-ring if a bottom-level process in the group had requested the token since the last passage of the token.  In effect, each top-level node keeps a list of bottom-level processes that wish to enter the critical section.  On receipt of the token, the top-level node thus checks whether there are any processes in the list and, if so, appends its own Pid to this queue before circulating in the group.   This ensures that the token does not take needless tours around the sub-ring if none of the processes of a group want to enter the critical section.

## Limitations

Due to time constraints, I have not made the system tolerant to faults.  Examples of these faults would be lost messages and node crashes.  However, I briefly discuss how these extensions could be implemented.

### Lost messages

A simple solution to cope with lost messages is to request an acknowledgement for each sent message.  If an ack is not received within a certain timeframe, the original message is resent.  Since the original message may have been received and the ack message lost, messages are sent with a sequence number.  This is only effective in a synchoronous system where it is possible to bound the travel time of a message; in an async system, there is no upper bound on the time to receive a message, and so slow messages and lost messages are indistinguishable.

### Process crashes

In order to cope with crashed processes, the other processes may periodically poll other processes by sending status requests.  If no response is received from a particular process, the other processes can re-establish any system invariants without the dead process (by regenerating the ring with the remaining processes, and regenerating any mutex tokens that may have been lost in the crash or calling a new leadership election if the dead process was previously the leader).