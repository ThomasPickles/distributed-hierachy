# Hierachical Systems
### Thomas Pickles

## Introduction

We consider a fully-connected network of top-level nodes, with each acting as a co-ordinator for a network of bottom-level nodes.

The hierarchical system confers benefits from an implementation point of view.  The main one of these is the ability to recurse on the levels.  For example, the aggregation that happens between top-level nodes between groups is identical to the aggregation between the processes within a group.  This leads to very straightforward and reusable algorithms.  The main difference is that, within a group, each process carries the same weight whereas, between groups, each top-level node may represent different group sizes.  Retaining these weightings complicates the implementation slightly, since each parent process must send an additional data item representing the count within its group.

### Broadcast

--> todo

Within a hierarchical system, the broadcast can be done down to a certain depth.  In this case, top-level nodes accept a parameter which determines whether or not to cascade the message down to the bottom-level processes in its group.

- Parent nodes are contacted first.  They accept a parameter saying whether to cascade the message to their child nodes.


### Consensus

The criterion for choosing a consensus value is fairly free.  Common approaches are to pick the highest or lowest id, or the most common value, or perhaps the median value.  However, for consistency, we cannot select an operation that gives a value that is not held by one of the processes; accordingly, operators like the mean or the sum cannot be used.

For our consensus, we choose a value based on the most-frequent in the group.  The reason is that, since we are also required to do a weighted consensus, we will need some notion of frequency, so it is simplest to build this directly into the selection criterion.  Note that (as I have interpreted the question), top-level nodes do not propose a value themselves, they only perform the aggregation and transmit values to other top-level nodes.

*Consensus Algorithm:*
- Aggregation proceeds using a map-reduce pipeline:
  - each child proposes a {value,count} tuple, with count=1 for children
  - parent maps these to a dictionary keyed by value and incremented by count
  - parent does a reduce over the dictionary keeping on the tuple with the largest count
  - parent sends {value,child_count} to other parents
- Since the aggregation is based on a dictionary-implementation, in the event of a tie, there is no guarantee on any particular ordering.  Accordingly, the consensus value may be unstable.
- Node values are not updated based on a calling of the consensus algorithm.
- Chose most frequent value for each group.  However, due to the grouped nature of the data simpson's paradox means this might not be the most frequent in the system.

A possible extension is to broadcast the consensus value to all processes within the system, and require them to update their values.  I have not implemented this, but it is not a complicated addition, requiring just one additional rule on bottom-level processes.

### Mutex

To implement a mutex, a reasonable question is why not simply use the consensus to determine which process can enter critical section?  We could, for example, change our consensus condition to return the id of the highest id process, and then all processes would have consensus on which process may enter the critical section.  To see why this is not a possible solution, we need to remember the two key properties of a successful mutex implementation: safely, and liveness.
- Safety: at most one process can enter the critical section at a time
- Liveness/Starvation-free: every process the wants to enter the critical section will get there in a finite amount of time

The safety condition ensures we avoid bad behaviour, the second enforces good behaviour.

So what's wrong with using the consensus algorithm to implement the mutex?  Consensus certainly ensures safety, since all processes agree on the one which can enter (if we set our consensus condition to be the max process id, for example).  However, we have no guarantee that any other process will eventually enter the critical section.

What about if we insist that a process that has finished in the critical section must pass the lock to another process?  That would stop a single process monopolising the lock. However, it doesn't avoid the case of two processes constantly passing the lock between themselves, or a clique retaining control of the lock so that some processes remain excluded indefinitely.  To provide the starvation-free guarantee that we want, we need to maintain a priority queue, so that all processes will eventually have their turn in the critical section.

There are a number of possible implemenations.  We restriction the discussion to token-based approaches. In this implementation, only the process holding the token has the right to enter the critical section. The token thus ensures the safety property.

Further, we ensure the liveness property by circulating the token in a virtual ring of all the gropus, and all the processes.  In this hierarchical architecture the ring is a natural choice, since we can link all parents though a main ring, and a parent and its children via sub rings off the main ring.  The token then circulates through the main ring, doing a loop through though each group before being passed to the next parent in the ring.

![Fig 1](ring.png)

After a process exits the critical section, it passes on the token through every process in the network before it can next receive it. In order to traverse the network, the token keeps track of two queues, the top-level groups, and the bottom-level processes within its current group.

### Mutex algorithm
top-level node starts with token,
node adds a list of its children to the token, appending its own id to the list
- while the queue is not empty
- next = queue.dequeue()
- send to next
- on receipt of token,
- if child wants to enter critical section
  -- it does
  -- it decrements its counter
- child puts its value to the tail of the list and sends token on to next child on the list by sending its value to the back of t
- when token gets back to parent, it sends on to next parent in list

An improvement to the algorithm is a request-based implementation.  In this case the token could circulate around the ring of top-level nodes; the top level node would only switch to the sub-rings if a bottom-level process requests the token.  In effect, each top-level node keeps a list of bottom-level processes that wish to enter the critical section.  On receipt of the token, the top-level node thus checks whether there are any processes in the list and, if so, appends its own Pid to the queue before circulating in the group.   This ensures that the token does not take needless trajectories around the sub-rings if none of the processes of a group want to enter the critical section.
- picture of ring within ring
- Todo: have i got time to make this improvement?  Seems simple enough

An alternative is the Suzuki-Kasami implementaion [reference](), where we ensure.

## Limitations

Due to time constraints, I have not made the system tolerant to faults.  Examples of these faults would be lost messages and node crashes.  However, I briefly discuss how these extensions could be implemented.

### Lost messages

A simple solution to cope with lost messages is to request an acknowledgement for each sent message.  If an ack is not received within a certain timeframe, the original message is resent.  Since the original message may have been received and the ack message lost, messages are sent with a sequence number.  This is only effective in a synchoronous system where it is possible to bound the travel time of a message; in an async system, there is no upper bound on the time to receive a message, and so slow messages and lost messages are indistinguishable.

### Process crashes

In order to cope with crashed processes, the other processes may periodically poll other processes by sending status requests.  If no response is received from a particular process, the other processes can re-establish any system invariants without the dead process (by regenerating the ring with the remaining processes, and regenerating any mutex tokens that may have been lost in the crash or calling a new leadership election if the dead process was previously the leader).