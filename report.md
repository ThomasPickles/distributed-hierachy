
Finally, you are expected to provide a small report which should details your choices of implementation and discussion about your solutions and features (strength, weaknesses and sketch of proofs for desired properties).


# Report

The hierarchical system confers benefits from an implementation point of view.  The main one of these is the ability to recurse on the levels.  For example, the aggregation that happens between top-level nodes between groups is identical to the aggregation between the processes within a group.  This leads to very straightforward and reusable algorithms.  The main difference is that, within a group, each process carries the same weight whereas, between groups, each top-level node may represent different group sizes.  Retaining these weightings complicates the implementation slightly, since each parent process must send an additional data item representing the count within its group.

## Broadcast

- Parent nodes are contacted first.  They accept a parameter saying whether to cascade the message to their child nodes.


## Consensus
The criterion for choosing a consensus value is fairly free.  Common approaches are to pick the highest or lowest id, or the most common value, or perhaps the median value.  However, for consistency, we cannot select an operation that gives a value that is not held by one of the processes; accordingly, operators like the mean or the sum cannot be used.

For our consensus, we choose a value based on the most-frequent in the group.  The reason is that, since we are also required to do a weighted consensus, we will need some notion of frequency, so it is simplest to build this directly into the selection criterion.
- Aggregation proceeds using a map-reduce pipeline:
  - each child proposes a {value,count} tuple, with count=1 for children
  - parent maps these to a dictionary keyed by value and incremented by count
  - parent does a reduce over the dictionary keeping on the tuple with the largest count
  - parent sends {value,child_count} to other parents
- Since the aggregation is based on a dictionary-implementation, in the event of a tie, there is no guarantee on any particular ordering.  Accordingly, the consensus value may be unstable.
- Node values are not updated based on a calling of the consensus algorithm.
- Chose most frequent value for each group.  However, due to the grouped nature of the data simpson's paradox means this might not be the most frequent in the system.

A possible extension is to broadcast the consensus value to all processes within the system, and require them to update their values.  I have not implemented this, but it is not a complicated addition, requiring just one additional rule on bottom-level processes.
- Todo: have i got time to make this improvement?  Seems simple enough

## Mutex

To implement a mutex, a reasonable question is why not simply use the consensus to determine which process can enter critical section?  We could, for example, change our consensus condition to return the id of the highest id process, and then all processes would have consensus on which process may enter the critical section.  To see why this is not a possible solution, we need to remember the two key properties of a successful mutex implementation: safely, and liveness.
- Safety: at most one process can enter the critical section at a time
- Liveness/Starvation-free: every process the wants to enter the critical section will get there in a finite amount of time
The safety condition ensures we avoid bad behaviour, the second allows good behaviour to happen.
So what's wrong with using the consensus algorithm?  It certainly ensures safety, since all processes agree on the one which can enter (if we set our consensus condition to be the max process id, for example).  However, we have no guarantee that any other process will eventually enter the critical section.

What about if we insist that a process that has finished in the critical section must pass the lock to another process?  That would stop a process monopolising the lock. However, it doesn't avoid the case of two processes constantly passing the lock between themselves, or a clique retaining control of the lock so that some processes remain excluded indefinitely.  To provide the starvation-free guarantee that we want, we need some sort of notion of a priority queue, so that all processes will eventually have their turn in the critical section.

There are a number of possible implemenations.  We restriction the discussion to token-based approaches. The token ensures a safety property , we could chose to link processes into a ring.  In this hierarchical architecture, this is a natural choice, since we can link all parents though a main ring, and a parent and its children via sub rings off the main ring.  The token could then circulate through the main ring, doing a loop through though each group before being passed to the next parent in the ring.

The circulation of the token plays the role of the queue.  After a process exits the critical section, it passes on the token through every process in the network before it can next receive it.

- parent starts with token,
- parent decorates token with a list of its children
- sends token to child at head of list
- on receipt of token,
- if child wants to enter critical section
  -- it does
  -- it decrements its counter
- child puts its value to the tail of the list and sends token on to next child on the list by sending its value to the back of t
- when token gets back to parent, it sends on to next parent in list

An alternative is the Suzuki-Kasami implementaion [reference](), where we ensure.

An improvement to the algorithm is a request-based implementation.  In this case the token could circulate around the ring of top-level nodes; the top level node would only switch to the sub-rings if a bottom-level process requests the token.  This ensures that the token does not take needless trajectories around the sub-rings if none of the processes of a group want to enter the critical section.
- Todo: have i got time to make this improvement?  Seems simple enough
