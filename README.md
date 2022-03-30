
### Setup

Start a number of terminals, sharing a cookie.\
`erl -sname alice -setcookie abc`\
`erl -sname bob -setcookie abc`\
`erl -sname charlie -setcookie abc`

Daisy chain them with the previous to get a fully connected network\
alice@unix: `ds:init().`\
bob@unix: `ds:init('alice@unix').`\
charlie@unix: `ds:init('bob@unix').`

### Queries

**To query the system**\
`count | aggregate | list`\
Own group: `ds:get_for_group(count).`\
Other group: `ds:get_for_group('bob@unix',count).`\
System-wide: `ds:get_for_system(count).`

**Consensus**\
Own group: `ds:get_for_group(consensus).`\
System-wide (each group has an equal say): `ds:get_for_group(consensus).`\
System-wide (each group counts in proportion to its weight): `ds:get_for_group(weighted_consensus).`\

**Consensus**\
System-wide: `ds:mutex().`\


## TODO
- add/delete processes dynamically

# Notes

## Broadcast

- Parent nodes are contacted first.  They accept a parameter saying whether to cascade the message to their child nodes.

## Consensus
For consensus, we choose a value based on the most-frequent in the group
- Aggregation proceeds using a map-reduce pipeline:
  - each child proposes a {value,count} tuple, with count=1 for children
  - parent maps these to a dictionary keyed by value and incremented by count
  - parent does a reduce over the dictionary keeping on the tuple with the largest count
  - parent sends {value,child_count} to other parents
- Since the aggregation is based on a dictionary-implementation, in the event of a tie, there is no guarantee on any particular ordering.  Accordingly, consensus value may be unstable.
- Node values are not updated based on a calling of the consensus algorithm.
- Chose most frequent value for each group.  However, due to the grouped nature of the data simpson's paradox means this might not be the most frequent in the system.

## Mutex
Why not use the consensus to determine which process can enter critical section?  We need a mutex to have two key properties: liveness, and starvation free.  The first one ensures the good behaviour, the second avoids bad
FIXME - is this the right way around?
- Safety: at most one process can enter the critical section at a time
- Liveness/Starvation-free: every process the wants to enter the critical section will get there in a finite amount of time
So what's wrong with using the consensus algorithm?  That certainly ensures liveness, since all processes agree on the one which can enter (if we set our consensus condition to be the max process id, for example).  However, we have no guarantee that any other process will eventually enter the critical section.

What about if we insist that a process that has finished in the critical section must pass the lock to another process?  That would stop a process monopolising the lock. However, it doesn't avoid the case of two processes constantly passing the lock between themselves, or a clique retaining control of the lock so that some processes remain excluded indefinitely.  To provide the starvation-free guarantee that we want, we need some sort of notion of a priority queue, so that all processes will eventually have their turn in the critical section.

There are a number of possible implemenations.  We restriction the discussion to token-based approaches. The token ensures a safety property , we could chose to link processes into a ring.  In this hierarchical architecture, this is a natural choice, since we can link all parents though a main ring, and a parent and its children via sub rings off the main ring.  The token could then circulate through the main ring, doing a loop through though each group before being passed to the next parent in the ring.

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