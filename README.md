
### Setup

Start a number of terminals, sharing a cookie.\
`erl -sname alice -setcookie abc`\
`erl -sname bob -setcookie abc`\
`erl -sname eve -setcookie abc`

Daisy chain them, starting with alice\
alice@unix: `ds:init().`\
bob@unix: `ds:init('alice@unix').`\
eve@unix: `ds:init('bob@unix').`

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