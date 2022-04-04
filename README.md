# TODO: REVERT TO EXPORT ALL?!

# HowTo

Start a number of terminals, sharing a cookie.  Each terminal is a group within the network\
`erl -sname alice -setcookie abc`\
`erl -sname bob -setcookie abc`\
`erl -sname charlie -setcookie abc`

Compile the code on one of the groups.
`c(ds).`

Daisy chain the groups with the previous to get a fully connected network\
alice@unix: `ds:init().`\
bob@unix: `ds:init('alice@unix').`\
charlie@unix: `ds:init('bob@unix').`

### Queries

**To add a process**
`create_bottom([Group])` ->
`kill_bottom(Pid,[Group])` ->


**To query the system**\
`count -> int | aggregate -> int | list -> List[int] | consensus -> {value:int,weight:int}`\
todo: could i modify this as `get(atom,[Group])`, and then pass `node()` for own group
Own group: `ds:get_for_group(count).`\
Other group: `ds:get_for_group('bob@unix',count).`\
System-wide: `ds:get_for_system(count).`

**Consensus**\
Own group: `ds:get_for_group(consensus).`\
System-wide (each group has an equal say): `ds:get_for_system(consensus).`\
System-wide (each group counts in proportion to its weight): `ds:get_for_system(weighted_consensus).`\


**Mutex**\
System-wide: `ds:mutex().`\


## TODO
- what does Q4d mean?  Dont wait for a group to finish... Is it one process from each group that has the mutex at a time, or one from the whole system?
- perhaps remove export_all

