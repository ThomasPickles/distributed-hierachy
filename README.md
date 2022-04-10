# Setup

Create a number of nodes, one on each terminal, sharing a cookie.  Each node represents a group within the network\
`erl -sname alice -setcookie abc`\
`erl -sname bob -setcookie abc`\
`erl -sname charlie -setcookie abc`

Compile the module on one of the groups:
`c(ds).`

On the first group (eg. alice), run:\
alice@unix > `ds:init().`\
Daisy chain each new group with the address of the last to get a fully-connected network (operation is transitive):\
bob@unix > `ds:init('alice@unix').`\
charlie@unix > `ds:init('bob@unix').`\
etc...

# Run

The following API is available from an erlang terminal.

**Message broadcasting**:\
`ds:broadcast(hello)` broadcasts "hello" to all processes\
`ds:broadcast(hello,no_cascade)` broadcasts "hello" to top-level nodes only

**Collective operations**\
`ds:do(What,[Group])` \
`What` = create | count | aggregate | list | consensus | weighted_consensus | mutex

*System-wide*: `ds:do(What).` \
*Own group*: `ds:do(What,node()).`\
*Another group*: `ds:do(What,'bob@unix').`

**To kill a process**\
`ds:do(kill,Group,Pid)` to terminate process `Pid` belonging to `Group`

