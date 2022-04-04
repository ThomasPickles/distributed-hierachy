# Setup

Start a number of terminals, sharing a cookie.  Each terminal is a group within the network\
`erl -sname alice -setcookie abc`\
`erl -sname bob -setcookie abc`\
`erl -sname charlie -setcookie abc`

Compile the code on one of the groups:
`c(ds).`

Daisy chain the groups with the previous group to get a fully connected network\
alice@unix: `ds:init().`\
bob@unix: `ds:init('alice@unix').`\
charlie@unix: `ds:init('bob@unix').`

# Run

The following API is available from an erlang terminal.

**Message broadcasting**:\
`ds:broadcast(hello)` broadcasts "hello" to all processes
`ds:broadcast(hello,no_cascade)` broadcasts "hello" to top-level nodes

**Collective operations**\
`ds:do(What,[Group])` \
`What` = count | aggregate | list | create | consensus | weighted-consensus | mutex

*System-wide*: `ds:do(What).` \
*Own group*: `ds:do(What,node()).`\
*Another group*: `ds:do(What,'bob@unix').`\

**To kill a process**\
`ds:do(kill,Group,Pid)` to terminate a process in {Group,Pid}

