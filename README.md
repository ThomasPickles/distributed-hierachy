
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
`count | number | list`\
Own group: `ds:req_parent(count).`\
Other group: `ds:req_parent('bob@unix',count).`\
System-wide: `ds:get_for_system(count).`

## TODO
- CHECK BROADCAST WORKS FROM CHILD NODE
- get rid of odd print message on same node when doing `ds:broadcast('hello',top,cascade).`
