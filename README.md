
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
`count | aggregate | list | consensus`\
Own group: `ds:get_for_group(count).`\
Other group: `ds:get_for_group('bob@unix',count).`\
System-wide: `ds:get_for_system(count).`

## TODO
- do we need to update values after consensus?

## consensus

- chose most frequent value for each group.  However, due to the grouped nature of the data simpson's paradox means this might not be the most frequent in the system.