
### Setup

Start a number of terminals.
`erl -sname alice -setcookie abc`
`erl -sname bob -setcookie abc`
`erl -sname eve -setcookie abc`

Daisy chain them, starting with alice
alice@unix: `ds:init().`
bob@unix: `ds:init('alice@unix').`
eve@unix: `ds:init('bob@unix').`

To get own count:
alice@unix:`ds:rpc(top,count).`
To get another group's count:
alice@unix:`ds:rpc('bob@unix',top,count).`
