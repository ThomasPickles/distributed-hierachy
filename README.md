### To compile:

From the erlang shell.
`c(ds).`

Initialise a parent process:
`Pid = spawn(node(), ds, initialise_parent, []).`

Get group count:
`ds:rpc(Pid,count).`
Get group total
`ds:rpc(Pid,number).`


### Option 2

`ds:init().`
`ds:rpc(<parent_proc_no>,count).`
