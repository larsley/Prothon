# Prothon

Prothon allows a (SICStus) Prolog client to execute arbitrary function calls on a Python server. 

**While the program is still in the 0.y.z versions the API might change with any new minor version (x.Y.z)**

## Running

### Server

Requires python3.10 and numpy.

You can run the server with default settings by runnng `python prothon/server/prothon.py`

Wether or not to use a virtual environment is up to you, but keep in mind that all the packages that you want to import into the server at runtime need to be accessable.

You have the ability to change the binding port and address as well as white listing modules or built-in functions. If you whitelist anything every other function or module will be implicitly blacklisted.

```
usage: prothon.py [-h] [-p PORT] [-a ADDRESS] [-m MODULE [MODULE ...]]
                  [-b BUILDIN [BUILDIN ...]]

Server executing arbitrary Python function calls

options:
  -h, --help            show this help message and exit
  -p PORT, --port PORT  port to listen on
  -a ADDRESS, --address ADDRESS
                        IP address of the server
  -m MODULE [MODULE ...], --module MODULE [MODULE ...]
                        adds the specified module(s) to the whitelist
  -b BUILDIN [BUILDIN ...], --buildin BUILDIN [BUILDIN ...]
                        adds the specified built-in(s) to the whitelist
```

### Client

The client requires SICStus Prolog

The Prothon client can be used either directly

`sicstus -l prothon/client/prothon.pl`

or as a module

`:- use_module('path-to/prothon/client/prothon')`

#### API

Proper documentation will follow

>pyconnect
>
>pyconnect(+Address)
>>Connects to the server. The address defaults to the localhost if omitted.
>	
>pycall(+Function, +Args)
>
>pycall(+Function, +Args, +KeywordArgs)
>
>pycall(+Function, +Args, +KeywordArgs, -Answer)
>>Calls the function with the name Function, which must be an atom. If it is not a built-in, it needs to be prefixed with the module it belongs to with its name 		exactly how it was imported. Args contains all positional arguments in a list. 		KeywordArgs can either be an AVL-tree with only Prothon compatible strings as its keys or a list of key(value) pairs.
>>
>>Answer is unified with a Python-reference to the object the function returns and in case an exception occurred on the server it is propagated. If Answer is omitted the return value needs to be fetched manually and therefore an eventual exception is not thrown.
>>
>>Example:
>>
>>pycall(’np.array’, [[1, 2, 3]], dict([dtype(string("int"))]), NpA)
>
>pymethod(+Object, +Method, +Args )
>
>pymethod(+Object, +Method, +Args, +KeywordArgs )
>
>pymethod(+Object, +Method, +Args, +KeywordArgs, -Answer )
>
>>Works analogous to pycall except that it does call a method on the provided object.
>>
>>Example:
>>
>>pymethod(NpA, ’sort’, [], dict([axis(0)]), NpA)
>
>pyanswer(-Answer )
>>Fetches the most recently returned object from the server and unifies its Python-reference with Answer. If an exception was thrown on the server while generating the object, the exception it is propagated.
>	
>pyimport(+Module )
>
>pyimport(+Module, +Alias )
>>Imports a module into the server, with an optional alias. If an alias is used the module can only be referenced by that alias.
>
>pygarbage(+Objects )
>>Removes the objects from the server.
>
>pykeep(+Objects )
>>Removes all objects except the given ones from the server.
>
>pyderef(+Object, -DerefObj )
>>Dereferences the object turning it into a constant representation of its value. If the object is a collection its contents will not be dereferenced. Notably dereferencing a list of integers returns a list of object references to those integers and not the integers themselves.
>>
>>Examples:
>>
>>pyderef(Subplot, tuple([Fig, Ax]))
>>
>>pyderef(NpA, [E1, E2, E3])
>
>pyderefall(+Object, -DerefObj )
>>Dereferences the object turning it into a constant representation of its value. For collections this is done recursively.
>>
>>Example:
>>
>>pyderefall(Npa, [1, 2, 3])

## Security

**Be aware that allowing connections to the server from anywhere else but the localhost is a serious security risk!**

The server allows anyone connected to execute arbitary Python code and therefore has complete control over the machine with the privileges of the user that runs the server.

Any barriers that are set up, like the whitelist, must not be understood as sound security features. Anyone that tries, will find their way around them with relative ease!

At the moment the only way to protect the server from misuse is by having a well configured firewall in place.

## License

> Copyright © 2022-2023 by Lars Leyendecker.
> 
> This program and the accompanying materials are made available under the terms of the Eclipse Public License v2.0 which accompanies this distribution and is available at:
> 
> > [http://www.eclipse.org/legal/epl-2.0/](http://www.eclipse.org/legal/epl-2.0/)
> 
> This program may also be made available under the following secondary licenses when the conditions for such availability set forth in the Eclipse Public License v2.0 are satisfied:
> 
> > GNU General Public License, Version 2.0, or any later versions of that license
> 
> SPDX-License-Identifier: EPL-2.0 OR GPL-2.0-or-later
