# efuse [![Build Status](https://travis-ci.org/mwri/erlang-efuse.svg?branch=master)](https://travis-ci.org/mwri/erlang-efuse)

This is an Erlang FUSE (Filesystem in Userspace) interface. You can use
it to create additional file structure in your filesystem, defined in
Erlang.

To do so, you need to write an implementation module (a module that
implements the 'efuse_fs' behaviour). All this really involves is
answer fuse's questions about your files, for example fuse asks
you what files are in a given directory and you return a list of
names, or fuse asks you what a given file is, and you reply that
it is a file with N bytes of content, P permissions, etc.

## Example filesystems

Three example file system come with efuse for you to peruse.

#### efuse_hellofs

More or less the simplest possible case, it has a file and a sym
link in the root directory.

#### efuse_examplefs

Not a lot more interesting than efuse_hellofs but with some more
objects.

#### efuse_erlfs

Much more interesting, more than just a static set of
objects, necessitating a more creative implementation. This
filesystem allows access to aspects of the run time state of
the Erlang VM via the filesystem (for example, in the "pids"
directory you will find a directory for every process currently
running.

To try out the example 'Erlang FS' filesystem implemented
by the 'efuse_erlfs' module, create the "/tmp/erlfs" directory and
do this this in your Erlang shell:

```
1> application:start(efuse).
ok
2> efuse:mount("/tmp/erlfs", efuse_erlfs, undefined).
{ok,<0.40.0>}
```

Now, look inside the /tmp/erlfs directory and you should find it populated.

You can unmount the filesystem like this:

```
3> efuse:umount("/tmp/erlfs").
{ok,{stopping, <0.40.0>}}
```

Your filesystem experience should then be something like this:

```
$ cd /tmp/erlfs/
$ ls -l
total 0
drwxr-xr-x 2 root root 0 Jan  1  1970 apps
drwxr-xr-x 2 root root 0 Jan  1  1970 code
drwxr-xr-x 2 root root 0 Jan  1  1970 names
drwxr-xr-x 2 root root 0 Jan  1  1970 nodes
drwxr-xr-x 2 root root 0 Jan  1  1970 pids
$ ls -l apps
total 0
drwxr-xr-x 2 root root 0 Jan  1  1970 efuse
drwxr-xr-x 2 root root 0 Jan  1  1970 kernel
drwxr-xr-x 2 root root 0 Jan  1  1970 stdlib
$ ls -l apps/efuse/
total 0
lrwxr-xr-x 1 root root  0 Jan  1  1970 app_proc -> /tmp/erlfs/pids/<0.36.0>
-rw-r--r-- 1 root root 38 Jan  1  1970 descr
drwxr-xr-x 2 root root  0 Jan  1  1970 env
lrwxr-xr-x 1 root root  0 Jan  1  1970 top_sup -> /tmp/erlfs/pids/<0.38.0>
-rw-r--r-- 1 root root  6 Jan  1  1970 vsn
$ cat apps/efuse/descr 
Erlang FUSE (Filesystem in Userspace)
$ ls -l apps/efuse/app_proc/
total 0
drwxr-xr-x 2 root root 0 Jan  1  1970 linked
drwxr-xr-x 2 root root 0 Jan  1  1970 process_info
$ ls -l apps/efuse/app_proc/linked/
total 0
lrwxr-xr-x 1 root root 0 Jan  1  1970 <0.37.0> -> /tmp/erlfs/pids/<0.37.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 <0.7.0> -> /tmp/erlfs/pids/<0.7.0>
$ cat code/modules/efuse/file 
/home/mjw/dev/erlang/efuse-1.0.0/ebin/efuse.beam
$ ls -l names/local/
total 0
lrwxr-xr-x 1 root root 0 Jan  1  1970 application_controller -> /tmp/erlfs/pids/<0.7.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 code_server -> /tmp/erlfs/pids/<0.20.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 efuse_mount_sup -> /tmp/erlfs/pids/<0.39.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 efuse_sup -> /tmp/erlfs/pids/<0.38.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 erl_prim_loader -> /tmp/erlfs/pids/<0.3.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 error_logger -> /tmp/erlfs/pids/<0.6.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 file_server_2 -> /tmp/erlfs/pids/<0.19.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 global_group -> /tmp/erlfs/pids/<0.18.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 global_name_server -> /tmp/erlfs/pids/<0.13.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 inet_db -> /tmp/erlfs/pids/<0.16.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 init -> /tmp/erlfs/pids/<0.0.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 kernel_safe_sup -> /tmp/erlfs/pids/<0.29.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 kernel_sup -> /tmp/erlfs/pids/<0.11.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 rex -> /tmp/erlfs/pids/<0.12.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 standard_error -> /tmp/erlfs/pids/<0.22.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 standard_error_sup -> /tmp/erlfs/pids/<0.21.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 user -> /tmp/erlfs/pids/<0.25.0>
lrwxr-xr-x 1 root root 0 Jan  1  1970 user_drv -> /tmp/erlfs/pids/<0.24.0>
$ ls -la pids/\<0.41.0\>/process_info/
total 0
drwxr-xr-x 2 root root  0 Jan  1  1970 .
drwxr-xr-x 2 root root  0 Jan  1  1970 ..
-rw-r--r-- 1 root root 33 Jan  1  1970 current_function
-rw-r--r-- 1 root root 74 Jan  1  1970 dictionary
-rw-r--r-- 1 root root 14 Jan  1  1970 error_handler
-rw-r--r-- 1 root root 93 Jan  1  1970 garbage_collection
-rw-r--r-- 1 root root  9 Jan  1  1970 group_leader
-rw-r--r-- 1 root root  4 Jan  1  1970 heap_size
-rw-r--r-- 1 root root 20 Jan  1  1970 initial_call
-rw-r--r-- 1 root root 19 Jan  1  1970 links
-rw-r--r-- 1 root root  2 Jan  1  1970 message_queue_len
-rw-r--r-- 1 root root  3 Jan  1  1970 messages
-rw-r--r-- 1 root root  7 Jan  1  1970 priority
-rw-r--r-- 1 root root  4 Jan  1  1970 reductions
-rw-r--r-- 1 root root  2 Jan  1  1970 stack_size
-rw-r--r-- 1 root root  8 Jan  1  1970 status
-rw-r--r-- 1 root root  3 Jan  1  1970 suspending
-rw-r--r-- 1 root root  4 Jan  1  1970 total_heap_size
-rw-r--r-- 1 root root  5 Jan  1  1970 trap_exit
$ cat pids/\<0.41.0\>/process_info/garbage_collection
[{min_bin_vheap_size,46422},
 {min_heap_size,233},
 {fullsweep_after,65535},
 {minor_gcs,1}]
$
``` 

## Supervision behaviour

Filesystems, once mounted, are supervised, so a crash should result in a
reinstatement. Since a filesystem can be terminated in a normal way though
you should be aware that this might not be true for every circumstance.

As you might expect, if you unmount the file system by calling efuse:umount/1
the filesystem will be unmounted and not reinstated.

A system umount call made by a system administrator, from the shell or by
other means will also cause the filesyste, to be unmounted and not reinstated.
This is less clear, but some cooperation is obviously necessary between efuse
and the system, and it seems a bit unfair on the system administrator if the
execution of a perfectly normal and deliberate activity is frustrated.

More controversial, if you kill the port process from the system with a TERM
signal, the filesystem is also not reinstated. However a request to TERMinate
is still an administrative action ultimately.

If the port receives a signal which is not obviously administrative, or it
crashes, then the supervisor (efuse_fs_sup) will reinstate the filesystem.
You can kill the port with signal 9 to invoke this action.

## Writing your own efuse implementation module

This is a brief explanation, because really the example filesystems
demonstrate well enough how to do this really.

To aid the explanation, it will be based on `efuse_hellofs`.

Your modules `efuse_init` function will be called when the filesystem is
mounted, it receives the state (which is the third parameter passed to
`efuse:mount/3`, and returns a new state. None of the example filesystems
really usefully use this state, all being essentially stateless, but some
filesystems wll require it.

Next, when you list the files in a directory in your filesystem `efuse_readdir`
will be called. Obviously the first call is likely to be to the root of
your filesystem, and `efuse_hellofs` handles this as follows:

```
efuse_readdir(State, <<"/">>) ->
    {ok, [<<"hello">>, <<"world">>], State}
    ;
efuse_readdir(State, _) ->
    {error, ?EFUSE_ERROR_NOENT, State}
    .
```

Since there are no directories in this filesystem apart from the root, it's
quite simple, if it's the root it returns a list of two objects, and if
not it returns a EFUSE_ERROR_NOENT error (note this is really a soft error
for the end user, NOENT is POSIX speak for no entity). The return list, as
you can see, is a list of binaries.

As soon as the user does something like `ls -l` instead of just `ls`, the
file system will suddenly have to answer details such as what type of
objects 'hello' and 'world' are, and it does this when it responses to
the `efuse_getattr` call. Here is the `efuse_hellofs` implementation:

```
efuse_getattr(State, <<"/">>) ->
    {ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
    ;
efuse_getattr(State, <<"/hello">>) ->
    {ok, {8#0644, ?EFUSE_ATTR_FILE, byte_size(<<"Hello world!">>)}, State}
    ;
efuse_getattr(State, <<"/world">>) ->
    {ok, {8#0755, ?EFUSE_ATTR_SYMLINK, length("hello")}, State}
    ;
efuse_getattr(State, _) ->
    {error, ?EFUSE_ERROR_NOENT, State}
    .
```

Our filesystem has three objects in it in total, the root directory, a
file and a symbolic link, and the `efuse_getattr` function matches
each individually and returns a response `{ok, {Perms, Type, Size}}`.
The permissions is the octal value you can use with UNIX's chmod, the
type is directory, file or sym link (use the macros) and the size, for
files is the length of the content, and for sym links is the length
of the path.

Anything else gets the not found response.

When a file is read, the content must be provided, so for the one file
that `efuse_getattr` has the implementation is:

```
efuse_readlink(State, <<"/world">>) ->
    {ok, <<"hello">>, State}
    ;
efuse_readlink(State, _) ->
    {error, ?EFUSE_ERROR_NOENT, State}
    .
```

For the one sym link `efuse_getattr` has the implementation is:

```
efuse_read(State, <<"/hello">>) ->
    {ok, <<"Hello world!">>, State}
    ;
efuse_read(State, _) ->
    {error, ?EFUSE_ERROR_NOENT, State}
    .
```

That's all folks!

## Licensing

Copyright 2015 Michael Wright <mjw@methodanalysis.com>

This file is part of the Erlang FUSE (Filesystem in Userspace)
interface called 'efuse'.

'efuse' is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
