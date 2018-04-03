%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @private
%% @doc Rebar plugin module for 'efuse' app build.


-module(efuse_defs).


-export([pre_compile/2, clean/2]).


defs() -> [
	{"EFUSE_STATUS_DATA",      "100"       },
	{"EFUSE_REQUEST_READDIR",  "3"         },
	{"EFUSE_REQUEST_GETATTR",  "4"         },
	{"EFUSE_REQUEST_READLINK", "5"         },
	{"EFUSE_REQUEST_READ",     "6"         },
	{"EFUSE_ATTR_DIR",         "1"         },
	{"EFUSE_ATTR_FILE",        "2"         },
	{"EFUSE_ATTR_SYMLINK",     "3"         },
	{"EFUSE_ERROR_NOENT",      "2"         },
	{"EFUSE_MAGICCOOKIE",      "3223410092"}
	].


pre_compile(_,_) ->
	{ok, IoDevA} = file:open("include/efuse_defs.hrl", [write]),
	{ok, IoDevB} = file:open("c_src/efuse_defs.h", [write]),
	lists:foreach(
		fun({O,V}) ->
			ok = file:write(IoDevA, io_lib:format("-define(~s, ~s).~n", [O, V])),
			ok = file:write(IoDevB, io_lib:format("#define ~s ~s~n", [O, V]))
			end,
		defs()
		),
	file:close(IoDevA),
	file:close(IoDevB),
	io:format("Created include/efuse_defs.hrl~n"),
	io:format("Created c_src/efuse_defs.h~n")
	.


clean(_,_) ->
	file:delete("include/efuse_defs.hrl"),
	file:delete("c_src/efuse_defs.h"),
	ok
	.


