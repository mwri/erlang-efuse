%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Example FS example filesystem callback module for 'efuse' app.
%%
%% The 'efuse_examplefs' is a fairly minimal example showing the
%% callbacks and how they need to be implemented.
%%
%% For a fuller more interesting example of an implementation, see the
%% 'efuse_erlfs' module.


-module(efuse_examplefs).


-behviour(efuse_fs).


-export([efuse_init/1, efuse_readdir/2, efuse_getattr/2, efuse_readlink/2, efuse_read/2]).


-include("efuse.hrl").
-include("efuse_defs.hrl").


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_init(_State) ->
	running_state
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_readdir(State, <<"/">>) ->
	{ok, [<<"dir1">>, <<"dir2">>, <<"file1">>, <<"file2">>, <<"link1">>, <<"link2">>], State}
	;
efuse_readdir(State, <<"/dir1">>) ->
	{ok, [<<"file1">>], State}
	;
efuse_readdir(State, <<"/dir2">>) ->
	{ok, [<<"file3">>, <<"link2">>], State}
	;
efuse_readdir(State, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_getattr(State, <<"/">>) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
efuse_getattr(State, <<"/dir1">>) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
efuse_getattr(State, <<"/dir2">>) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
efuse_getattr(State, <<"/file1">>) ->
	{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, <<"/file1">>)}, State}
	;
efuse_getattr(State, <<"/file2">>) ->
	{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, <<"/file2">>)}, State}
	;
efuse_getattr(State, <<"/link1">>) ->
	{ok, {8#0755, ?EFUSE_ATTR_SYMLINK, 0}, State}
	;
efuse_getattr(State, <<"/link2">>) ->
	{ok, {8#0755, ?EFUSE_ATTR_SYMLINK, 0}, State}
	;
efuse_getattr(State, <<"/dir1/file1">>) ->
	{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, <<"/dir1/file1">>)}, State}
	;
efuse_getattr(State, <<"/dir2/file3">>) ->
	{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, <<"/dir2/file3">>)}, State}
	;
efuse_getattr(State, <<"/dir2/link2">>) ->
	{ok, {8#0755, ?EFUSE_ATTR_SYMLINK, 0}, State}
	;
efuse_getattr(State, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_readlink(State, <<"/link1">>) ->
	{ok, <<"file1">>, State}
	;
efuse_readlink(State, <<"/link2">>) ->
	{ok, <<"dir1/file1">>, State}
	;
efuse_readlink(State, <<"/dir2/link2">>) ->
	{ok, <<"../file2">>, State}
	;
efuse_readlink(State, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_read(State, <<"/file1">>) ->
	{ok, <<"This is file one in the root directory.">>, State}
	;
efuse_read(State, <<"/file2">>) ->
	{ok, <<"This is file two in the root directory.">>, State}
	;
efuse_read(State, <<"/dir1/file1">>) ->
	{ok, <<"This is file one in directory one.">>, State}
	;
efuse_read(State, <<"/dir2/file3">>) ->
	{ok, <<"This is file three in directory two.">>, State}
	;
efuse_read(State, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


% Return the file size of a given file.

file_size(State, FilePath) ->
	{ok, Content, State} = efuse_read(State, FilePath),
	byte_size(Content)
	.


