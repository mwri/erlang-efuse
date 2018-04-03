%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Hello world FS example filesystem callback module for 'efuse' app.
%%
%% The 'efuse_examplefs' is a very minimal example showing the
%% callbacks and how they need to be implemented.
%%
%% For a fuller more interesting example of an implementation, see the
%% 'efuse_erlfs' module, or 'efuse_examplefs' is like this, but with a
%% few more objects.


-module(efuse_hellofs).


-behviour(efuse_fs).


-export([efuse_init/1, efuse_readdir/2, efuse_getattr/2, efuse_readlink/2, efuse_read/2]).


-include("efuse.hrl").
-include("efuse_defs.hrl").


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_init(_State) ->
	ready
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_readdir(State, <<"/">>) ->
	{ok, [<<"hello">>, <<"world">>], State}
	;
efuse_readdir(State, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

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


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_readlink(State, <<"/world">>) ->
	{ok, <<"hello">>, State}
	;
efuse_readlink(State, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_read(State, <<"/hello">>) ->
	{ok, <<"Hello world!">>, State}
	;
efuse_read(State, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


