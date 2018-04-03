%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Filesystem behaviour module for 'efuse' app.


-module(efuse_fs).


-include("efuse.hrl").


-callback efuse_readdir
	(FsState :: #efuse_fs{}, Path :: binary()) ->
		{ok, [binary()], NewFsState :: #efuse_fs{}}
		| {error, ErrCode :: integer()} .

-callback efuse_getattr
	(FsState :: #efuse_fs{}, Path :: binary()) ->
		{ok, {
			Mode :: efuse:mode(),
			Type :: efuse:type(),
			Size :: integer()
			}, NewFsState :: #efuse_fs{}}
		| {error, ErrCode :: integer()} .

-callback efuse_readlink
	(FsState :: #efuse_fs{}, Path :: binary()) ->
		{ok, LinkDest :: binary(), NewFsState :: #efuse_fs{}}
		| {error, ErrCode :: integer()} .

-callback efuse_read
	(FsState :: #efuse_fs{}, Path :: binary()) ->
		{ok, Content :: binary(), NewFsState :: #efuse_fs{}}
		| {error, ErrCode :: integer()} .




