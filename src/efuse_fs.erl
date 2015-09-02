%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
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




