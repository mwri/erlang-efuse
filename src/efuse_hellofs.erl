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
	{ok, {8#0755, ?EFUSE_ATTR_SYMLINK, 0}, State}
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


