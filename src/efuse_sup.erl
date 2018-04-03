%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Top supervisor module for 'efuse' app.


-module(efuse_sup).


-behaviour(supervisor).


-export([start_link/0]).

-export([init/1]).


%% @private

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [])
	.


%% @private

init([]) ->
	RestartSpec = {one_for_all, 3, 300},
	ChildrenSpec = [
		{efuse_mount_sup,
			{efuse_mount_sup, start_link, []},
				permanent, 30, worker, [efuse_mount_sup]}
		],
	{ok, {RestartSpec, ChildrenSpec}}
	.


