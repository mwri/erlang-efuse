%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Mount point supervisor module for 'efuse' app.


-module(efuse_mount_sup).


-behaviour(supervisor).


-export([start_link/0]).

-export([init/1]).


%% @private

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [])
	.


%% @private

init([]) ->
	RestartSpec = {simple_one_for_one, 10, 300},
	ChildrenSpec = [
		{efuse,
			{efuse, start_link, []},
				transient, 30, worker, [efuse]}
		],
	{ok, {RestartSpec, ChildrenSpec}}
	.


