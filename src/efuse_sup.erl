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


