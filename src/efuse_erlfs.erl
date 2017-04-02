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
%% @doc Erlang FS example filesystem callback module for 'efuse' app.
%%
%% The actual implementation of the 'efuse_fs' can of course be achieved
%% in a variety of ways. Here the callbacks all break down the file path
%% leaf by leaf by the traverse/4 function, and then, assuming the
%% traversal was succesful, a function to deal with the request is
%% invoked (for example, the get attributes filesystem function is
%% resolved by a call to efuse_getattr/2, any number of calls to
%% traverse/4 and finally by a call to erlfs_getattr/2).
%%
%% The traverse/4 function verifies each element of the path as it is
%% called. This is necessary because it is no use continuing past the
%% pids/&lt;0.63.0> point if PID &lt;0.63.0> does not exist! The first two
%% parameters to traverse/4 are passed on and through so they are
%% available when the traversal finishes, the third parameter is the
%% context in which to evaluate the next path element (for example
%% traversing the "pids" directory results in the context being
%% 'all_pids', and the element after that is {pid, Pid}, enabling, for
%% example, the read directory callback, if it is invoked at that
%% point, to enumerate all objects that are inside a PID.
%%
%% See 'efuse_examplefs' for a simpler example of a filesystem that
%% just shows the callbacks and how they need to be implemented.
%%
%% NOTE: This filesystem implementation has known issues, if you easily
%% crash it by running "find . -exec ls -lad {} \;" in the filesystem
%% root, it exists to serve as an example only.


-module(efuse_erlfs).


-behviour(efuse_fs).


-export([efuse_init/1, efuse_readdir/2, efuse_getattr/2, efuse_readlink/2, efuse_read/2]).

-export([erlfs_readdir/2, erlfs_getattr/2, erlfs_readlink/2, erlfs_read/2]).


-include("efuse.hrl").
-include("efuse_defs.hrl").


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_init(State) ->
	State
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_readdir(State, <<"/",Path/binary>>) ->
	PathLeaves = binary:split(Path, <<"/">>, [global]),
	traverse(State, erlfs_readdir, root, PathLeaves)
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_getattr(State, <<"/",Path/binary>>) ->
	PathLeaves = binary:split(Path, <<"/">>, [global]),
	traverse(State, erlfs_getattr, root, PathLeaves)
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_readlink(State, <<"/",Path/binary>>) ->
	PathLeaves = binary:split(Path, <<"/">>, [global]),
	traverse(State, erlfs_readlink, root, PathLeaves)
	.


%% @doc Behaviour callback implementation for 'efuse_fs'.

efuse_read(State, <<"/",Path/binary>>) ->
	PathLeaves = binary:split(Path, <<"/">>, [global]),
	traverse(State, erlfs_read, root, PathLeaves)
	.


%% @private

traverse(State, Request, root, [<<>>]) ->
	?MODULE:Request(State, root)
	;

traverse(State, Request, root, [<<"pids">>|More]) ->
	traverse(State, Request, all_pids, More)
	;
traverse(State, Request, all_pids, []) ->
	?MODULE:Request(State, all_pids)
	;
traverse(State, Request, all_pids, [Pid|More]) ->
	traverse(State, Request, {pid, list_to_pid(binary_to_list(Pid))}, More)
	;
traverse(State, Request, {pid, Pid}, []) ->
	?MODULE:Request(State, {pid, Pid})
	;

traverse(State, Request, root, [<<"names">>|More]) ->
	traverse(State, Request, names, More)
	;

traverse(State, Request, names, []) ->
	?MODULE:Request(State, names)
	;
traverse(State, Request, names, [<<"local">>|More]) ->
	traverse(State, Request, local_names, More)
	;
traverse(State, Request, local_names, []) ->
	?MODULE:Request(State, local_names)
	;
traverse(State, Request, local_names, [Name|More]) ->
	traverse(State, Request, {local_name, list_to_atom(binary_to_list(Name))}, More)
	;
traverse(State, Request, {local_name, Name}, []) ->
	?MODULE:Request(State, {local_name, Name})
	;

traverse(State, Request, names, [<<"global">>|More]) ->
	traverse(State, Request, global_names, More)
	;
traverse(State, Request, global_names, []) ->
	?MODULE:Request(State, global_names)
	;
traverse(State, Request, global_names, [Name|More]) ->
	traverse(State, Request, {global_name, list_to_atom(binary_to_list(Name))}, More)
	;
traverse(State, Request, {global_name, Name}, []) ->
	?MODULE:Request(State, {global_name, Name})
	;

traverse(State, Request, {pid, Pid}, [<<"process_info">>|More]) ->
	traverse(State, Request, {proc_info, Pid}, More)
	;
traverse(State, Request, {proc_info, Pid}, []) ->
	?MODULE:Request(State, {proc_info, Pid})
	;
traverse(State, Request, {proc_info, Pid}, [ItemSpec]) ->
	?MODULE:Request(State, {proc_info, Pid, list_to_atom(binary_to_list(ItemSpec))})
	;

traverse(State, Request, {pid, Pid}, [<<"linked">>|More]) ->
	traverse(State, Request, {link_from, Pid}, More)
	;
traverse(State, Request, {link_from, Pid}, []) ->
	?MODULE:Request(State, {link_from, Pid})
	;
traverse(State, Request, {link_from, _Pid}, [LinkedPid]) ->
	?MODULE:Request(State, {link_to, list_to_pid(binary_to_list(LinkedPid))})
	;

traverse(State, _Request, {pid, Pid}, [<<"get_status">>]) when Pid == self() ->
	{error, ?EFUSE_ERROR_NOENT, State}
	;
traverse(State, Request, {pid, Pid}, [<<"get_status">>]) ->
	case process_info(Pid, initial_call) of
		{initial_call, {proc_lib, init_p, 5}} ->
			?MODULE:Request(State, {get_status, Pid});
		_ ->
			{error, ?EFUSE_ERROR_NOENT, State}
		end
	;

traverse(State, Request, root, [<<"nodes">>|More]) ->
	traverse(State, Request, nodes, More)
	;
traverse(State, Request, nodes, []) ->
	?MODULE:Request(State, nodes)
	;
traverse(State, Request, nodes, [Node]) ->
	?MODULE:Request(State, {node, list_to_atom(binary_to_list(Node))})
	;

traverse(State, Request, root, [<<"apps">>|More]) ->
	traverse(State, Request, apps, More)
	;
traverse(State, Request, apps, []) ->
	?MODULE:Request(State,apps)
	;
traverse(State, Request, apps, [Name|More]) ->
	traverse(State, Request, {app, list_to_atom(binary_to_list(Name))}, More)
	;
traverse(State, Request, {app, App}, []) ->
	?MODULE:Request(State, {app, App})
	;
traverse(State, Request, {app, App}, [AppSubDir|More]) ->
	traverse(State, Request, {app, App, AppSubDir}, More)
	;
traverse(State, Request, {app, App, AppSubDir}, []) ->
	?MODULE:Request(State, {app, App, AppSubDir})
	;
traverse(State, Request, {app, App, <<"env">>}, [Opt]) ->
	?MODULE:Request(State, {app_env, App, list_to_atom(binary_to_list(Opt))})
	;

traverse(State, Request, root, [<<"code">>|More]) ->
	traverse(State, Request, code, More)
	;
traverse(State, Request, code, []) ->
	?MODULE:Request(State, code)
	;
traverse(State, Request, code, [<<"modules">>|More]) ->
	traverse(State, Request, {code, modules}, More)
	;
traverse(State, Request, {code, modules}, []) ->
	?MODULE:Request(State, {code, modules})
	;
traverse(State, Request, {code, modules}, [Module|More]) ->
	traverse(State, Request, {code, module, list_to_atom(binary_to_list(Module))}, More)
	;
traverse(State, Request, {code, module, Module}, []) ->
	?MODULE:Request(State, {code, module, Module})
	;
traverse(State, Request, {code, module, Module}, [<<"file">>]) ->
	?MODULE:Request(State, {code, module, Module, file})
	;

traverse(State, _, _, _) ->
	{error, ?EFUSE_ERROR_NOENT, State}
	.


%% @private

erlfs_readdir(State, root) ->
	{ok, [<<"pids">>, <<"names">>, <<"nodes">>, <<"apps">>, <<"code">>], State}
	;
erlfs_readdir(State, all_pids) ->
	{ok, lists:map(
		fun(Pid) -> list_to_binary(io_lib:format("~w", [Pid])) end,
		processes()
		), State}
	;
erlfs_readdir(State, names) ->
	{ok, [<<"local">>, <<"global">>], State}
	;
erlfs_readdir(State, local_names) ->
	{ok, lists:map(
		fun(Name) -> list_to_binary(io_lib:format("~s", [Name])) end,
		erlang:registered()
		), State}
	;
erlfs_readdir(State, global_names) ->
	{ok, lists:map(
		fun(Name) -> list_to_binary(io_lib:format("~s", [Name])) end,
		global:registered_names()
		), State}
	;
erlfs_readdir(State, {pid, Pid}) when Pid == self() ->
	{ok, [<<"process_info">>, <<"linked">>], State}
	;
erlfs_readdir(State, {pid, Pid}) ->
	case process_info(Pid, initial_call) of
		{initial_call, {proc_lib, init_p, 5}} ->
			try
				sys:get_status(Pid, 100),
				{ok, [<<"process_info">>, <<"linked">>, <<"get_status">>], State}
			catch _:_ ->
				{ok, [<<"process_info">>, <<"linked">>], State}
			end;
		_ ->
			{ok, [<<"process_info">>, <<"linked">>], State}
		end
	;
erlfs_readdir(State, {proc_info, Pid}) ->
	ProcInfo = erlang:process_info(Pid),
	{ok, lists:map(
		fun(K) -> list_to_binary(atom_to_list(K)) end,
		[K || {K,_} <- ProcInfo]
		), State}
	;
erlfs_readdir(State, {link_from, Pid}) ->
	{links, Linked} = erlang:process_info(Pid, links),
	{ok, [list_to_binary(io_lib:format("~w", [P])) || P <- Linked, is_pid(P)], State}
	;
erlfs_readdir(State, {local_name, _Name}) ->
	{ok, [], State}
	;
erlfs_readdir(State, nodes) ->
	{ok, lists:map(
		fun(Node) -> list_to_binary(atom_to_list(Node)) end,
		[node()|nodes()]
		), State}
	;
erlfs_readdir(State, {node, _Node}) ->
	{ok, [], State}
	;
erlfs_readdir(State, apps) ->
	{ok, [list_to_binary(atom_to_list(N)) || {N,_} <- running_apps()], State}
	;
erlfs_readdir(State, {app, _App}) ->
	{ok, [<<"app_proc">>, <<"top_sup">>, <<"descr">>, <<"vsn">>, <<"env">>], State}
	;

erlfs_readdir(State, {app, App, <<"env">>}) ->
	{ok, [list_to_binary(atom_to_list(Opt)) || {Opt,_Val} <- application:get_all_env(App)], State}
	;

erlfs_readdir(State, code) ->
	{ok, [<<"modules">>], State}
	;
erlfs_readdir(State, {code, modules}) ->
	{ok, [list_to_binary(atom_to_list(M)) || {M,_} <- code:all_loaded()], State}
	;
erlfs_readdir(State, {code, module, _Module}) ->
	{ok, [<<"file">>], State}
	.


%% @private

erlfs_getattr(State, root) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, all_pids) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, names) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, local_names) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, global_names) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {local_name, _Name}) ->
	{ok, {8#0755, ?EFUSE_ATTR_SYMLINK, 0}, State}
	;
erlfs_getattr(State, {global_name, _Name}) ->
	{ok, {8#0755, ?EFUSE_ATTR_SYMLINK, 0}, State}
	;
erlfs_getattr(State, {pid, _Pid}) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {proc_info, _Pid}) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {proc_info, Pid, ItemSpec}) ->
	{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, {proc_info, Pid, ItemSpec})}, State}
	;
erlfs_getattr(State, {get_status, Pid}) ->
	case process_info(Pid, initial_call) of
		{initial_call, {proc_lib, init_p, 5}} ->
			try
				sys:get_status(Pid, 100),
				{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, {get_status, Pid})}, State}
			catch _:_ ->
				{error, ?EFUSE_ERROR_NOENT, State}
			end;
		_ ->
			{error, ?EFUSE_ERROR_NOENT, State}
		end
	;
erlfs_getattr(State, nodes) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {node, _Node}) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, apps) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {app, _Name}) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {app, App, AppSubDir}) ->
	{ok, case AppSubDir of
		<<"app_proc">> -> {8#0755, ?EFUSE_ATTR_SYMLINK, 0};
		<<"top_sup">> -> {8#0755, ?EFUSE_ATTR_SYMLINK, 0};
		<<"descr">> -> {8#0644, ?EFUSE_ATTR_FILE, file_size(State, {app, App, AppSubDir})};
		<<"vsn">> -> {8#0644, ?EFUSE_ATTR_FILE, file_size(State, {app, App, AppSubDir})};
		<<"env">> -> {8#0755, ?EFUSE_ATTR_DIR, 0}
		end, State}
	;
erlfs_getattr(State, {app_env, App, Opt}) ->
	{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, {app_env, App, Opt})}, State}
	;
erlfs_getattr(State, {link_from, _Pid}) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {link_to, _LinkedPid}) ->
	{ok, {8#0755, ?EFUSE_ATTR_SYMLINK, 0}, State}
	;

erlfs_getattr(State, code) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {code, modules}) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {code, module, _Module}) ->
	{ok, {8#0755, ?EFUSE_ATTR_DIR, 0}, State}
	;
erlfs_getattr(State, {code, module, _Module, file}) ->
	{ok, {8#0644, ?EFUSE_ATTR_FILE, file_size(State, {code, module, _Module, file})}, State}
	.


%% @private

erlfs_readlink(State, {local_name, Name}) ->
	Pid = whereis(Name),
	SymLinkDest = <<(list_to_binary(State#efuse_fs.mount_point))/binary, "/pids/", (list_to_binary(io_lib:format("~w", [Pid])))/binary>>,
	{ok, SymLinkDest, State}
	;
erlfs_readlink(State, {app, App, <<"app_proc">>}) ->
	[AppPid] = [P || {N,P} <- running_apps(), N == App],
	SymLinkDest = <<(list_to_binary(State#efuse_fs.mount_point))/binary, "/pids/", (list_to_binary(io_lib:format("~w", [AppPid])))/binary>>,
	{ok, SymLinkDest, State}
	;
erlfs_readlink(State, {app, App, <<"top_sup">>}) ->
	[AppPid] = [P || {N,P} <- running_apps(), N == App],
	{SupPid,_} = application_master:get_child(AppPid),
	SymLinkDest = <<(list_to_binary(State#efuse_fs.mount_point))/binary, "/pids/", (list_to_binary(io_lib:format("~w", [SupPid])))/binary>>,
	{ok, SymLinkDest, State}
	;
erlfs_readlink(State, {link_to, LinkedPid}) ->
	SymLinkDest = <<(list_to_binary(State#efuse_fs.mount_point))/binary, "/pids/", (list_to_binary(io_lib:format("~w", [LinkedPid])))/binary>>,
	{ok, SymLinkDest, State}
	.


%% @private

erlfs_read(State, {proc_info, Pid, ItemSpec}) ->
	{ItemSpec, ItemData} = erlang:process_info(Pid, ItemSpec),
	Content = list_to_binary(lists:flatten(io_lib:format("~p", [ItemData]))++"\n"),
	{ok, Content, State}
	;
erlfs_read(State, {app, App, <<"descr">>}) ->
	[Descr] = [D || {N,D,_} <- loaded_apps(), N == App],
	Content = list_to_binary(Descr++"\n"),
	{ok, Content, State}
	;
erlfs_read(State, {app, App, <<"vsn">>}) ->
	[Vsn] = [V || {N,_,V} <- loaded_apps(), N == App],
	Content = list_to_binary(Vsn++"\n"),
	{ok, Content, State}
	;
erlfs_read(State, {get_status, Pid}) ->
	try
		Content = list_to_binary(lists:flatten(io_lib:format("~p", [sys:get_status(Pid)]))++"\n"),
		{ok, Content, State}
	catch _:_ ->
		{error, ?EFUSE_ERROR_NOENT, State}
	end
	;
erlfs_read(State, {app_env, App, Opt}) ->
	{ok, Val} = application:get_env(App, Opt),
	Content = list_to_binary(lists:flatten(io_lib:format("~p", [Val]))++"\n"),
	{ok, Content, State}
	;
erlfs_read(State, {code, module, Module, file}) ->
	case code:which(Module) of
		Path when is_list(Path) ->
			{ok, list_to_binary(Path++"\n"), State};
		Atom when is_atom(Atom) ->
			{ok, list_to_binary(atom_to_list(Atom)++"\n"), State}
		end
	.


file_size(State, Context) ->
	{ok, Content, State} = erlfs_read(State, Context),
	size(Content)
	.


loaded_apps() ->
	{loaded, Loaded} = lists:keyfind(loaded, 1, application_controller:info()),
	Loaded
	.


running_apps() ->
	{running, Running} = lists:keyfind(running, 1, application_controller:info()),
	Running
	.


