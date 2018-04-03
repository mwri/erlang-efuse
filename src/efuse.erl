%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Main API modile for 'efuse' app.


-module(efuse).


-behaviour(gen_server).


-export([start/3, start_link/3]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([mount/3, umount/1]).


-include("efuse.hrl").
-include("efuse_defs.hrl").

-type type() :: ?EFUSE_ATTR_DIR | ?EFUSE_ATTR_FILE | ?EFUSE_ATTR_SYMLINK .
-type mode() :: 8#755 | 8#644 .


-export_type([type/0, mode/0]).


-record(efuse, {
	mount_point             :: string(),
	cb_mod                  :: module(),
	fs_state                :: term(),
	port                    :: port(),
	port_os_pid = undefined :: undefined | integer()
	}).


%% @doc Mount a filesystem.
%%
%% Mount takes 3 parameters, the mount point, the module which implements the
%% filesystem and a term which is provided to the filesystem implemention
%% (which can be undefined, or anything else, depending on the implementation).
%%
%% The mount point must be a suitable directory which is part of an already
%% mounted filesystem.
%%
%% The implementation module must implement the 'efuse_fs' behaviour.
%%
%% For example, you could mount the example 'Erlang FS' filesystem implemented
%% by the 'efuse_erlfs' module like this:
%%
%% <pre>
%%     1> application:start(efuse).
%%     ok
%%     2> efuse:mount("/tmp/erlfs", efuse_erlfs, undefined).
%%     {ok,&lt;0.40.0>}
%% </pre>
%%
%% Now, look inside the /tmp/erlfs directory and you should find it populated.
%% When you do this the filesystem requests are passed to a port which was
%% started when you performed the mount, and those requests are passed on to
%% the 'efuse_erlfs' module, which responds, and those responses are passed
%% back to the port and then on to FUSE and the kernel.
%%
%% Filesystems, once mounted, are supervised, so a crash should result in a
%% reinstatement. Since a filesystem can be terminated in a normal way though
%% you should be aware that this might not be true for every circumstance.
%%
%% As you might expect, if you unmount the file system by calling efuse:umount/1
%% the filesystem will be unmounted and not reinstated.
%%
%% A system umount call made by a system administrator, from the shell or by
%% other means will also cause the filesyste, to be unmounted and not reinstated.
%% This is less clear, but some cooperation is obviously necessary between efuse
%% and the system, and it seems a bit unfair on the system administrator if the
%% execution of a perfectly normal and deliberate activity is frustrated.
%%
%% More controversial, if you kill the port process from the system with a TERM
%% signal, the filesystem is also not reinstated. However a request to TERMinate
%% is still an administrative action ultimately.
%%
%% If the port receives a signal which is not obviously administrative, or it
%% crashes, then the supervisor (efuse_fs_sup) will reinstate the filesystem.
%% You can kill the port with signal 9 to invoke this action.

-spec mount
	(MountPoint :: string(), CbMod :: module(), FsState :: term()) ->
		{ok, pid()} .

mount(MountPoint, CbMod, FsState) ->
	supervisor:start_child(efuse_mount_sup, [MountPoint, CbMod, FsState])
	.

%% @doc Unmount a filesystem.
%%
%% You just need to provide the same mount point used to mount the filesystem
%% when efuse:mount/3 was called. For example:
%%
%% <pre>
%%     3> efuse:umount("/tmp/erlfs").
%%     {ok,{stopping, &lt;0.40.0>}}
%% </pre>

-spec umount
	(MountPoint :: string()) ->
		{ok, {stopping, pid()}} | {error, not_mounted} .

umount(MountPoint) ->
	case lists:filter(
			fun({_Pid, {MountPoint2, _CbMod, _FsState}}) ->
				MountPoint2 == MountPoint
				end,
			lists:map(
				fun({_Id, Pid, _Type, _Modules}) ->
					{Pid, gen_server:call(Pid, status)}
					end,
				supervisor:which_children(efuse_mount_sup)
				)
			) of
		[] ->
			{error, not_mounted};
		[{Pid, _Status}] ->
			gen_server:cast(Pid, stop),
			{ok, {stopping, Pid}}
		end
	.


%% @private

-spec start
	(MountPoint :: string(), CbMod :: module(), FsState :: term()) ->
		{ok, pid()} .

start(MountPoint, CbMod, FsState) ->
	gen_server:start(?MODULE, [MountPoint, CbMod, FsState], [])
	.

%% @private

-spec start_link
	(MountPoint :: string(), CbMod :: module(), FsState :: term()) ->
		{ok, pid()} .

start_link(MountPoint, CbMod, FsState) ->
	gen_server:start_link(?MODULE, [MountPoint, CbMod, FsState], [])
	.


%% @private

init([MountPoint, CbMod, CbData]) ->
	PortPath = case code:lib_dir(?MODULE) of
		{error, bad_name} -> "./priv/efuse";
		LibPath -> LibPath ++ "/priv/efuse"
		end,
	true = filelib:is_file(PortPath),
    Port = open_port(
		{spawn, PortPath++" -f "++MountPoint},
		[{packet, 4}, nouse_stdio, exit_status, binary]
		),
	FsState = #efuse_fs{
		mount_point = MountPoint,
		fs_data     = CbData
		},
	State = #efuse{
		mount_point = MountPoint,
		cb_mod      = CbMod,
		fs_state    = CbMod:efuse_init(FsState),
		port        = Port
		},
	{ok, State}
.


%% @private

terminate(_Reason, #?MODULE{port_os_pid=undefined}) ->
	undefined
	;
terminate(_Reason, #?MODULE{
		mount_point = MountPoint,
		port_os_pid = PortOsPid
		}) ->
	os:cmd(lists:flatten(io_lib:format("umount ~s", [MountPoint]))),
	os:cmd(lists:flatten(io_lib:format("kill ~b", [PortOsPid]))),
	undefined
	.


%% @private

code_change(_OldVsn, State, _Extra) ->
	{ok, State}
	.


%% @private

port_tx(Data, State = #?MODULE{port=Port}) ->
	true = port_command(Port, <<?EFUSE_MAGICCOOKIE:32/integer, Data/binary>>),
	State
	.


%% @private

-spec handle_fusereq
	(State :: #?MODULE{}, ReqCode :: integer(), CbFun :: atom(),
			CbArgs :: list(), ReplyFun :: fun()) ->
		NewState :: #?MODULE{} .

handle_fusereq(
		State = #?MODULE{cb_mod=CbMod,fs_state=FsState},
		ReqCode, CbFun, CbArgs, ReplyFun) ->
	{PortReply, NewFsState} = try case apply(
				CbMod, CbFun, [FsState|CbArgs]
				) of
			{ok, Reply, PostCbFsState} ->
				{<<ReqCode:32/integer, 0:32/integer, (ReplyFun(Reply))/binary>>, PostCbFsState};
			{error, ErrCode, PostCbFsState} ->
				{<<ReqCode:32/integer, ErrCode:32/integer>>, PostCbFsState}
			end
		catch A:B ->
			error_logger:error_msg(
				"Caught exception ~p:~p from ~s filesystem implementation",
				[A, B, CbMod]
				),
			{<<ReqCode:32/integer, ?EFUSE_ERROR_NOENT:32/integer>>, FsState}
		end,
	port_tx(PortReply, State#?MODULE{fs_state=NewFsState})
	.


%% @private

handle_info(
		{Port, {data, <<
			?EFUSE_MAGICCOOKIE:32/integer,
			?EFUSE_REQUEST_READDIR:32/integer, Path/binary
			>>}},
		State
		) when (Port == State#efuse.port) ->
	NewState = handle_fusereq(
		State, ?EFUSE_REQUEST_READDIR, efuse_readdir, [Path],
		fun(Reply) ->
			lists:foldl(
				fun(E, A) -> <<A/binary, E/binary, 0:8/integer>> end,
				<<>>,
				Reply
				)
			end
		),
	{noreply, NewState}
	;

handle_info(
		{Port, {data, <<
			?EFUSE_MAGICCOOKIE:32/integer,
			?EFUSE_REQUEST_GETATTR:32/integer,
			Path/binary>>
			}},
		State
		) when (Port == State#efuse.port) ->
	NewState = handle_fusereq(
		State, ?EFUSE_REQUEST_GETATTR, efuse_getattr, [Path],
		fun({Mode, Type, Size}) ->
			<<Mode:32/integer, Type:32/integer, Size:32/integer>>
			end
		),
	{noreply, NewState}
	;

handle_info(
		{Port, {data, <<
			?EFUSE_MAGICCOOKIE:32/integer,
			?EFUSE_REQUEST_READLINK:32/integer,
			Path/binary>>
			}},
		State
		) when (Port == State#efuse.port) ->
	NewState = handle_fusereq(
		State, ?EFUSE_REQUEST_READLINK, efuse_readlink, [Path],
		fun(LinkDest) ->
			<<LinkDest/binary, 0:8/integer>>
			end
		),
	{noreply, NewState}
	;

handle_info(
		{Port, {data, <<
			?EFUSE_MAGICCOOKIE:32/integer,
			?EFUSE_REQUEST_READ:32/integer,
			Path/binary>>
			}},
		State
		) when (Port == State#efuse.port) ->
	NewState = handle_fusereq(
		State, ?EFUSE_REQUEST_READ, efuse_read, [Path],
		fun(Content) ->
			<<Content/binary>>
			end
		),
	{noreply, NewState}
	;

handle_info(
		{Port, {data, <<
			?EFUSE_MAGICCOOKIE:32/integer,
			?EFUSE_STATUS_DATA:32/integer,
			PortOsPid:32/integer>>
			}},
		State = #?MODULE{}
		) when (Port == State#efuse.port) ->
	{noreply, State#?MODULE{port_os_pid=PortOsPid}}
	;

handle_info(
	{Port, {exit_status, 0}},
	State = #?MODULE{}
	) when (Port == State#efuse.port) ->
	{stop, normal, State}
	;

handle_info(
		{Port, {data, <<?EFUSE_MAGICCOOKIE:32/integer, Data/binary>>}},
		State = #?MODULE{}
		) when (Port == State#efuse.port) ->
	error_logger:error_msg("Port data ~p to efuse ~p unrecognised.~n", [Data, self()]),
	{noreply, State}
	;

handle_info(
		{Port, {data, Data}},
		State = #?MODULE{}
		) when (Port == State#efuse.port) ->
	error_logger:error_msg("Port data ~p without correct cookie to efuse ~p.~n", [Data, self()]),
	{stop, {error, "communication with port fatally compromised (bad cookie)"}, State}
	.


%% @private

handle_cast(stop, State) ->
	{stop, normal, State}
	.


%% @private

handle_call(status, _From, State) ->
	{reply, {
		State#?MODULE.mount_point,
		State#?MODULE.cb_mod,
		State#?MODULE.fs_state
		}, State}
	.


