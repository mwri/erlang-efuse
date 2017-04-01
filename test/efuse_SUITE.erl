-module(efuse_SUITE).


-compile(export_all).


-include_lib("common_test/include/ct.hrl").


all() -> [
	{group, efuse_erlfs},
	{group, efuse_examplefs},
	{group, efuse_hellofs}
	].

groups() -> [
	{efuse_erlfs,     [], [mount, read_files, umount]},
	{efuse_examplefs, [], [mount, read_files, umount]},
	{efuse_hellofs,   [], [mount, read_files, umount]}
	].


suite() ->
	[{timetrap, {seconds,30}}, {logdir, "logs"}]
	.


init_per_suite(Config) ->
	ok = application:start(efuse),
	Config
	.

end_per_suite(_Config) ->
	ok = application:stop(efuse)
	.

init_per_group(Group, Config) ->
	{priv_dir, PrivateDir} = lists:keyfind(priv_dir, 1, Config),
	MountDir = "/tmp/erlang_ct_mount_"++atom_to_list(Group),
	case file:make_dir(MountDir) of
		ok -> ok;
		{error, eexist} ->
			ok = file:del_dir(MountDir),
			ok = file:make_dir(MountDir)
		end,
	FileReads = fs_files(Group),
	[{mount_dir, MountDir} | [{erlfs_cbmod, Group} | [{file_reads, FileReads} | Config]]]
	.

end_per_group(_Group, Config) ->
	{mount_dir, MountDir} = lists:keyfind(mount_dir, 1, Config),
	ok = file:del_dir(MountDir),
	ok
	.


mount(Config) ->
	{mount_dir, MountDir} = lists:keyfind(mount_dir, 1, Config),
	{erlfs_cbmod, CbMod} = lists:keyfind(erlfs_cbmod, 1, Config),
	{ok, _Pid} = efuse:mount(MountDir, CbMod, undefined),
	ok
	.

umount(Config) ->
	{mount_dir, MountDir} = lists:keyfind(mount_dir, 1, Config),
	{ok, {stopping, _Pid}} = efuse:umount(MountDir),
	ok
	.

read_files(Config) ->
	{mount_dir, MountDir} = lists:keyfind(mount_dir, 1, Config),
	{file_reads, FileReads} = lists:keyfind(file_reads, 1, Config),
	lists:foreach(
		fun ({Filename, ExpectContent}) ->
			{ok, ActualBinContent} = file:read_file(MountDir++"/"++Filename),
			ExpectContent = binary_to_list(ActualBinContent)
			end,
		FileReads
		),
	ok
	.


fs_files(efuse_erlfs) -> [
	{"apps/efuse/descr", "Erlang FUSE (Filesystem in Userspace)\n"}
	];
fs_files(efuse_examplefs) -> [
	{"file1", "This is file one in the root directory."}
	];
fs_files(efuse_hellofs) -> [
	{"hello", "Hello world!"},
	{"world", "Hello world!"}
	].
