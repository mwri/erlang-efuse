%% This file is part of the Erlang FUSE (Filesystem in Userspace)
%% interface called 'efuse'.
%% 
%% 'efuse' is free software, licensed under the MIT license.
%% 
%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc App callback module for 'efuse' app.


-module(efuse_app).


-behaviour(application).


-export([start/2, stop/1]).


%% @private

start(_StartType, _StartArgs) ->
    efuse_sup:start_link()
    .


%% @private

stop(_State) ->
    ok
    .


