% Copyright (C) 2021-2021 Olivier Boudeville
%
% This file belongs to the US-Main project, a part of the Universal Server
% framework.
%
% This program is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.
%
% You should have received a copy of the GNU Affero General Public License along
% with this program. If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Sunday, November 15, 2020.


% @doc Actual US-Main <b>stopping logic</b>, as a (Myriad) application.
%
% Typically called through the us_main/priv/bin/stop-us-main-native-build.sh
% script.
%
-module(us_main_stop_app).

% For exec/0 export:
-include_lib("myriad/include/app_facilities.hrl").

% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").



% For trace_aggregator_name:
-include_lib("traces/include/class_TraceAggregator.hrl").



% @doc Runs this stop application.
-spec exec() -> no_return().
exec() ->

	% No app_start here, hence we need the following (see
	% traces_for_apps:app_start/2 for a detailed explanation):
	%
	erlang:process_flag( trap_exit, false ),

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad_from_module(),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_app(),

	init_from_command_line(),

	% Hardcoded node name:
	TargetNodeName = text_utils:atom_format( "us_main@~ts",
											 [ net_utils:localhost() ] ),

	app_facilities:display( "Connecting to node '~ts'.", [ TargetNodeName ] ),

	case net_adm:ping( TargetNodeName ) of

		pong ->
			ok;

		pang ->
			trace_utils:error_fmt( "Unable to connect to '~ts'.~nIf the target "
				"node is really running and is named exactly like that, check "
				"that the cookies and EPMD ports match and, finally, that "
				"no firewall is in the way (ex: a server may filter the EPMD "
				"port of interest).", [ TargetNodeName ] ),

			throw( { unable_to_connect_to, TargetNodeName } )

	end,

	% Otherwise the remote node could not be known before use:
	global:sync(),

	% Rather than finding the US-Main server (whose registered is set in its
	% configuration file) and stopping it, at least currently we simply stop its
	% Erlang node as a whole:

	% All applications are taken down smoothly, all code is unloaded, and all
	% ports are closed before the system terminates by calling halt(Status):
	%
	case rpc:call( TargetNodeName, init, stop, [] ) of

		{ badrpc, Reason } ->
			trace_utils:warning_fmt( "The stopping of node '~ts' failed with "
				"following RPC reason: ~p", [ TargetNodeName, Reason ] );

		ok ->
			trace_utils:info_fmt( "Node '~ts' stopped successfully",
								  [ TargetNodeName ] )

	end,

	% ?app_stop should not be used here as its wait_for_any_trace_supervisor
	% macro would wait for a non-launched supervisor.
	%
	% ?app_stop_without_waiting_for_trace_supervisor() is not used either, as
	% no aggregator was started from that test.
	%
	app_facilities:finished().



% @doc Initialises this application from the command line.
init_from_command_line() ->

	% To force options for testing:
	%ArgTable = shell_utils:generate_argument_table( "--help" ),

	ArgTable = shell_utils:get_argument_table(),

	%trace_utils:debug_fmt( "Argument table: ~ts",
	%					   [ list_table:to_string( ArgTable ) ] ),

	% Argument expected to be set by the caller script:
	{ RemoteCookie, CookieShrunkTable } =
		case list_table:extract_entry_if_existing( '-target-cookie',
												   ArgTable ) of

		false ->
			throw( no_target_cookie_set );

		{ [ [ Cookie ] ], CookShrunkTable } ->
			{ text_utils:string_to_atom( Cookie ), CookShrunkTable };

		{ OtherCookieArg, _CookTable } ->
			throw( { unexpected_cookie_argument, OtherCookieArg } )

	end,

	%trace_utils:debug_fmt( "Setting remote cookie: '~ts'.", [ RemoteCookie ] ),

	net_utils:set_cookie( RemoteCookie ),

	case list_table:is_empty( CookieShrunkTable ) of

		true ->
			ok;

		false ->
			throw( { unexpected_arguments,
					 list_table:enumerate( CookieShrunkTable ) } )

	end.
