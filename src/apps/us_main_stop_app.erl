% Copyright (C) 2020-2025 Olivier Boudeville
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

-module(us_main_stop_app).

-moduledoc """
Actual US-Main **stopping logic**, as a (Myriad) application.

Meant to stop gracefully a host-local US-Main instance.

Typically called through the us_main/priv/bin/stop-us-main-native-build.sh
script.
""".


-export([ exec/0 ]).


% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").


% For trace_aggregator_name:
-include_lib("traces/include/class_TraceAggregator.hrl").



-doc "Runs this stop application.".
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

	% Hardcoded atom node name, yet adapting to the current node naming
	% convention:
	%
	TargetNodeName = net_utils:get_complete_node_name( "us_main" ),

	app_facilities:display( "Connecting to node '~ts'.", [ TargetNodeName ] ),

	net_adm:ping( TargetNodeName ) =:= pong orelse
		begin
			trace_utils:error_fmt( "Unable to connect to '~ts'.~nIf the target "
				"node is really running and is named exactly like that, check "
				"that the cookies and EPMD ports match and, finally, that "
				"no firewall is in the way (e.g. a server may filter the EPMD "
				"port of interest).", [ TargetNodeName ] ),

			throw( { unable_to_connect_to, TargetNodeName } )

		end,

	% Otherwise the remote node could not be known before use:
	global:sync(),

	% Rather than finding the US-Main server (whose registered name is set in
	% its configuration file) and stopping it, at least currently we simply stop
	% its Erlang node as a whole:

	%trace_utils:warning_fmt( "Just about to stop node '~ts'.",
	%                         [ TargetNodeName ] ),

	% All applications are taken down smoothly, all code is unloaded, and all
	% ports are closed before the system terminates by calling halt(Status):
	%
	case rpc:call( TargetNodeName, _Mod=init, _Func=stop, _Args=[] ) of

		{ badrpc, Reason } ->
			trace_utils:warning_fmt( "The stopping of node '~ts' failed with "
				"following RPC reason: ~p", [ TargetNodeName, Reason ] );

		ok ->
			trace_utils:info_fmt( "Node '~ts' stopped successfully",
								  [ TargetNodeName ] )

	end,

	% Instead a (safer) asynchronous operation would be of no interest:
	%rpc:cast( TargetNodeName, _Mod=init, _Func=stop, _Args=[] ),

	% ?app_stop should not be used here as its wait_for_any_trace_supervisor
	% macro would wait for a non-launched supervisor.
	%
	% ?app_stop_without_waiting_for_trace_supervisor() is not used either, as
	% no aggregator was started from that test.

	us_client:teardown().



-doc "Initialises this application from the command line.".
init_from_command_line() ->

	% To force options for testing:
	%ArgTable = cmd_line_utils:generate_argument_table( "--help" ),

	ArgTable = cmd_line_utils:get_argument_table(),

	%trace_utils:debug_fmt( "Argument table: ~ts",
	%                       [ list_table:to_string( ArgTable ) ] ),

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

	list_table:is_empty( CookieShrunkTable ) orelse
		throw( { unexpected_arguments,
				 list_table:enumerate( CookieShrunkTable ) } ).
