% Copyright (C) 2020-2023 Olivier Boudeville
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
% Creation date: Sunday, January 19, 2020.


% @doc Actual US-Main <b>trace monitoring logic</b>, as a (Myriad) application.
%
% Typically called through the us_main/priv/bin/monitor-us-main.sh script.
%
% Designed to monitor a US-Main instance typically from any remote host able to
% connect to the VM hosting that instance.
%
-module(us_main_monitor_app).


-export([ exec/0 ]).


% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").


% For trace_aggregator_name:
-include_lib("traces/include/class_TraceAggregator.hrl").



% @doc Runs this monitoring app.
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

	CfgFilePath = init_from_command_line(),

	Cfg = file_utils:read_terms( CfgFilePath ),

	%trace_utils:debug_fmt( "Read configuration from '~ts': ~p",
	%                       [ CfgFilePath, Cfg ] ),

	[ MainTargetNodeName, UserTargetNodeName ] = get_target_node_names( Cfg ),

	app_facilities:display( "Trying to connect to US-Main node '~ts'.",
							[ MainTargetNodeName ] ),

	net_adm:ping( MainTargetNodeName ) =:= pong orelse
		begin
			trace_utils:warning_fmt( "Unable to connect to a target main "
				"node '~ts'; trying an alternate one, based on "
				"user name: '~ts'.",
				[ MainTargetNodeName, UserTargetNodeName ] ),

			net_adm:ping( UserTargetNodeName ) =:= pong orelse
				begin
					trace_utils:error_fmt( "Unable to connect to either node "
						"names, the main one ('~ts') or the user one ('~ts')."
						"~nIf the target node is really running and is named "
						"like either of the two, check that the cookies match "
						"and, finally, that no firewall is in the way "
						"(ex: a server may filter the EPMD port of interest).",
						[ MainTargetNodeName, UserTargetNodeName ] ),

					throw( { unable_to_connect_to,
							 { MainTargetNodeName, UserTargetNodeName } } )

				end

		end,

	% Otherwise the remote node could not be known before use:
	global:sync(),

	%app_facilities:display( "Globally registered names: ~w.",
	%                        [ global:registered_names() ] ),

	AggregatorName = ?trace_aggregator_name,

	%app_facilities:display( "Looking up aggregator by name: ~ts.",
	%                        [ AggregatorName ] ),

	% The trace aggregator is expected to run in the target node, but to be
	% registered there only locally, to avoid clashing with any other
	% aggregator:
	%
	AggregatorPid = naming_utils:get_locally_registered_pid_for(
		AggregatorName, MainTargetNodeName ),

	app_facilities:display( "Creating now a local trace listener." ),

	TraceListenerPid = case get_tcp_port_range( Cfg ) of

		undefined ->
			class_TraceListener:synchronous_new_link( AggregatorPid,
				_CloseListenerPid=self() );

		{ MinTCPPort, MaxTCPPort } ->
			class_TraceListener:synchronous_new_link( AggregatorPid,
				MinTCPPort, MaxTCPPort, _CloseListenerPid=self() )

	end,

	app_facilities:display( "Waiting for the trace listener to be closed." ),

	receive

		{ trace_listening_finished, TraceListenerPid } ->
			app_facilities:display( "Trace listener closed." )

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
	%                       [ list_table:to_string( ArgTable ) ] ),

	% Argument expected to be set by the caller script:
	{ CfgFilePath, ConfigShrunkTable } =
			case list_table:extract_entry_if_existing( '-config-file',
													   ArgTable ) of

		false ->
			throw( no_configuration_file_set );

		{ [ [ CfgPath ] ], CfgShrunkTable } ->
			case file_utils:is_existing_file_or_link( CfgPath ) of

				true ->
					{ CfgPath, CfgShrunkTable };

				false ->
					throw( { configuration_file_not_found, CfgPath } )

			end;

		{ OtherCfgArg, _CfgTable } ->
			throw( { unexpected_configuration_argument, OtherCfgArg } )


	end,

	%trace_utils:debug_fmt( "Configuration file: '~ts'.", [ CfgFilePath ] ),

	% Argument also expected to be set by the caller script:
	{ RemoteCookie, CookieShrunkTable } =
			case list_table:extract_entry_if_existing( '-target-cookie',
													   ConfigShrunkTable ) of

		false ->
			throw( no_target_cookie_set );

		{ [ [ Cookie ] ], CookShrunkTable } ->
			{ text_utils:string_to_atom( Cookie ), CookShrunkTable };

		{ OtherCookieArg, _CookTable } ->
			throw( { unexpected_cookie_argument, OtherCookieArg } )

	end,

	trace_utils:debug_fmt( "Setting remote cookie: '~ts'.", [ RemoteCookie ] ),

	net_utils:set_cookie( RemoteCookie ),

	list_table:is_empty( CookieShrunkTable ) orelse
		throw( { unexpected_arguments,
				 list_table:enumerate( CookieShrunkTable ) } ),

	CfgFilePath.



% @doc Returns the possible node names (main or user-based one) corresponding to
% the target server US-Main instance.
%
% Two names are considered, as two approaches can be used to launch US-Main
% nodes.
%
get_target_node_names( Cfg ) ->

	RemoteHostname = list_table:get_value( us_main_hostname, Cfg ),

	%trace_utils:debug_fmt( "Remote host: '~ts'.", [ RemoteHostname ] ),

	net_utils:localnode() =/= local_node orelse
		throw( { node_not_networked, node() } ),

	% Supposing here uniform client/server conventions in terms of short or long
	% names:
	%
	NodeNamingMode = net_utils:get_node_naming_mode(),

	% Note that two hardcoded node names are used here, the main one (when run
	% as a service) and one embedding the name of the current user (when run as
	% an app, typically for testing):

	BaseNodeNames = [ "us_main", text_utils:format( "us_main_exec-~ts",
										[ system_utils:get_user_name() ] ) ],

	% Returns relevant, ordered candidates
	[ net_utils:get_complete_node_name( N, RemoteHostname, NodeNamingMode )
		|| N <- BaseNodeNames ].



% @doc Returns the TCP port range to use (if any).
get_tcp_port_range( Cfg ) ->

	MaybePortRange = list_table:get_value_with_default( _K=tcp_port_range,
												_Default=undefined, Cfg ),

	%trace_utils:debug_fmt( "TCP port range: ~p.", [ MaybePortRange ] ),

	MaybePortRange.
