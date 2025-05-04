% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Tuesday, December 31, 2024.

-module(us_main_client).

-moduledoc """
Gathering of code for **US-Main clients**.

Typically used by the monitor and control scripts.

Designed to help interacting with a US-Main instance typically from any remote
host able to connect to the VM hosting that instance.
""".


-export([ setup/0, teardown/0, get_tcp_port_range/1 ]).


% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").



% Type shorthands:

-type atom_node_name() :: net_utils:atom_node_name().

-type argument_table() :: cmd_line_utils:argument_table().

-type file_path() :: file_utils:file_path().



-doc """
Setups the client.

Returns the name of the target US-Main node, the corresponding
configuration, and the table of remaining command-line arguments (if any).
""".
-spec setup() -> { atom_node_name(), Cfg :: [ term() ], argument_table() }.
setup() ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad_from_module(),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_app(),

	{ CfgFilePath, FinalArgTable } = init_from_command_line(),

	Cfg = file_utils:read_terms( CfgFilePath ),

	%trace_utils:debug_fmt( "Read configuration from '~ts':~n ~p",
	%                       [ CfgFilePath, Cfg ] ),

	[ MainTargetNodeName, UserTargetNodeName ] = get_target_node_names( Cfg ),

	app_facilities:display( "Trying to connect to US-Main node '~ts', "
		"as client node '~ts'.", [ MainTargetNodeName, node() ] ),

	ActualTargetNodeName = case net_adm:ping( MainTargetNodeName ) of

		pong ->
			MainTargetNodeName;

		pang ->
			trace_utils:warning_fmt( "Unable to connect to a target main "
				"node '~ts'; trying an alternate one, based on "
				"user name: '~ts' (in case of ad hoc launch).",
				[ MainTargetNodeName, UserTargetNodeName ] ),

			case net_adm:ping( UserTargetNodeName ) of

				pong ->
					UserTargetNodeName;

				pang ->
					trace_utils:error_fmt( "Unable to connect to either node "
						"names, the main one ('~ts') or the user one ('~ts')."
						"~nIf the target node is really running and is named "
						"like either of the two, check that the cookies match "
						"and, finally, that no firewall is in the way "
						"(e.g. a server may filter the EPMD port of interest).",
						[ MainTargetNodeName, UserTargetNodeName ] ),

					throw( { unable_to_connect_to,
							 { MainTargetNodeName, UserTargetNodeName } } )

				end

		end,


	% Otherwise the remote node could not be known before use:
	global:sync(),

	%app_facilities:display( "Globally registered names: ~w.",
	%                        [ global:registered_names() ] ),

	{ ActualTargetNodeName, Cfg, FinalArgTable }.



-doc """
Tears down the client gracefully.
""".
-spec teardown() -> void().
teardown() ->

	app_facilities:display( "Client terminating now "
		"(while known other nodes are ~w).", [ nodes() ] ),

	% Feeble attempt of avoiding non-systematic "'global' at node us_main@xxx
	% requested disconnect from node 'us_main_controller_exec-uu@yyy' in order
	% to prevent overlapping partitions":
	%
	% (far less brutal than erlang:halt/{0,1}, yet awfully slow, and
	% actually non-blocking)
	%
	%global:disconnect(),

	%timer:sleep( 500 ),

	%global:sync(),

	init:stop( _StatusCode=0 ),

	%timer:sleep( 500 ),

	% We thought that the actual reason was actually that the client host had a
	% firewall that blocked incoming EPMD (on a specific port) connections from
	% the US server host, yet after fixing that the "overlapping partitions"
	% problem remained. DNS names, EPMD ports, node names, cookies, TCP ranges
	% seem to be legit, though.
	%
	% Finally the only solution left would be to resort to disabling this
	% prevent_overlapping_partitions options (see DIST_OPTS in Myriad's
	% GNUmakevars.inc).


	% ?app_stop should not be used here as its wait_for_any_trace_supervisor
	% macro would wait for a non-launched supervisor.
	%
	% ?app_stop_without_waiting_for_trace_supervisor() is not used either, as
	% no aggregator was started from that client.
	%
	app_facilities:finished().




-doc """
Initialises this application from the command line.

Returns the remaining argument table.
""".
-spec init_from_command_line() -> { file_path(), argument_table() }.
init_from_command_line() ->

	% To force options for testing:
	%ArgTable = cmd_line_utils:generate_argument_table( "--help" ),

	ArgTable = cmd_line_utils:get_argument_table(),

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

	%trace_utils:debug_fmt( "Setting remote cookie: '~ts'.", [ RemoteCookie ] ),

	net_utils:set_cookie( RemoteCookie ),

	% Depends on the client:

	%trace_utils:debug_fmt( "Remaining arguments: ~ts",
	%   [ cmd_line_utils:argument_table_to_string( CookieShrunkTable ) ] ),

	%list_table:is_empty( CookieShrunkTable ) orelse
	%   throw( { unexpected_arguments,
	%            list_table:enumerate( CookieShrunkTable ) } ),

	{ CfgFilePath, CookieShrunkTable }.



-doc """
Returns the possible node names (main or user-based one) corresponding to the
target server US-Main instance.

Two names are considered, as two approaches can be used to launch US-Main nodes.
""".
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
	% an app, typically for testing; we used to suppose that the user names -
	% local and server - matched, but now we expect a conventional user to be
	% used on the server):
	%
	BaseNodeNames = [ "us_main", text_utils:format( "us_main_exec-~ts",
										%[ system_utils:get_user_name() ] ) ],
										[ "main-srv" ] ) ],

	% Returns relevant, ordered candidates:
	[ net_utils:get_complete_node_name( N, RemoteHostname, NodeNamingMode )
		|| N <- BaseNodeNames ].



-doc "Returns the TCP port range to use (if any).".
get_tcp_port_range( Cfg ) ->

	MaybePortRange = list_table:get_value_with_default( _K=tcp_port_range,
		_Default=undefined, Cfg ),

	%trace_utils:debug_fmt( "TCP port range: ~p.", [ MaybePortRange ] ),

	MaybePortRange.
