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

-module(us_main_controller_app).

-moduledoc """
Actual US-Main client-side **controlling logic**, as a (Myriad) application.

Typically called through the us_main/priv/bin/control-us-main.sh script.

Designed to control a US-Main instance typically from any remote host able to
connect to the VM hosting that instance.
""".


-export([ exec/0 ]).


% For us_main_home_automation_server_registration_name:
-include("us_main_defines.hrl").


-doc "Runs this controller app.".
-spec exec() -> no_return().
exec() ->

	{ ActualTargetNodeName, _Cfg, ArgTable } = us_main_client:setup(),

	TestNode = 'us_main_monitor_exec-wondersye@mini',

	trace_utils:debug_fmt( "Testing node '~ts': ~ts.",
						   [ TestNode, net_adm:ping( TestNode ) ] ),

	{ AllArgs, FinalArgTable } =
		cmd_line_utils:extract_optionless_command_arguments( ArgTable ),

	{ Cmd, CmdArgs } = case AllArgs of

		[ SingleCmd ] ->
			{ SingleCmd, [] };

		[ SomeCmd | Args ] ->
			{ SomeCmd, Args }

	end,

	list_table:is_empty( FinalArgTable ) orelse
		throw( { unexpected_arguments,
				 list_table:enumerate( FinalArgTable ) } ),

	trace_utils:debug_fmt( "Execution command '~ts' with arguments ~p.",
						   [ Cmd, CmdArgs ] ),

	% The home automation server is expected to run on the target node, but to
	% be registered there only locally, to avoid clashing with any other
	% home automation server:
	%
	HomeAutoSrvPid = naming_utils:get_locally_registered_pid_for(
		?us_main_home_automation_server_registration_name,
		ActualTargetNodeName ),

	app_facilities:display( "Will interact with the home automation server ~w.",
							[ HomeAutoSrvPid ] ),

	case Cmd of

		"is_present" ->
			handle_is_present( CmdArgs, HomeAutoSrvPid );

		"declare_present" ->
			handle_declare_present( CmdArgs, HomeAutoSrvPid );

		"declare_not_present" ->
			handle_declare_not_present( CmdArgs, HomeAutoSrvPid );

		"start_alarm" ->
			handle_start_alarm( CmdArgs, HomeAutoSrvPid );

		"stop_alarm" ->
			handle_stop_alarm( CmdArgs, HomeAutoSrvPid );

		"is_alarm_active" ->
			handle_is_alarm_active( CmdArgs, HomeAutoSrvPid );

		"start_lighting" ->
			handle_start_lighting( CmdArgs, HomeAutoSrvPid );

		"stop_lighting" ->
			handle_stop_lighting( CmdArgs, HomeAutoSrvPid );

		Other ->
			throw( { unsupported_command, Other, CmdArgs } )

	end,

	timer:sleep( 2000 ),

	us_main_client:teardown().




% Section for command implementations.


%% Presence-related subsection.

handle_is_present( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! { getPresenceStatus, [], self() },

	receive

		{ wooper_result, IsPresent } ->
			app_facilities:display( "Presence at home: ~ts.", [ IsPresent ] )

	end;

handle_is_present( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).



handle_declare_present( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! { setPresenceStatus, true };

handle_declare_present( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).



handle_declare_not_present( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! { setPresenceStatus, false };

handle_declare_not_present( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).




%% Alarm-related subsection.

handle_is_alarm_active( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! { getAlarmStatus, [], self() },

	receive

		{ wooper_result, IsAlarmActive } ->
			app_facilities:display( "Is alarm active: ~ts.", [ IsAlarmActive ] )

	end;

handle_is_alarm_active( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).



handle_start_alarm( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! { setAlarmStatus, true };

handle_start_alarm( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).



handle_stop_alarm( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! { setAlarmStatus, false };

handle_stop_alarm( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).



%% Lighting-related subsection.

handle_start_lighting( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! startLighting;

handle_start_lighting( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).


handle_stop_lighting( _CmdArgs=[], HomeAutoSrvPid ) ->
	HomeAutoSrvPid ! stopLighting;

handle_stop_lighting( CmdArgs, _HomeAutoSrvPid ) ->
	throw( { unexpected_arguments, CmdArgs } ).
