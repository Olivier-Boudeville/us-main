% Copyright (C) 2021-2025 Olivier Boudeville
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
% Creation date: Wednesday, June 16, 2021.

-module(us_main_sup).

-moduledoc """
**Root OTP supervisor** of the US-Main application.

Directly spawned from `us_main_app`.
""".


-behaviour(supervisor).


-export([ start_link/0, start_link/1 ]).

-export([ init/1 ]).

-define( server_registration_name, ?MODULE ).


% For the general_main_settings record:
-include("class_USMainConfigServer.hrl").


% Type shorthands:

-type execution_target() :: basic_utils:execution_target().
-type application_run_context() :: otp_utils:application_run_context().
-type child_spec() :: supervisor:child_spec().



% Implementation notes:
%
% Spawns automatically the US-Main:
% - Configuration Server
% - Contact Directory
% - Communication Gateway
% - Sensor Manager (which relies on the US-Common infrastructure, notably its
% scheduler)
% - Home Automation Server


-doc """
Starts and links the US-Main supervisor, with OTP conventions.

(function probably useless)
""".
-spec start_link() -> otp_utils:supervisor_pid().
start_link() ->
	supervisor:start_link( { local, ?server_registration_name }, ?MODULE,
						   _Default=[ as_otp_release ] ).



-doc """
Starts and links the US-Main supervisor, with OTP conventions or not.
""".
-spec start_link( application_run_context() ) -> otp_utils:supervisor_pid().
start_link( AppRunContext ) ->
	supervisor:start_link( { local, ?server_registration_name }, ?MODULE,
						   _Default=[ AppRunContext ] ).



-doc "Initialises this OTP supervisor.".
-spec init( [ application_run_context() ] ) ->
				{ 'ok', { supervisor:sup_flags(), [ child_spec() ] } }.
init( _Args=[ AppRunContext ] ) ->

	otp_utils:check_application_run_context( AppRunContext ),

	% Preparing a trace bridge to collect traces and errors:

	BinTraceEmitterName = <<"Supervisor">>,

	BinTraceCategory = <<"US.US-Main.Supervision">>,

	AggregatorPid =
		class_TraceAggregator:get_aggregator( _LaunchAggregator=false ),

	BridgeSpec = trace_bridge:get_bridge_spec( BinTraceEmitterName,
		BinTraceCategory, AggregatorPid ),

	trace_bridge:register( BridgeSpec ),

	ExecTarget = class_USConfigServer:get_execution_target(),

	trace_bridge:debug_fmt( "Starting us_main supervisor (run context: ~ts; "
		"execution target: ~ts)...", [ AppRunContext, ExecTarget ] ),

	% Watchdog check every 15 minutes:
	AggregatorPid ! { enableWatchdog, [ _PeriodInSecs=15*60 ] },


	SupSettings = otp_utils:get_supervisor_settings(
		% If a child process terminates, only that process is restarted:
		_RestartStrategy=one_for_one,
		class_USConfigServer:get_execution_target() ),

	% Five children; first is an OTP supervisor bridge in charge of the US-Main
	% configuration server:
	%
	ConfigServerSpec = get_config_bridge_spec( ExecTarget ),

	% Second child, another supervisor bridge in charge of the (US-Main) contact
	% directory:
	%
	ContactDirectorySpec = get_contact_directory_bridge_spec( ExecTarget ),

	% Third, a bridge in charge of the (US-Main) the communication manager:
	CommGatewaySpec = get_communication_manager_bridge_spec( ExecTarget ),

	% Fourth, a bridge in charge of the (US-Main, local) sensor manager:
	SensorManagerSpec = get_sensor_manager_bridge_spec( ExecTarget ),

	% Fifth, a bridge in charge of the (Oceanic-based, local) home automation
	% server, acting as an Enocean gateway:
	%
	HomeAutomationServerSpec = get_home_automation_bridge_spec( ExecTarget ),

	ChildSpecs = [ ConfigServerSpec, ContactDirectorySpec, CommGatewaySpec,
				   SensorManagerSpec, HomeAutomationServerSpec ],

	%trace_bridge:debug_fmt( "Initialisation of the US-Main main supervisor "
	%   "returning supervisor settings ~p and child specs ~p.",
	%   [ SupSettings, ChildSpecs ] ),

	{ ok, { SupSettings, ChildSpecs } }.



-doc "Returns the bridge spec for the US-Main configuration server.".
-spec get_config_bridge_spec( execution_target() ) -> child_spec().
get_config_bridge_spec( ExecTarget ) ->

	#{ id => us_main_configuration_server_id,

	   start => { _Mod=class_USMainConfigServer, _Fun=start_link,
                  % We used to specify the PID of their supervisor to such
                  % children (with a '_SupervisorPid=self()' argument here), yet
                  % this does not seem relevant, hence was removed:
                  %
				  _Args=[ _AppRunContext=as_otp_release ] },

	   % Always restarted in production:
	   restart => otp_utils:get_restart_setting( ExecTarget ),

	   % This child process is of the 'supervisor' type, and, in
	   % https://erlang.org/doc, the
	   % design_principles/sup_princ.html#child-specification page explains that
	   % 'infinity' is required here (rather than, say, a 2-second termination
	   % was allowed before brutal killing):
	   %
	   shutdown => infinity,

	   % As it is a WOOPER instance (not for example a gen_server):
	   type => supervisor,

	   modules => [ class_USMainConfigServer ] }.



-doc "Returns the bridge spec for the US-Main contact directory.".
-spec get_contact_directory_bridge_spec( execution_target() ) -> child_spec().
get_contact_directory_bridge_spec( ExecTarget ) ->

	% Refer to get_config_bridge_spec/1 for comments:
	#{ id => us_contact_directory,

	   start => { _Mod=class_USContactDirectory, _Fun=start_link, _Args=[] },

	   restart => otp_utils:get_restart_setting( ExecTarget ),

	   shutdown => infinity,

	   type => supervisor,

	   modules => [ class_USContactDirectory ] }.



-doc "Returns the bridge spec for the US-Main communication manager.".
-spec get_communication_manager_bridge_spec( execution_target() ) ->
			child_spec().
get_communication_manager_bridge_spec( ExecTarget ) ->

	% Refer to get_config_bridge_spec/1 for comments:
	#{ id => us_communication_gateway,

	   start =>
			{ _Mod=class_USCommunicationGateway, _Fun=start_link, _Args=[] },

	   restart => otp_utils:get_restart_setting( ExecTarget ),

	   shutdown => infinity,

	   type => supervisor,

	   modules => [ class_USCommunicationGateway ] }.



-doc "Returns the bridge spec for the US-Main (local) sensor manager.".
-spec get_sensor_manager_bridge_spec( execution_target() ) -> child_spec().
get_sensor_manager_bridge_spec( ExecTarget ) ->

	% Refer to get_config_bridge_spec/1 for comments:
	#{ id => us_sensor_manager,

	   start => { _Mod=class_USSensorManager, _Fun=start_link, _Args=[] },

	   restart => otp_utils:get_restart_setting( ExecTarget ),

	   shutdown => infinity,

	   type => supervisor,

	   modules => [ class_USSensorManager ] }.



-doc """
Returns the bridge spec for the home automation gateway (a server integrating
Oceanic).
""".
-spec get_home_automation_bridge_spec( execution_target() ) -> child_spec().
get_home_automation_bridge_spec( _ExecTarget ) ->

	% Refer to get_config_bridge_spec/1 for comments:
	#{ id => us_home_automation_server,

	   start => { _Mod=class_USHomeAutomationServer, _Fun=start_link,
				  _Args=[] },

	   % Currently forcing any restart needed, as rather stable:
	   restart => permanent,
				  %otp_utils:get_restart_setting( ExecTarget ),

	   shutdown => infinity,

	   type => supervisor,

	   modules => [ class_USHomeAutomationServer ] }.
