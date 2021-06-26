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
% Creation date: Wednesday, June 16, 2021.


% @doc <b>Root OTP supervisor</b> of the us_main application.
%
% Directly created by us_main_app.
%
-module(us_main_sup).

-behaviour(supervisor).


-export([ start_link/0, start_link/1 ]).

-export([ init/1 ]).

-define( server_registration_name, ?MODULE ).



% Shorthand:
-type application_run_context() :: otp_utils:application_run_context().



% Implementation notes:
%



% @doc Starts and links the US-Main supervisor, with OTP conventions.
%
% (function probably useless)
%
-spec start_link() -> otp_utils:supervisor_pid().
start_link() ->
	supervisor:start_link( { local, ?server_registration_name }, ?MODULE,
						   _Default=[ as_otp_release ] ).



% @doc Starts and links the US-Main supervisor, with OTP conventions or not.
-spec start_link( application_run_context() ) -> otp_utils:supervisor_pid().
start_link( AppRunContext ) ->
	supervisor:start_link( { local, ?server_registration_name }, ?MODULE,
						   _Default=[ AppRunContext ] ).



% @doc Initialises this OTP supervisor.
-spec init( [ application_run_context() ] ) ->
				{ 'ok', { supervisor:sup_flags(), supervisor:child_spec() } }.
init( _Args=[ AppRunContext ] ) ->

	otp_utils:check_application_run_context( AppRunContext ),

	% Preparing a trace bridge to collect traces and errors:

	BinTraceEmitterName = <<"Supervisor">>,

	BinTraceCategory = <<"US.US-Main.Supervision">>,

	AggregatorPid =
		class_TraceAggregator:get_aggregator( _LaunchAggregator=false ),

	BridgeSpec = { BinTraceEmitterName, BinTraceCategory, AggregatorPid },

	trace_bridge:register( BridgeSpec ),

	trace_bridge:debug_fmt( "Starting us_main supervisor (run context: ~ts)...",
							[ AppRunContext ] ),

	% Watchdog check every 15 minutes:
	AggregatorPid ! { enableWatchdog, [ _PeriodInSecs=15*60 ] },

	% The logic below shall better be in a (single) supervised child, for a
	% better logic separation.

	% See
	% https://ninenines.eu/docs/en/cowboy/2.8/guide/listeners/#_secure_tls_listener
	% for https.

	SupSettings = otp_utils:get_supervisor_settings(
					_RestartStrategy=one_for_one,
					class_USConfigServer:get_execution_target() ),

	SensorManagerSpec = #{ id => us_sensor_manager,
						   start => { class_USSensorManager, new_link, [] },
						   restart => permanent,
						   shutdown => 2000,
						   type => worker,
						   modules => [ class_USSensorManager ] },

	ChildSpecs = [ SensorManagerSpec ],
	%ChildSpecs = [],

	{ ok, { SupSettings, ChildSpecs } }.
