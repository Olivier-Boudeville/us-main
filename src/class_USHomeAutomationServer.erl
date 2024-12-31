% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: Wednesday, November 23, 2022.

-module(class_USHomeAutomationServer).

-moduledoc """
US server in charge of **providing home automation services**, based on Enocean,
thanks to Ceylan-Oceanic.
""".


-define( class_description, "US server in charge of providing home "
		 "automation services, based on Enocean, thanks to Ceylan-Oceanic" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").

% For the event records:
-include_lib("oceanic/include/oceanic.hrl").


% Design notes:
%
% We rely here on Ceylan-Oceanic (https://oceanic.esperide.org/), which itself
% relies on our fork of erlang-serial
% (https://github.com/Olivier-Boudeville/erlang-serial).


% TO-DO: deactivate the presence simulation when someone is declared at home /
% activate it otherwise.


% This home automation server is designed to be able to integrate to an OTP
% supervision tree thanks to a supervisor bridge, whose behaviour is directly
% defined in this module. See https://wooper.esperide.org/#otp-guidelines for
% further information.
%
-behaviour(supervisor_bridge).

% User API of the bridge:
-export([ start_link/0 ]).


% Callbacks of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).


% Silencing:
-export([ send_psc_trace/3, send_psc_trace_fmt/4, from_decimal_hour/1 ]).


-define( bridge_name, ?MODULE ).



-doc "US-Server for home automation.".
-type home_automation_server_pid() :: class_USServer:server_pid().


% For defines:
-include_lib("myriad/include/utils/time_utils.hrl").



% For the presence_simulation_setting record:
-include("class_USHomeAutomationServer.hrl").


-doc "Setting of a given instance of presence simulation.".
-type presence_simulation_setting() :: #presence_simulation_setting{}.



-doc "All (user-level) settings for a whole presence simulation service.".
-type presence_simulation_settings() ::
	{ 'default', MaybeTargetActuatorEurid :: option( eurid() ) }
  | [ presence_simulation_setting() ].



-doc "Describes a start/stop logical moment to simulate presence.".
-type presence_milestone() ::
	time().  % A time in the day
  % Actually these two are transparently managed by the server, if smart
  % lighting is enabled:
  %
  %| 'dawn'  % First light (if any) of the day
  %| 'dusk'. % Last light (if any) of the day



-doc "A time slot during which a presence shall be simulated.".
-type presence_slot() ::
	{ Start :: presence_milestone(), Stop :: presence_milestone() }.



-doc """
An intra-day general program regarding a presence to simulate.

If slots are used, at least one shall be defined (otherwise one of the
constant_* atoms shall be used).
""".
-type presence_program() :: [ presence_slot() ]
						  | 'constant_presence'
						  | 'constant_absence'.



-doc """
The time for dawn and dusk (if any - think to the extreme latitudes), at a given
(implicit) location and date.

Dawn and dusk are defined as actual transitions between darkness/daylight.
""".
-type celestial_timing() ::
	{ Dawn :: option( time() ), Dusk :: option( time() ) }
  | 'constant_daylight' | 'constant_darkness'.



-doc """
The time for dawn and dusk (if any - think to the extreme latitudes), at a given
(implicit) location, for the specified date.
""".
-type celestial_info() :: { date(), celestial_timing() }.


-export_type([ home_automation_server_pid/0,
			   presence_simulation_settings/0, presence_simulation_setting/0,
			   presence_milestone/0, presence_slot/0, presence_program/0,
			   celestial_timing/0, celestial_info/0 ]).




% Local types:


-doc "Internal identifier of a registered presence simulation instance.".
-type presence_sim_id() :: count().



-doc "A pair of telegrams corresponding to a press/release transaction.".
-type telegram_pair() :: { Press :: telegram(), Release :: telegram() }.



-doc """
Information regarding a planned (presence) task, to be able to check/control it.
""".
-type task_info() :: { task_id(), time() }.



-doc """
An hour in the day or duration, as a floating-point value (e.g. 12.5 for 30
minutes past noon).
""".
-type decimal_hour() :: float().



-doc """
Time equation table, to correct sunrise/sunset times based on day rank.

A simpler indexed list would have been sufficient.

See also <https://en.wikipedia.org/wiki/Equation_of_time>.
""".
-type time_equation_table() :: table( day_in_the_year(), decimal_hour() ).


% Originally in
% https://www.astrolabe-science.fr/wp-content/uploads/2023/02/EdTimcce.csv.
%
-define( time_equation_data_file, "time_equation_data.csv").





% Internal information regarding an instance of presence simulation:
-record( presence_simulation, {

	% The identifier of this presence simulation:
	id :: presence_sim_id(),

	% Tells whether this presence simulation is enabled:
	%
	% (as it may be disabled, for example when somebody is at home)
	%
	enabled = true :: boolean(),

	% Tells whether the presence simulation is currently activated, typically if
	% lighting is currently on or off (allows avoiding the sending of
	% unnecessary switching orders).
	%
	% Note that we consider that this server is the sole controller of the
	% target actuator; if not (e.g. should the user be able to switch on/off a
	% given lamp thanks to a physical button), we may:
	%  - either force states (setting them explicitly regardless of their
	%  inferred status), and do that more frequently (e.g. not jumping over
	%  milestones known to have no effect)
	%  - or, better, listen to Oceanic events for notifications about state
	%  changes, and update our knowledge according to unexpected ones
	%
	% Setting it by default to false, to force any needed activation, should it
	% be not set by mistake:
	%
	activated = false :: boolean(),


	% The specific EURID (if any) to be used to identify the source of the
	% emitted (Enocean) telegrams; expected to have been already learnt by the
	% target actuator(s):
	%
	source_eurid :: eurid(),

	% The EURID of the target actuator (typically a smart plug controlling a
	% lamp):
	%
	% (possibly a broadcast address)
	%
	target_eurid :: eurid(),


	% The general daily program of this presence simulation, generally a
	% chronologically-ordered intra-day (from midnight to midnight) non-empty
	% list of presence slots, or simpler policies:
	%
	program :: presence_program(),


	% The next planned action (if any) that shall happen:
	% Not used, as relying now on a "stateless" algorithm.
	%next_action :: option( { timestamp(), presence_action() } ),


	% Tells whether lighting shall be switched off during a presence slot when
	% the light of day should be available (provided that the geographical
	% position of the server is known):
	%
	smart_lighting = 'true' :: boolean(),


	% Tells whether, during a period of simulated presence, lighting shall be,
	% at random times, stopped and restarted after a random duration, to better
	% simulate a local presence (otherwise constant lighting happens):
	%
	random_activity = 'true' :: boolean(),

	% The mean duration, in seconds, of a period of lighting (if random activity
	% is enabled):
	%
	mean_light_duration :: second_duration(),

	% The mean duration, in seconds, of an interruption of lighting (if random
	% activity is enabled):
	%
	mean_no_light_duration :: second_duration(),



	% The press/release telegrams to be emitted to switch the target actuator
	% (typically a smart plug) on:
	%
	activation_telegrams :: telegram_pair(),

	% The press/release telegrams to be emitted to switch the target actuator
	% (typically a smart plug) off:
	%
	deactivation_telegrams :: telegram_pair(),


	% The identifier and planned time of the currently-pending scheduling task
	% (if any) declared to manage that presence during the current day:
	%
	presence_task_info :: option( task_info() ) } ).



-doc """
Internal information regarding an instance of presence simulation.

Counterpart of the user-level presence_simulation_setting().
""".
-type presence_simulation() :: #presence_simulation{}.



-doc "A table keeping track of the known presence simulations.".
-type presence_table() :: table( presence_sim_id(), presence_simulation() ).



-doc """
A (higher-level) status of a device, as known by this server.

It can be unset as not known or as not sufficiently relevant (e.g. any reported
temperature reported by a sensor should just be read from any last event).
""".
-type device_status() :: 'unknown' | single_input_contact_status().



% Rather than 'opened' | 'closed':
-type single_input_contact_status() :: oceanic:contact_status().


% Internal information regarding a known (Enocean) device:
-record( device_state, {

	% The identifier of this device:
	eurid :: eurid(),

	% For example: <<"Opening detector of the front door">>:
	name :: device_name(),

	% The (supposedly single) EEP identifier known (if any) for this device
	% (e.g. 'double_rocker_multipress'):
	%
	eep_id :: option( eep_id() ),

	% Records the initial event (if any) received from this device (a configured
	% one being first seen, a taught-in one, or one not known a priori but
	% discovered):
	%
	initial_event :: option( device_event() ),

	% Records the last event (if any) sent by this device:
	last_event :: option( device_event() ),

	% The currently known status for this device:
	current_status :: device_status() } ).


-doc "Internal information regarding a known (Enocean) device.".
-type device_state() :: #device_state{}.



-doc """
A table recording the information regarding the Enocean devices known by this
automation server, notably in order to detect state transitions and trigger
events.
""".
-type device_table() :: table( eurid(), device_state() ).



% Type shorthands:

-type count() :: basic_utils:count().
-type user_data() :: basic_utils:user_data().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type timestamp() :: time_utils:timestamp().

-type device_path() :: file_utils:device_path().
%-type bin_directory_path() :: file_utils:bin_directory_path().

-type bytes_per_second() :: system_utils:bytes_per_second().

-type date() :: time_utils:date().
-type time() :: time_utils:time().
-type second_duration() :: time_utils:second_duration().
-type day_in_the_year() :: time_utils:day_in_the_year().

-type milliseconds() :: unit_utils:milliseconds().


-type trace_severity() :: trace_utils:trace_severity().
-type trace_message() :: trace_utils: trace_message().

-type trace_format() :: text_utils:trace_format().
-type trace_values() :: text_utils:trace_values().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
-type task_id() :: class_USScheduler:task_id().

-type position() :: unit_utils:position().
-type declination() :: unit_utils:declination().
-type radians() :: unit_utils:radians().

-type oceanic_server_pid() :: oceanic:oceanic_server_pid().
-type device_name() :: oceanic:device_name().
-type device_description() :: oceanic:device_description().
-type device_event() :: oceanic:device_event().
-type back_online_info() :: oceanic:back_online_info().
-type eurid_string() :: oceanic:eurid_string().
-type eurid() :: oceanic:eurid().
-type telegram() :: oceanic:telegram().
-type eep_id() :: oceanic:eep_id().



% The class-specific attributes:
-define( class_attributes, [

	{ oc_srv_pid, option( oceanic_server_pid() ),
	  "the PID of the Oceanic server (if any can exist) used by this server" },

	{ oc_periodic_restart, boolean(), "tells whether Oceanic shall be "
	  "periodically restarted, in order to overcome any risk of freeze of "
	  "the USB-based serial interface" },

	{ oc_base_id, option( eurid() ), "the base identifier (if any) of "
	  "the local Enocean gateway to use, as determined by Oceanic" },

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" },

	% Typically for the presence simulator:
	{ scheduler_pid, scheduler_pid(),
	  "the PID of the scheduler used by this server" },

	{ app_base_directory, bin_directory_path(),
	  "the base directory of the US-Main application (the root where "
	  "src, priv, ebin, etc. can be found)" },

	{ actual_presence, boolean(), "tells whether there is someone at home" },

	% Better defined separately from presence_table, as the program shall
	% remain, whereas presence simulation service may be regularly
	% enabled/disabled:
	%
	{ presence_simulation_enabled, boolean(),
	  "tells whether the presence simulation service is currently enabled" },

	{ presence_table, presence_table(),
	  "registery of the presence simulations, recording all their settings" },

	{ next_presence_id, presence_sim_id(),
	  "the next presence identifier that will be assigned" },

	{ time_equation_table, option( time_equation_table() ),
	  "a table (if any) allowing to correct sunrise/sunset times, for smart "
	  "lighting" },

	{ midnight_task_id, option( task_id() ),
	  "a task to be triggered, if the presence simulation is activated, "
	  "each day at midnight to determine and update the activity of the "
	  "presence simulations for the next day (and to ensure that any "
	  "potential switching discrepancy does not linger, and if enabled to "
	  "restart the serial interface)" },

	{ server_location, option( position() ),
	  "the (geographic) location, as a position, of this US-Main server" },

	{ presence_switching_device, option( eurid() ),
	  "the device (typically any that can be interpreted as being pressed and "
	  "released, like a push-button or a double-rocker), if any, used as "
	  "first-level indicator about whether somebody is at home" },

	{ presence_switching_device_desc, option( bin_string() ),
	  "a textual description of the presence switching device (if any)" },

	{ celestial_info, option( celestial_info() ),
	  "any precomputed dawn/dusk time, for the current day" },

	{ comm_gateway_pid, gateway_pid(),
	  "the PID of the US communication gateway used to send user "
	  "notifications" },

	{ device_table, device_table(),
	  "the table recording the current state of devices, notably to "
	  "detect their state transitions" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Home Automation" ).



% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Implementation notes:


% Regarding presence simulation:
%
% The various presence periods in a slot may overlap (typically because the time
% of dusk and dawn varies in the course of the year); anyway the presence will
% be simulated in all cases with no interruption.
%
% A robust mode of operation has been retained, with which states are enforced
% (e.g smart plug is on) rather than transitions (e.g. toggling smart plug
% "blindly" on->off or off->on).


% Regarding the computation of the time in the day of dawn and dusk:
%
% The duration of a given day depends on the date and latitude of the location
% of interest; the actual moments for dawn and dusk depend also on longitude.
%
% More information (in French):
% https://www.astrolabe-science.fr/duree-du-jour-et-latitude/. Many thanks to
% David Alberto for the sharing.


% Regarding scheduling:
%
% Each presence simulation is planned from the current event to (only) the next.
% As soon there is at least one presence simulation, an additional overall
% update (daily, at midnight) is scheduled, so that the programs for this new
% day are established.


% Regarding the Enocean actuators:
%
% We suppose that these devices already learnt the USB gateway being used by
% Oceanic. This server will act upon each of these actuators as if it was a
% double-rocker switch (e.g. not two single-contact buttons), whose 'on' button
% is button_ao, and whose 'off' button is button_ai.




% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this home automation server with an OTP
% supervision tree.


-doc """
Starts and links a supervision bridge for the home automation system.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see us_main_sup:init/1), hence generally triggered by the application
initialisation.
""".
-spec start_link() -> term().
start_link() ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the home automation system." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
		_Module=?MODULE, _InitArgs=[] ).



-doc """
Callback to initialise this supervisor bridge, typically in answer to
start_link/0 being executed.
""".
-spec init( list() ) -> { 'ok', pid(), State :: term() } | 'ignore'
					  | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_bridge:info_fmt( "Initializing the US-Main supervisor bridge ~w for "
						   "the home automation system.", [ self() ] ),

	% Not specifically synchronous:
	HomeAutomSrvPid = ?MODULE:new_link(),

	{ ok, HomeAutomSrvPid, _InitialBridgeState=HomeAutomSrvPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=HomeAutomSrvPid )
								when is_pid( HomeAutomSrvPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
		"the home automation system (reason: ~w, "
		"home automation server: ~w).", [ Reason, HomeAutomSrvPid ] ),

	% Synchronicity needed, otherwise a potential race condition exists, leading
	% this process to be killed by its OTP supervisor instead of being normally
	% stopped:
	%
	wooper:delete_synchronously_instance( HomeAutomSrvPid ),

	trace_bridge:debug_fmt( "US-Main home automation server ~w terminated.",
							[ HomeAutomSrvPid ] ).




% Actual implementation of the home automation server.


-doc """
Constructs an home automation server, based on the default, local TTY allocated
to the USB Enocean gateway, whose base identifier will be used as source EURID
for telegram sendings, and not providing a presence simulator.
""".
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
	construct( State, _TtyPath=oceanic:get_default_tty_path() ).



-doc """
Constructs an home automation server, based on the specified local TTY allocated
to the USB Enocean gateway, whose base identifier will be used as source EURID
for telegram sendings, and not providing a presence simulator.
""".
-spec construct( wooper:state(), device_path() ) -> wooper:state().
construct( State, TtyPath ) ->
	construct( State, TtyPath, _MaybePresenceSimSettings=undefined ).



-doc """
Constructs an home automation server, based on the specified local TTY allocated
to the USB Enocean gateway.

Unless MaybePresenceSimSettings is 'undefined', a presence simulation will be
performed, specified either as a complete list of presence settings, or only
through the EURID of the target actuator(s) - in which case a default presence
policy will apply; the source used for the sent telegrams will be the base
identifier of the gateway.
""".
-spec construct( wooper:state(), device_path(),
				 option( presence_simulation_settings() | eurid_string() ) ) ->
										wooper:state().
construct( State, TtyPath, MaybePresenceSimSettings ) ->
	construct( State, TtyPath, MaybePresenceSimSettings,
			   _MaybeSourceEuridStr=undefined ).



-doc """
Constructs an home automation server, based on the specified local TTY allocated
to the USB Enocean gateway.

Unless MaybePresenceSimSettings is 'undefined', a presence simulation will be
performed, specified either as a complete list of presence settings, or only
through the EURID of the target actuator(s) - in which case a default presence
policy will apply; the source used for the sent telegrams will be the specified
one (if any), otherwise the base identifier of the gateway.

(most complete constructor)
""".
-spec construct( wooper:state(), device_path(),
	option( presence_simulation_settings() | eurid_string() ),
	option( eurid_string() ) ) -> wooper:state().
construct( State, TtyPath, MaybePresenceSimSettings, MaybeSourceEuridStr ) ->

	ServerTraceName = "Home automation server",

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize(ServerTraceName),
		?us_main_home_automation_server_registration_name,
		?us_main_home_automation_server_registration_scope ),

	% Do not start Oceanic if it is bound to fail:
	{ MaybeOcSrvPid, MaybeSrcEuridStr, MaybeBaseId } =
			case oceanic:is_available( TtyPath ) of

		{ true, _SerialRootDir } ->
			OcPid = oceanic:start_link( TtyPath, [ _EventListenerPid=self() ] ),
			BaseId = oceanic:get_oceanic_eurid( OcPid ),
			SourceIdStr = case MaybeSourceEuridStr of

				undefined ->
					% Then go for the BaseId; having to convert it back to
					% string, as to be fed to a presence_simulation_setting:
					%
					oceanic:eurid_to_short_string( BaseId );

				SrcIdStr ->
					SrcIdStr

			end,
			{ OcPid, SourceIdStr, BaseId };

		{ false, ReasonStr, ErrorTerm } ->
			% No house automation can be done then (newline needed, otherwise
			% bad formatting):
			%
			?send_warning_fmt( SrvState,
				"The Oceanic support will not be available. ~ts~n"
				"(error term: ~p).", [ ReasonStr, ErrorTerm ] ),
			{ undefined, undefined, undefined }

	end,

	UsMainCfgSrvPid = class_USMainConfigServer:get_us_main_config_server(),

	% Common to all home-automation services; beware to blocking calls:
	UsMainCfgSrvPid ! { getHomeAutomationSettings, [], self() },

	SchedPid = class_USScheduler:get_main_scheduler(),

	% Interleaved:
	{ MaybeUserSrvLoc, BinAppBaseDirectoryPath, MaybePscSwitchEurid,
	  MaybeConfiguredPscSimSettings, OceanicSettings } = receive

		{ wooper_result, HomeAutoMatSettings } ->
			HomeAutoMatSettings

	end,

	MaybeRetainedPscSimSettings = case MaybeConfiguredPscSimSettings of

		undefined ->
			case MaybePresenceSimSettings of

				undefined ->
					send_psc_trace( info, "Neither construction-level nor "
						"configuration-level presence simulation settings "
						"defined, defaults will apply.", SrvState );

				PresenceSimSettings ->
					send_psc_trace_fmt( info, "Construction-level presence "
						"simulation settings will apply "
						"(no configuration-level ones were defined):~n ~p",
						[ PresenceSimSettings ], SrvState )

			end,

			MaybePresenceSimSettings;


		ConfiguredPscSimSettings ->
			case MaybePresenceSimSettings of

				undefined ->
					send_psc_trace_fmt( info, "No construction-level presence "
						"simulation settings defined, configuration-level ones "
						"will apply:~n ~p", [ ConfiguredPscSimSettings ],
						SrvState ),
					ConfiguredPscSimSettings;

				PresenceSimSettings ->
					send_psc_trace_fmt( info, "The construction-specified "
						"presence simulation settings (~p) will override the "
						"configuration-specified ones (~p).",
						[ PresenceSimSettings, ConfiguredPscSimSettings ],
						SrvState ),
					PresenceSimSettings

			end

	end,


	% Sooner:
	MaybeOcSrvPid =:= undefined orelse
		oceanic:add_configuration_settings( OceanicSettings, MaybeOcSrvPid ),

	% Geographical location:
	MaybeSrvLoc = case MaybeUserSrvLoc of

		undefined ->
			undefined;

		% Already checked as floats by the US-Main configuration server; these
		% are degrees:
		%
		_UserSrvLoc={ Lat, _Long } when Lat > 90.0 orelse Lat < -90.0 ->
			throw( { invalid_latitude, Lat } );

		_UserSrvLoc={ _Lat, Long } when Long > 180.0 orelse Long < -180.0 ->
			throw( { invalid_longitude, Long } );

		UserSrvLoc ->
			UserSrvLoc

	end,

	MaybePscSwitchDesc = case MaybePscSwitchEurid of

		undefined ->
			undefined;

		PscSwitchEurid ->
			case MaybeOcSrvPid of

				% Suspect:
				undefined ->
					text_utils:format( "device of EURID ~ts",
						[ oceanic:eurid_to_string( PscSwitchEurid ) ] );

				OcSrvPid ->
					oceanic:get_device_description( PscSwitchEurid, OcSrvPid )

			end

	end,

	MoreCompleState = setAttributes( SrvState, [
		{ scheduler_pid, SchedPid },
		{ app_base_directory, BinAppBaseDirectoryPath } ] ),

	{ InitPscTable, NextPscId, MaybeTimeEqTable, MaybeMidnightTaskId } =
		init_presence_simulation( MaybeRetainedPscSimSettings, MaybeOcSrvPid,
			MaybeSrcEuridStr, MoreCompleState ),

	% Later:
	%AlarmState = init_alarm( MaybeOcSrvPid, MoreCompleState ),
	AlarmState = MoreCompleState,

	% To report any issue:
	CommGatewayPid = class_USCommunicationGateway:get_communication_gateway(),

	InitalPscEnabled = not table:is_empty( InitPscTable ),

	SetState = setAttributes( AlarmState, [
		{ oc_srv_pid, MaybeOcSrvPid },
		{ oc_periodic_restart, true },
		{ oc_base_id, MaybeBaseId },
		{ us_config_server_pid, UsMainCfgSrvPid },
		{ device_table, table:new() },

		% Expecting to be launching this server while being at home:
		{ actual_presence, true },

		{ presence_simulation_enabled, InitalPscEnabled },
		{ presence_table, InitPscTable },
		{ next_presence_id, NextPscId },
		{ time_equation_table, MaybeTimeEqTable },
		{ midnight_task_id, MaybeMidnightTaskId },
		{ server_location, MaybeSrvLoc },
		{ presence_switching_device, MaybePscSwitchEurid },
		{ presence_switching_device_desc, MaybePscSwitchDesc },
		{ celestial_info, undefined },
		{ comm_gateway_pid, CommGatewayPid } ] ),

	ApplyState = apply_presence_simulation( SetState ),

	?send_notice_fmt( ApplyState, "Constructed: ~ts.",
					  [ to_string( ApplyState ) ] ),

	ApplyState.



-doc """
Initialises the overall presence simulation.

(helper)
""".
-spec init_presence_simulation( option( presence_simulation_settings() ),
	option( oceanic_server_pid() ), option( eurid_string() ),
	wooper:state() ) ->
		{ presence_table(), presence_sim_id(), option( time_equation_table() ),
		  option( task_id() ) }.
init_presence_simulation( MaybePresenceSimSettings, MaybeOcSrvPid,
						  MaybeSrcEuridStr, State ) ->

	% Already available:
	%send_psc_trace_fmt( debug, "Initialising presence simulation, "
	%   "from following settings:~n ~p.", [ MaybePresenceSimSettings ], State ),

	init_presence_simulation( MaybePresenceSimSettings, MaybeOcSrvPid,
		MaybeSrcEuridStr, _PscTable=table:new(), _NextPscId=1,
		_TimeEqTableNeeded=false, State ).



% (helper)
%
% No presence simulation wanted here:
init_presence_simulation( _MaybePresenceSimSettings=undefined, _MaybeOcSrvPid,
		_MaybeSrcEuridStr, EmptyPscTable, InitPscId, _TimeEqTableNeeded,
		State ) ->
	send_psc_trace( info, "No presence simulation wanted.", State ),
	{ EmptyPscTable, InitPscId, _MaybeTimeEqTable=undefined,
	  _MaybeMidTaskId=undefined };

init_presence_simulation( PresenceSimSettings, _MaybeOcSrvPid=undefined,
		_MaybeSrcEuridStr, _PscTable, _NextPscId, _TimeEqTableNeeded,
		State ) ->

	send_psc_trace_fmt( error, "No Oceanic support available, the requested "
		"presence simulation (~p) cannot be performed.",
		[ PresenceSimSettings ], State ),

	throw( { no_presence_simulation, no_oceanic } );


% Not expected to happen, as set with OcSrvPid:
init_presence_simulation( PresenceSimSettings, _OcSrvPid,
		_MaybeSrcEuridStr=undefined, _PscTable, _NextPscId, _TimeEqTableNeeded,
		State ) ->

	send_psc_trace_fmt( error, "No overall source EURID available, the "
		"requested presence simulation (~p) cannot be performed.",
		[ PresenceSimSettings ], State ),

	throw( { no_presence_simulation, no_source_eurid } );


% Default settings, just switching to the actual clause:
init_presence_simulation(
		_PresenceSimSettings={ default, MaybeTargetActuatorEuridStr },
		OcSrvPid, SrcEuridStr, PscTable, NextPscId, _TimeEqTableNeeded,
		State ) ->

	send_psc_trace( info, "Applying default presence simulation settings.",
					State ),

	% Principle: no useless lighting during the expected presence slots, which
	% were initially:
	%  - in the morning: from 7:30 AM to 8:30 AM
	%  - in the evening: from 6:30:00 PM to 11:45 PM

	% TMorningStart = { 7, 30, 0 },
	% TMorningStop = { 8, 30, 0 },

	% TEveningStart = { 18, 30, 00 },
	% TEveningStop = { 23, 45, 00 },

	% PscSlots = [ { TMorningStart, TMorningStop },
	%              { TEveningStart, TEveningStop } ],

	% Now, default settings are safer: if not at home, simulating all day long
	% (unless there is daylight); so, in logical terms, from 7:00 to around
	% 00:30, translating to:

	TEndOfNightStart = { 0, 0, 1 },
	TEndOfNightStop = { 0, 31, 15 },

	TDayStart = { 7, 0, 0 },
	% Bound to be stopped before, due to daylight:
	TDayStop = { 23, 59, 59 },

	PscSlots = [ { TEndOfNightStart, TEndOfNightStop },
				 { TDayStart, TDayStop } ],

	DefaultSettings = #presence_simulation_setting{
		source_eurid=SrcEuridStr,
		target_eurid=MaybeTargetActuatorEuridStr,
		presence_program=PscSlots,
		smart_lighting=true,
		random_activity=true
		% Defaults apply for mean_light_duration/mean_no_light_duration.
	},

	% Branch then to the general rule:
	init_presence_simulation( _PscSimSettings=[ DefaultSettings ], OcSrvPid,
		SrcEuridStr, PscTable, NextPscId, _TimeEqTableIsNeeded=true, State );


% The "actual" clauses now.
%
% Only exit point:
init_presence_simulation( _PresenceSimSettings=[], _OcSrvPid, _SrcEuridStr,
		PscTable, NextPscId, TimeEqTableNeeded, State ) ->

	% Schedules a periodic midnight presence update iff necessary:
	MaybeMidTaskId = case table:is_empty( PscTable ) of

		true ->
			send_psc_trace( warning, "Presence simulation enabled, yet "
							"no specific one requested.", State ),
			undefined;

		false ->
			TomorrowDate = time_utils:get_date_after( date(), _DaysOffset=1 ),

			% Define from time_utils.hrl:
			NextMidnightTimestamp = { TomorrowDate, ?first_time },

			send_psc_trace_fmt( debug, "Registering a daily presence "
				"program update task (every midnight), starting from ~ts.",
				[ time_utils:timestamp_to_string( NextMidnightTimestamp ) ],
				State ),

			% Every day:
			DHMSPeriodicity = { _D=1, _H=0, _M=0, _S=0 },

			?getAttr(scheduler_pid) ! { registerTask,
				[ _CmdMsg=updatePresencePrograms,
				  _StartTime=NextMidnightTimestamp, DHMSPeriodicity,
				  _Count=unlimited ], self() },

			receive

				{ wooper_result, { task_registered, MidTaskId } } ->

					send_psc_trace_fmt( debug, "Midnight presence program "
						"update task #~B defined.", [ MidTaskId ], State ),

					MidTaskId

			end

	end,

	MaybeTimeEqTable = case TimeEqTableNeeded of

		true ->
			TimeTableCsvPath = file_utils:join( [ ?getAttr(app_base_directory),
				"priv", "data", ?time_equation_data_file ] ),

			file_utils:is_existing_file_or_link( TimeTableCsvPath ) orelse
				throw( { time_equation_data_file_not_found,
						 TimeTableCsvPath } ),

			{ Rows, _RowCount=365, _FieldCount=2 } =
				csv_utils:read_file( TimeTableCsvPath, _Separator=$\t ),

			RowsAsNums = [ { text_utils:string_to_integer( DayRankStr ),
							 text_utils:string_to_float( CorrStr ) } ||
				{ DayRankStr, CorrStr } <- Rows ],

			table:new( RowsAsNums );

		false ->
			undefined

	end,

	{ PscTable, NextPscId, MaybeTimeEqTable, MaybeMidTaskId };


% Main clause:
init_presence_simulation( _PresenceSimSettings=[
		#presence_simulation_setting{
			source_eurid=MaybeUserSrcEuridStr,
			target_eurid=MaybeTargetActuatorEuridStr,
			presence_program=Program,
			smart_lighting=SmartBool,
			random_activity=RandomBool,
			mean_light_duration=MLightDur,
			mean_no_light_duration=MNoLightDur } | T ], OcSrvPid, SrcEuridStr,
						  PscTable, NextPscId, TimeEqTableNeeded, State ) ->

	ActualSrcEurid = case MaybeUserSrcEuridStr of

		undefined ->
			oceanic:string_to_eurid( SrcEuridStr );

		UserSrcEuridStr ->
			oceanic:string_to_eurid( UserSrcEuridStr )

	end,

	ActualTargetEurid = case MaybeTargetActuatorEuridStr of

		undefined ->
			E = oceanic:get_broadcast_eurid(),
			send_psc_trace_fmt( info, "Setting target EURID actuator to the "
				"broadcast one, ~ts.", [ oceanic:eurid_to_string( E ) ],
				State ),
			E;

		TargetActuatorEuridStr ->
			send_psc_trace_fmt( info, "Setting target EURID actuator to ~ts.",
								[ TargetActuatorEuridStr ], State ),
			oceanic:string_to_eurid( TargetActuatorEuridStr )

	end,

	% We check the program, not taking into account here dawn/dusk, as they
	% change each day:
	%
	VettedProgram = vet_program( Program ),

	RandomProgram = case RandomBool of

		true ->
			type_utils:check_strictly_positive_integer( MLightDur ),
			type_utils:check_strictly_positive_integer( MNoLightDur ),

			RandProgram = randomise_program( VettedProgram, MLightDur,
											 MNoLightDur, State ),

			send_psc_trace_fmt( debug,
				"Original program ~ts~nResulting randomised program ~ts",
				[ program_to_string( VettedProgram ),
				  program_to_string( RandProgram ) ], State ),

			%RandProgram;
			vet_program( RandProgram );

		false ->
			send_psc_trace_fmt( debug,
				"Vetted verbatim program ~ts",
				[ program_to_string( VettedProgram ) ], State ),

			VettedProgram

	end,

	% We have also to forge the Enocean telegrams necessary for switching on/off
	% the actuator(s), with following conventions (learnt double-rocker, whose
	% top button is "on"):

	SwitchOnButton = button_ao,

	OnPressTelegram = oceanic:encode_double_rocker_switch_telegram(
		ActualSrcEurid, ActualTargetEurid, SwitchOnButton, pressed ),

	OnReleaseTelegram = oceanic:encode_double_rocker_switch_telegram(
		ActualSrcEurid, ActualTargetEurid, SwitchOnButton, released ),


	SwitchOffButton = button_ai,

	OffPressTelegram = oceanic:encode_double_rocker_switch_telegram(
		ActualSrcEurid, ActualTargetEurid, SwitchOffButton, pressed ),

	OffReleaseTelegram = oceanic:encode_double_rocker_switch_telegram(
		ActualSrcEurid, ActualTargetEurid, SwitchOffButton, released ),

	DoSmartLighting = type_utils:check_boolean( SmartBool ),

	DoRandomActivity = type_utils:check_boolean( RandomBool ),

	PscSim = #presence_simulation{
		id=NextPscId,
		enabled=true,
		activated=false,
		source_eurid=ActualSrcEurid,
		target_eurid=ActualTargetEurid,
		program=RandomProgram,
		smart_lighting=DoSmartLighting,
		random_activity=DoRandomActivity,
		activation_telegrams={ OnPressTelegram, OnReleaseTelegram },
		deactivation_telegrams={ OffPressTelegram, OffReleaseTelegram } },

	NewPscTable = table:add_new_entry( _K=NextPscId, PscSim, PscTable ),

	send_psc_trace_fmt( info, "Registered a new presence simulation: ~ts",
		[ presence_simulation_to_string( PscSim ) ], State ),

	NewTimeEqTableNeeded = DoSmartLighting orelse TimeEqTableNeeded,

	init_presence_simulation( T, OcSrvPid, SrcEuridStr, NewPscTable,
		NextPscId+1, NewTimeEqTableNeeded, State );


init_presence_simulation( _PresenceSimSettings=[ Other | _T ], _OcSrvPid,
		_SrcEuridStr, _PscTable, _NextPscId, _TimeEqTableNeeded, _State ) ->
	throw( { invalid_presence_setting, Other } );


init_presence_simulation( _PresenceSimSettings=Other, _OcSrvPid, _SrcEuridStr,
		_PscTable, _NextPscId, _TimeEqTableNeeded, _State ) ->
	throw( { invalid_presence_settings, Other } ).



-doc "Vets the specified user program.".
-spec vet_program( user_data() ) -> presence_program().
vet_program( _PresenceProgram=constant_presence ) ->
	constant_presence;

vet_program( _PresenceProgram=constant_absence ) ->
	constant_absence;

% Then expecting slots:
vet_program( PresenceProgram ) ->
	vet_program( PresenceProgram, _MaybeLastStop=undefined, _AccSlots=[] ).



% (helper)
vet_program( _PresenceProgram=[], _MaybeLastStop, _AccSlots=[] ) ->
	% At least one slot required:
	throw( empty_slot_list );

vet_program( _PresenceProgram=[], _MaybeLastStop, AccSlots ) ->
	lists:reverse( AccSlots );


vet_program( _PresenceProgram=[ Slot={ StartMilestone, StopMilestone } | T ],
			 MaybeLastStop, AccSlots ) ->

	time_utils:is_time( StartMilestone ) orelse
		throw( { invalid_slot_start_milestone, StartMilestone } ),

	time_utils:is_time( StopMilestone ) orelse
		throw( { invalid_slot_stop_milestone, StopMilestone } ),

	StartMilestone < StopMilestone orelse
		throw( { inconsistent_slot_milestones, StartMilestone,
				 StopMilestone } ),

	% As Erlang term order tells 'undefined' is lower than all tuples:
	MaybeLastStop =:= undefined orelse
		( MaybeLastStop < StartMilestone orelse
		  throw( { interleaved_slots, Slot, MaybeLastStop } ) ),

	vet_program( T, StopMilestone, [ Slot | AccSlots ] );


vet_program( Other, _MaybeLastStop, _AccSlots ) ->
	throw( { invalid_presence_program, Other } ).



-doc "Adds random interruptions to the specified program, if it is slot-based.".
-spec randomise_program( presence_program(), second_duration(),
			second_duration(), wooper:state() ) -> presence_program().
randomise_program( PresenceProgram=constant_presence, _MeanLightDuration,
				   _MeanNoLightDuration, _State ) ->
	PresenceProgram;

randomise_program( PresenceProgram=constant_absence, _MeanLightDuration,
				   _MeanNoLightDuration, _State ) ->
	PresenceProgram;

randomise_program( _PresenceProgram=Slots, MeanLightDuration,
				   MeanNoLightDuration, State ) ->

	StdDevLightDur = MeanLightDuration div 2,

	% No less than 8 seconds:
	MinNoLightDur = max( 8, MeanNoLightDuration div 2 ),

	% Max symetrical of Min around Mean:
	MaxNoLightDur = MeanNoLightDuration + 2*(MeanNoLightDuration-MinNoLightDur),

	send_psc_trace_fmt( info, "Randomising presence slots, with, for lighting, "
		"a Gaussian law of mean ~w seconds and standard deviation ~w seconds "
		"and, for non-lighting, a uniform law in [~w,~w] seconds.",
		[ MeanLightDuration, StdDevLightDur, MinNoLightDur, MaxNoLightDur ],
		State ),

	% Sanity checks:

	MeanLightDuration > 0 orelse
		throw( { invalid_mean_light_duration, MeanLightDuration } ),

	MinNoLightDur > 0 orelse
		throw( { invalid_min_no_light_duration, MinNoLightDur } ),

	MaxNoLightDur > MinNoLightDur orelse
		throw( { invalid_max_no_light_duration, MaxNoLightDur } ),

	randomise_program( Slots, MeanLightDuration, StdDevLightDur,
					   MinNoLightDur, MaxNoLightDur, _Acc=[] ).



% (helper)
randomise_program( _Slots=[], _MeanLightDuration, _StdDevLightDur,
				   _MinNoLightDur, _MaxNoLightDur, Acc ) ->
	lists:reverse( Acc );

% Initially, dawn/dusk are not resolved as actual times, so let's leave them:
% (they are not presence milestones anymore)
% randomise_program( _Slots=[ H={ _Start=dawn, Stop } | T ],
%                    MeanLightDuration, MeanNoLightDuration, Acc ) ->
%   randomise_program( T, [ H | T ] );
%
% randomise_program( _Slots=[ H={ Start, _Stop=dusk } | T ],
%                    MeanLightDuration, MeanNoLightDuration, Acc ) ->
%   randomise_program( T, [ H | T ] );

% Replacing each slot by as many sub-slots that fit:
randomise_program( _Slots=[ { Start, Stop } | T ], MeanLightDuration,
				   StdDevLightDur, MinNoLightDur, MaxNoLightDur, Acc ) ->
	% We replace a single overall slot by as many we can fit:
	Duration = time_utils:get_intertime_duration( Start, Stop ),

	RevSubSlots = draw_subslots( Start, Duration, MeanLightDuration,
		StdDevLightDur, MinNoLightDur, MaxNoLightDur, _AccSlots=[] ),

	randomise_program( T, MeanLightDuration, StdDevLightDur, MinNoLightDur,
					   MaxNoLightDur, RevSubSlots ++ Acc ).



% We try here to insert a light slot and a no-light slot in the remaining
% duration - provided that their duration fit.
%
% (helper)
draw_subslots( Start, Duration, MeanLightDuration, StdDevLightDur,
			   MinNoLightDur, MaxNoLightDur, AccSlots ) ->

	LightDur = random_utils:get_positive_integer_gaussian_value(
		MeanLightDuration, StdDevLightDur ),

	NoLightDur = random_utils:get_uniform_value( MinNoLightDur,
												 MaxNoLightDur ),

	SubDur = LightDur + NoLightDur,

	NewDuration = Duration - SubDur,

	case NewDuration > 0 of

		true ->
			% Then we can insert at least one on/off subslot:
			NextStop = time_utils:offset_time( Start, LightDur ),
			NewAccSlots = [ { Start, NextStop } | AccSlots ],
			NewStart = time_utils:offset_time( Start, SubDur ),
			draw_subslots( NewStart, NewDuration, MeanLightDuration,
				StdDevLightDur, MinNoLightDur, MaxNoLightDur, NewAccSlots );

		false ->
			% Then with a last one bridging the gap, we stop introducing
			% subslots:
			%
			Stop = time_utils:offset_time( Start, Duration ),
			[ { Start, Stop } | AccSlots ]

	end.



-doc """
Applies, from the current moment, the intra-day presence program.

Defined for reuse (at creation or afterwards, if toggling this service as a
whole, updating presence simulations, etc.).
""".
-spec apply_presence_simulation( wooper:state() ) -> wooper:state().
apply_presence_simulation( State ) ->

	case ?getAttr(presence_simulation_enabled) of

		true ->
			PscTable = ?getAttr(presence_table),

			case table:values( PscTable ) of

				[] ->
					cond_utils:if_defined( us_main_debug_presence_simulation,
						send_psc_trace( debug, "(no specific presence "
							"simulation to apply)", State ) ),
					State;

				PscSims ->
					update_presence_simulations( PscSims, State )

			end;

		false ->
			State

	end.



-doc """
Updates, in turn and if necessary, from scratch, the specified presence
simulations.

Defined for reuse.
""".
-spec update_presence_simulations( [ presence_simulation() ],
								   wooper:state() ) -> wooper:state().
update_presence_simulations( PscSims, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( notice, "Updating ~B presence simulation(s) now.",
			[ length( PscSims ) ], State ) ),

	% Keep only relevant information (if any):
	CleanedMaybeCelestialInfo = case ?getAttr(celestial_info) of

		undefined ->
			undefined;

		CelestialInfo={ Date, _Timing } ->
			case erlang:date() of

				Date ->
					CelestialInfo;

				% Outdated:
				_ ->
					undefined

			end

	end,

	update_presence_simulations( PscSims, _CurrentTime=erlang:time(),
		CleanedMaybeCelestialInfo, _EmptyTable=table:new(), State ).



-doc """
Updates, in turn and if necessary, from scratch, the specified presence
simulations.

If defined, the celestial times are expected to be correct here.
""".
-spec update_presence_simulations( [ presence_simulation() ], time(),
		option( celestial_info() ), presence_table(), wooper:state() ) ->
			wooper:state().
% No more presence simulation:
update_presence_simulations( _PscSims=[], _CurrentTime, MaybeCelestialInfo,
							 PscTable, State ) ->
	setAttributes( State, [ { presence_table, PscTable },
							{ celestial_info, MaybeCelestialInfo } ] );

% Disabled presence:
update_presence_simulations( _PscSims=[
		PscSim=#presence_simulation{ id=Id,
									 enabled=false } | T ], CurrentTime,
							 MaybeCelestialInfo, PscTable, State ) ->

	% Nothing to be done if disabled:
	NewPscTable = table:add_new_entry( _K=Id, PscSim, PscTable ),

	update_presence_simulations( T, CurrentTime, MaybeCelestialInfo,
								 NewPscTable, State );

% Thus enabled:
update_presence_simulations(
		_PscSims=[ PscSim=#presence_simulation{ id=Id } | T ], CurrentTime,
		MaybeCelestialInfo, PscTable, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( info, "Managing presence simulation ~ts",
			[ presence_simulation_to_string( PscSim ) ], State ) ),

	% Probably not a maybe-CelestialInfo anymore:
	{ StartedPscSim, CelestialInfo } = manage_presence_simulation( PscSim,
		CurrentTime, MaybeCelestialInfo, State ),

	NewPscTable = table:add_new_entry( _K=Id, StartedPscSim, PscTable ),

	update_presence_simulations( T, CurrentTime, CelestialInfo, NewPscTable,
								 State ).



-doc """
Manages the specified presence simulation from scratch (callable at any time),
covering all cases, and acting accordingly iff necessary.

A large function, but defined only once.
""".
-spec manage_presence_simulation( presence_simulation(), time(),
		option( celestial_info() ), wooper:state() ) ->
			{ presence_simulation(), option( celestial_info() ) }.
% Not enabled:
manage_presence_simulation( PscSim=#presence_simulation{
		enabled=false,
		activated=IsActivated,
		presence_task_info=MaybePscTaskInfo },
							_CurrentTime, MaybeCelestialInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace( debug, "Simulated presence not enabled, "
			"hence not lighting, and no planned presence transition.",
			State ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),

	NoPlanPscSim = ensure_no_planned_presence_transition( UnlitPscSim,
		MaybePscTaskInfo, State ),

	{ NoPlanPscSim, MaybeCelestialInfo };


% From here, 'enabled' expected to be true.
%
% Splitting per program now, starting with always on, first with no smart
% lighting:
manage_presence_simulation( PscSim=#presence_simulation{
		activated=IsActivated,
		program=constant_presence,
		smart_lighting=false,
		presence_task_info=MaybePscTaskInfo },
							_CurrentTime, MaybeCelestialInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace( debug, "Enabled, no smart lighting requested, "
			"simulating a constant presence, hence always-on lighting "
			"and no planned presence transition.",
			State ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),

	NoPlanPscSim = ensure_no_planned_presence_transition( LitPscSim,
		MaybePscTaskInfo, State ),

	{ NoPlanPscSim, MaybeCelestialInfo };

% Now still with always on, but with smart lighting:
manage_presence_simulation( PscSim=#presence_simulation{
		activated=IsActivated,
		program=constant_presence,
		smart_lighting=true,
		presence_task_info=MaybePscTaskInfo },
							CurrentTime, MaybeCelestialInfo, State ) ->

	case get_celestial_info( MaybeCelestialInfo, State ) of

		CI={ _TodayDate, constant_daylight } ->
			% Naturally lighted, no lighting then:
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace( debug, "Enabled, smart lighting requested, "
					"simulating a constant presence yet in constant daylight,"
					"hence not lighting and no planned presence transition.",
					State ) ),

			UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
			NoPlanPscSim = ensure_no_planned_presence_transition( UnlitPscSim,
				MaybePscTaskInfo, State ),
			{ NoPlanPscSim, CI };


		CI={ _TodayDate, constant_darkness } ->
			% No natural lighting, so lighting needed:
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace( debug, "Enabled, smart lighting requested, "
					"simulating a constant presence and in constant darkness,"
					"hence lighting with no planned presence transition.",
					State ) ),

			LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
			NoPlanPscSim = ensure_no_planned_presence_transition( LitPscSim,
				MaybePscTaskInfo, State ),
			{ NoPlanPscSim, CI };


		CI={ _TodayDate, { MaybeDawnTime, MaybeDuskTime } } ->

			NewPscSim = case MaybeDawnTime of

				undefined ->
					% No dawn, thus supposing constant darkness, hence lighting
					% needed and no switching off planned:
					%
					cond_utils:if_defined( us_main_debug_presence_simulation,
						send_psc_trace( debug, "Enabled, smart lighting "
							"requested, simulating a constant presence, "
							"no dawn thus in constant darkness, "
							"hence lighting with no planned presence "
							"transition.", State ) ),
					LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
					ensure_no_planned_presence_transition( LitPscSim,
						MaybePscTaskInfo, State );

				DawnTime when CurrentTime > DawnTime ->
					% We are after the dawn; but before or after any dusk?
					case MaybeDuskTime of

						undefined ->
							% No dusk, hence with light at least until the end
							% of day:
							%
							cond_utils:if_defined(
								us_main_debug_presence_simulation,
								send_psc_trace( debug, "Enabled, smart lighting"
									" requested, simulating a constant "
									"presence, being after dawn and with no "
									"dusk, thus with light at least until "
									"the end of the day, hence not lighting and"
									" no planned presence transition.",
									State ) ),

							UnlitPscSim = ensure_not_lighting( PscSim,
								IsActivated, State ),

							ensure_no_planned_presence_transition( UnlitPscSim,
								MaybePscTaskInfo, State );


						DuskTime when CurrentTime > DuskTime ->
							% Already after dusk; lighting needed from now and
							% till the end of day:
							%
							cond_utils:if_defined(
								us_main_debug_presence_simulation,
								send_psc_trace( debug, "Enabled, smart lighting"
									" requested, simulating a constant "
									"presence, already after dusk, hence "
									"lighting needed from now and till "
									"the end of the day.", State ) ),

							LitPscSim = ensure_lighting( PscSim, IsActivated,
														 State ),

							ensure_no_planned_presence_transition(
								LitPscSim, MaybePscTaskInfo, State );


						% Hence CurrentTime <= DuskTime:
						DuskTime ->
							% Between dawn and dusk, so no lighting needed until
							% dusk:
							%
							cond_utils:if_defined(
								us_main_debug_presence_simulation,
								send_psc_trace( debug, "Enabled, smart lighting"
									" requested, simulating a constant "
									"presence, between dawn and dusk, so no "
									"lighting needed until dusk.", State ) ),

							UnlitPscSim = ensure_not_lighting( PscSim,
								IsActivated, State ),

							ensure_planned_presence_transition( UnlitPscSim,
								DuskTime, MaybePscTaskInfo, State )

					end

			end,

			{ NewPscSim, CI }

	end;


% Now always off, the easiest case:
manage_presence_simulation( PscSim=#presence_simulation{
		activated=IsActivated,
		program=constant_absence,
		% Does not matter: smart_lighting
		presence_task_info=MaybePscTaskInfo },
							_CurrentTime, MaybeCelestialInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace( debug, "Enabled yet in constant-absence simulation "
			"program, hence not lighting and not planning any presence "
			"transition.",
			State ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),

	NoPlanPscSim = ensure_no_planned_presence_transition( UnlitPscSim,
		MaybePscTaskInfo, State ),

	{ NoPlanPscSim, MaybeCelestialInfo };


% Now a slot-based program; the most complex case, yet with no smart lighting
% first:
%
manage_presence_simulation( PscSim=#presence_simulation{
		activated=IsActivated,
		program=Slots,
		smart_lighting=false,
		presence_task_info=MaybePscTaskInfo },
							CurrentTime, MaybeCelestialInfo, State ) ->

	NewPscSim = case get_programmed_presence( Slots, CurrentTime ) of

		% For good, today:
		always_present ->
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace( debug, "Enabled, no smart lighting requested, "
					"simulating a constant presence from now, hence lighting "
					"and not planning any presence transition.", State ) ),

			LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
			ensure_no_planned_presence_transition( LitPscSim,
				MaybePscTaskInfo, State );

		always_absent ->
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace( debug, "Enabled, no smart lighting requested "
					"simulating a constant absence from now, hence "
					"not lighting and not planning any presence transition.",
					State ) ),

			UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
			ensure_no_planned_presence_transition( UnlitPscSim,
				MaybePscTaskInfo, State );

		{ present_until, AbsStartTime } ->
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Enabled, no smart lighting "
					"requested, simulating a presence until ~ts, hence "
					"lighting and planning a presence transition then.",
					[ time_utils:time_to_string( AbsStartTime ) ],
					State ) ),

			LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
			ensure_planned_presence_transition( LitPscSim, AbsStartTime,
				MaybePscTaskInfo, State );

		{ absent_until, PresStartTime } ->
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Enabled, no smart lighting "
					"requested, simulating an absence until ~ts, hence "
					"not lighting and planning a presence transition then.",
					[ time_utils:time_to_string( PresStartTime ) ],
					State ) ),

			UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
			ensure_planned_presence_transition( UnlitPscSim, PresStartTime,
				MaybePscTaskInfo, State )

	end,

	{ NewPscSim, MaybeCelestialInfo };


% The real most complex case: slots with smart lighting.
manage_presence_simulation( PscSim=#presence_simulation{
		activated=IsActivated,
		program=Slots,
		% Here smart_lighting is true
		presence_task_info=MaybePscTaskInfo },
							CurrentTime, MaybeCelestialInfo, State ) ->

	CelestialInfo = { _Date, { MaybeDawnTime, MaybeDuskTime } } =
		get_celestial_info( MaybeCelestialInfo, State ),

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Getting programmed presence for "
			"slots ~ts.", [ text_utils:strings_to_string(
				[ slot_to_string( S ) || S <- Slots ] ) ], State ) ),

	NewPscSim = case get_programmed_presence( Slots, CurrentTime ) of

		% For good, today, hence light always needed:
		always_present ->
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace( debug, "Enabled, smart lighting requested, "
					"simulating a constant presence from now, hence lighting "
					"and not planning any presence transition.", State ) ),

			ensure_constant_light( CurrentTime, MaybeDawnTime, MaybeDuskTime,
				PscSim, IsActivated, MaybePscTaskInfo, State );

		always_absent ->
			% Easy case:
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace( debug, "Enabled, smart lighting requested, "
					"simulating a constant absence from now, hence not "
					"lighting and not planning any presence transition.",
					State ) ),

			UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
			ensure_no_planned_presence_transition( UnlitPscSim,
												   MaybePscTaskInfo, State );

		{ present_until, AbsStart } ->
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Enabled, smart lighting requested, "
					"simulating a presence until ~ts, hence lighting "
					"and planning a presence transition.",
					[ time_utils:time_to_string( AbsStart ) ], State ) ),

			ensure_light_until( AbsStart, CurrentTime, MaybeDawnTime,
				MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State );

		{ absent_until, PresStart } ->
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Enabled, smart lighting requested, "
					"simulating a presence from ~ts, hence not lighting "
					"and planning a presence transition.",
					[ time_utils:time_to_string( PresStart ) ], State ) ),

			ensure_light_from( PresStart, CurrentTime, MaybeDawnTime,
				MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State )

	end,

	{ NewPscSim, CelestialInfo }.




-doc """
Returns whether, for the specified time, a presence shall be simulated, and what
is the transition (if any) to plan next this day.
""".
-spec get_programmed_presence( [ presence_slot() ], time() ) ->
		'always_present' | 'always_absent'
	| { 'present_until', time() } | { 'absent_until', time() }.
get_programmed_presence( _Slots=[], _Time ) ->
	% No more presence slot, hence:
	always_absent;

% We must include the equal case in the tests, as by design we are awoken most
% of the time at the exact threshold millisecond:
%
get_programmed_presence( _Slots=[ { _StartPscTime, StopPscTime } | T ], Time )
								when Time >= StopPscTime ->
	% Time of interest not reached in the list, continuing:
	get_programmed_presence( T, Time );

% We are before a slot:
get_programmed_presence( _Slots=[ { StartPscTime, _StopPscTime } | _T ], Time )
								when Time < StartPscTime ->
	{ absent_until, StartPscTime };

% We are in a slot:
get_programmed_presence( _Slots=[ { _StartPscTime, StopPscTime } | _T ],
						 _Time ) ->
	% Implicit: StartPscTime <= Time < StopPscTime; comparing to next midnight:
	case StopPscTime < ?last_time of

		true ->
			{ present_until, StopPscTime };

		false ->
			always_present

	end.



-doc "Ensures that the lighting is on for this presence simulation.".
-spec ensure_lighting( presence_simulation(), boolean(), wooper:state() ) ->
			presence_simulation().
ensure_lighting( PscSim, _IsActivated=true, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug,
			"Presence simulation #~B already activated, nothing to do.",
			[ PscSim#presence_simulation.id ], State ),
		basic_utils:ignore_unused( State ) ),

	PscSim;

ensure_lighting( PscSim=#presence_simulation{
							activation_telegrams=ActivationTelegrams },
				 _IsActivated=false, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Activating presence simulation #~B.",
			[ PscSim#presence_simulation.id ], State ) ),

	send_telegram_pair( ActivationTelegrams, State ),

	PscSim#presence_simulation{ activated=true }.



-doc "Ensures that the lighting is off for this presence simulation.".
-spec ensure_not_lighting( presence_simulation(), boolean(), wooper:state() ) ->
			presence_simulation().
ensure_not_lighting( PscSim=#presence_simulation{
								deactivation_telegrams=DeactivationTelegrams },
					 _IsActivated=true, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Deactivating presence simulation #~B.",
				[ PscSim#presence_simulation.id ], State ) ),

	send_telegram_pair( DeactivationTelegrams, State ),

	PscSim#presence_simulation{ activated=false };


ensure_not_lighting( PscSim, _IsActivated=false, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug,
			"Presence simulation #~B already deactivated, nothing to do.",
			[ PscSim#presence_simulation.id ], State ),
		basic_utils:ignore_unused( State ) ),

	PscSim.



-doc """
Ensures that the specified presence task (and only it) is planned for an update
at the specified time.
""".
-spec ensure_planned_presence_transition( presence_simulation(), time(),
			option( task_info() ), wooper:state() ) -> presence_simulation().
% Plan if not already planned:
ensure_planned_presence_transition( PscSim=#presence_simulation{ id=PscId },
		PlannedTime, _MaybePscTaskInfo=undefined, State ) ->

	TaskCmd = { updatePresenceSimulation, [ PscId ] },

	PlannedTimestamp = { date(), PlannedTime },

	?getAttr(scheduler_pid) !
		{ registerOneshotTask, [ TaskCmd, PlannedTimestamp ], self() },

	receive

		{ wooper_result, task_done } ->
			% Better than waiting for ever:
			throw( unexpected_done_task );

		{ wooper_result, { task_registered, TaskId } } ->
			TaskInfo = { TaskId, PlannedTime },
			PscSim#presence_simulation{ presence_task_info=TaskInfo }

	end;

% Here such planning exists (as task info) and is already at the correct time:
ensure_planned_presence_transition( PscSim, PlannedTime,
		_PscTaskInfo={ _PscTaskId, PlannedTime }, _State ) ->
	PscSim;

% Planning exists, yet with different times; correcting it:
ensure_planned_presence_transition( PscSim, PlannedTime,
		_PrevPscTaskInfo={ PrevPscTaskId, PrevPlannedTime }, State ) ->

	% A priori may be legit:
	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( notice, "Switching, for presence simulation #~B, "
			"planned time from ~ts to ~ts.", [ PscSim#presence_simulation.id,
				time_utils:time_to_string( PrevPlannedTime ),
				time_utils:time_to_string( PlannedTime ) ], State ),
		basic_utils:ignore_unused( [ State, PrevPlannedTime ] ) ),

	% Clearer:
	%ClearedPsim = ensure_no_planned_presence_transition( PscSim,
	%                           PrevPscTaskInfo, State ),

	?getAttr(scheduler_pid) ! { unregisterTaskAsync, [ PrevPscTaskId ] },

	% Force rescheduling:
	ensure_planned_presence_transition( PscSim, PlannedTime,
		_MaybePscTaskInfo=undefined, State ).



-doc "Ensures that there is no planned presence task.".
-spec ensure_no_planned_presence_transition( presence_simulation(),
				option( task_id() ), wooper:state() ) -> presence_simulation().
ensure_no_planned_presence_transition( PscSim, _MaybePscTaskInfo=undefined,
									   _State ) ->
	PscSim;

ensure_no_planned_presence_transition( PscSim, { PscTaskId, _TaskTime },
									   State ) ->
	?getAttr(scheduler_pid) ! { unregisterTaskAsync, [ PscTaskId ] },
	PscSim#presence_simulation{ presence_task_info=undefined }.



-doc """
Ensures that constant light is available from the specified current time: lights
iff no daylight.

Note that we just plan the next transition, not any next one that could be
determined here.
""".
-spec ensure_constant_light( time(), option( time() ), option( time() ),
	presence_simulation(), boolean(), option( task_info() ), wooper:state() ) ->
			presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence constant lighting
% needed:
ensure_constant_light( _CurrentTime, _MaybeDawnTime=undefined,
		_MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace( debug, "Constant light requested, whereas no dawn, "
			"supposedly no dusk either, constant darkness, hence constant "
			"lighting needed.", State ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( LitPscSim, MaybePscTaskInfo, State );

% A dawn but no dusk: lights (only) until that dawn.
%
% Before dawn here:
ensure_constant_light( CurrentTime, DawnTime, _MaybeDuskTime=undefined, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Constant light requested, we are before "
			"dawn, to happen at ~ts (and there is no dusk): lights (only) "
			"until that dawn.",
			[ time_utils:time_to_string( DawnTime ) ], State ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
										State );

% After dawn here (implicit: CurrentTime >= DawnTime):
ensure_constant_light( _CurrentTime, DawnTime, _MaybeDuskTime=undefined,
		PscSim, IsActivated, MaybePscTaskInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Constant light requested, we are "
			"after dawn (was at ~ts) and there is no dusk: "
			"not lighting from now.",
			[ time_utils:time_to_string( DawnTime ) ], State ),
		basic_utils:ignore_unused( DawnTime ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( UnlitPscSim, MaybePscTaskInfo,
										   State );

% General case: a dawn and a dusk.
%
% Here before dawn, still in darkness, thus lighting until dawn:
ensure_constant_light( CurrentTime, DawnTime, _DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Constant light requested, we are before "
			"dawn (at ~ts), still in darkness, thus lighting until dawn.",
			[ time_utils:time_to_string( DawnTime ) ], State ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
										State );

% Here in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting until
% dusk:
%
ensure_constant_light( CurrentTime, _DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->

	% In daylight, thus no lighting until dusk:
	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Constant light requested, we are after "
			"dawn but before dusk (at ~ts), thus in daylight, thus not "
			"lighting until dusk.",
			[ time_utils:time_to_string( DuskTime ) ], State ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
										State );

% Here are already past dusk (CurrentTime >= DuskTime), hence in darkness, thus
% lighting from now on:
%
ensure_constant_light( _CurrentTime, _DawnTime, DuskTime, PscSim,
					   IsActivated, MaybePscTaskInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Constant light requested, we are already "
			"after dusk (was at ~ts), thus in darkness, thus lighting "
			"from now on.",
			[ time_utils:time_to_string( DuskTime ) ], State ),
		basic_utils:ignore_unused( DuskTime ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( LitPscSim, MaybePscTaskInfo, State ).




-doc """
Ensures that light is available until the specified stop time, expected to be in
the future of the specified current time.

Note that we just plan the next transition, not any next one that could be
determined here.
""".
-spec ensure_light_until( time(), time(), option( time() ), option( time() ),
	presence_simulation(), boolean(), option( task_info() ), wooper:state() ) ->
			presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence lighting needed
% until stop time:
ensure_light_until( StopTime, _CurrentTime, _MaybeDawnTime=undefined,
		_MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested until ~ts, no dawn, "
			"supposedly no dusk either, constant darkness, hence "
			"lighting needed until that time.",
			[ time_utils:time_to_string( StopTime ) ], State ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, StopTime, MaybePscTaskInfo,
										State );

% A dawn but no dusk: lights (only) until that dawn.
%
% Before dawn here, stop does not matter yet:
ensure_light_until( StopTime, CurrentTime, DawnTime, _MaybeDuskTime=undefined,
		PscSim, IsActivated, MaybePscTaskInfo, State )
								when CurrentTime < DawnTime ->

	StopLightingTime = erlang:min( StopTime, DawnTime ),

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested until ~ts, dawn at ~ts, "
			"so lighting until ~ts.",
			[ time_utils:time_to_string( StopTime ),
			  time_utils:time_to_string( DawnTime ),
			  time_utils:time_to_string( StopLightingTime ) ], State ),
		basic_utils:ignore_unused( [ State, StopLightingTime ] ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
										State );

% After dawn here (implicit: CurrentTime >= DawnTime), no dusk, hence no
% lighting needed:
%
ensure_light_until( _StopTime, _CurrentTime, _DawnTime,
		_MaybeDuskTime=undefined, PscSim, IsActivated, MaybePscTaskInfo,
		State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace( debug, "Light requested until a stop time, "
			"we are already after dawn, no dusk, hence no lighting needed.",
			State ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( UnlitPscSim, MaybePscTaskInfo,
										   State );

% General case: a dawn and a dusk.
%
% Here we are before dawn, still in darkness, thus lighting until first to come
% between dawn and stop:
%
ensure_light_until( StopTime, CurrentTime, DawnTime, _DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->

	StopLightingTime = erlang:min( StopTime, DawnTime ),

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested until ~ts, "
			"we are before dawn (at ~ts), hence lighting until ~ts.",
			[ time_utils:time_to_string( StopTime ),
			  time_utils:time_to_string( DawnTime ),
			  time_utils:time_to_string( StopLightingTime ) ], State ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, StopLightingTime,
										MaybePscTaskInfo, State );

% Here we are in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting
% until dusk (and stop time does not matter):
%
ensure_light_until( _StopTime, CurrentTime, _DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested until a stop time, yet "
			"we are between dawn and dusk (~ts), hence no lighting needed.",
			[ time_utils:time_to_string( DuskTime ) ], State ) ),

	% In daylight, thus no lighting until dusk:
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
										State );

% Here we are already past dusk (CurrentTime >= DuskTime), hence in darkness,
% thus lighting from now on to the stop time (by design in the future):
%
ensure_light_until( StopTime, _CurrentTime, _DawnTime, _DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested until ~ts, "
			"we are after dusk, hence lighting before stop time.",
			[ time_utils:time_to_string( StopTime ) ], State ) ),

	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, StopTime, MaybePscTaskInfo,
										State ).



-doc """
Ensures that light is available from the specified start time, expected to be in
the future of the specified current time.

Note that we just plan the next transition, not any next one that could be
determined here.
""".
-spec ensure_light_from( time(), time(), option( time() ), option( time() ),
	presence_simulation(), boolean(), option( task_info() ), wooper:state() ) ->
			presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence lighting (only)
% needed from that start time:
ensure_light_from( StartTime, _CurrentTime, _MaybeDawnTime=undefined,
		_MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested from ~ts, "
			"lighting (only) from then, as there is no dawn.",
			[ time_utils:time_to_string( StartTime ) ], State ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, StartTime,
										MaybePscTaskInfo, State );

% A dawn but no dusk: lights (only) until that dawn happens (if not already).
%
% As always, we are still before start time, so no lighting, which is to happen
% if start time happens before dawn:
%
ensure_light_from( StartTime, CurrentTime, DawnTime, _MaybeDuskTime=undefined,
		PscSim, IsActivated, MaybePscTaskInfo, State )
								when CurrentTime < DawnTime ->
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	case StartTime < DawnTime of

		true ->
			% A bit of lighting needed from StartTime (till dawn):
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Light requested from ~ts, "
					"so will light from then, and before dawn at ~ts "
					"(and there is no dusk).",
					[ time_utils:time_to_string( StartTime ),
					  time_utils:time_to_string( DawnTime ) ], State ) ),

			ensure_planned_presence_transition( UnlitPscSim, StartTime,
												MaybePscTaskInfo, State );

		false ->
			% Already daylight when starting, no dusk today, no lighting to
			% plan:
			%
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Light requested from ~ts, "
					"which is to happen after dawn (at ~ts), "
					"hence no lighting to do or plan (and there is no dusk).",
					[ time_utils:time_to_string( StartTime ),
					  time_utils:time_to_string( DawnTime ) ], State ) ),

			ensure_no_planned_presence_transition( UnlitPscSim,
												   MaybePscTaskInfo, State )

	end;

% We are after dawn here (implicit: CurrentTime >= DawnTime), no dusk, hence no
% lighting ever needed:
%
ensure_light_from( StartTime, _CurrentTime, DawnTime,
		_MaybeDuskTime=undefined, PscSim, IsActivated, MaybePscTaskInfo,
		State ) ->
	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested from ~ts, "
			"we are already after dawn (at ~ts) and there is no dusk, "
			"hence no lighting to do or plan.",
			[ time_utils:time_to_string( StartTime ),
			  time_utils:time_to_string( DawnTime ) ], State ),
		basic_utils:ignore_unused( [ StartTime, DawnTime ] ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( UnlitPscSim, MaybePscTaskInfo,
										   State );

% General case: a dawn and a dusk.
%
% Here we are before dawn, still in darkness, thus lighting only to be triggered
% at start time if it is to happen before dawn:
%
ensure_light_from( StartTime, CurrentTime, DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	case StartTime < DawnTime of

		true ->
			% A bit of lighting needed then between start and dawn:
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Light requested from ~ts, "
					"so will light from then, and before dawn at ~ts.",
					[ time_utils:time_to_string( StartTime ),
					  time_utils:time_to_string( DawnTime ) ], State ) ),

			ensure_planned_presence_transition( UnlitPscSim, StartTime,
												MaybePscTaskInfo, State );

		false ->
			% Start to be done already in daylight (StartTime>=DawnTime), next
			% event is switching on at the latest between dusk and the start
			% time:
			%
			LitTime = erlang:max( DuskTime, StartTime ),
			cond_utils:if_defined( us_main_debug_presence_simulation,
				send_psc_trace_fmt( debug, "Light requested from ~ts, "
					"which is to happen after dawn (at ~ts), "
					"hence no lighting to be done currently.",
					[ time_utils:time_to_string( StartTime ),
					  time_utils:time_to_string( DawnTime ) ], State ) ),

			ensure_planned_presence_transition( UnlitPscSim, LitTime,
												MaybePscTaskInfo, State )

	end;

% Here in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting until
% dusk (and start time does not matter at this point):
%
ensure_light_from( _StartTime, CurrentTime, _DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->

	% In daylight, thus no lighting until at least dusk:
	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested from a start time, yet "
			"we are between dawn and dusk (~ts), hence no lighting needed.",
			[ time_utils:time_to_string( DuskTime ) ], State ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
										State );

% Here already past dusk (CurrentTime >= DuskTime), hence in darkness, thus
% will light up at the start time (by design in the future):
%
ensure_light_from( StartTime, _CurrentTime, _DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) ->

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Light requested from start time ~ts, "
			"which will require lighting, as we are already after dusk (~ts).",
			[ time_utils:time_to_string( StartTime),
			  time_utils:time_to_string( DuskTime ) ], State ),
		basic_utils:ignore_unused( DuskTime ) ),

	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, StartTime,
										MaybePscTaskInfo, State ).



-doc """
Returns the relevent celestial times, once computed if necessary.

If defined, celestial times are expected to be correct (deprecated times shall
have been set to 'undefined').
""".
-spec get_celestial_info( option( celestial_info() ), wooper:state() ) ->
								celestial_info().
get_celestial_info( _MaybeCelestialTimes=undefined, State ) ->
	resolve_logical_milestones( ?getAttr(server_location), State );

get_celestial_info( CelestialTimes, _State ) ->
	CelestialTimes.



-doc """
Computes (if possible) the actual dawn/dusk times.

(helper)
""".
-spec resolve_logical_milestones( option( position() ), wooper:state() ) ->
									celestial_info().
resolve_logical_milestones( _MaybeSrvLoc=undefined, State ) ->

	DefaultDawnTime = { 8, 15, 0 },
	DefaultDuskTime = { 19, 15, 0 },

	?warning_fmt( "No server location specified, logical milestones cannot be "
		"determined; using fixed, default deadlines: "
		"~ts for dawn, ~ts for dusk. ",
		[ time_utils:time_to_string( DefaultDawnTime ),
		  time_utils:time_to_string( DefaultDuskTime ) ] ),

	{ erlang:date(), { DefaultDawnTime, DefaultDuskTime } };

resolve_logical_milestones( SrvLoc={ LatDegrees, LongDegrees }, State ) ->

	Date = erlang:date(),

	LatRadians = math_utils:degrees_to_radians( LatDegrees ),
	% LongRadians = math_utils:degrees_to_radians( LongDegrees ),

	% Refer to https://www.astrolabe-science.fr/duree-du-jour-et-latitude/; we
	% compute based on universal, UTC time before converting to local time:

	AngleRad = math_utils:degrees_to_radians( 23.4 ),

	DayInYear = time_utils:get_day_in_year( Date ),

	Degrees = 360 * ( DayInYear - 81 ) / 365.2422,

	DeclinationDegrees = math_utils:radians_to_degrees(
		math:asin( math:sin( AngleRad )
			* math:sin( math_utils:degrees_to_radians( Degrees ) ) ) ),

	{ RiseHour, SetHour } =
		get_sun_rise_and_set_times( LatRadians, DeclinationDegrees ),

	% Correction regarding longitude:
	%
	% (4 minutes per degree, then converted in decimal hours)
	%
	LongCorrection = LongDegrees * 4 / 60,

	LongCorrectedRiseHour = RiseHour - LongCorrection,
	LongCorrectedSetHour  = SetHour  - LongCorrection,

	% Clamping leap years:
	DayIndex = case DayInYear of

		366 ->
			365;

		_ ->
			DayInYear

	end,

	% Applying the time equation:

	% Index start at 1:
	TECorrection =
		table:get_value( DayIndex, ?getAttr(time_equation_table) ) / 60,

	TECorrectedRiseHour = LongCorrectedRiseHour + TECorrection,
	TECorrectedSetHour  = LongCorrectedSetHour  + TECorrection,

	UTCDawnTime = from_decimal_hour( TECorrectedRiseHour ),
	UTCDuskTime = from_decimal_hour( TECorrectedSetHour ),


	UTCDawnTimestamp = { Date, UTCDawnTime },

	% Both for time zone and Daylight Saving Time:
	LocalDawnTimestamp = { LocalDawnDate, LocalDawnTime } =
		calendar:universal_time_to_local_time( UTCDawnTimestamp ),

	% If ever dates where to change (always possible if distant from Greenwich):
	LocalDawnDate =:= Date orelse
		?error_fmt( "Unexpected date change when transforming UTC "
			"to local dawn time: ~ts converts into ~ts.",
			[ time_utils:timestamp_to_string( UTCDawnTimestamp ),
			  time_utils:timestamp_to_string( LocalDawnTimestamp ) ] ),


	UTCDuskTimestamp = { Date, UTCDuskTime },

	LocalDuskTimestamp = { LocalDuskDate, LocalDuskTime } =
		calendar:universal_time_to_local_time( UTCDuskTimestamp ),

	LocalDuskDate =:= Date orelse
		?error_fmt( "Unexpected date change when transforming UTC "
			"to local dusk time: ~ts converts into ~ts.",
			[ time_utils:timestamp_to_string( UTCDuskTimestamp ),
			  time_utils:timestamp_to_string( LocalDuskTimestamp ) ] ),

	% To account for dim daylight (forcing to switch on longer, in order to
	% compensate for insufficient light) - even if atmospheric refraction will
	% help a bit - the following duration margins, expressed in seconds, apply:

	EnoughLightAfterDawnMargin  = 15*60,
	EnoughLightBeforeDuskMargin = 15*60,

	% To end the lighting a little later than the actual forecast dawn:
	RetainedDawnTime = time_utils:offset_time( LocalDawnTime,
											   EnoughLightAfterDawnMargin ),

	% To start the lighting a little earlier than the actual forecast dusk:
	RetainedDuskTime = time_utils:offset_time( LocalDuskTime,
											   -EnoughLightBeforeDuskMargin ),

	send_psc_trace_fmt( info,
		"For the specified server location (~ts) and date, "
		"computed following deadlines this day (with some light "
		"margins included): ~ts for dawn and ~ts for dusk.~n~n"
		"Raw celestial timestamps were indeed:~n"
		" - in UTC time: ~ts for dawn and ~ts for dusk~n"
		" - in local time (with time zone and DST, before applying "
		"margins): ~ts for dawn and ~ts for dusk",
		[ unit_utils:position_to_string( SrvLoc ),
		  time_utils:time_to_string( RetainedDawnTime ),
		  time_utils:time_to_string( RetainedDuskTime ),
		  time_utils:timestamp_to_string( UTCDawnTimestamp ),
		  time_utils:timestamp_to_string( UTCDuskTimestamp ),
		  time_utils:timestamp_to_string( LocalDawnTimestamp ),
		  time_utils:timestamp_to_string( LocalDuskTimestamp ) ],
		State ),

	{ Date, { RetainedDawnTime, RetainedDuskTime } }.



% Not unit_utils:latitude(), as would be degrees:
-spec get_sun_rise_and_set_times( radians(), declination() ) ->
										{ float(), float() }.
get_sun_rise_and_set_times( LatRadians, DeclinationDegrees ) ->

	DeclinationRadians = math_utils:degrees_to_radians( DeclinationDegrees ),

	% Decimal hour:
	ZeroHourDegrees = math_utils:radians_to_degrees( math:acos(
		-math:tan( LatRadians ) * math:tan( DeclinationRadians ) ) ),

	ZeroHourFrac = ZeroHourDegrees / 15,

	{ 12 - ZeroHourFrac, 12 + ZeroHourFrac }.



-doc "Converts a decimal hour to hours and minutes.".
-spec from_decimal_hour( float() ) -> time().
from_decimal_hour( DecHour ) ->
	Hours = math_utils:floor( DecHour ),
	DecMinutes = 60 * ( DecHour - Hours ),
	Minutes = math_utils:floor( DecMinutes ),

	% Not round/1, as could lead to (improper) 60 seconds:
	Seconds = math_utils:floor( 60 * ( DecMinutes - Minutes ) ),

	{ Hours, Minutes, Seconds }.



-doc "Sends (presumably reliably) the specified telegram pair.".
-spec send_telegram_pair( telegram_pair(), wooper:state() ) -> void().
send_telegram_pair( Telegrams, State ) ->

	OcSrvPid = ?getAttr(oc_srv_pid),

	SendCount = 1,
	%SendCount = 3,

	cond_utils:if_defined( us_main_debug_presence_simulation,
		?debug_fmt( "Sending telegram pair ~B times; "
			"for press: ~ts / for release: ~ts.",
			[ SendCount,
			  oceanic:telegram_to_hexastring( pair:first( Telegrams ) ),
			  oceanic:telegram_to_hexastring( pair:second( Telegrams ) ) ] ) ),

	% Despite all robustification efforts, regularly at least some actuators do
	% not trigger. So testing now multiple attempts (short of relying on actual
	% state feedback yet):
	%
	% (multiple sending are not such a problem, as we are using rockers, not
	% contact buttons that toggle, so the target operation is idempotent):
	%
	send_telegram_pair( Telegrams, SendCount, OcSrvPid ).



-doc "Sends the specified telegram pair the specified number of times.".
-spec send_telegram_pair( telegram_pair(), count(), oceanic_server_pid() ) ->
											void().
send_telegram_pair( _Telegrams, _Count=0, _OcSrvPid ) ->
	ok;

send_telegram_pair( Telegrams={ PressTelegram, ReleaseTelegram }, Count,
					OcSrvPid ) ->

	oceanic:send( PressTelegram, OcSrvPid ),

	% Always better to separate telegrams:
	timer:sleep( _ShortMs=200 ),

	oceanic:send( ReleaseTelegram, OcSrvPid ),

	timer:sleep( _LongerMs=1000 ),

	send_telegram_pair( Telegrams, Count-1, OcSrvPid ).



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug_fmt( "Deletion initiated, while state is: ~ts.",
				[ to_string( State ) ] ),

	case ?getAttr(oc_srv_pid) of

		undefined ->
			ok;

		OcSrvPid ->
			oceanic:synchronous_stop( OcSrvPid )

	end,

	?info( "Deleted." ),
	State.



% Method section, except for remote commands.


-doc """
Requests the specified presence simulation to be updated, typically after a
milestone, as planned through a scheduler.
""".
-spec updatePresenceSimulation( wooper:state(), presence_sim_id() ) ->
											oneway_return().
updatePresenceSimulation( State, PscId ) ->

	PscTable = ?getAttr(presence_table),

	PscSim = table:get_value( _K=PscId, PscTable ),

	cond_utils:if_defined( us_main_debug_presence_simulation,
		send_psc_trace_fmt( debug, "Requested to update ~ts.",
			[ presence_simulation_to_string( PscSim ) ], State ) ),

	% Newer scheduling possibly planned:
	{ UpdatedPscSim, MaybeCelestialInfo } = manage_presence_simulation( PscSim,
		erlang:time(), ?getAttr(celestial_info), State ),

	UpdatedPscTable = table:add_entry( PscId, UpdatedPscSim, PscTable ),

	UpdateState = setAttributes( State, [
		{ presence_table, UpdatedPscTable },
		{ celestial_info, MaybeCelestialInfo } ] ),

	wooper:return_state( UpdateState ).



-doc """
Called whenever having to update presence programs, typically at midnight for
this new day.

See also the midnight_task_id attribute.
""".
-spec updatePresencePrograms( wooper:state() ) -> oneway_return().
updatePresencePrograms( State ) ->

	?getAttr(oc_periodic_restart) andalso
		begin

			OcSrvPid = ?getAttr(oc_srv_pid),

			send_psc_trace_fmt( info, "Restarting the serial interface "
				"of the Oceanic server ~w.", [ OcSrvPid ], State ),

			oceanic:restart_serial_interface( OcSrvPid ),

			send_psc_trace( info, "Oceanic serial interface restarted.", State )

		end,

	PscTable = ?getAttr(presence_table),

	UpdatedState = case table:values( PscTable ) of

		[] ->
			case ?getAttr(midnight_task_id) of

				undefined ->
					send_psc_trace( error, "Triggered whereas no presence "
						"simulation is to update; moreover no midnight task "
						"is tracked.", State ),
					State;

				MidnightTaskId ->
					send_psc_trace_fmt( warning, "Requested to update presence "
						"programs whereas no presence simulation is registered;"
						" unscheduling these updates (task #~B).",
						[ MidnightTaskId ], State ),

					?getAttr(scheduler_pid) !
						{ unregisterTaskAsync, [ MidnightTaskId ] },

					setAttribute( State, midnight_task_id, undefined )

			end;

		PscSims ->
			% Force a reevaluation thereof, once any past presence task is
			% cleared:

			SchedPid = ?getAttr(scheduler_pid),

			ClearedPscSims = [ clear_any_presence_task( Psc, SchedPid, State )
									|| Psc <- PscSims ],

			update_presence_simulations( ClearedPscSims, State )

	end,

	wooper:return_state( UpdatedState ).



-doc """
Ensures that no past presence task lingers in the specified simulation.
""".
-spec clear_any_presence_task( presence_simulation(), scheduler_pid(),
							   wooper:state() ) -> presence_simulation().
clear_any_presence_task( Psc=#presence_simulation{
					presence_task_info={ TaskId, _Time } }, SchedPid, State ) ->
	?warning_fmt( "Clearing a task during the daily presence simulation "
		"update (abnormal) for ~ts.",
		[ presence_simulation_to_string( Psc ) ] ),

	SchedPid ! { unregisterTaskAsync, [ TaskId ] },
	Psc#presence_simulation{ presence_task_info=undefined };

% presence_task_info already set to 'undefined', as expected:
clear_any_presence_task( Psc, _SchedPid, _State ) ->
	Psc.




% Management of messages sent by the Oceanic server:


-doc """
Handles a device-related first detection of a configured device (typically a
sensor report) notified by the specified Oceanic server.
""".
-spec onEnoceanConfiguredDeviceFirstSeen( wooper:state(), device_event(),
		device_description(), oceanic_server_pid() ) -> oneway_return().
onEnoceanConfiguredDeviceFirstSeen( State, DeviceEvent, BinDevDesc, OcSrvPid )
										when is_tuple( DeviceEvent ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

	% Longer description when first seen:
	Msg = text_utils:format( "The device '~ts', declared in the configuration, "
		"has been detected for the first time, based on the following "
		"event: ~ts.~n~nFull device information: ~ts.",
		[ BinDevName, oceanic:device_event_to_string( DeviceEvent ),
		  BinDevDesc ] ),

	class_TraceEmitter:send_named_emitter( notice, State, Msg,
		get_trace_emitter_name_from( BinDevName ) ),

	RecState = record_new_device( DeviceEvent, State ),

	PresState = manage_presence_switching( DeviceEvent, RecState ),

	wooper:return_state( PresState );


onEnoceanConfiguredDeviceFirstSeen( State, OtherEvent, BinDevDesc, OcSrvPid ) ->

	?error_fmt( "Received an invalid first-seen device event (~p) "
		"from ~w for '~p', ignoring it.",
		[ OtherEvent, OcSrvPid, BinDevDesc ] ),

	wooper:const_return().



-doc """
Handles a device-related discovery of a device (typically a sensor report) that
was not in the configuration, as notified by the specified Oceanic server.
""".
-spec onEnoceanDeviceDiscovery( wooper:state(), device_event(),
		device_description(), oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceDiscovery( State, DeviceEvent, BinDevDesc, OcSrvPid )
								when is_tuple( DeviceEvent ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

	% Longer description at this discovery time:
	Msg = text_utils:format( "The device '~ts' has been discovered "
		"(detected yet not declared in the configuration), based on "
		"the following event: ~ts.~n~nFull device information: ~ts.",
		[ BinDevName, oceanic:device_event_to_string( DeviceEvent ),
		  BinDevDesc ] ),

	class_TraceEmitter:send_named_emitter( warning, State, Msg,
		get_trace_emitter_name_from( BinDevName ) ),

	RecState = record_new_device( DeviceEvent, State ),

	% If ever the device was not configured yet declared as presence-switching:
	PresState = manage_presence_switching( DeviceEvent, RecState ),

	wooper:return_state( PresState );


onEnoceanDeviceDiscovery( State, OtherEvent, BinDevDesc, OcSrvPid ) ->

	?error_fmt( "Received an invalid discovery device event (~p) "
		"from ~w for '~p', ignoring it.",
		[ OtherEvent, OcSrvPid, BinDevDesc ] ),

	wooper:const_return().




-doc """
Records the existence of a device not expected to be already known.

(helper)
""".
-spec record_new_device( device_event(), wooper:state() ) -> wooper:state().
record_new_device( DeviceEvent, State ) ->

	DevTable = ?getAttr(device_table),

	DevEurid = oceanic:get_source_eurid( DeviceEvent ),

	DevState = #device_state{
		eurid=DevEurid,
		name=oceanic:get_best_device_name_from( DeviceEvent ),
		eep_id=oceanic:get_maybe_eep( DeviceEvent ),
		initial_event=DeviceEvent,
		last_event=DeviceEvent,
		current_status=interpret_status( DeviceEvent ) },

	NewDevTable = table:add_entry( DevEurid, DevState, DevTable ),

	setAttribute( State, device_table, NewDevTable ).



-doc """
Returns the higher-level status that can be deduced from the specified event.
""".
-spec interpret_status( device_event() ) -> device_status().
interpret_status( _DeviceEvent=#single_input_contact_event{
										contact=ContactStatus } ) ->
	ContactStatus;

interpret_status( _DeviceEvent ) ->
	unknown.



-doc """
Handles a device-related event (typically a sensor report) notified by the
specified Oceanic server.

This device has been already discovered; tells whether this device was
considered lost until now.

This is by far the most frequent device-related message (once a
discovery-related message has been received first).
""".
-spec onEnoceanDeviceEvent( wooper:state(), device_event(), back_online_info(),
							oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceEvent( State, DeviceEvent, _BackOnlineInfo=undefined, OcSrvPid )
								when is_tuple( DeviceEvent ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	Msg = oceanic:device_event_to_short_string( DeviceEvent ),

	BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

	class_TraceEmitter:send_named_emitter( info, State, Msg,
		get_trace_emitter_name_from( BinDevName ) ),

	ProcessedState = process_device_event( DeviceEvent, State ),

	PresState = manage_presence_switching( DeviceEvent, ProcessedState ),

	wooper:return_state( PresState );


onEnoceanDeviceEvent( State, DeviceEvent, _BackOnlineInfo=BinDevDesc, OcSrvPid )
								when is_tuple( DeviceEvent ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

	Msg = text_utils:format( "The device '~ts', which was considered lost, "
		"is back online: ~ts.~n~nFull device information: ~ts.",
		[ BinDevName, oceanic:device_event_to_string( DeviceEvent ),
		  BinDevDesc ] ),

	class_TraceEmitter:send_named_emitter( notice, State, Msg,
		get_trace_emitter_name_from( BinDevName ) ),

	ProcessedState = process_device_event( DeviceEvent, State ),

	PresState = manage_presence_switching( DeviceEvent, ProcessedState ),

	wooper:return_state( PresState );


onEnoceanDeviceEvent( State, OtherEvent, _BackOnlineInfo, OcSrvPid ) ->

	?error_fmt( "Received an invalid device event (~p) from ~w, "
				"ignoring it.", [ OtherEvent, OcSrvPid ] ),

	wooper:const_return().



-doc """
Updates, based on the specified event, the stored state of the corresponding
device.
""".
-spec process_device_event( device_event(), wooper:state() ) -> wooper:state().
process_device_event( DeviceEvent, State ) ->

	DevEurid = oceanic:get_source_eurid( DeviceEvent ),

	NewStatus = interpret_status( DeviceEvent ),

	DevTable = ?getAttr(device_table),

	case table:lookup_entry( _K=DevEurid, DevTable ) of

		key_not_found ->
			?error_fmt( "No device found for ~ts, after event ~ts, "
						"which is therefore ignored.", [
					oceanic:eurid_to_string( DevEurid ),
					oceanic:device_event_to_string( DeviceEvent ) ] ),
			State;

		% No state change:
		{ value, #device_state{ current_status=NewStatus } } ->
			%?notice_fmt( "(still in status ~ts)", [ NewStatus ] ),
			State;

		% State change; currently not doing much besides tracing and recording:
		{ value, DevState=#device_state{ name=BinDevName,
										 current_status=PrevStatus } } ->

			Msg = text_utils:format( "State transition from ~ts to ~ts.",
									 [ PrevStatus, NewStatus ] ),

			class_TraceEmitter:send_named_emitter( notice, State, Msg,
				get_trace_emitter_name_from( BinDevName ) ),

			NewDevState =
				DevState#device_state{ current_status=NewStatus },

			NewDevTable = table:add_entry( DevEurid, NewDevState, DevTable ),

			setAttribute( State, device_table, NewDevTable )

	end.



-doc """
Handles a teach-in event of a device, as notified by the specified Oceanic
server.
""".
-spec onEnoceanDeviceTeachIn( wooper:state(), device_event(),
		device_description(), oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceTeachIn( State, DeviceEvent, BinDevDesc, OcSrvPid )
								when is_tuple( DeviceEvent ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

	% Longer description at this teach-in time:
	Msg = text_utils:format( "The device '~ts' has emitted the following "
		"teach-in event: ~ts.~n~nFull device information: ~ts.",
		[ BinDevName, oceanic:device_event_to_string( DeviceEvent ),
		  BinDevDesc ] ),

	class_TraceEmitter:send_named_emitter( notice, State, Msg,
		get_trace_emitter_name_from( BinDevName ) ),

	RecState = record_new_device( DeviceEvent, State ),

	wooper:return_state( RecState );


onEnoceanDeviceTeachIn( State, OtherEvent, BinDevDesc, OcSrvPid ) ->
	?error_fmt( "Received an invalid teach-in device event (~p) "
		"from ~w for '~p', ignoring it.",
		[ OtherEvent, OcSrvPid, BinDevDesc ] ),

	wooper:const_return().



-doc """
Handles the detection of the vanishing of the specified device by the specified
Oceanic server.
""".
-spec onEnoceanDeviceLost( wooper:state(), eurid(), device_name(),
		device_description(), boolean(), timestamp(), milliseconds(),
		oceanic_server_pid() ) -> const_oneway_return().
onEnoceanDeviceLost( State, DeviceEurid, BinDeviceName, BinDevDesc,
		_IsNewLoss=true, LastSeenTimestamp, TimeOutMs, OcSrvPid ) ->

	Msg = text_utils:format( "The device '~ts' (EURID: ~ts) is just considered "
		"lost (last seen on ~ts, after a waiting of ~ts) "
		"by the Oceanic server ~w.~n~nFull device information: ~ts.",
		[ BinDeviceName, oceanic:eurid_to_string( DeviceEurid ),
		  time_utils:timestamp_to_string( LastSeenTimestamp ),
		  time_utils:duration_to_string( TimeOutMs ),
		  OcSrvPid, BinDevDesc ] ),

	class_TraceEmitter:send_named_emitter( error, State, Msg,
		get_trace_emitter_name_from( BinDeviceName ) ),

	wooper:const_return();

onEnoceanDeviceLost( State, DeviceEurid, BinDeviceName, BinDevDesc,
		_IsNewLoss=false, LastSeenTimestamp, TimeOutMs, OcSrvPid ) ->

	Msg = text_utils:format( "Device '~ts' (EURID: ~ts) still considered lost "
		"(last seen on ~ts, after a waiting of ~ts) "
		"by Oceanic server ~w.~n~nFull device information: ~ts.",
		[ BinDeviceName, oceanic:eurid_to_string( DeviceEurid ),
		  time_utils:timestamp_to_string( LastSeenTimestamp ),
		  time_utils:duration_to_string( TimeOutMs ),
		  OcSrvPid, BinDevDesc ] ),

	class_TraceEmitter:send_named_emitter( warning, State, Msg,
		get_trace_emitter_name_from( BinDeviceName ) ),

	wooper:const_return().



-doc """
Handles a possible jamming attempt, as suspected and reported by the specified
Oceanic server.
""".
-spec onEnoceanJamming( wooper:state(), bytes_per_second(),
						oceanic_server_pid() ) -> const_oneway_return().
onEnoceanJamming( State, TrafficLevel, OcSrvPid ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	?alert_fmt( "Received a notification from Oceanic server ~w of a "
		"possible jamming attempt (traffic level of ~B bytes per second).",
		[ OcSrvPid, TrafficLevel ] ),

	wooper:const_return().




% Section for remote commands, sent by clients like the US-Main controller
% script (see control-us-main.sh).


-doc """
Requests whether, from the point of view of this server, somebody is at home.
""".
-spec isPresent( wooper:state() ) -> const_request_return( boolean() ).
isPresent( State ) ->
	wooper:const_return_result( ?getAttr(actual_presence) ).




% Helper section.

-doc """
Returns a suitable name for a trace emitter for the specified device.
""".
-spec get_trace_emitter_name_from( device_name() ) -> bin_string().
get_trace_emitter_name_from( BinDevName ) ->
	text_utils:bin_concatenate( <<"Devices.">>, BinDevName ).



-doc """
Checks whether an actual presence switching (somebody being at home or not) is
to be done.
""".
-spec manage_presence_switching( device_event(), wooper:state() ) ->
										wooper:state().
manage_presence_switching( DevEvent, State ) ->

	cond_utils:if_defined( us_main_debug_home_automation,
		?debug_fmt( "Examining whether following event relates to presence "
			"switching: ~ts",
			[ oceanic:device_event_to_string( DevEvent ) ] ) ),

	case ?getAttr(presence_switching_device) of

		undefined ->
			%?debug( "(no presence switching device defined)" ),
			State;

		PscSwitchEurid ->
			case oceanic:get_source_eurid( DevEvent ) of

				% This is the device we were looking for:
				PscSwitchEurid ->
					% We cannot just use the receiving of a telegram to toggle
					% presence, as pushing a button/rocker sends multiple
					% telegrams (one when pressing, one when releasing); we act
					% only on one of these two messages - the press one:

					case oceanic:device_triggered( DevEvent ) of

						true ->
							NewPsc = not ?getAttr(actual_presence),

							SetState =
								setAttribute( State, actual_presence, NewPsc ),

							BaseMsg = text_utils:format( "Told by the presence "
								"switching device (~ts) that ",
								[ ?getAttr(presence_switching_device_desc) ] ),

							case NewPsc of

								true ->
									?info( BaseMsg
										++ "somebody is at home now." ),
									% Time to disable the alarm and the presence
									% simulation (TODO).
									%
									SetState;

								false ->
									?info( BaseMsg
										++ "nobody is at home now." ),
									% Time to enable the alarm and the presence
									% simulation (TODO: add timer).
									%
									SetState

							end;

						false ->
							State

					end;


				_OtherEurid ->
					State

			end

	end.



-doc """
Callback triggered, if this server enabled the trapping of exits, whenever a
linked process terminates.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
		basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StoppedPid, _ExitType=normal ) ->
	% Possibly useless to trace:
	?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
	wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%  {wooper_oneway_failed,<0.44.0>,class_XXX,
	%   FunName,Arity,Args,AtomCause}}, [...]}"

	% Redundant information, yet useful for console outputs:
	?warning_fmt( "US home automation server ~w received and ignored "
		"following exit message from ~w:~n  ~p",
		[ self(), CrashPid, ExitType ] ),

	wooper:const_return().




% Static subsection.


-doc """
Returns the PID of the supposedly already-launched home automation server; waits
for it if needed.
""".
-spec get_home_automation_server() ->
			static_return( home_automation_server_pid() ).
get_home_automation_server() ->

	OcSrvPid = naming_utils:wait_for_registration_of(
		?us_main_home_automation_server_registration_name,
		naming_utils:registration_to_look_up_scope(
			?us_main_home_automation_server_registration_scope ) ),

	wooper:return_static( OcSrvPid ).




% Helper section.


-doc """
Sends the specified presence simulation trace, to have it correctly categorised.
""".
-spec send_psc_trace( trace_severity(), trace_message(), wooper:state() ) ->
								void().
send_psc_trace( TraceSeverity, TraceMsg, State ) ->
	class_TraceEmitter:send_named_emitter( TraceSeverity, State, TraceMsg,
		<<"Presence simulation">> ).



-doc """
Sends the specified presence simulation formatted trace, to have it correctly
categorised.
""".
-spec send_psc_trace_fmt( trace_severity(), trace_format(), trace_values(),
						  wooper:state() ) -> void().
send_psc_trace_fmt( TraceSeverity, TraceFormat, TraceValues, State ) ->
	TraceMsg = text_utils:format( TraceFormat, TraceValues ),
	send_psc_trace( TraceSeverity, TraceMsg, State ).



-doc "Returns a textual description of this server.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	OcSrvStr = case ?getAttr(oc_srv_pid) of

		undefined ->
			"not relying on an Oceanic server";

		OcSrvPid ->
			text_utils:format( "relying on its Oceanic server ~w "
				"(base identifier being EURID ~ts; "
				"periodic restarts enabled: ~ts)",
				[ OcSrvPid, oceanic:eurid_to_string( ?getAttr(oc_base_id) ),
				  ?getAttr(oc_periodic_restart) ] )

	end,

	LocStr = case ?getAttr(server_location) of

		undefined ->
			"with no location defined";

		{ Lat, Long } ->
			% Degrees as raw floats rather than wtih °, minutes and al:
			text_utils:format( "located at latitude ~f degrees and "
				"longitude ~f degrees", [ Lat, Long ] )

	end,

	AtHomeStr = "considering that " ++ case ?getAttr(actual_presence) of

		true ->
			"someone";

		famse ->
			"nobody"

	end ++ " is at home",

	PscStr = case ?getAttr(presence_simulation_enabled) of

		true ->
			PscSims = table:values( ?getAttr(presence_table) ),
			"enabled, with " ++ case PscSims of

				[] ->
					"no presence defined";

				[ PscSim ] ->
					"a single presence defined: "
						++ presence_simulation_to_string( PscSim );

				PscSims ->
					text_utils:format( "~B presences defined: ~ts",
						[ length( PscSims ), text_utils:strings_to_string(
							[ presence_simulation_to_string( PS )
								|| PS <- PscSims ] ) ] )

			end ++ "and " ++ case ?getAttr(time_equation_table) of

				undefined ->
					"no time equation table used";

				TimeEqTable ->
					text_utils:format( "using a time equation table "
						"comprising ~B entries", [ table:size( TimeEqTable ) ] )

							 end;

		false ->
			"disabled"

	end,

	% Defined as much as presence_switching_device:
	PscSwitchStr = case ?getAttr(presence_switching_device_desc) of

		undefined ->
			"no presence-switching device has been defined";

		BinPscSwitchDesc ->
			text_utils:format( "a presence-switching device has been "
				"defined, ~ts", [ BinPscSwitchDesc ] )

	end,

	MidTaskStr = case ?getAttr(midnight_task_id) of

		undefined ->
			"no midnight update task defined";

		MidTaskId ->
			text_utils:format( "midnight update task #~B defined",
							   [ MidTaskId ] )

	end,

	text_utils:format( "US home automation server ~ts, using the US-Main "
		"configuration server ~w, the scheduler ~w and the communication "
		"gateway ~w, ~ts, ~ts; it is ~ts; "
		"the presence simulator is currently ~ts, and ~ts; ~ts",
		[ OcSrvStr, ?getAttr(us_config_server_pid), ?getAttr(scheduler_pid),
		  ?getAttr(comm_gateway_pid), LocStr, AtHomeStr,
		  device_table_to_string( ?getAttr(device_table) ),
		  PscStr, PscSwitchStr, MidTaskStr ] ).



-doc """
Returns a textual description of the specified presence simulation internal
record.
""".
-spec presence_simulation_to_string( presence_simulation() ) -> ustring().
presence_simulation_to_string( #presence_simulation{
		id=Id,
		enabled=IsEnabled,
		activated=IsActivated,
		source_eurid=SourceEurid,
		target_eurid=TargetEurid,
		program=Program,
		smart_lighting=IsSmart,
		random_activity=IsRandom,
		activation_telegrams={ PressOn, ReleaseOn },
		deactivation_telegrams={ PressOff, ReleaseOff },
		presence_task_info=MaybeTaskInfo } ) ->

	SmartStr = case IsSmart of

		true ->
			"";

		false ->
			"not"

	end,

	RandomStr = case IsRandom of

		true ->
			"";

		false ->
			"not"

	end,

	PscTaskStr = case MaybeTaskInfo of

		undefined ->
			"no presence task defined";

		{ PscTaskId, PscTime } ->
			text_utils:format( "presence task #~B scheduled at ~ts",
				[ PscTaskId, time_utils:time_to_string( PscTime ) ] )

	end,

	text_utils:format( "presence simulation of id #~B, ~ts, "
		"~tsusing smart lighting, ~tsemulating random activity, "
		"whose presence program ~ts~n"
		"Its source EURID is ~ts and target EURID is ~ts "
		"(activation telegrams are ~ts for pressed, "
		"~ts for released, deactivation telegrams are ~ts for pressed, "
		"~ts for released); ~ts",
		[ Id, case IsEnabled of
					true -> "enabled";
					false -> "disabled"
			  end ++ " and currently "
		  ++ case IsActivated of
					true -> "activated";
					false -> "non-activated"
			 end, SmartStr, RandomStr, program_to_string( Program ),
		  oceanic:eurid_to_string( SourceEurid ),
		  oceanic:eurid_to_string( TargetEurid ),
		  oceanic:telegram_to_string( PressOn ),
		  oceanic:telegram_to_string( ReleaseOn ),
		  oceanic:telegram_to_string( PressOff ),
		  oceanic:telegram_to_string( ReleaseOff ),
		  PscTaskStr ] ).



-doc "Returns a textual description of the specified presence program.".
-spec program_to_string( presence_program() ) -> ustring().
program_to_string( _Prog=constant_presence ) ->
	"is constant presence";

program_to_string( _Prog=constant_absence ) ->
	"is constant absence";

program_to_string( _Slots=[] ) ->
	"has no slot defined";

program_to_string( _Slots=[ SingleSlot ] ) ->
	text_utils:format( "has a single presence slot defined: ~ts",
					   [ slot_to_string( SingleSlot ) ] );

program_to_string( Slots ) ->

	SlotStrs = program_to_string( Slots, _FirstMaybePrevEndTime=undefined,
								  _Acc=[] ),

	text_utils:format( "has ~B presence slots defined: ~ts",
		[ length( Slots ), text_utils:strings_to_string( SlotStrs ) ] ).


% (helper)
program_to_string( _Slots=[], _FirstMaybePrevEndTime, Acc ) ->
	lists:reverse( Acc );

program_to_string( _Slots=[ S={ _Start, Stop } | T  ], MaybePrevEndTime,
				   Acc ) ->
	SlotStr = slot_to_string( S, MaybePrevEndTime ),
	program_to_string( T, Stop, [ SlotStr | Acc ] ).



-doc "Returns a textual description of the specified presence slot.".
-spec slot_to_string( presence_slot() ) -> ustring().
slot_to_string( Slot ) ->
	slot_to_string( Slot, _PrevEndTime=undefined ).



-doc "Returns a textual description of the specified presence slot.".
-spec slot_to_string( presence_slot(), option( time() ) ) -> ustring().
slot_to_string( _Slot={ StartTime, StopTime }, _MaybePrevEndTime=undefined ) ->

	DurSec = time_utils:get_intertime_duration( StartTime, StopTime ),

	text_utils:format( "from ~ts to ~ts (duration: ~ts)", [
		time_utils:time_to_string( StartTime ),
		time_utils:time_to_string( StopTime ),
		time_utils:duration_to_string( 1000 * DurSec ) ] );


slot_to_string( _Slot={ StartTime, StopTime }, PrevEndTime ) ->

	FromPrevDurSec = time_utils:get_intertime_duration( PrevEndTime,
														StartTime ),

	DurSec = time_utils:get_intertime_duration( StartTime, StopTime ),

	text_utils:format( "(after ~ts) from ~ts to ~ts (duration: ~ts)", [
		time_utils:duration_to_string( 1000 * FromPrevDurSec ),
		time_utils:time_to_string( StartTime ),
		time_utils:time_to_string( StopTime ),
		time_utils:duration_to_string( 1000 * DurSec ) ] ).



-doc "Returns a textual description of the specified device state.".
-spec device_state_to_string( device_state() ) -> ustring().
device_state_to_string( #device_state{
		eurid=Eurid,
		name=BinName,
		eep_id=EEPId,
		% Skipped: initial_event / last_event
		current_status=Status } ) ->
	text_utils:format( "device '~ts' (EURID: ~ts) implementing EEP ~ts, "
		"whose current status is ~ts",
		[ BinName, oceanic:eurid_to_string( Eurid ), EEPId, Status ] ).



-doc "Returns a textual description of the specified device table.".
-spec device_table_to_string( device_table() ) -> ustring().
device_table_to_string( DevTable ) ->
	case table:values( DevTable ) of

		[] ->
			"not registering any device";

		[ SingleDev ] ->
			text_utils:format( "registering a single device: ~ts",
				[ device_state_to_string( SingleDev ) ] );

		Devs ->
			text_utils:format( "registering ~B devices: ~ts",
				[ length( Devs ), text_utils:strings_to_string(
					[ device_state_to_string( D ) || D <- Devs ] ) ] )

	end.
