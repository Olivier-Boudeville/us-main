% Copyright (C) 2022-2023 Olivier Boudeville
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


% @doc US server in charge of the <b>providing home automation services</b>,
% based on Enocean, thanks to Ceylan-Oceanic.
%
-module(class_USHomeAutomationServer).


-define( class_description, "US server in charge of the providing home "
		 "automation services, based on Enocean, thanks to Ceylan-Oceanic" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").


% Design notes:
%
% We rely here on Ceylan-Oceanic (https://oceanic.esperide.org/), which itself
% relies on our fork of erlang-serial
% (https://github.com/Olivier-Boudeville/erlang-serial).



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

-define( bridge_name, ?MODULE ).


-type home_automation_server_pid() :: class_USServer:server_pid().


% For the presence_simulation_setting record:
-include("class_USHomeAutomationServer.hrl").


-type presence_simulation_setting() :: #presence_simulation_setting{}.
% Setting of a given instance of presence simulation.


-type presence_simulation_settings() ::
	{ 'default', MaybeTargetActuatorEurid :: maybe( eurid() ) }
  | [ presence_simulation_setting() ].
% All settings for a whole presence simulation service.


-type presence_milestone() ::
	time()  % A time in the day
  | 'dawn'  % First light (if any) of the day
  | 'dusk'. % Last light (if any) of the day
% Describes a start/stop logical moment to simulate presence.


-type presence_slot() ::
	{ Start :: presence_milestone(), Stop :: presence_milestone() }.
% A time slot during which a presence shall be simulated.


-type presence_program() :: [ presence_slot() ]
						  | 'constant_presence'
						  | 'constant_absence'.
% A intra-day general program regarding a presence to simulate.
%
% If slots are used, at least one shall be defined (otherwise a constant_* atom
% shall be used).


-type celestial_timing() :: { Dawn :: maybe( time() ), Dusk :: maybe( time() ) }
						  | 'constant_daylight' | 'constant_darkness'.
% The time for dawn and dusk (if any - think to the extreme latitudes), at a
% given (implicit) location and date.
%
% Dawn and dusk are defined as actual transitions between darkness/daylight.


-type celestial_info() :: { date(), celestial_timing() }.
% The time for dawn and dusk (if any - think to the extreme latitudes), at a
% given (implicit) location, for the specified date.


-export_type([ home_automation_server_pid/0,
			   presence_simulation_settings/0, presence_simulation_setting/0,
			   presence_milestone/0, presence_slot/0,
			   celestial_timing/0, celestial_info/0 ]).





% Local types:


-type presence_sim_id() :: count().
% Internal identifier of a presence simulation.


-type telegram_pair() :: { Press :: telegram(), Release :: telegram() }.
% A pair of telegrams corresponding to a press/release transaction.


-type task_info() :: { task_id(), time() }.
% Information regarding a planned (presence) task, to be able to check/control
% it.


% Internal information regarding an instance of presence simulation:
-record( presence_simulation, {

	% The identifier of this presence simulation:
	id :: presence_sim_id(),

	% Tells whether this presence simulation is enabled:
	%
	% (for example when somebody is at home, it may be disabled)
	%
	enabled = true :: boolean(),

	% Tells whether the presence simulation is currently activated, that is if
	% lighting is currently on or off (avoids sending unnecessary switching
	% orders).
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
	activated :: boolean(),


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


	% The general program of this presence simulation (to be applied each day),
	% generally a chronologically-ordered intra-day (from midnight to midnight)
	% non-empty list of presence slots, or simpler policies:
	%
	program :: presence_program(),


	% The next planned action (if any) that shall happen:
	% Not used as relying now on a "stateless" algorithm.
	%next_action :: maybe( { timestamp(), presence_action() } ),

	% Tells whether lighting shall be switched off during a presence slot when
	% the light of day should be available (provided that the position of the
	% server is known):
	%
	smart_lighting = true :: boolean(),


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
	presence_task_info :: maybe( task_info() ) } ).


-type presence_simulation() :: #presence_simulation{}.
% Internal information regarding an instance of presence simulation.


-type presence_table() :: table( presence_sim_id(), presence_simulation() ).
% A table keeping track of the known presence simulations.



% Shorthands:

-type count() :: basic_utils:count().
-type user_data() :: basic_utils:user_data().

-type ustring() :: text_utils:ustring().

-type device_path() :: file_utils:device_path().

-type bytes_per_second() :: system_utils:bytes_per_second().

-type date() :: time_utils:date().
-type time() :: time_utils:time().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
-type task_id() :: class_USScheduler:task_id().

-type position() :: unit_utils:position().

-type oceanic_server_pid() :: oceanic:oceanic_server_pid().
-type device_event() :: oceanic:device_event().
-type eurid_string() :: oceanic:eurid_strin().
-type eurid() :: oceanic:eurid().
-type telegram() :: oceanic:eurid().



% The class-specific attributes:
-define( class_attributes, [

	{ oc_srv_pid, maybe( oceanic_server_pid() ),
	  "the PID of the Oceanic server (if any can exist) used by this server" },

	{ oc_base_id, maybe( eurid() ), "the base identifier (if any) of "
	  "the local Enocean gateway to use, as determined by Oceanic" },

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" },

	% Typically for the presence simulator:
	{ scheduler_pid, scheduler_pid(),
	  "the PID of the scheduler used by this server" },

	% Better defined separately from presence_table, as the program shall remain
	% whereas presence simulation may be regularly enabled/disabled:
	%
	{ presence_simulation_enabled, boolean(),
	  "tells whether the presence simulation service is currently enabled" },

	{ presence_table, presence_table(),
	  "referential of the presence simulations" },

	{ next_presence_id, presence_sim_id(),
	  "the next presence identifier that will be assigned" },

	{ midnight_task_id, maybe( task_id() ),
	  "A task triggered, if the presence simulation is activated, each day "
	  "at midnight to determine and update the activity of the presence "
	  "simulations for the next day (and to ensure that any potential error "
	  "does not linger)" },

	{ server_location, maybe( position() ),
	  "the (geographic) location, as a position, of the US-Main server" },

	{ celestial_info, maybe( celestial_info() ),
	  "any precomputed dawn/dusk time, for the current day" },

	{ comm_gateway_pid, gateway_pid(),
	  "the PID of the US communication gateway used to send user "
	  "notifications" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.HomeAutomation" ).



% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Implementation notes:


% Regarding presence simulation:
%
% The presence milestones of a slot may overlap (typically because the time of
% dusk and dawn varies in the course of the year); anyway the presence will be
% simulated in all cases with no interruption.
%
% A robust mode of operation has been retained, with which states are enforced
% (e.g smart plug is on) rather than transitions (e.g. toggling smart plug).


% Regarding the computation of the time in the day of dawn and dusk:
%
% The duration of a given day depends on the date and latitude of the location
% of interest; the actual moments for dawn and dusk depend also on longitude.
%
% More information (in French):
% - https://www.astrolabe-science.fr/duree-du-jour-et-latitude/


% Regarding scheduling:
%
% Each presence simulation is planned from the current event to (only) the next.
% As soon there is at least one presence simulation, an additional overall
% update (daily, at midnight) is scheduled, so that the programs for this day
% are established.


% Regarding the Enocean actuators:
%
% We suppose that these devices already learnt the USB gateway being used by
% Oceanic. This server will act upon these actuators as a double-rocker switch
% (e.g. not two single-contact buttons), whose on button is button_ao, and whose
% off button is button_ai.





% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this home automation server with an OTP
% supervision tree.



% @doc Starts and links a supervision bridge for the home automation system.
%
% Note: typically spawned as a supervised child of the US-Main root supervisor
% (see us_main_sup:init/1), hence generally triggered by the application
% initialisation.
%
-spec start_link() -> term().
start_link() ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the home automation system." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
		_Module=?MODULE, _InitArgs=[] ).



% @doc Callback to initialise this supervisor bridge, typically in answer to
% start_link/0 being executed.
%
-spec init( list() ) -> { 'ok', pid(), State :: term() } | 'ignore'
					  | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_bridge:info_fmt( "Initializing the US-Main supervisor bridge ~w for "
						   "the home automation system.", [ self() ] ),

	% Not specifically synchronous:
	HomeAutomSrvPid = ?MODULE:new_link(),

	{ ok, HomeAutomSrvPid, _InitialBridgeState=HomeAutomSrvPid }.



% @doc Callback to terminate this supervisor bridge.
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


% @doc Constructs an home automation server, based on the default, local TTY
% allocated to the USB Enocean gateway, whose base identifier will be used as
% source EURID for telegram sendings, and not providing a presence simulator.
%
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
	construct( State, oceanic:get_default_tty_path() ).


% @doc Constructs an home automation server, based on the specified local TTY
% allocated to the USB Enocean gateway, whose base identifier will be used as
% source EURID for telegram sendings, and not providing a presence simulator.
%
-spec construct( wooper:state(), device_path() ) -> wooper:state().
construct( State, TtyPath ) ->
	construct( State, TtyPath, _MaybePresenceSimSettings=undefined ).


% @doc Constructs an home automation server, based on the specified local TTY
% allocated to the USB Enocean gateway.
%
% Unless MaybePresenceSimSettings is 'undefined', a presence simulation will be
% performed, specified either as a complete list of presence settings, or only
% through the EURID of the target actuator(s), in which case a default presence
% policy will apply; the source used for the sent telegrams will be the base
% identifier of the gateway.
%
-spec construct( wooper:state(), device_path(),
				 maybe( presence_simulation_settings() | eurid_string() ) ) ->
										wooper:state().
construct( State, TtyPath, MaybePresenceSimSettings ) ->
	construct( State, TtyPath, MaybePresenceSimSettings,
			   _MaybeSourceEuridStr=undefined ).


% @doc Constructs an home automation server, based on the specified local TTY
% allocated to the USB Enocean gateway.
%
% Unless MaybePresenceSimSettings is 'undefined', a presence simulation will be
% performed, specified either as a complete list of presence settings, or only
% through the EURID of the target actuator(s), in which case a default presence
% policy will apply; the source used for the sent telegrams will be the
% specified one (if any), otherwise the base identifier of the gateway.
%
-spec construct( wooper:state(), device_path(),
	maybe( presence_simulation_settings() | eurid_string() ),
	maybe( eurid_string() ) ) -> wooper:state().
construct( State, TtyPath, MaybePresenceSimSettings, MaybeSourceEuridStr ) ->

	ServerTraceName = "Home automation server",

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize(ServerTraceName),
		?us_main_home_automation_server_registration_name,
		?us_main_home_automation_server_registration_scope ),

	% Do not start Oceanic if it is bound to fail:
	{ MaybeOcSrvPid, MaybeSrcEurid, MaybeBaseId } =
			case oceanic:is_available( TtyPath ) of

		{ true, _SerialRootDir } ->
			OcPid = oceanic:start_link( TtyPath, _EventListenerPid=self() ),
			BaseId = oceanic:get_oceanic_eurid( OcPid ),
			SourceId = case MaybeSourceEuridStr of

				undefined ->
					% Then go for the BaseId:
					BaseId;

				SrcId ->
					oceanic:string_to_eurid( SrcId )

			end,
			{ OcPid, SourceId, BaseId };

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
	MaybeSrvLoc = receive

		{ wooper_result, _MaybeUserSrvLoc=undefined } ->
			undefined;

		% Already checked as floats by the configuration server:
		{ wooper_result, UserSrvLoc={ Lat, Long } } ->
			( Lat > 90.0 orelse Lat < -90.0 ) andalso
				throw( { invalid_latitude, Lat } ),

			( Long > 180.0 orelse Long < -180.0 ) andalso
				throw( { invalid_longitude, Long } ),

			UserSrvLoc

	end,

	{ InitPscTable, NextPscId, MaybeMidnightTaskId } = init_presence_simulation(
		MaybePresenceSimSettings, MaybeOcSrvPid, MaybeSrcEurid, SrvState ),

	% Later:
	%AlarmState = init_alarm( MaybeOcSrvPid, SrvState ),
	AlarmState = SrvState,

	% To report any issue:
	CommGatewayPid = class_USCommunicationGateway:get_communication_gateway(),

	InitalPscEnabled = not table:is_empty( InitPscTable ),

	SetState = setAttributes( AlarmState, [
		{ oc_srv_pid, MaybeOcSrvPid },
		{ oc_base_id, MaybeBaseId },
		{ us_config_server_pid, UsMainCfgSrvPid },
		{ scheduler_pid, SchedPid },
		{ presence_simulation_enabled, InitalPscEnabled },
		{ presence_table, InitPscTable },
		{ next_presence_id, NextPscId },
		{ midnight_task_id, MaybeMidnightTaskId },
		{ server_location, MaybeSrvLoc },
		{ celestial_info, undefined },
		{ comm_gateway_pid, CommGatewayPid } ] ),

	ApplyState = apply_presence_simulation( SetState ),

	?send_notice( ApplyState, "Constructed: " ++ to_string( ApplyState ) ),

	ApplyState.



% (helper)
-spec init_presence_simulation( maybe( presence_simulation_settings() ),
	maybe( oceanic_server_pid() ), maybe( eurid() ), wooper:state() ) ->
	{ presence_table(), presence_sim_id(), maybe( task_id() ) }.
init_presence_simulation( MaybePresenceSimSettings, MaybeOcSrvPid,
						  MaybeSrcEurid, State ) ->
	init_presence_simulation( MaybePresenceSimSettings, MaybeOcSrvPid,
		MaybeSrcEurid, _PscTable=table:new(), _NextPscId=1, State ).



% @doc Initialises the overall presence simulation.
%
% (helper)
%
% No presence simulation wanted:
init_presence_simulation( _MaybePresenceSimSettings=undefined, _MaybeOcSrvPid,
						  _MaybeSrcEurid, EmptyPscTable, InitPscId, _State ) ->
	{ EmptyPscTable, InitPscId, _MaybeMidTaskId=undefined };

init_presence_simulation( PresenceSimSettings, _MaybeOcSrvPid=undefined,
		_MaybeSrcEurid, _PscTable, _NextPscId, State ) ->

	?error_fmt( "No Oceanic support available, the requested presence "
		"simulation (~p) cannot be performed.", [ PresenceSimSettings ] ),

	throw( { no_presence_simulation, no_oceanic } );


% Not expected to happen, as set with OcSrvPid:
init_presence_simulation( PresenceSimSettings, _OcSrvPid,
		_MaybeSrcEurid=undefined, _PscTable, _NextPscId, State ) ->

	?error_fmt( "No source EURID available, the requested presence "
		"simulation (~p) cannot be performed.", [ PresenceSimSettings ] ),

	throw( { no_presence_simulation, no_source_eurid } );


% Default settings, just switching to the actual clause:
init_presence_simulation(
		_PresenceSimSettings={ default, MaybeTargetActuatorEuridStr },
		OcSrvPid, SrcEurid, PscTable, NextPscId, State ) ->

	% Principle: no useless lighting during the expected presence slots, which
	% are:
	%  - in the morning: from 7:30 AM to 8:30 AM
	%  - in the evening: from 7:00 PM to 11:45 PM

	TMorningStart = { 7, 30, 0 },
	TMorningStop = { 8, 30, 0 },

	TEveningStart = { 19, 00, 00 },
	TEveningStop = { 23, 45, 00 },

	Slots = [ { TMorningStart, TMorningStop },
			  { TEveningStart, TEveningStop } ],

	DefaultSettings = #presence_simulation_setting{
		source_eurid=SrcEurid,
		target_eurid=MaybeTargetActuatorEuridStr,
		presence_program=Slots,
		smart_lighting=true },

	% Branch then to the general rule:
	init_presence_simulation( _PscSimSettings=[ DefaultSettings ], OcSrvPid,
		SrcEurid, PscTable, NextPscId, State );


% The "actual" clauses now.
%
% Only exit point:
init_presence_simulation( _PresenceSimSettings=[], _OcSrvPid, _SrcEurid,
						  PscTable, NextPscId, State ) ->

	% Schedules a periodic midnight presence update iff necessary:
	MaybeMidTaskId = case table:is_empty( PscTable ) of

		true ->
			undefined;

		false ->
			TomorrowDate = time_utils:get_date_after( date(), _DaysOffset=1 ),
			NextMidnightTimestamp = { TomorrowDate, _Midnight={ 0, 0, 0 } },

			% Every day:
			DHMSPeriodicity = { _D=1, _H=0, _M=0, _S=0 },

			?getAttr(scheduler_pid) ! { registerTask,
				[ _CmdMsg=updatePresencePrograms,
				  _StartTime=NextMidnightTimestamp,
				  DHMSPeriodicity,
				  _Count=unlimited ], self() },

			receive

				{ wooper_result, { task_registered, MidTaskId } } ->
					?debug_fmt( "Midnight presence update task #~B defined.",
								[ MidTaskId ] ),
					MidTaskId

			end

	end,
	{ PscTable, NextPscId, MaybeMidTaskId };


% Main clause:
init_presence_simulation( _PresenceSimSettings=[
		#presence_simulation_setting{
			source_eurid=MaybeUserSrcEuridStr,
			target_eurid=MaybeTargetActuatorEuridStr,
			presence_program=Program,
			smart_lighting=SmartBool } | T ], OcSrvPid, SrcEurid, PscTable,
						  NextPscId, State ) ->

	ActualSrcEurid = case MaybeUserSrcEuridStr of

		undefined ->
			SrcEurid;

		UserSrcEuridStr ->
			oceanic:string_to_eurid( UserSrcEuridStr )

	end,

	ActualTargetEurid = case MaybeTargetActuatorEuridStr of

		undefined ->
			oceanic:get_broadcast_eurid();

		TargetActuatorEuridStr ->
			oceanic:string_to_eurid( TargetActuatorEuridStr )

	end,


	% We check the program, not taking into account here dawn/dusk, as they
	% change each day:
	%
	VettedProgram = vet_program( Program ),

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

	PscSim = #presence_simulation{
		id=NextPscId,
		enabled=true,
		activated=false,
		source_eurid=ActualSrcEurid,
		target_eurid=ActualTargetEurid,
		program=VettedProgram,
		smart_lighting=type_utils:ensure_boolean( SmartBool ),
		activation_telegrams={ OnPressTelegram, OnReleaseTelegram },
		deactivation_telegrams={ OffPressTelegram, OffReleaseTelegram } },

	NewPscTable = table:add_new_entry( _K=NextPscId, PscSim, PscTable ),

	init_presence_simulation( T, OcSrvPid, SrcEurid, NewPscTable, NextPscId+1,
							  State );


init_presence_simulation( _PresenceSimSettings=[ Other | _T ], _OcSrvPid,
						  _SrcEurid, _PscTable, _NextPscId, _State ) ->
	throw( { invalid_presence_setting, Other } );


init_presence_simulation( _PresenceSimSettings=Other, _OcSrvPid, _SrcEurid,
						  _PscTable, _NextPscId, _State ) ->
	throw( { invalid_presence_settings, Other } ).



% @doc Vets the specified user program.
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

	time_utils:is_time( StartMilestone )
		orelse throw( { invalid_slot_start_milestone, StartMilestone } ),

	time_utils:is_time( StopMilestone )
		orelse throw( { invalid_slot_stop_milestone, StopMilestone } ),

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




% @doc Applies, from the current moment, the intra-day presence program.
%
% Defined for reuse (at creation or afterwards, if toggling this service as a
% whole, updating presence simulations, etc.).
%
-spec apply_presence_simulation( wooper:state() ) -> wooper:state().
apply_presence_simulation( State ) ->

	case ?getAttr(presence_simulation_enabled) of

		true ->
			PscTable = ?getAttr(presence_table),

			case table:values( PscTable ) of

				[] ->
					State;

				PscSims ->
					update_presence_simulations( PscSims, State )

			end;

		false ->
			State

	end.



% @doc Updates, in turn and if necessary, from scratch, the specified presence
% simulations.
%
% Defined for reuse.
%
-spec update_presence_simulations( [ presence_simulation() ],
								   wooper:state() ) -> wooper:state().
update_presence_simulations( PscSims, State ) ->

	?debug_fmt( "Updating ~B presence simulations now.",
				[ length( PscSims ) ] ),

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



% @doc Updates, in turn and if necessary, from scratch, the specified presence
% simulations.
%
% If defined, celestial times are expected to be correct here.
%
-spec update_presence_simulations( [ presence_simulation() ], time(),
		maybe( celestial_info() ), presence_table(), wooper:state() ) ->
					wooper:state().
update_presence_simulations( _PscSims=[], _CurrentTime, _MaybeCelestialInfo,
							 PscTable, State ) ->
	setAttribute( State, presence_table, PscTable );

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

	?debug_fmt( "Managing presence simulation ~ts",
				[ presence_simulation_to_string( PscSim ) ] ),

	{ StartedPscSim, CelestialInfo } = manage_presence_simulation( PscSim,
		CurrentTime, MaybeCelestialInfo, State ),

	NewPscTable = table:add_new_entry( _K=Id, StartedPscSim, PscTable ),

	update_presence_simulations( T, CurrentTime, CelestialInfo, NewPscTable,
								 State ).




% @doc Manages the specified presence simulation from scratch (callable at any
% time), covering all cases, and acting iff necessary.
%
% A large function, but defined only once.
%
-spec manage_presence_simulation( presence_simulation(), time(),
								  maybe( celestial_info() ), wooper:state() ) ->
		{ presence_simulation(), maybe( celestial_info() ) }.
% Not enabled:
manage_presence_simulation( PscSim=#presence_simulation{
		enabled=false,
		activated=IsActivated,
		presence_task_info=MaybePscTaskInfo },
							_CurrentTime, MaybeCelestialInfo, State ) ->

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
			UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
			NoPlanPscSim = ensure_no_planned_presence_transition( UnlitPscSim,
												MaybePscTaskInfo, State ),
			{ NoPlanPscSim, CI };

		CI={ _TodayDate, constant_darkness } ->
			% No natural lighting, so lighting needed:
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
							UnlitPscSim = ensure_not_lighting( PscSim,
								IsActivated, State ),

							ensure_no_planned_presence_transition( UnlitPscSim,
								MaybePscTaskInfo, State );

						DuskTime when CurrentTime > DuskTime ->
								% Already after dusk; lighting needed from now
								% and till the end of day:
								%
								LitPscSim = ensure_lighting( PscSim,
									IsActivated, State ),

								ensure_no_planned_presence_transition(
									LitPscSim, MaybePscTaskInfo, State );

						% Hence CurrentTime <= DuskTime:
						DuskTime ->
								% Between dawn and dusk, so no lighting needed
								% until dusk:
								%
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
			LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
			ensure_no_planned_presence_transition( LitPscSim,
				MaybePscTaskInfo, State );

		always_absent ->
			UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
			ensure_no_planned_presence_transition( UnlitPscSim,
				MaybePscTaskInfo, State );

		{ present_until, AbsStartTime } ->
			LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
			ensure_planned_presence_transition( LitPscSim, AbsStartTime,
				MaybePscTaskInfo, State );

		{ absent_until, PresStartTime } ->
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

	NewPscSim = case get_programmed_presence( Slots, CurrentTime ) of

		% For good, today, hence light always needed:
		always_present ->
			ensure_constant_light( CurrentTime, MaybeDawnTime, MaybeDuskTime,
				PscSim, IsActivated, MaybePscTaskInfo, State );

		always_absent ->
			% Easy case:
			UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
			ensure_no_planned_presence_transition( UnlitPscSim,
												   MaybePscTaskInfo, State );

		{ present_until, AbsStart } ->
			ensure_light_until( AbsStart, CurrentTime, MaybeDawnTime,
				MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State );

		{ absent_until, PresStart } ->
			ensure_light_from( PresStart, CurrentTime, MaybeDawnTime,
				MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State )

	end,

	{ NewPscSim, CelestialInfo }.




% @doc Returns whether, for the specified time, a presence shall be simulated,
% and what is the transition (if any) to plan next this day.
%
-spec get_programmed_presence( [ presence_slot() ], time() ) ->
		'always_present' | 'always_absent'
	| { 'present_until', time() } | { 'absent_until', time() }.
get_programmed_presence( _Slots=[], _Time ) ->
	% No more presence slot, hence:
	always_absent;

get_programmed_presence( _Slots=[ { _StartPscTime, StopPscTime } | T ], Time )
								when Time > StopPscTime ->
	% Time of interest not reached in the list, continuing:
	get_programmed_presence( T, Time );

% We are before a slot:
get_programmed_presence( _Slots=[ { StartPscTime, _StopPscTime } | _T ], Time )
								when Time < StartPscTime ->
	{ absent_until, StartPscTime };

% We are in a slot:
get_programmed_presence( _Slots=[ { _StartPscTime, StopPscTime } | _T ],
						 _Time ) ->
	% Implicit: StartPscTime <= Time <= StopPscTime:
	Midnight = { 0, 0, 0 },
	case StopPscTime < Midnight of

		true ->
			{ present_until, StopPscTime };

		false ->
			always_present

	end.



% @doc Ensures that the lighting is on for this presence simulation.
-spec ensure_lighting( presence_simulation(), boolean(), wooper:state() ) ->
			presence_simulation().
ensure_lighting( PscSim, _IsActivated=true, State ) ->
	?debug_fmt( "Presence simulation #~B already activated, nothing to do.",
				[ PscSim#presence_simulation.id ] ),
	PscSim;

ensure_lighting( PscSim=#presence_simulation{
							activation_telegrams=ActivationTelegrams },
				 _IsActivated=false, State ) ->

	?debug_fmt( "Activating presence simulation #~B.",
				[ PscSim#presence_simulation.id ] ),

	send_telegram_pair( ActivationTelegrams, State ),

	PscSim#presence_simulation{ activated=true }.



% @doc Ensures that the lighting is off for this presence simulation.
-spec ensure_not_lighting( presence_simulation(), boolean(), wooper:state() ) ->
			presence_simulation().
ensure_not_lighting( PscSim=#presence_simulation{
								deactivation_telegrams=DeactivationTelegrams },
					 _IsActivated=true, State ) ->

	?debug_fmt( "Deactivating presence simulation #~B.",
				[ PscSim#presence_simulation.id ] ),

	send_telegram_pair( DeactivationTelegrams, State ),

	PscSim#presence_simulation{ activated=false };


ensure_not_lighting( PscSim, _IsActivated=false, State ) ->
	?debug_fmt( "Presence simulation #~B already deactivated, nothing to do.",
				[ PscSim#presence_simulation.id ] ),
	PscSim.



% @doc Ensures that the specified presence task (and only it) is planned for an
% update at the specified time.
%
-spec ensure_planned_presence_transition( presence_simulation(), time(),
			maybe( task_info() ), wooper:state() ) -> presence_simulation().
% Plan if not already planned:
ensure_planned_presence_transition( PscSim=#presence_simulation{ id=PscId },
		PlannedTime, _MaybePscTaskInfo=undefined, State ) ->

	TaskCmd = { updatePresenceSimulation, [ PscId ] },

	?getAttr(scheduler_pid) !
		{ registerOneshotTask, [ TaskCmd, PlannedTime ], self() },

	receive

		{ wooper_result, task_done } ->
			% Better than waiting for ever:
			throw( unexpected_done_task );

		{ wooper_result, { task_registered, TaskId } } ->
			TaskInfo = { TaskId, PlannedTime },
			PscSim#presence_simulation{ presence_task_info=TaskInfo }

	end;

% Here such planning exists and is already at the correct time:
ensure_planned_presence_transition( PscSim, PlannedTime,
		_PscTaskInfo={ _PscTaskId, PlannedTime }, _State ) ->
	PscSim;

% Exists yet with different times, correcting it:
ensure_planned_presence_transition( PscSim, PlannedTime,
		_PrevPscTaskInfo={ PrevPscTaskId, PrevPlannedTime }, State ) ->

	?warning_fmt( "Switching, for presence simulation #~B, planned time "
		"from ~ts to ~ts.", [ PscSim#presence_simulation.id,
			time_utils:time_to_string( PrevPlannedTime ),
			time_utils:time_to_string( PlannedTime ) ] ),

	% Clearer:
	%ClearedPsim = ensure_no_planned_presence_transition( PscSim,
	%                           PrevPscTaskInfo, State ),
	?getAttr(scheduler_pid) ! { unregisterTaskAsync, [ PrevPscTaskId ] },

	% Force rescheduling:
	ensure_planned_presence_transition( PscSim, PlannedTime,
		_MaybePscTaskInfo=undefined, State ).



% @doc Ensures that there is no planned presence task.
-spec ensure_no_planned_presence_transition( presence_simulation(),
				maybe( task_id() ), wooper:state() ) -> presence_simulation().
ensure_no_planned_presence_transition( PscSim, _MaybePscTaskInfo=undefined,
									   _State ) ->
	PscSim;

ensure_no_planned_presence_transition( PscSim, { PscTaskId, _TaskTime },
									   State ) ->
	?getAttr(scheduler_pid) ! { unregisterTaskAsync, [ PscTaskId ] },
	PscSim#presence_simulation{ presence_task_info=undefined }.



% @doc Ensures that a constant light is available from the specified current
% time: lights iff no daylight.
%
% Note that we just plan the next transition, not any next one that could be
% determined here.
%
-spec ensure_constant_light( time(), maybe( time() ), maybe( time() ),
	presence_simulation(), boolean(), maybe( task_info() ), wooper:state() ) ->
			presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence constant lighting
% needed:
ensure_constant_light( _CurrentTime, _MaybeDawnTime=undefined,
		_MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( LitPscSim, MaybePscTaskInfo, State );

% A dawn but no dusk: lights (only) until that dawn.
%
% Before dawn here:
ensure_constant_light( CurrentTime, DawnTime, _MaybeDuskTime=undefined, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
										State );

% After dawn here (implicit: CurrentTime >= DawnTime):
ensure_constant_light( _CurrentTime, _DawnTime, _MaybeDuskTime=undefined,
		PscSim, IsActivated, MaybePscTaskInfo, State ) ->
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( UnlitPscSim, MaybePscTaskInfo,
										   State );

% General case: a dawn and a dusk.
%
% Here before dawn, still in darkness, thus lighting until dawn:
ensure_constant_light( CurrentTime, DawnTime, _DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
										State );

% Here in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting until
% dusk:
%
ensure_constant_light( CurrentTime, _DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->
	% In daylight, thus no lighting until dusk:
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
										State );

% Here already past dusk (CurrentTime >= DuskTime), hence in darkness, thus
% lighting from now on:
%
ensure_constant_light( _CurrentTime, _DawnTime, _DuskTime, PscSim,
					   IsActivated, MaybePscTaskInfo, State ) ->
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_no_planned_presence_transition( LitPscSim, MaybePscTaskInfo, State ).




% @doc Ensures that light is available until the specified stop time, expected
% to be in the future of the specified current time.
%
% Note that we just plan the next transition, not any next one that could be
% determined here.
%
-spec ensure_light_until( time(), time(), maybe( time() ), maybe( time() ),
	presence_simulation(), boolean(), maybe( task_info() ), wooper:state() ) ->
			presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence lighting needed
% until stop time:
ensure_light_until( StopTime, _CurrentTime, _MaybeDawnTime=undefined,
		_MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, StopTime, MaybePscTaskInfo,
										State );

% A dawn but no dusk: lights (only) until that dawn.
%
% Before dawn here, stop does not matter yet:
ensure_light_until( _StopTime, CurrentTime, DawnTime, _MaybeDuskTime=undefined,
		PscSim, IsActivated, MaybePscTaskInfo, State )
								when CurrentTime < DawnTime ->
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
										State );

% After dawn here (implicit: CurrentTime >= DawnTime), no dusk, hence no
% lighting needed:
%
ensure_light_until( _StopTime, _CurrentTime, _DawnTime,
		_MaybeDuskTime=undefined, PscSim, IsActivated, MaybePscTaskInfo,
		State ) ->
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
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	UnlitTime = erlang:min( DawnTime, StopTime ),
	ensure_planned_presence_transition( LitPscSim, UnlitTime, MaybePscTaskInfo,
										State );

% Here we are in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting
% until dusk (and stop time does not matter):
%
ensure_light_until( _StopTime, CurrentTime, _DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->
	% In daylight, thus no lighting until dusk:
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
										State );

% Here we are already past dusk (CurrentTime >= DuskTime), hence in darkness,
% thus lighting from now on to the stop time (by design in the future):
%
ensure_light_until( StopTime, _CurrentTime, _DawnTime, _DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) ->
	LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( LitPscSim, StopTime, MaybePscTaskInfo,
										State ).



% @doc Ensures that light is available from the specified start time, expected
% to be in the future of the specified current time.
%
% Note that we just plan the next transition, not any next one that could be
% determined here.
%
-spec ensure_light_from( time(), time(), maybe( time() ), maybe( time() ),
	presence_simulation(), boolean(), maybe( task_info() ), wooper:state() ) ->
			presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence lighting (only)
% needed from that start time:
ensure_light_from( StartTime, _CurrentTime, _MaybeDawnTime=undefined,
		_MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->
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
			% A bit of lighting needed then:
			ensure_planned_presence_transition( UnlitPscSim, StartTime,
												MaybePscTaskInfo, State );

		false ->
			% Already daylight when starting, no dusk today, no lighting to
			% plan:
			%
			ensure_no_planned_presence_transition( UnlitPscSim,
												   MaybePscTaskInfo, State )

	end;

% We are after dawn here (implicit: CurrentTime >= DawnTime), no dusk, hence no
% lighting ever needed:
%
ensure_light_from( _StartTime, _CurrentTime, _DawnTime,
		_MaybeDuskTime=undefined, PscSim, IsActivated, MaybePscTaskInfo,
		State ) ->
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
			ensure_planned_presence_transition( UnlitPscSim, StartTime,
												MaybePscTaskInfo, State );

		false ->
			% Start to be doen already in daylight (StartTime>=DawnTime), next
			% event is switching on at the latest between dusk and the start
			% time:
			%
			LitTime = erlang:max( DuskTime, StartTime ),
			ensure_planned_presence_transition( UnlitPscSim, LitTime,
												MaybePscTaskInfo, State )

	end;

% Here in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting until
% dusk (and start time does not matter at this point):
%
ensure_light_from( _StartTime, CurrentTime, _DawnTime, DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->
	% In daylight, thus no lighting until at least dusk:
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
										State );

% Here already past dusk (CurrentTime >= DuskTime), hence in darkness, thus
% will light up at the start time (by design in the future):
%
ensure_light_from( StartTime, _CurrentTime, _DawnTime, _DuskTime, PscSim,
		IsActivated, MaybePscTaskInfo, State ) ->
	UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
	ensure_planned_presence_transition( UnlitPscSim, StartTime,
										MaybePscTaskInfo, State ).





% @doc Returns the relevent celestial times, once computed if necessary.
%
% If defined, celestial times are expected to be correct (deprecated times shall
% have been set to 'undefined').
%
-spec get_celestial_info( maybe( celestial_info() ), wooper:state() ) ->
								celestial_info().
get_celestial_info( _MaybeCelestialTimes=undefined, State ) ->
	resolve_logical_milestones( ?getAttr(server_location), State );

get_celestial_info( CelestialTimes, _State ) ->
	CelestialTimes.



% Computes (if possible) the actual dawn/dusk times.
%
% (helper)
%
-spec resolve_logical_milestones( maybe( position() ), wooper:state() ) ->
									celestial_info().
resolve_logical_milestones( _MaybeSrvLoc=undefined, State ) ->

	DefaultDawnTime = { 8, 15, 0 },
	DefaultDuskTime = { 19, 15, 0 },

	?warning_fmt( "No server location specified, logical milestones cannot be "
		"determined; using default deadlines: ~ts for dawn, ~ts for dusk. ",
		[ time_utils:time_to_string( DefaultDawnTime ),
		  time_utils:time_to_string( DefaultDuskTime ) ] ),

	{ erlang:date(), { DefaultDawnTime, DefaultDuskTime } };


resolve_logical_milestones( SrvLoc={ _Lat, _Long }, State ) ->

	Date = erlang:date(),

	% Temporary:
	DawnTime = { 8, 0, 0 },
	DuskTime = { 19, 0, 0 },

	?debug_fmt( "For the specified server location (~ts) and date, "
		"computed following deadlines: ~ts for dawn and ~ts for dusk. ",
		[ unit_utils:position_to_string( SrvLoc ),
		  time_utils:time_to_string( DawnTime ),
		  time_utils:time_to_string( DuskTime ) ] ),

	{ Date, { DawnTime, DuskTime } }.



% @doc Sends the specified telegram pair.
-spec send_telegram_pair( telegram_pair(), wooper:state() ) -> void().
send_telegram_pair( _Telegrams= { PressTelegram, ReleaseTelegram }, State ) ->

	OcSrvPid = ?getAttr(oc_srv_pid),

	oceanic:send( PressTelegram, OcSrvPid ),

	% Always better to separate telegrams:
	timer:sleep( _Ms=200 ),

	oceanic:send( ReleaseTelegram, OcSrvPid ).



% @doc Overridden destructor.
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



% Method section.


% @doc Requests the specified presence simulation to be updated, typically after
% a milestone, as planned through a scheduler.
%
-spec updatePresenceSimulation( wooper:state(), presence_sim_id() ) ->
											oneway_return().
updatePresenceSimulation( State, PscId ) ->

	PscTable = ?getAttr(presence_table),

	PscSim = table:get_value( _K=PscId, PscTable ),

	?debug_fmt( "Requested to update ~ts.",
				[ presence_simulation_to_string( PscSim ) ] ),

	% Newer scheduling possibly planned:
	{ UpdatedPscSim, MaybeCelestialInfo } = manage_presence_simulation( PscSim,
		erlang:time(), ?getAttr(celestial_info), State ),

	UpdatedPscTable = table:add_entry( PscId, UpdatedPscSim, PscTable ),

	UpdateState = setAttributes( State, [
		{ presence_table, UpdatedPscTable },
		{ celestial_info, MaybeCelestialInfo } ] ),

	wooper:return_state( UpdateState ).



% @doc Called whenever having to update presence programs, typically at midnight
% for this new day.
%
% See also the midnight_task_id attribute.
%
-spec updatePresencePrograms( wooper:state() ) -> oneway_return().
updatePresencePrograms( State ) ->

	PscTable = ?getAttr(presence_table),

	UpdatedState = case table:values( PscTable ) of

		[] ->
			case ?getAttr(midnight_task_id) of

				undefined ->
					?error( "Triggered whereas no presence simulation to "
						"update, moreover no midnight task is tracked." ),
					State;

				MidnightTaskId ->
					?warning_fmt( "Requested to update presence programs "
						"whereas no presence simulation is known; unscheduling "
						"these updates (task #~B).", [ MidnightTaskId ] ),

					?getAttr(scheduler_pid) !
						{ unregisterTaskAsync, [ MidnightTaskId ] },

					setAttribute( State, midnight_task_id, undefined )

			end;

		PscSims ->
			% Force a reevaluation thereof, once any past presence task is
			% cleared:

			SchedPid = ?getAttr(scheduler_pid),

			ClearedPscSims = [ clear_any_presence_task( Psc, SchedPid )
									|| Psc <- PscSims ],

			update_presence_simulations( ClearedPscSims, State )

	end,

	wooper:return_state( UpdatedState ).



% @doc Ensures that no past presence task lingers in the specified simulation.
-spec clear_any_presence_task( presence_simulation(), scheduler_pid() ) ->
											presence_simulation().
clear_any_presence_task( Psc=#presence_simulation{
					presence_task_info={ TaskId, _Time } }, SchedPid ) ->
	SchedPid ! { unregisterTaskAsync, [ TaskId ] },
	Psc#presence_simulation{ presence_task_info=undefined };

% presence_task_info expected to be already set to 'undefined':
clear_any_presence_task( Psc, _SchedPid ) ->
	Psc.



% Management of messages sent by Oceanic:


% @doc Handles a device event notified by the specified Oceanic server.
-spec onEnoceanEvent( wooper:state(), device_event(),
					  oceanic_server_pid() ) -> const_oneway_return().
onEnoceanEvent( State, Event, OcSrvPid ) when is_tuple( Event ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	cond_utils:if_defined( us_main_debug_home_automation,
		?debug_fmt( "Received (and ignored) the following device event "
			"from Oceanic server ~w: ~ts",
			[ OcSrvPid, oceanic:device_event_to_string( Event ) ] ) ),

	wooper:const_return();


onEnoceanEvent( State, OtherEvent, OcSrvPid ) ->

	?error_fmt( "Received an unexpected device event (~p) from ~w, "
				"ignoring it.", [ OtherEvent, OcSrvPid ] ),

	wooper:const_return().



% @doc Handles a possible jamming attempt, as suspected and reported by the
% specified Oceanic server.
%
-spec onEnoceanJamming( wooper:state(), bytes_per_second(),
						oceanic_server_pid() ) -> const_oneway_return().
onEnoceanJamming( State, TrafficLevel, OcSrvPid ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	?alert_fmt( "Received a notification from Oceanic server ~w of a "
		"possible jamming attempt (traffic level of ~B bytes per second).",
		[ OcSrvPid, TrafficLevel ] ),

	wooper:const_return().


% Later:
%
%-spec onEnoceanSensorLost( wooper:state(), device_info(),
%    oceanic_server_pid() ) -> const_oneway_return().





% @doc Callback triggered, if this server enabled the trapping of exits,
% whenever a linked process terminates.
%
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

	% Redundant information yet useful for console outputs:
	?warning_fmt( "US home automation server ~w received and ignored "
		"following exit message from ~w:~n  ~p",
		[ self(), CrashPid, ExitType ] ),

	wooper:const_return().




% Static subsection.


% @doc Returns the PID of the supposedly already-launched home automation
% server; waits for it if needed.
%
-spec get_home_automation_server() ->
			static_return( home_automation_server_pid() ).
get_home_automation_server() ->

	OcSrvPid = naming_utils:wait_for_registration_of(
		?us_main_home_automation_server_registration_name,
		naming_utils:registration_to_look_up_scope(
			?us_main_home_automation_server_registration_scope ) ),

	wooper:return_static( OcSrvPid ).



% Helper section.




% @doc Returns a textual description of this server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	OcSrvStr = case ?getAttr(oc_srv_pid) of

		undefined ->
			"not relying on an Oceanic server";

		OcSrvPid ->
			text_utils:format( "relying on its Oceanic server ~w "
				"(base identifier being EURID ~ts)",
				[ OcSrvPid, oceanic:eurid_to_string( ?getAttr(oc_base_id) ) ] )

	end,

	LocStr = case ?getAttr(server_location) of

		undefined ->
			"with no location defined";

		{ Lat, Long } ->
			% Degrees as raw floats rather than wtih °, minutes and al:
			text_utils:format( "located at latitude ~f degrees and "
				"longitude ~f degrees", [ Lat, Long ] )

	end,

	PscStr = case ?getAttr(presence_simulation_enabled) of

		true ->
			PscSims = table:values( ?getAttr(presence_simulations) ),
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
								|| PS <-PscSims ] ) ] )

			end;

		false ->
			"disabled"

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
		"gateway ~w, ~ts; the presence simulator is currently ~ts; ~ts",
		[ OcSrvStr, ?getAttr(us_config_server_pid), ?getAttr(scheduler_pid),
		  ?getAttr(comm_gateway_pid), LocStr, PscStr, MidTaskStr ] ).



% @doc Returns a textual description of the specified presence simulation
% internal record.
%
-spec presence_simulation_to_string( presence_simulation() ) -> ustring().
presence_simulation_to_string( #presence_simulation{
		id=Id,
		enabled=IsEnabled,
		activated=IsActivated,
		source_eurid=SourceEurid,
		target_eurid=TargetEurid,
		program=Program,
		activation_telegrams={ PressOn, ReleaseOn },
		deactivation_telegrams={ PressOff, ReleaseOff },
		presence_task_info=MaybeTaskInfo } ) ->

	PrgStr = case Program of

		constant_presence ->
			"constant presence";

		constant_absence ->
			"constant absence";

		[] ->
			"has no slot defined";

		[ SingleSlot ] ->
			text_utils:format( "has a single presence slot defined: ~ts",
							   [ slot_to_string( SingleSlot ) ] );

		Slots ->
			text_utils:format( "has ~B presence slots defined: ~ts",
				[ length( Slots ), text_utils:strings_to_listed_string(
					[ slot_to_string( S ) || S <- Slots ] ) ] )

	end,

	PscTaskStr = case MaybeTaskInfo of

		undefined ->
			"no presence task defined";

		{ PscTaskId, PscTime } ->
			text_utils:format( "presence task #~B scheduled at ~ts",
				[ PscTaskId, time_utils:time_to_string( PscTime ) ] )

	end,


	text_utils:format( "~ts presence simulation of id #~B, "
		"whose program is ~ts; "
		"its source EURID is ~ts and target EURID is ~ts "
		"(activation telegrams are ~ts for pressed, "
		"~ts for released, deactivation telegrams are ~ts for pressed, "
		"~ts for released); ~ts",
		[ case IsEnabled of
			true -> "enabled";
			false -> "disabled"
		  end ++ "and " ++ case IsActivated of
			true -> "activated";
			false -> "non-activated"
		  end, Id, PrgStr,
		  oceanic:eurid_to_string( SourceEurid ),
		  oceanic:eurid_to_string( TargetEurid ),
		  oceanic:telegram_to_string( PressOn ),
		  oceanic:telegram_to_string( ReleaseOn ),
		  oceanic:telegram_to_string( PressOff ),
		  oceanic:telegram_to_string( ReleaseOff ),
		  PscTaskStr ] ).



% @doc Returns a textual description of the specified presence slot.
-spec slot_to_string( presence_slot() ) -> ustring().
slot_to_string( _Slot={ StartTime, StopTime } ) ->
	text_utils:format( "from ~ts to ~ts", [
		time_utils:time_to_string( StartTime ),
		time_utils:time_to_string( StopTime ) ] ).
