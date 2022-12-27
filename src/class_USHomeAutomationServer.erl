% Copyright (C) 2022-2022 Olivier Boudeville
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

-type presence_simulation_setting() :: #presence_simulation_setting{}.
% Setting of a given instance of presence simulation.


-type presence_simulation_settings() ::
	{ 'default', MaybeTargetActuatorEurid :: maybe( eurid() ) }
  | [ presence_simulation_setting() ].
% All settings for a whole presence simulation service.


-type presence_milestone() ::
	time()  % A time in the day
  | 'dawn'  % First light of the day
  | 'dusk'. % Last light of the day
% Describes a start/stop logical moment to simulate presence.


-type presence_slot() :: { presence_milestone(), presence_milestone() }.
% A time slot during which a presence shall be simulated.


-type presence_program() :: [ { presence_milestone(), presence_milestone() } ]
						  | 'always_on'
						  | 'always_off'.
% A intra-day general program regarding the presence to simulate.


-type celestial_times() :: { Dawn :: time(), Dusk :: time() }.
% The time this day for dawn and dusk, at a given location.


-export_type([ home_automation_server_pid/0, presence_simulator_settings/0,
			   presence_milestone/0, presence_slot/0, celestial_times/0 ]).





% Local types:


-type presence_sim_id() :: count().
% Internal identifier of a presence simulation.


% Internal information regarding an instance of presence simulation:
-record( presence_simulation, {

	% The identifier of this presence simulation:
	id :: presence_sim_id(),

	% Tells whether this presence simulation is enabled:
	%
	% (for example when somebody is at home, it may be disabled)
	%
	enabled = true :: boolean(),

	% Tells whether the presence simulation is currently activated (avoids
	% sending unnecessary orders):
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


	% The program of this presence simulation, generally a
	% chronologically-ordered intra-day (from midnight to midnight) list of
	% presence slots:
	%
	program :: presence_program(),


	% The press/release telegrams to be emitted to switch the target actuator
	% (typically a smart plug) on:
	%
	activation_telegrams :: { Press :: telegram(), Release :: telegram() },

	% The press/release telegrams to be emitted to switch the target actuator
	% (typically a smart plug) off:
	%
	deactivation_telegrams :: { Press :: telegram(), Release :: telegram() },

	% The identifier (if any) of the currently-pending scheduling task declared
	% to manage that presence:
	%
	task_id :: maybe( task_id() )

 } ).



-type presence_table() :: table( presence_sim_id(), presence_simulation() )
% A table keeping track of the known presence simulations.


-type presence_action() :: 'start' | 'stop'.
% Tells whether a presence simulation shall be started or stopped.


% Shorthands:

-type count() :: basic_utils:count().
-type user_data() :: basic_utils:user_data().

-type ustring() :: text_utils:ustring().

-type device_path() :: file_utils:device_path().

-type bytes_per_second() :: system_utils:bytes_per_second().

-type time() :: time_utils:time().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().

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

	{ server_location, maybe( position() ),
	  "the (geographic) location, as a position, of the US-Main server" },

	{ celestial_times, maybe( celestial_times() ),
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
% It is planned from the current event to (only) the next.


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
	{ MaybeOcSrvPid, MaybeSrcEurid } = case oceanic:is_available( TtyPath ) of

		{ true, _SerialRootDir } ->
			OcPid = oceanic:start_link( TtyPath, _EventListenerPid=self() ),
			SourceId = case MaybeSourceEuridStr of

				undefined ->
					% Then go for the BaseId:
					oceanic:get_oceanic_eurid( OcPid );

				SourceId ->
					oceanic:string_to_eurid( SourceIdStr )

			end,
			{ OcPid, SourceId };

		{ false, ReasonStr, ErrorTerm } ->
			% No house automation can be done then:
			?send_warning_fmt( SrvState,
				"The Oceanic support will not be available. ~ts~n"
				"(error term: ~p).", [ ReasonStr, ErrorTerm ] ),
			{ undefined, undefined }

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

	{ InitPscTable, NextPscId } = init_presence_simulation(
		MaybePresenceSimSettings, MaybeOcSrvPid, MaybeSrcEurid, SchedPid,
		SrvState ),

	% Later:
	%AlarmState = init_alarm( MaybeOcSrvPid, PresenceState ),
	AlarmState = PresenceState,

	% To report any issue:
	CommGatewayPid = class_USCommunicationGateway:get_communication_gateway(),

	InitalPscEnabled = MaybePscSims =/= undefined,

	SetState = setAttributes( AlarmState, [
		{ oc_srv_pid, MaybeOcSrvPid },
		{ oc_base_id, MaybeBaseId },
		{ us_config_server_pid, UsMainCfgSrvPid },
		{ scheduler_pid, SchedPid },
		{ presence_simulation_enabled, InitalPscEnabled },
		{ presence_table, InitPscTable },
		{ next_presence_id, NextPscId },
		{ server_location, MaybeSrvLoc },
		{ celestial_times, undefined },
		{ comm_gateway_pid, CommGatewayPid } ] ),

	ApplyState = apply_presence_simulation( SetState ),

	?send_notice( ApplyState, "Constructed: " ++ to_string( ApplyState ) ),

	ApplyState.



% (helper)
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
	{ EmptyPscTable, InitPscId };

init_presence_simulation( PresenceSimSettings, _MaybeOcSrvPid=undefined,
		_MaybeSrcEurid, _PscTable, _NextPscId, State ) ->

	?error_fmt( "No Oceanic support available, the requested presence "
		"simulation (~p) cannot be performed.", [ PresenceSimSettings ] ),

	throw( { no_presence_simulation, no_oceanic } );


% Not expected to happen, as set with OcSrvPid:
init_presence_simulation( _PresenceSimSettings, _OcSrvPid,
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
	init_presence_simulation( _PresenceSimSettings=[ DefaultSettings ],
							  OcSrvPid, SrcEurid, PscTable, NextPscId, State );


% The "actual" clauses:
init_presence_simulation( _PresenceSimSettings=[], _OcSrvPid, _SrcEurid,
						  PscTable, NextPscId, _State ) ->
	{ PscTable, NextPscId };

% Main clause:
init_presence_simulation( _PresenceSimSettings=[
		#presence_simulation_setting{
			source_eurid=MaybeUserSrcEuridStr,
			target_eurid=MaybeTargetActuatorEuridStr,
			presence_program=Program,
			smart_lighting=SmartBool } | T ], OcSrvPid, SrcEurid, PscTable,
						  NextPscId ) ->

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


	% We check the program, not taking into account here dawn/dusk as they
	% change each day:
	%
	VettedSlots = vet_program( Program ),

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
		slots=VettedSlots,
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
vet_program( _PresenceProgram=always_on ) ->
	Midnight = { 0, 0, 0 },
	[ { Midnight, Midnight } ];

vet_program( _PresenceProgram=always_off ) ->
	[];

vet_program( PresenceProgram ) ->
	vet_program( PresenceProgram, _MaybeLastStop=undefined, _Acc=[] ).



% (helper)
vet_program( _PresenceProgram=[], _MaybeLastStop, Acc ) ->
	lists:reverse( Acc );


vet_program( _PresenceProgram=[ Slot={ StartMilestone, StopMilestone } | T ],
			 MaybeLastStop, Acc ) ->

	time_utils:is_time( StartMilestone )
		orelse throw( { invalid_slot_start_milestone, StartMilestone } ),

	time_utils:is_time( StopMilestone )
		orelse throw( { invalid_slot_stop_milestone, StopMilestone } ),

	StartMilestone < StopMilestone orelse
		throw( { inconsistent_slot_milestones, StartMilestone,
				 StopMilestone } ),

	% Term order tells undefined is lower than all tuples:
	MaybeLastStop =:= undefined orelse
		( MaybeLastStop < StartMilestone orelse
		  throw( { interleaved_slots, Slot, MaybeLastStop} ) ),

	vet_program( T, StopMilestone, [ Slot | Acc ] );

vet_program( Other, _MaybeLastStop, _Acc ) ->
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
					update_presence_simulations( PscSims,
						_NowTime=erlang:time(), ?getAttr(celestial_times),
						_EmptyTable=table:new(), State )

			end;

		false ->
			State

	end.



% @doc Updates, in turn and if necessary, the specified presence simulations.
-spec update_presence_simulations( [ presence_simulation() ], time(),
		maybe( celestial_times() ), presence_table(), wooper:state() ) ->
					wooper:state().
update_presence_simulations( _PscSims=[], _NowTime, _MaybeCelestialTimes,
							 PscTable, State ) ->
	setAttribute( State, presence_table, PscTable );

update_presence_simulations( _PscSims=[
		PscSim=#presence_simulation{ id=Id,
									 enabled=false } | T ], NowTime,
							 MaybeCelestialTimes, PscTable, State ) ->
	% Nothing done if disabled:
	NewPscTable = table:add_new_entry( _K=Id, PscSim, PscTable ),

	update_presence_simulations( T, NowTime, MaybeCelestialTimes,
								 NewPscTable, State );

% Thus enabled:
update_presence_simulations(
		_PscSims=[ PscSim=#presence_simulation{ id=Id } | T ], NowTime,
		MaybeCelestialTimes, PscTable, State ) ->

	{ StartedPscSim, CelestialTimes } = start_presence_simulation( PscSim,
		NowTime, MaybeCelestialTimes, State ),

	NewPscTable = table:add_new_entry( _K=Id, StartedPscSim, PscTable ),

	update_presence_simulations( T, NowTime, CelestialTimes, NewPscTable,
								 State ).



% @doc Starts the specified presence simulation, either at creation or when
% enabling it (at any moment).
%
-spec start_presence_simulation( presence_simulation(), time(),
	maybe( celestial_times() ), wooper:state() ) ->
		{ presence_simulation(), maybe( celestial_times() ) }.
start_presence_simulation( PscSim=#presence_simulation{
		enabled=false }, _NowTime, _MaybeCelestialTimes, _State ) ->
	throw( { not_enabled, PscSim } );

% From here, 'enabled' expected to be true:
start_presence_simulation( PscSim=#presence_simulation{
		activated=true }, _NowTime, _MaybeCelestialTimes, _State ) ->
	throw( { already_activated, PscSim } );

% From here, 'enabled' expected to be true, 'activated' to be false.
% Nothing to be done:
start_presence_simulation( PscSim=#presence_simulation{
		slots=[], task_id=undefined }, NowTime, MaybeCelestialTimes, State ) ->
	{ PscSim, MaybeCelestialTimes };

% Main clause:
start_presence_simulation( PscSim=#presence_simulation{
		slots=Slots,
		task_id=undefined }, NowTime, MaybeCelestialTimes, State ) ->

	case next_slot( NowTime, Slots ) of

		{ DoStart, NextEvent } ->

	SchedPid = ?getAttr(scheduler),

	SchedPid !

	TaskId =

	PscSim#presence_simulation{ activated=true,
								task_id=TaskId };




	LightNeeded = case in_slot( NowTime, Slots ) of

		true ->
			CelestialTimes = get_celestial_times( MaybeCelestialTimes
			LightingNeeded = case



	% Stops earlier if dawn happens soon enough:
	TMorningStop = erlang:min( TMorningFixedStop, DawnTime ),



	% Delays if dusk happens late enough:
	TEveningStart = erlang:max( TEveningFixedStart, DuskTime ),


	DefSlots =
		[ { TMorningStart, TMorningStop }, { TEveningStart, TEveningStop } ],


% Here task_id is not 'undefined':
start_presence_simulation( PscSim, _NowTime, _MaybeCelestialTimes, _State ) ->
	throw( { already_scheduled, PscSim } ).



% @doc Returns whether a presence shall be immediately started or stopped, and
% at what timestamp (if any) the next (opposite) action is to happen.
%
-spec next_slot( time(), [ presence_slot() ] ) ->
								{ maybe( presence_action() ), timestamp() }.
next_slot( NowTime, Slots ) ->
	next_slot( NowTime, Slots, Slots ).


% (helper)
next_slot( _NowTime, _Slots=[], OriginalSlots ) ->

next_slot( _NowTime, _Slots=[] ) ->

next_slot( NowTime, Slots ) ->

next_slot( NowTime, Slots ) ->

next_slot( NowTime, Slots ) ->


% Computes the actual dawn/dusk times.
%
% (helper)
resolve_logical_milestones( _MaybeSrvLoc=undefined, State ) ->

	DefaultDawnTime = { 8, 15, 0 },
	DefaultDuskTime = { 19, 15, 0 },

	?warning_fmt( "No server location specified, logical milestones cannot be "
		"determined; using default deadlines: ~ts for dawn, ~ts for dusk. ",
		[ time_utils:time_to_string( DefaultDawnTime ),
		  time_utils:time_to_string( DefaultDuskTime ) ] ),

	{ DefaultDawnTime, DefaultDuskTime };

resolve_logical_milestones( SrvLoc={ Lat, Long }, State ) ->
	% Temporary:
	DawnTime = { 8, 0, 0 },
	DuskTime = { 19, 0, 0 },

	?debug_fmt( "For the specified server location (~ts), "
		"computed following deadlines: ~ts for dawn and ~ts for dusk. ",
		[ unit_utils:position_to_string( SrvLoc ),
		  time_utils:time_to_string( DawnTime ),
		  time_utils:time_to_string( DuskTime ) ] ),

	{ DawnTime, Dusk }.



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


% Management of messages sent by Oceanic:


% @doc Handles a device event notified by the specified Oceanic server.
-spec onEnoceanEvent( wooper:state(), device_event(),
					  oceanic_server_pid() ) -> const_oneway_return().
onEnoceanEvent( State, Event, OcSrvPid ) when is_tuple( Event ) ->

	% Check:
	OcSrvPid = ?getAttr(oc_srv_pid),

	cond_utils:if_defined( us_main_debug_home_automation,
		?debug_fmt( "Received following device event from Oceanic "
			"server ~w: ~ts",
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



% @doc Called (typically by the scheduler) whenever a presence simulation shall
% be started.
%
-spec onPresenceSimulationToStart( wooper:state(), presence_sim_id() ) ->
											oneway_return().
onPresenceSimulationToStart( State, PscSimId ) ->


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


% @doc Returns a textual description of the specified presence simulation
% internal record.
%
-spec presence_simulation_to_string( presence_simulation() ) -> ustring().
presence_simulation_to_string( #presence_simulation{
		source_eurid=SourceEurid,
		target_eurid=TargetEurid,
		slots=Slots,
		activation_telegram={ OnPressTelegram, OnReleaseTelegram },
		deactivation_telegram={ OffPressTelegram, OffReleaseTelegram } } ) ->
	text_utils:format( "presence simulation from EURID ~ts to EURID ~ts "
		"during following slots: ~p "
		"(activation telegrams: '~ts' (press) and '~ts' (release); "
		"deactivation ones: '~ts' and '~ts')",
		[ oceanic:eurid_to_string( SourceEurid ),
		  oceanic:eurid_to_string( TargetEurid ), Slots,
		  oceanic:telegram_to_string( OnPressTelegram ),
		  oceanic:telegram_to_string( OnReleaseTelegram ),
		  oceanic:telegram_to_string( OffPressTelegram ),
		  oceanic:telegram_to_string( OffReleaseTelegram ) ] ).



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
			text_utils:format( "located at latitude ~ts degrees and "
							   "longitude ~ts degrees", [ Lat, Long ] )

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

	text_utils:format( "US home automation server ~ts, using the US-Main "
		"configuration server ~w, the scheduler ~w and the communication "
		"gateway ~w, ~ts; the presence simulator is currently ~ts",
		[ OcSrvStr, ?getAttr(us_config_server_pid), ?getAttr(scheduler_pid),
		  ?getAttr(comm_gateway_pid), LocStr, PscStr ] ).



% @doc Returns a textual description of the specified presence simulation.
-spec presence_simulation_to_string( presence_simulation() ) -> ustring().
presence_simulation_to_string( #presence_simulation{
		id=Id,
		enabled=IsEnabled,
		activated=IsActivated,
		source_eurid=SourceEurid,
		target_eurid=TargetEurid,
		slots=Slots,
		activation_telegrams={ PressOn, ReleaseOn },
		deactivation_telegrams={ PressOff, ReleaseOff } } ) ->

	SlotStr = case Slots of

		[] ->
			"yet with no slot defined";

		[ SingleSlot ] ->
			text_utils:format( "with a single slot defined: ~ts",
							   [ slot_to_string( SingleSlot ) ] );

		_ ->
			text_utils:format( "with ~B slots defined: ~ts",
				[ length( Slots ), text_utils:strings_to_listed_string(
					[ slot_to_string( S ) || S <- Slots ] ) ] )

	end,

	text_utils:format( "~ts presence of id #~B, whose source EURID is ~ts, "
		"target EURID is ~ts (activation telegrams are ~ts for pressed, "
		 "~ts for released, deactivation telegrams are ~ts for pressed, "
		 "~ts for released), during ~ts",
		[ case IsEnabled of
				true -> "enabled";
				false -> "disabled"
		  end ++ "and " ++ case IsActivated of
				true -> "activated";
				false -> "non-activated"
		  end, Id, SourceEurid, TargetEurid, PressOn, ReleaseOn,
		  PressOff, ReleaseOff, SlotStr ] ).



% @doc Returns a textual description of the specified presence slot.
-spec slot_to_string( slot() ) -> ustring().
slot_to_string( _Slot={ StartTime, StopTime } ) ->
	text_utils:format( "from ~ts to ~ts", [
		time_utils:time_to_string( StartTime ),
		time_utils:time_to_string( StopTime ) ] ).
