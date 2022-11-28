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


% @doc US server in charge of the <b>providing house automation services</b>,
% based on Enocean, thanks to Ceylan-Oceanic.
%
-module(class_USHomeAutomationServer).


-define( class_description, "US server in charge of the providing house "
		 "automation services, based on Enocean, thanks to Ceylan-Oceanic " ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").



% Design notes:
%
% We rely here on Ceylan-Oceanic (https://oceanic.esperide.org/), which itself
% relies on our fork of erlang-serial
% (https://github.com/Olivier-Boudeville/erlang-serial).




% Implementation notes:


% This house automation server is designed to be able to integrate to an OTP
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


-type house_automation_server_pid() :: class_USServer:server_pid().


-export_type([ house_automation_server_pid/0 ]).


% Shorthands:

%-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type device_path() :: file_utils:device_path().

-type bytes_per_second() :: system_utils:bytes_per_second().

-type oceanic_server_pid() :: oceanic:oceanic_server_pid().

-type device_event() :: oceanic:device_event().


% The class-specific attributes:
-define( class_attributes, [

	{ oc_srv_pid, maybe( oceanic_server_pid() ),
	  "the PID of the Oceanic server (if any can exist) used by this server" },

	{ comm_gateway_pid, gateway_pid(),
	  "the PID of the US communication gateway used to send user "
	  "notifications" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.HouseAutomation" ).



% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").





% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this house automation server with an OTP
% supervision tree.


% @doc Starts and links a supervision bridge for the house automation system.
%
% Note: typically spawned as a supervised child of the US-Main root supervisor
% (see us_main_sup:init/1), hence generally triggered by the application
% initialisation.
%
-spec start_link() -> term().
start_link() ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the house automation system." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
		_Module=?MODULE, _InitArgs=[] ).



% @doc Callback to initialise this supervisor bridge, typically in answer to
% start_link/0 being executed.
%
-spec init( list() ) -> { 'ok', pid(), State :: term() } | 'ignore'
					  | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_bridge:info_fmt( "Initializing the US-Main supervisor bridge ~w for "
						   "the house automation system.", [ self() ] ),

	% Not specifically synchronous:
	HouseAutomSrvPid = ?MODULE:new_link(),

	{ ok, HouseAutomSrvPid, _InitialBridgeState=HouseAutomSrvPid }.



% @doc Callback to terminate this supervisor bridge.
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=HouseAutomSrvPid )
								when is_pid( HouseAutomSrvPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
		"the house automation system (reason: ~w, "
		"house automation server: ~w).", [ Reason, HouseAutomSrvPid ] ),

	% Synchronicity needed, otherwise a potential race condition exists, leading
	% this process to be killed by its OTP supervisor instead of being normally
	% stopped:
	%
	wooper:delete_synchronously_instance( HouseAutomSrvPid ),

	trace_bridge:debug_fmt( "US-Main house automation server ~w terminated.",
						   [ HouseAutomSrvPid ] ).




% Actual implementation of the house automation server.


% @doc Constructs an house automation server, based on the default, local TTY
% allocated to the USB Enocean gateway.
%
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
	construct( State, oceanic:get_default_tty_path() ).


% @doc Constructs an house automation server, based on the specified local TTY
% allocated to the USB Enocean gateway.
%
-spec construct( wooper:state(), device_path() ) -> wooper:state().
construct( State, TtyPath ) ->

	ServerTraceName = "House automation server",

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize(ServerTraceName),
		?us_main_house_automation_server_registration_name,
		?us_main_house_automation_server_registration_scope ),

	% Do not start Oceanic if it is bound to fail:
	MaybeOcSrvPid = case oceanic:is_available( TtyPath ) of

		{ true, _SerialRootDir } ->
			oceanic:start_link( TtyPath, _EventListenerPid=self() );

		{ false, ReasonStr, ErrorTerm } ->
			?send_warning_fmt( SrvState,
				"The Oceanic support will not be available. ~ts~n"
				"(error term: ~p).", [ ReasonStr, ErrorTerm ] ),
			undefined

	end,

	% To report any issue:
	CommGatewayPid = class_USCommunicationGateway:get_communication_gateway(),

	SetState = setAttributes( SrvState, [
		{ oc_srv_pid, MaybeOcSrvPid },
		{ comm_gateway_pid, CommGatewayPid } ] ),

	?send_notice( SetState, "Constructed: " ++ to_string( SetState ) ),

	SetState.



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

	cond_utils:if_defined( us_main_debug_house_automation,
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
-spec get_house_automation_server() ->
			static_return( house_automation_server_pid() ).
get_house_automation_server() ->

	OcSrvPid = naming_utils:wait_for_registration_of(
		?us_main_house_automation_server_registration_name,
		naming_utils:registration_to_look_up_scope(
			?us_main_house_automation_server_registration_scope ) ),

	wooper:return_static( OcSrvPid ).



% Helper section.



% @doc Returns a textual description of this server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	OcSrvStr = case ?getAttr(oc_srv_pid) of

		undefined ->
			"not relying on an Oceanic server";


		 OcSrvPid ->
			text_utils:format( "relying on its Oceanic server ~w",
							   [ OcSrvPid ] )

	end,

	text_utils:format( "US house automation server ~ts "
		"and using the communication gateway ~w",
		[ OcSrvStr, ?getAttr(comm_gateway_pid) ] ).
