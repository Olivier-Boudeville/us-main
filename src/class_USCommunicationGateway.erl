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
% Creation date: Sunday, August 8, 2021.

-module(class_USCommunicationGateway).

-moduledoc """
US server in charge of **managing the communication of the US infrastructure**,
typically with associated users, through emails and/or SMS.

Such gateways rely on a contact directory.
""".



-define( class_description, "US server in charge of managing the "
	"communication of the US infrastructure, typically with "
	"associated users." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").



% Design notes:
%
% User information generally come from an associated contact directory (see
% class_USContactDirectory).
%
% The SMS support relies on Ceylan-Mobile (see http://mobile.esperide.org).


% Each US-Main instance is expected to have its own, local communication
% gateway, even if it may be in link with another one of such instance
% concentrating more communication solutions (such as SMS sending).

% This seems to be a better scheme than having only one overall, global
% communication gateway: each US-Main instance is more autonomous and for
% example can at least log messages even in the absence of a SMS-sending
% communication gateway.


% This communication gateway is designed to be able to integrate to an OTP
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


-doc "The PID of a communication gateway.".
-type gateway_pid() :: class_USServer:server_pid().



% Type shorthands:

-type ustring() :: text_utils:ustring().

%-type directory_pid() :: class_USContactDirectory:directory_pid().



% The class-specific attributes:
-define( class_attributes, [

	{ us_main_config_server_pid, server_pid(),
	  "the PID of the US-Main configuration server" },

	{ contact_directory_pid, option( directory_pid() ),
	  "the PID of any associated contact directory" },

	{ sms_support_operational, boolean(), "tells whether an actual SMS "
	  "support is enabled and operational, notably to be able to send them" },

	{ parent_comm_gateway_pid, option( gateway_pid() ),
	  "the PID of any parent, authoritative gateway (e.g. able to send SMS)" }

						   ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Communication" ).



% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").


% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this communication gateway with an OTP
% supervision tree.


-doc """
Starts and links a supervision bridge for the communication gateway.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see us_main_sup:init/1), hence generally triggered by the application
initialisation.
""".
-spec start_link() -> term().
start_link() ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the communication gateway." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
								  _Module=?MODULE, _InitArgs=[] ).



-doc """
Callback to initialise this supervisor bridge, typically in answer to
start_link/0 above being executed.
""".
-spec init( [] ) -> { 'ok', pid(), State :: term() }
					| 'ignore' | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_bridge:info_fmt( "Initialising the US-Main supervisor bridge ~w for "
						   "the communication gateway.", [ self() ] ),

	% Not specifically synchronous:
	CommGatewayPid = ?MODULE:new_link(),

	{ ok, CommGatewayPid, _InitialBridgeState=CommGatewayPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=CommGatewayPid )
								when is_pid( CommGatewayPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
		"the communication gateway (reason: ~w, communication gateway: ~w).",
		[ Reason, CommGatewayPid ] ),

	% Synchronicity needed, otherwise a potential race condition exists, leading
	% this process to be killed by its OTP supervisor instead of being normally
	% stopped:
	%
	wooper:delete_synchronously_instance( CommGatewayPid ),

	trace_bridge:debug_fmt( "US-Main communication gateway ~w terminated.",
							[ CommGatewayPid ] ).



% Actual implementation of the communication gateway.


-doc "Constructs a communication gateway.".
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother classes, then this class-specific actions:
	% (traps EXITs)
	%
	SrvState = class_USServer:construct( State,
		?trace_categorize("Communication gateway"),
		?us_main_communication_server_registration_name,
		?us_main_communication_server_registration_scope ),

	InitCommState = init_communications( SrvState ),

	USMainCfgServerPid = class_USMainConfigServer:get_us_main_config_server(),

	SetState = setAttributes( InitCommState, [
		{ us_main_config_server_pid, USMainCfgServerPid },
		{ parent_comm_gateway_pid, undefined } ] ),

	?send_notice_fmt( SetState, "Constructed: ~ts.",
					  [ to_string( SetState ) ] ),

	SetState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug_fmt( "Deletion initiated, while state is: ~ts.",
				[ to_string( State ) ] ),

	?info( "Deleted." ),
	State.



% Method section.



-doc """
Callback triggered, if this server enabled the trapping of exits, whenever a
linked process terminates.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
		basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StoppedPid, _ExitType=normal ) ->
	?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
	wooper:const_return();

onWOOPERExitReceived( State, CrashedPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%   {wooper_oneway_failed,<0.44.0>,class_XXX,
	%       FunName,Arity,Args,AtomCause}}, [...]}"

	% Redundant information yet useful for console outputs:
	?warning_fmt( "US Communication Gateway  ~w received and ignored "
		"following exit message from ~w:~n  ~p",
		[ self(), CrashedPid, ExitType ] ),

	wooper:const_return().




% Static subsection.


-doc """
Returns the PID of the supposedly already-launched communication gateway; waits
for it if needed.
""".
-spec get_communication_gateway() -> static_return( gateway_pid() ).
get_communication_gateway() ->

	GatewayPid = naming_utils:wait_for_registration_of(
		?us_main_communication_server_registration_name,
		naming_utils:registration_to_lookup_scope(
			?us_main_communication_server_registration_scope ) ),

	wooper:return_static( GatewayPid ).




% Helper section.


-doc "Initialises the communication gateway.".
-spec init_communications( wooper:state() ) ->  wooper:state().
init_communications( State ) ->

	mobile:start(),

	?debug( "Testing whether a usable Ceylan-Mobile exists." ),

	SMSSupportEnabled = case mobile:is_available() of

		true ->

			case mobile:has_actual_device() of

				true ->

					% Also an early test that Mobile is available and functional
					% indeed:
					%
					MobInfo = mobile:get_textual_information(),

					?info_fmt( "SMS communication via Ceylan-Mobile "
						"initialised (available, and reported as connected to "
						"an actual device); mobile information: ~ts.",
						[ MobInfo ] ),

					true;

				false ->
					?warning( "No SMS communication will be available: "
						"Ceylan-Mobile found operational yet not connected "
						"to an actual device (just emulated)." ),
					mobile:stop(),
					false

			end;


		false ->
			?info( "No SMS communication will be available, no Ceylan-Mobile "
				   "found available." ),
			mobile:stop(),
			false

	end,

	% Needing the contact directory for operation, done as late as possible:
	ContactDirPid = class_USContactDirectory:get_contact_directory(),

	setAttributes( State, [ { contact_directory_pid, ContactDirPid },
							{ sms_support_enabled, SMSSupportEnabled } ] ).



-doc "Returns a textual description of this gateway.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ContactStr = case ?getAttr(contact_directory_pid) of

		undefined ->
			"not knowing a contact directory";

		ContactDirPid ->
			text_utils:format( "knowing the contact directory ~w",
							   [ ContactDirPid ] )

	end,

	CfgStr = case ?getAttr(us_main_config_server_pid) of

		% Would be surprising:
		undefined ->
			"not knowing a US-Main configuration server";

		CfgSrvPid ->
			text_utils:format( "knowing the US-Main configuration server ~w",
							   [ CfgSrvPid ] )

	end,

	ParentGatewayStr = case ?getAttr(parent_comm_gateway_pid) of

		undefined ->
			"not registering a parent communication gateway";

		GtwPid ->
			text_utils:format( "registering parent communication gateway ~w",
							   [ GtwPid ] )

	end,

	text_utils:format( "US communication gateway, ~ts, ~ts, ~ts",
					   [ ContactStr, CfgStr, ParentGatewayStr ] ).
