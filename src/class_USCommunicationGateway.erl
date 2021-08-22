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
% Creation date: Sunday, August 8, 2021.


% @doc US server in charge of <b>managing the communication of the US
% infrastructure</b>, typically with associated users, through emails and/or
% SMS.
%
-module(class_USCommunicationGateway).


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
% The SMS support relies on Ceylan-Mobile (see mobile.esperide.org).


% Implementation notes:
%


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


-type gateway_pid() :: class_USServer:server_pid().
% The PID of a communication gateway.




% Shorthands:

%-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
%-type bin_string() :: text_utils:bin_string().

%-type bin_mobile_number() :: mobile:bin_mobile_number().

%-type bin_address() :: email_utils:bin_address().

%-type directory_pid() :: class_USContactDirectory:directory_pid().


%-type scheduler_pid() :: class_USScheduler:scheduler_pid().
%-type task_id() :: class_USScheduler:task_id().



% The class-specific attributes:
-define( class_attributes, [

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" },

	{ contact_directory_pid, maybe( directory_pid() ),
	  "the PID of any associated contact directory" }

 ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Communication" ).


% Exported helpers:
-export([]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").


% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this communication gateway with an OTP
% supervision tree.


% @doc Starts and links a supervision bridge for the communication gateway.
%
% Note: typically spawned as a supervised child of the US-Main root supervisor
% (see us_main_sup:init/1), hence generally triggered by the application
% initialisation.
%
-spec start_link() -> term().
start_link() ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the communication gateway." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
								  _Module=?MODULE, _InitArgs=[] ).



% @doc Callback to initialise this supervisor bridge, typically in answer to
% start_link/0 being executed.
%
-spec init( list() ) -> { 'ok', pid(), State :: term() }
							| 'ignore' | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_bridge:info_fmt( "Initializing the US-Main supervisor bridge ~w for "
						   "the communication gateway.", [ self() ] ),

	% Not specifically synchronous:
	CommGatewayPid = ?MODULE:new_link(),

	{ ok, CommGatewayPid, _InitialBridgeState=CommGatewayPid }.



% @doc Callback to terminate this supervisor bridge.
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=CommGatewayPid )
  when is_pid( CommGatewayPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
		"the communication gateway (reason: ~w, communication gateway: ~w).",
		[ Reason, CommGatewayPid ] ),

	% No synchronicity especially needed:
	CommGatewayPid ! delete.



% Actual implementation of the communication gateway.


% @doc Constructs a communication gateway.
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

	?send_notice( InitCommState,
				  "Constructed: " ++ to_string( InitCommState ) ),

	InitCommState.



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug_fmt( "Deletion initiated, while state is: ~ts.",
				[ to_string( State ) ] ),

	?info( "Deleted." ),
	State.



% Method section.



% @doc Callback triggered, if this server enabled the trapping of exits,
% whenever a linked process terminates.
%
-spec onWOOPERExitReceived( wooper:state(), pid(),
		basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StoppedPid, _ExitType=normal ) ->
	?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
	wooper:const_return();

onWOOPERExitReceived( State, CrashedPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%						{wooper_oneway_failed,<0.44.0>,class_XXX,
	%							FunName,Arity,Args,AtomCause}}, [...]}"

	% Redundant information yet useful for console outputs:
	?warning_fmt( "US Communication Gateway  ~w received and ignored "
		"following exit message from ~w:~n  ~p",
		[ self(), CrashedPid, ExitType ] ),

	wooper:const_return().




% Static subsection.


% @doc Returns the PID of the supposedly already-launched communication gateway;
% waits for it if needed.
%
-spec get_communication_gateway() -> static_return( gateway_pid() ).
get_communication_gateway() ->

	GatewayPid = naming_utils:wait_for_registration_of(
		?us_main_communication_server_registration_name,
		naming_utils:registration_to_look_up_scope(
			?us_main_communication_server_registration_scope ) ),

	wooper:return_static( GatewayPid ).



% Helper section.


% @doc Initialises the communication gateway.
-spec init_communications( wooper:state() ) ->  wooper:state().
init_communications( State ) ->
	setAttribute( State, contact_directory_pid, undefined ).



% @doc Returns a textual description of this gateway.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ContactStr = case ?getAttr(contact_directory_pid) of

		undefined ->
			"not knowing a contact directory";

		false ->
			text_utils:format( "knowing the contact directory ~w",
							   [ ?getAttr(contact_directory_pid) ] )

	end,

	text_utils:format( "US communication gateway, ~ts",
					   [ ContactStr ] ).
