% Copyright (C) 2025-2025 Olivier Boudeville
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
% Creation date: Tuesday, August 12, 2025.

-module(class_USSecurityManager).

-moduledoc """
US server in charge of the **overall security of the local premises**.
""".



-define( class_description,
         "US server in charge of the overall security of the local premises" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").



% Design notes:
%




% Implementation notes:



% This security manager is designed to be able to integrate to an OTP
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



% Entries in the US-configuration files for security:

-define( us_main_security_key, security_management ).


-doc """
`DEfense Readiness CONdition` (DEFCON) corresponds to an alert level of US-Main,
in terms of security.

See [https://en.wikipedia.org/wiki/DEFCON] and appreciate
[https://en.wikipedia.org/wiki/WarGames].
""".
-type defcon() ::

   5  % No threat spotten (business as usual).

 | 4  % Increased vigilance requested (typically if not is at home).

 | 3  % Ongoing suspicious activity; hints of problem; "Get SAC on the line".

 | 2  % Immediate danger detected; on the verge of panic; things likely to
      % become ugly soon.

 | 1. % Established hostilities in progress; panic mode; triggers all flashes
      % and sirens, and hope for the best;
      % "All right. Flush the bombers. Get the subs in launch mode."



-doc "PID of a security manager.".
-type security_manager_pid() :: class_USServer:server_pid().

-export_type([ defcon/0, security_manager_pid/0]).




% The class-specific attributes:
%
% (no more server PIDs in state, as for an increased robustness, servers shall
% be resolved on the fly)
%
-define( class_attributes, [

	{ defcon, defcon(), "the current DEFCON" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Security" ).


% Exported helpers:
-export([ get_licit_config_keys/0, manage_configuration/2 ]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").




% Type shorthands:

-type ustring() :: text_utils:ustring().


% In state definition:

-type us_main_config_table() ::
    class_USMainCentralServer:us_main_config_table().




% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this security manager with an OTP supervision
% tree.


-doc """
Starts and links a supervision bridge for the security management.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see `us_main_sup:init/1`), hence generally triggered by the application
initialisation.
""".
-spec start_link() -> term().
start_link() ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the security management." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
		_Module=?MODULE, _InitArgs=[] ).



-doc """
Callback to initialise this supervisor bridge, typically in answer to
`start_link/0` being executed.
""".
-spec init( list() ) -> { 'ok', pid(), State :: term() }
                      | 'ignore' | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_bridge:info_fmt( "Initialising the US-Main supervisor bridge ~w for "
						   "the security management.", [ self() ] ),

	% Not specifically synchronous:
	SecurityManagerPid = ?MODULE:new_link(),

	{ ok, SecurityManagerPid, _InitialBridgeState=SecurityManagerPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=SecurityManagerPid )
								when is_pid( SecurityManagerPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
		"the security management (reason: ~w, security manager: ~w).",
		[ Reason, SecurityManagerPid ] ),

	% Synchronicity needed, otherwise a potential race condition exists, leading
	% this process to be killed by its OTP supervisor instead of being normally
	% stopped:
	%
	wooper:delete_synchronously_instance( SecurityManagerPid ),

	trace_bridge:debug_fmt( "US-Main security manager ~w terminated.",
						   [ SecurityManagerPid ] ).




% Actual implementation of the security manager.


-doc "Constructs a security manager.".
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize("Security manager"),
		?us_main_security_server_registration_name,
		?us_main_security_server_registration_scope ),

	InitSecurityState = init_security( SrvState ),

    % Start cool:
    SetState = setAttribute( InitSecurityState, defcon, 5 ),

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


% Base (non-action implementations) methods:

-doc "Returns the current DEFCON.".
-spec getDefcon( wooper:state() ) -> const_request_return( defcon() ).
getDefcon( State ) ->
    wooper:const_return_result( ?getAttr(defcon) ).


-doc "Sets the current DEFCON, and acts accordingly.".
-spec setDefcon( wooper:state(), defcon() ) -> oneway_return().
setDefcon( State, NewDefcon ) ->

    vet_defcon( NewDefcon ),
    PrevDefcon = ?getAttr(defcon),

    case NewDefcon of

        PrevDefcon ->
            ?warning_fmt( "Setting DEFCON to its already current level (~ts), "
                          "nothing done.", [ describe_defcon( NewDefcon ) ] );

        1 ->
            ?emergency_fmt( "Setting the worst DEFCON, 1: ~ts (was: ~B). "
                "This is WWIII baby.", [ defcon_to_string( 1 ), PrevDefcon ] );

        5 ->
            ?notice_fmt( "Restoring the best DEFCON, 5: ~ts (was: ~B).",
                         [ defcon_to_string( 5 ), PrevDefcon ] );

        % Preferring not flagging them as errors:

        NewDefcon when NewDefcon < PrevDefcon ->
            ?warning_fmt( "Aggravating DEFCON, from ~B to ~B, which is ~ts. "
                "Increasing countermeasures.", [ PrevDefcon, NewDefcon,
                    defcon_to_string( NewDefcon ) ] );

        % _ -> would have sufficed
        NewDefcon when NewDefcon > PrevDefcon ->
            ?warning_fmt( "Improving DEFCON, from ~B to ~B, which is ~ts. "
                "Loosening countermeasures.", [ PrevDefcon, NewDefcon,
                    defcon_to_string( NewDefcon ) ] )

    end,

     SetState = setAttribute( State, defcon, NewDefcon ),

     wooper:return_state( SetState ).


-doc """
Callback triggered, if this server enabled the trapping of EXIT messages,
whenever a linked process terminates.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
		basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StoppedPid, _ExitType=normal ) ->
	?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
	wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%   {wooper_oneway_failed,<0.44.0>,class_XXX,
	%      FunName,Arity,Args,AtomCause}}, [...]}"

	% Redundant information yet useful for console outputs:
	?warning_fmt( "US Security Manager ~w received and ignored following exit "
				  "message from ~w:~n  ~p", [ self(), CrashPid, ExitType ] ),

	wooper:const_return().



% Action implementation (see init_security/1).
%
% We use the recommended basic_utils:fallible*/* and akin conventions here.


-doc "Returns a description of the current DEFCON (see the `defcon` action).".
-spec getDefconAction( wooper:state() ) ->
                                const_request_return( successful( ustring() ) ).
% As getDefcon/1 already exists:
getDefconAction( State ) ->

    Str = text_utils:format(
        "Current alert level is ~ts. Trigger the set_defcon/1 "
        "(or panic/0 or peace/0) actions to change it.",
        [ describe_defcon( ?getAttr(defcon) ) ] ),

    wooper:const_return_result( { ok, Str } ).



-doc "Sets the specified DEFCON (see the `set_defcon` action).".
% As setDefcon/2 already exists:
-spec setDefconAction( wooper:state(), defcon() ) ->
                                request_return( string_fallible() ).
setDefconAction( State, NewDefcon ) ->

    case is_defcon( NewDefcon ) of

        true ->
            SetState = executeOneway( State, setDefcon, NewDefcon ),

            Str = text_utils:format( "Alert level set to ~ts, knowing that the "
                "previous one was ~ts. Consider triggering the panic/0 or "
                "peace/0 actions if needed.",
                [ describe_defcon( NewDefcon ),
                  describe_defcon( ?getAttr(defcon) ) ] ),

            wooper:return_state_result( SetState, { ok, Str } );

        false ->
             Str = text_utils:format( "Invalid DEFCON specified ('~p'), hence "
                                      "not changed.", [ NewDefcon ] ),
            wooper:const_return_result( { error, Str } )

    end.



-doc """
Sets the panic mode: goes immediately to the worst DEFCON (see the `panic`
action).
""".
-spec panic( wooper:state() ) -> request_return( successful( ustring() ) ).
panic( State ) ->

    NewDefcon = 1,

    Str = text_utils:format( "Panic declared: setting immediately the worst "
        "alert level, ~ts. Trigger the peace/0 or set_defcon/1 actions "
        "to stop. Previous alert level was ~ts.",
        [ describe_defcon( NewDefcon ), describe_defcon( ?getAttr(defcon) ) ] ),

    SetState = setAttribute( State, defcon, NewDefcon ),

    wooper:return_state_result( SetState, { ok, Str } ).



-doc """
Sets the peace mode: goes immediately to the quietest DEFCON (see the `peace`
action).
""".
-spec peace( wooper:state() ) -> request_return( successful( ustring() ) ).
peace( State ) ->

    NewDefcon = 5,

    Str = text_utils:format( "Peace declared: setting immediately the quietest "
        "alert level, ~ts. Trigger the set_defcon/1 or panic/0 actions "
        "to aggravate. Previous alert level was ~ts.",
        [ describe_defcon( NewDefcon ), describe_defcon( ?getAttr(defcon) ) ] ),

    SetState = setAttribute( State, defcon, NewDefcon ),

    wooper:return_state_result( SetState, { ok, Str } ).




% Static subsection.


-doc """
Returns the PID of the current, supposedly already-launched, security manager,
waiting (up to a few seconds, as all US servers are bound to be launched mostly
simultaneously) if needed.

It is better to obtain the PID of a server each time from the naming service
rather than to resolve and store its PID once for all, as, for an increased
robustness, servers may be restarted (hence any stored PID may not reference a
live process anymore).
""".
-spec get_server_pid () -> static_return( security_manager_pid() ).
get_server_pid() ->

	ManagerPid = class_USServer:resolve_server_pid(
        _RegName=?us_main_security_server_registration_name,
        _RegScope=?us_main_security_server_registration_scope ),

	wooper:return_static( ManagerPid ).




% Helper section.


-doc "Initialises the security management.".
-spec init_security( wooper:state() ) -> wooper:state().
init_security( State ) ->

    % We just define our action table; inherited
    % class_USServer:notifyAutomatedActions/2 will manage them automatically, so
    % that the central server can request and get them (asynchronously).

    UserActSpecs = [

        % As the terseness of action description is key:
        { _ActName=defcon, _Desc="returns the current DEFCON",
          _ReqName=getDefconAction },

        % Could/should be defcon():
        { set_defcon, "sets the current DEFCON", setDefconAction,
          [ { dynamic, new_defcon, "integer()" } ] },

        { panic, "sets immediately the worst DEFCON (1)" },

        { peace, "sets immediately the quietest DEFCON (5)" } ],

    ActionTable = us_action:register_action_specs( UserActSpecs,
        ?getAttr(action_table), wooper:get_classname( State ) ),

    setAttribute( State, action_table, ActionTable ).



-doc "Tells whether the specified term is a valid DEFCON.".
-spec is_defcon( term() ) -> boolean().
is_defcon( Defcon ) when is_integer( Defcon)
                         andalso Defcon >= 1 andalso Defcon =< 5 ->
    true;

is_defcon( _InvDefcon ) ->
    false.



-doc "Vets the specified DEFCON.".
-spec vet_defcon( term() ) -> defcon().
vet_defcon( Defcon )  ->
    case is_defcon( Defcon ) of

        true ->
            Defcon;

        false ->
            throw( { invalid_defcon, Defcon } )

    end.



% Section related to the US-Main configuration files.


-doc """
Returns the known security-related keys in the US-Main configuration files.
""".
-spec get_licit_config_keys() -> [ list_table:key() ].
get_licit_config_keys() ->
	[ ?us_main_security_key ].


-doc """
Handles the security-related entries in the user settings specified in US-Main
configuration files.

Note that the specified state is the one of a US-Main configuration server.
""".
-spec manage_configuration( us_main_config_table(), wooper:state() ) ->
										wooper:state().
manage_configuration( ConfigTable, State ) ->

	case table:lookup_entry( ?us_main_security_key, ConfigTable ) of

		key_not_found ->
			?info( "No user settings regarding security." ),
			[];

		{ value, SecuritySettings } ->
			?warning_fmt( "Ignoring security settings ~p'.",
                          [ SecuritySettings ] )

	end,

	% Not specifically checked at this level, will be done by the security
	% manager:
	%
    State.


-doc "Returns a textual description of the specified DEFCON.".
-spec defcon_to_string( defcon() ) -> ustring().
defcon_to_string( _Defcon=5 ) ->
    "no threat spotten";

defcon_to_string( _Defcon=4 ) ->
    "increased vigilance requested";

defcon_to_string( _Defcon=3 ) ->
    "ongoing suspicious activity";

defcon_to_string( _Defcon=2 ) ->
    "immediate danger detected";

defcon_to_string( _Defcon=1 ) ->
    "established hostilities in progress".


-doc "Returns a full textual description of the specified DEFCON.".
-spec describe_defcon( defcon() ) -> ustring().
describe_defcon( Defcon ) ->
    text_utils:format( "DEFCON ~B (~ts)",
                       [ Defcon, defcon_to_string( Defcon ) ] ).


-doc "Returns a textual description of this manager.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
    text_utils:format( "US security manager in ~ts",
                       [ describe_defcon( ?getAttr(defcon) ) ] ).
