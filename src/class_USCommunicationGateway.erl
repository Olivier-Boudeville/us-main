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
% At least currently, "user message" means SMS.
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


-doc "Settings gathered for the communication server.".
-type communication_settings() ::
    { NotifiedBinNumbers :: [ bin_phone_number() ],
      MasterBinNumbers :: [ bin_phone_number() ] }.


-doc "The PID of a communication gateway.".
-type gateway_pid() :: class_USServer:server_pid().

-type polling_delay() :: time_utils:milliseconds().


-export_type([ communication_settings/0, gateway_pid/0, polling_delay/0 ]).



% Default delays, in milliseconds:


-define( sms_polling_min_delay, 500 ).



% For testing:
-define( sms_polling_max_delay, 2000 ).

% Operationally:
%-define( sms_polling_max_delay, 5000 ).



% Entries in the US-configuration files for communication:

-define( us_main_notified_numbers_key, communication_mobile_notified_numbers ).
-define( us_main_master_numbers_key,   communication_mobile_master_numbers ).




% The class-specific attributes:
%
% (now, for a better robustness, servers are resolved on the fly, their PIDs are
% not to be stored anymore)
%
-define( class_attributes, [

    { sms_support_active, boolean(), "tells whether an actual SMS "
      "support is active (enabled and operational)" },

    { sms_polling_delay, polling_delay(), "the current delay between two "
      "pollings of the SMSs" },

    { sms_polling_min_delay, polling_delay(), "the minimum delay between two "
      "pollings of the SMSs" },

    { sms_polling_max_delay, polling_delay(), "the maximum delay between two "
      "pollings of the SMSs" },

    { notified_phone_numbers, [ bin_phone_number() ], "the phone numbers that, "
      "through SMSs, shall be notified of the major server-side events" },

    { master_phone_numbers, [ bin_phone_number() ], "the phone numbers that, "
      "through SMSs, can control the overall US-Main instance" },

    { parent_comm_gateway_lookup_info, option( lookup_info() ),
      "the lookup naming information  of any parent, authoritative gateway "
      "(e.g. able to send SMSs)" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Communication" ).

% Exported helpers:
-export([ get_licit_config_keys/0, manage_configuration/2 ]).



% Note: include order matters.

% For the received_sms record:
-include_lib("mobile/include/mobile.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").




% Implementation notes
%
% As the polling frequency od SMS is adaptative (it is set to high as soon as a
% SMS is received, and gradually increases back to its minimal frequency), this
% server does not rely on the US scheduler, but takes care directly of this
% polling.



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().

-type trace_format() :: text_utils:trace_format().
-type trace_values() :: text_utils:trace_values().

-type trace_severity() :: trace_utils:trace_severity().
-type trace_message() :: trace_utils: trace_message().

-type bin_phone_number() :: sms_utils:bin_phone_number().

-type bin_mobile_number() :: mobile:bin_mobile_number().
-type bin_sms_message() :: mobile:bin_sms_message().
-type received_sms() :: mobile:received_sms().

-type us_main_config_table() ::
    class_USMainCentralServer:us_main_config_table().


%-type directory_pid() :: class_USContactDirectory:directory_pid().



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

    USMainCtrSrvPid = class_USMainCentralServer:get_server_pid(),

    % Blocking; beware of not creating deadlocks that way:
    USMainCtrSrvPid ! { getCommunicationSettings, [], self() },

    InitCommState = init_communications( SrvState ),

    % Interleaving of call to getCommunicationSettings/1:
    { NotifiedBinPhoneNumbers, MasterBinPhoneNumbers } = receive

        % Already checked by the US-Main configuration server to be a list:
        { wooper_result,
          CommSettings={ _NotifiedBinPhoneNumbers, _MasterBinPhoneNumbers } } ->
            CommSettings

    end,

    SetState = setAttributes( InitCommState, [
        % (sms_support_active already set)
        { sms_polling_delay, ?sms_polling_max_delay }, % Start nominal
        { sms_polling_min_delay, ?sms_polling_min_delay },
        { sms_polling_max_delay, ?sms_polling_max_delay },
        { notified_phone_numbers, NotifiedBinPhoneNumbers },
        { master_phone_numbers, MasterBinPhoneNumbers },
        { parent_comm_gateway_lookup_info, undefined } ] ),

    % Starts the SMS polling mechanism if relevant:
    getAttribute( SetState, sms_support_active ) =:= true andalso
         case NotifiedBinPhoneNumbers of

            [] ->
                ?send_notice( SetState, "SMS support active, yet with "
                    "no phone number to notify defined; SMS-based "
                    "server-side notifications disabled." );

            _ ->
                ?send_info_fmt( SetState, "SMS support active, with "
                    "on the following phone numbers to notify: ~ts.",
                    [ text_utils:strings_to_listed_string(
                        NotifiedBinPhoneNumbers ) ] )

        end,

         case MasterBinPhoneNumbers of

            [] ->
                ?send_notice( SetState, "SMS support active, yet with "
                    "no master phone number defined; SMS-based "
                    "actions disabled." );

            _ ->
                ?send_info_fmt( SetState, "SMS support active, with "
                    "the following master phone numbers defined: ~ts.",
                    [ text_utils:strings_to_listed_string(
                        MasterBinPhoneNumbers ) ] ),
                 self() ! pollSMS

        end,

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


-doc "Notifies by SMS the recipients of interest of the specified message.".
-spec notifyBySMS( wooper:state(), any_string() ) -> const_oneway_return().
notifyBySMS( State, Msg ) ->

    case ?getAttr(sms_support_active) of

        true ->
            case ?getAttr(notified_phone_numbers) of

                [] ->
                    cond_utils:if_defined( us_main_debug_sms_communication,
                        ?debug_fmt( "No registered phone numbers to be "
                                    "notified of '~ts'.", [ Msg ] ), ok );

                RecipientPhoneNumbers ->

                    cond_utils:if_defined( us_main_debug_sms_communication,
                        ?debug_fmt( "Sending a SMS notification to ~p, "
                            "for the following message:~n ~ts",
                            [ RecipientPhoneNumbers, BinMsg ] ) ),

                    Reports = mobile:send_sms_multi( Msg,
                                                     RecipientPhoneNumbers ),

                    case mobile:detect_failed_sendings( Reports,
                            RecipientPhoneNumbers ) of

                        [] ->
                            ok;

                        FailedPhoneNumbers ->
                            ?error_fmt( "The sending of the SMS notification "
                                "'~ts' failed for the following recipient "
                                "mobile phone numbers: ~ts.", [ Msg,
                                    text_utils:strings_to_listed_string(
                                        FailedPhoneNumbers ) ] )

                    end

            end;


        false ->
            cond_utils:if_defined( us_main_debug_sms_communication,
                ?debug_fmt( "No active SMS support, notification '~ts' "
                            "not sent.", [ Msg ] ), ok )

    end,

    wooper:const_return().



-doc "Polls adaptatively any incoming SMSs, to trigger associated actions.".
-spec pollSMS( wooper:state() ) -> oneway_return().
pollSMS( State ) ->

    cond_utils:if_defined( us_main_debug_sms_communication,
        ?debug( "Polling now for action-related SMSs." ) ),

    { NewDelay, NewState } = case process_sms_messages(
            ?getAttr(master_phone_numbers), State ) of

        % As soon as a relevant SMS is received, we switch to the most frequent
        % "interactive" polling:
        %
        { _RelevantSMSRead=true, ReadState } ->
            % Resetting delay:
            { ?getAttr(sms_polling_min_delay), ReadState };

        { _RelevantSMSRead=false, ReadState } ->
            % Ramping up delay slowly:
            MaxDelay = ?getAttr(sms_polling_max_delay),
            AugmentedDelay = ?getAttr(sms_polling_delay) + 10,
            RetainedDelay = case AugmentedDelay > MaxDelay of

                true ->
                    MaxDelay;

                false ->
                    AugmentedDelay

            end,

            { RetainedDelay, ReadState }

    end,

    cond_utils:if_defined( us_main_debug_sms_communication, ?debug_fmt(
        "Next SMS polling in ~B ms.", [ NewDelay ] ) ),

    % Thus a delayed, recursive call:
    case timer:send_after( _DurMs=NewDelay, _Msg=pollSMS ) of

        { ok, _TRef } ->
            ok;

        { error, Reason } ->
            ?error_fmt( "Failed to register a SMS polling delayed "
                "of ~ts : ~p.",
                [ time_utils:duration_to_string( NewDelay ), Reason ] )

    end,

    UpdatedState = setAttribute( NewState, sms_polling_delay, NewDelay ),

    wooper:return_state( UpdatedState ).





-doc """
Processes any incoming SMSs: reads (and deletes) them, and processes any
corresponding action.
""".
-spec process_sms_messages( [ bin_phone_number() ], wooper:state() ) ->
                        { RelevantSMSRead :: boolean(), wooper:state() }.
process_sms_messages( MasterBinPhoneNumbers, State ) ->

    % Nominal setting:
    DeleteOnReading = true,

    % Just for testing (note that this server will then endlessly loop with the
    % same pollings):
    %
    %DeleteOnReading = false,

    case mobile:read_all_sms( DeleteOnReading ) of

        [] ->
            cond_utils:if_defined( us_main_debug_sms_communication,
                                   ?debug( "No SMS to read found." ) ),
            { false, State };

        SMSs ->
            cond_utils:if_defined( us_main_debug_sms_communication, ?debug_fmt(
                "Processing the ~B SMS that were just read.",
                [ length( SMSs ) ] ) ),

            filter_sms_messages( SMSs, _RelevantSMSFound=false,
                                 MasterBinPhoneNumbers, State )

    end.



-spec filter_sms_messages( [ received_sms() ], boolean(),
                           [ bin_phone_number() ], wooper:state() ) ->
                                        { boolean(), wooper:state() }.
filter_sms_messages( _SMSs=[], RelevantSMSFound, _MasterBinPhoneNumbers,
                     State ) ->
    { RelevantSMSFound, State };

filter_sms_messages( _SMSs=[ SMS=#received_sms{
                                sender_number=BinSenderNumber } | T ],
                     RelevantSMSFound, MasterBinPhoneNumbers, State ) ->

    case lists:member( _Elem=BinSenderNumber, MasterBinPhoneNumbers ) of

        true ->
            % Hopefully no spoofing:
            cond_utils:if_defined( us_main_debug_sms_communication, ?info_fmt(
                "Processing the following SMS, as it is expected to emanate "
                "from an authorised sender: ~ts.",
                [ mobile:received_sms_to_string( SMS ) ] ) ),

            ApplyState = apply_text( SMS#received_sms.text, BinSenderNumber,
                                     State ),

            filter_sms_messages( T, _RelevantSMSFound=true,
                                 MasterBinPhoneNumbers, ApplyState );

        false ->
            ?warning_fmt( "Dropping the following SMS, not emanating from "
                "any authorised sender: ~ts.",
                [ mobile:received_sms_to_string( SMS ) ] ),

            filter_sms_messages( T, RelevantSMSFound, MasterBinPhoneNumbers,
                                 State )

    end.



-spec apply_text( bin_sms_message(), bin_mobile_number(), wooper:state() ) ->
                                                    wooper:state().
% At least currenty const:
apply_text( BinText, BinSenderNumber, State ) ->

    CtrlSrvPid = class_USMainCentralServer:get_server_pid(),

    cond_utils:if_defined( us_main_debug_sms_communication, ?debug_fmt(
        "Requesting the SMS-triggered execution of the action "
        "corresponding to '~ts'.", [ BinText ] ) ),

    % Currently relying on a synchronous call:
    CtrlSrvPid ! { performActionFromTokenString, BinText, self() },

    receive

        { wooper_result, { _ActOutcome={ action_done, Res }, CtrlSrvPid } } ->

            cond_utils:if_defined( us_main_debug_sms_communication, ?debug_fmt(
                "SMS-triggered action succeeded and returned "
                "the following full result:~n~p", [ Res ] ) ),

            FullAnswer = case Res of

                { ok, Str } ->
                    % Needed so that at least ~n become newlines:
                    text_utils:format( Str, [] );

                { error, ErrorStr } ->
                    text_utils:format( "The requested action failed: ~ts",
                                       [ ErrorStr ] );

                Other ->
                    text_utils:format( "Unexpected action result: ~p",
                                       [ Other ] )

            end,

            % There are around 70 UCS-2 characters per SMS, 25 SMSs are already
            % a lot, so:
            %
            ToSendStr = text_utils:term_to_bounded_string( FullAnswer,
                                                           _MaxLen=25*70 ),

            % Sending back the result to the caller:
            case mobile:send_sms( ToSendStr, BinSenderNumber ) of

                { send_success, MsgRef } ->
                    cond_utils:if_defined( us_main_debug_sms_communication,
                        ?debug_fmt( "Sending of this SMS answer succeeded "
                                    "(TPMR reference: ~B).", [ MsgRef ] ),
                        basic_utils:ignore_unused( MsgRef ) ),
                    State;

                { send_failure, MsgRef } ->
                    ?error_fmt( "The SMS sending to the '~ts' mobile number "
                        "of the following text failed (TPMR reference: ~B): "
                        "~ts.", [ BinSenderNumber, MsgRef, ToSendStr ] ),
                    State

            end;

        { wooper_result, { _ActOutcome={ action_failed, FailureReport },
                           CtrlSrvPid } } ->

            ReportStr = us_action:interpret_failure_report( FailureReport ),

            ?warning_fmt( "The action triggered by the incoming SMS '~ts' "
                "failed and returned the following report: ~ts~n"
                "Notifying the caller ('~ts') of it.",
                [ BinText, ReportStr, BinSenderNumber ] ),

           ToSendStr = text_utils:term_to_bounded_string(
               "US-Main reports that " ++ ReportStr, _MaxLen=8*70 ),

            case mobile:send_sms( ToSendStr, BinSenderNumber ) of

                { send_success, MsgRef } ->
                    cond_utils:if_defined( us_main_debug_sms_communication,
                        ?debug_fmt( "Sending of this SMS failure report "
                            "succeeded (TPMR reference: ~B).", [ MsgRef ] ),
                        basic_utils:ignore_unused( MsgRef ) ),
                    State;

                { send_failure, MsgRef } ->
                    ?error_fmt( "The SMS sending to the '~ts' mobile number "
                        "of the following failure report failed "
                        "(TPMR reference: ~B): ~ts.",
                        [ BinSenderNumber, MsgRef, ToSendStr ] ),
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
    ?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
    wooper:const_return();

onWOOPERExitReceived( State, CrashedPid, ExitType ) ->

    % Typically: "Received exit message '{{nocatch,
    %   {wooper_oneway_failed,<0.44.0>,class_XXX,
    %       FunName,Arity,Args,AtomCause}}, [...]}"

    % Redundant information yet useful for console outputs:
    ?warning_fmt( "US Communication Gateway ~w received and ignored "
        "following exit message from ~w:~n  ~p",
        [ self(), CrashedPid, ExitType ] ),

    wooper:const_return().




% Static subsection.


-doc """
Returns the PID of the current, supposedly already-launched, communication
gateway, waiting for it if needed.

It is better to obtain it each time from the naming service rather than to
resolve and store its PID once for all, as, for an increased robustness, servers
may be restarted (hence any former PID may not reference a live process
anymore).
""".
-spec get_server_pid() -> static_return( gateway_pid() ).
get_server_pid() ->

    GatewayPid = class_USServer:resolve_server_pid(
        _RegName=?us_main_communication_server_registration_name,
        _RegScope=?us_main_communication_server_registration_scope ),

    wooper:return_static( GatewayPid ).



% Helper section.


-doc "Initialises the communication gateway.".
-spec init_communications( wooper:state() ) ->  wooper:state().
init_communications( State ) ->

    mobile:start(),

    ?debug( "Testing whether a usable Ceylan-Mobile exists." ),

    SMSSupportActive = case mobile:is_available() of

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
            ?notice( "No SMS communication will be available, no Ceylan-Mobile "
                     "found available." ),
            mobile:stop(),
            false

    end,

    setAttribute( State, sms_support_active, SMSSupportActive ).



% Section related to the US-Main configuration files.


-doc """
Returns the known sensor-related keys in the US-Main configuration files.
""".
-spec get_licit_config_keys() -> [ list_table:key() ].
get_licit_config_keys() ->
    [ ?us_main_notified_numbers_key, ?us_main_master_numbers_key ].



-doc """
Handles the communication-related entries in the user settings specified in
US-Main configuration files.

Note that the specified state is the one of a US-Main configuration server, as
it calls this function from its context.
""".
-spec manage_configuration( us_main_config_table(), wooper:state() ) ->
                                        wooper:state().
manage_configuration( ConfigTable, State ) ->

    % First checkings:

    NotifiedBinNumbers = case table:lookup_entry( ?us_main_notified_numbers_key,
                                                ConfigTable ) of

        key_not_found ->
            send_comm_trace( info, "No phone number to notify has been "
                "defined, so no server-side event notification will be sent "
                "by SMS.", State ),
            [];

        { value, NotifiedNumbers } when is_list( NotifiedNumbers ) ->
            NotifBinNums = [ sms_utils:check_phone_number( PN )
                        || PN <- NotifiedNumbers ],
            case NotifBinNums of

                [] ->
                    send_comm_trace( info, "No phone number to notify "
                        "has been set, so no server-side event notification "
                        "by SMS will be sent by SMS.", State );

                [ NotifBinNum ] ->
                    send_comm_trace_fmt( info, "A single phone number to "
                        "notify has been set: ~ts; it will thus receive "
                        "SMS-based server-side event notifications.",
                        [ NotifBinNum ], State );

                _ ->
                    send_comm_trace_fmt( info,"~B phone numbers to notify "
                        "have been set: ~ts; they will thus receive "
                        "SMS-based server-side event notifications.",
                        [ length( NotifBinNums ),
                          text_utils:strings_to_listed_string( NotifBinNums ) ],
                        State )

            end,
            NotifBinNums

    end,

    MasterBinNumbers = case table:lookup_entry( ?us_main_master_numbers_key,
                                                ConfigTable ) of

        key_not_found ->
            send_comm_trace( info, "No master phone number has been defined, "
                "so no control by SMS will be granted.", State ),
            [];

        { value, MasterNumbers } when is_list( MasterNumbers ) ->
            MstBinNums = [ sms_utils:check_phone_number( PN )
                        || PN <- MasterNumbers ],
            case MstBinNums of

                [] ->
                    send_comm_trace( info,
                        "No master phone number has been set, so no control "
                        "by SMS will be granted.", State );

                [ MstBinNum ] ->
                    send_comm_trace_fmt( info, "A single master phone number "
                        "has been set: ~ts; it will provide control by SMS.",
                        [ MstBinNum ], State );

                _ ->
                    send_comm_trace_fmt( info,"~B master phone numbers have "
                        "been set: ~ts; they will provide control by SMS.",
                        [ length( MstBinNums ),
                          text_utils:strings_to_listed_string( MstBinNums ) ],
                        State )

            end,
            MstBinNums

    end,

    CommSettings = { NotifiedBinNumbers, MasterBinNumbers },

    setAttribute( State, communication_settings, CommSettings ).



-doc """
Sends the specified communication trace, to have it correctly categorised.
""".
-spec send_comm_trace( trace_severity(), trace_message(), wooper:state() ) ->
                                void().
send_comm_trace( TraceSeverity, TraceMsg, State ) ->
    class_TraceEmitter:send_categorised_named_emitter( TraceSeverity, State,
        TraceMsg,
        _EmitterCateg=?trace_emitter_categorization ".Communication",
        _EmitterName="Configuration" ).


-doc """
Sends the specified communication trace, to have it correctly categorised.
""".
-spec send_comm_trace_fmt( trace_severity(), trace_format(), trace_values(),
                           wooper:state() ) -> void().
send_comm_trace_fmt( TraceSeverity, TraceFormat, TraceValues, State ) ->
    TraceMsg = text_utils:format( TraceFormat, TraceValues ),
    send_comm_trace( TraceSeverity, TraceMsg, State ).



-doc "Returns a textual description of this gateway.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    SMSStr = case ?getAttr(sms_support_active) of

        true ->
            NotifiedStr = case ?getAttr(notified_phone_numbers) of

                [] ->
                    "no phone number to notify is set";

                NotifL=[ _SingleNPNBinStr ] ->
                    text_utils:format(
                        "a single phone number to notify is set, ~ts", NotifL );

                NPNBinStrs ->
                    text_utils:format(
                        "~B phone numbers to notify are set, ~ts",
                        [ length( NPNBinStrs ),
                          text_utils:strings_to_listed_string( NPNBinStrs ) ] )

            end,

            MasterStr = case ?getAttr(master_phone_numbers) of

                [] ->
                    "no master phone number is set";

                MstL=[ _SingleMPNBinStr ] ->
                    text_utils:format( "a single master phone number is set, "
                                       "~ts", MstL );

                MPNBinStrs ->
                    text_utils:format( "~B master phone numbers are set, ~ts",
                        [ length( MPNBinStrs ),
                          text_utils:strings_to_listed_string( MPNBinStrs ) ] )

            end,

            text_utils:format( "an active SMS support (current polling delay: "
                "~ts; min: ~ts / max: ~ts) for which ~ts and ~ts",
                [ time_utils:duration_to_string( ?getAttr(sms_polling_delay) ),
                  time_utils:duration_to_string(
                    ?getAttr(sms_polling_min_delay) ),
                  time_utils:duration_to_string(
                    ?getAttr(sms_polling_max_delay) ),
                  NotifiedStr, MasterStr ] );

        false ->
            "no active SMS support"

    end,

    ParentGatewayStr = case ?getAttr(parent_comm_gateway_lookup_info) of

        undefined ->
            "not referencing any parent communication gateway";

        LI ->
            text_utils:format( "referencing a parent communication gateway "
                "based on a ~ts", [ naming_utils:lookup_info_to_string( LI ) ] )

    end,

    text_utils:format( "US communication gateway, with ~ts; ~ts",
                       [ SMSStr, ParentGatewayStr ] ).
