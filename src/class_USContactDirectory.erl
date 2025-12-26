% Copyright (C) 2021-2026 Olivier Boudeville
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

-module(class_USContactDirectory).

-moduledoc """
US server in charge of **managing a contact directory**, recording various
information about users and roles.
""".


-define( class_description, "US server in charge of managing a contact "
    "directory, recording various information about US users." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").



% Design notes:
%
% The user contact information are usually known thanks to ETF file(s)
% ("contact files"), whose paths are obtained from the US-Main configuration
% server.
%
% See https://myriad.esperide.org/#etf.
%
% User identifiers are not set by a contact directory (multiple directories may
% exist, and we prefer stable identifiers, like the ones used by operating
% systems).



-doc """
The lines expected to be read from a contact ETF file shall respect this
structure.

Nickname acts like a pseudo or a shorter, more familiar name.

Comment is free text.

Birth date of interest to wish happy birthdays.

Postal address not of use here, yet possibly specified for completeness.

Refer to the fields of the `user_settings` record for more information.
""".
-type contact_line() :: { UserId :: user_id(),
    FirstName :: ustring(), LastName :: ustring(), Nickname :: ustring(),
    Status :: user_status(),
    Comment :: ustring(), BirthDate :: option( ustring() ),
    LandlineNumber :: option( ustring() ), MobileNumber :: option( ustring() ),
    PrimaryEmailAddress :: option( ustring() ),
    SecondaryEmailAddress :: option( ustring() ),
    PostalAddress :: option( ustring() ),
    Roles :: [ role() ] }.


% Silencing:
-export_type([ contact_line/0 ]).




% This contact directory is designed to be able to integrate to an OTP
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


% Entries in the US-configuration files for contact directory:

-define( us_main_contact_files_key, us_contact_files ).


-doc "PID of a contact directory server.".
-type contact_directory_pid() :: class_USServer:server_pid().



% Section about user settings.


-doc "Identifier of a user, as read by this directory. Starts at 1.".
-type user_id() :: count().


-doc "Status of an user.".
-type user_status() :: 'enabled'
                     | 'disabled'. % Sadly for defunct entries.



-doc "The (canonical) date of birth, typically of a person.".
-type birth_date() :: time_utils:date().



-doc "A phone number (landline or mobile).".
-type bin_phone_number() :: bin_string().



-doc "The main, full postal address (with country information) for this user.".
-type postal_address() :: bin_string().


-doc "The supported roles, to be managed by this gateway.".
-type role() :: 'administrator'.



-record( user_settings, {

    % The identifier of this user:
    id :: user_id(),

    % The first name of this user (organisations do not have first names):
    first_name :: option( bin_string() ),

    % The last name of this user (corresponding to the full name of an
    % organisation):
    %
    last_name :: bin_string(),

    % A pseudo or a shorter, more familiar name:
    nickname :: option( bin_string() ),

    % The current status of this entry:
    status = enabled :: user_status(),

    % Any comment associated to this user:
    comment :: option( bin_string() ),

    % The birth date (if any) of this user:
    birth_date :: option( birth_date() ),


    % The landline number (if any) associated to a user device:
    landline_number :: option( bin_phone_number() ),

    % The mobile number (if any) associated to a user device (more interesting
    % than the landline as able to receive SMS):
    %
    mobile_number :: option( bin_phone_number() ),


    % The primary email address (if any) to be used for this user (typically
    % their personal one):
    %
    primary_email_address :: option( bin_email_address() ),

    % The secondary email address (if any) to be used for this user (typically
    % their professional one):
    %
    secondary_email_address :: option( bin_email_address() ),


    % The known postal address (if any) to be used for this user:
    postal_address :: option( postal_address() ),


    % The roles (if any) taken in charge by this user.
    roles = [] :: [ role() ] } ).



-doc """
Settings corresponding to a user. Note that users may be persons or
organisations.
""".
-type user_settings() :: #user_settings{}.


-export_type([ contact_directory_pid/0, user_id/0, birth_date/0, role/0,
               user_settings/0 ]).


% Exported helpers:
-export([ get_licit_config_keys/0, manage_configuration/2 ]).


% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type date() :: time_utils:date().

-type any_file_path() :: file_utils:any_file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().

-type bin_mobile_number() :: mobile:bin_mobile_number().

-type bin_email_address() :: email_utils:bin_email_address().


-type us_main_config_table() ::
    class_USMainCentralServer:us_main_config_table().



% Local types:

-doc "A table storing the settings for users.".
-type user_table() :: table( user_id(), user_settings() ).



-doc """
A table allowing to translate a given role into the users to which it was
assigned.
""".
-type role_table() :: table( role(), [ user_id() ] ).



% The class-specific attributes:
-define( class_attributes, [

    { user_table, user_table(),
      "the table recording the settings of all known users" },

    { role_table, role_table(), "the table listing all identifiers "
      "corresponding to a given role" },

    { contact_files, [ bin_file_path() ], "a list of the read contact files" },

    { execution_context, option( basic_utils:execution_context() ),
      "tells whether this server is to run in development or production mode" },

    % Not set anymore by this server, but read from contact file(s):
    %{ next_user_id, user_id(),
    %  "the next user identifier that will be assigned by this directory" },

    { config_base_directory, option( bin_directory_path() ),
      "the base directory where all US configuration is to be found" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Contact" ).



% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").


-define( server_name, "Contact directory" ).



% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this contact directory to an OTP supervision
% tree.


-doc """
Starts and links a supervision bridge for the contact directory.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see `us_main_sup:init/1`), hence generally triggered by the application
initialisation.
""".
-spec start_link() -> term().
start_link() ->

    % Apparently not displayed in a release context, yet executed:
    trace_bridge:debug( "Starting the US-Main supervisor bridge for "
                        "the contact directory." ),

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
                           "the contact directory.", [ self() ] ),

    % Not specifically synchronous:
    ContactDirectoryPid = ?MODULE:new_link(),

    { ok, ContactDirectoryPid, _InitialBridgeState=ContactDirectoryPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=ContactDirectoryPid )
                                    when is_pid( ContactDirectoryPid ) ->

    trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
        "the contact directory (reason: ~w, contact directory: ~w).",
        [ Reason, ContactDirectoryPid ] ),

    % Synchronicity needed, otherwise a potential race condition exists, leading
    % this process to be killed by its OTP supervisor instead of being normally
    % stopped:
    %
    wooper:delete_synchronously_instance( ContactDirectoryPid ),

    trace_bridge:debug_fmt( "US-Main contact directory ~w terminated.",
                            [ ContactDirectoryPid ] ).



% Actual implementation of the contact directory.


-doc """
Constructs a blank contact directory. Typically useful when launched as an
(initially) stateless service when started from an OTP application context.
""".
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

    % First the direct mother classes, then this class-specific actions:
    % (traps EXITs)
    %
    SrvState = class_USServer:construct( State,
        ?trace_categorize(?server_name),
        ?us_main_contact_server_registration_name,
        ?us_main_contact_server_registration_scope ),

    % As US-Main depends on US-Common, the current server may rely on the US
    % configuration server. However, to locate it, the US configuration file
    % must have been located and parsed:

    CfgState = load_and_apply_configuration( SrvState ),

    ?send_notice( CfgState, "Constructed: " ++ to_string( CfgState ) ),

    CfgState.



-doc """
Constructs a contact directory from specified ETF contact file (with no link to
any US configuration server); mainly used for autonomous testing.
""".
-spec construct( wooper:state(), any_file_path() ) -> wooper:state().
construct( State, ContactFilePath ) ->

    % First the direct mother classes, then this class-specific actions:
    % (traps EXITs)
    %
    SrvState = class_USServer:construct( State,
        ?trace_categorize(?server_name),
        ?us_main_contact_server_registration_name,
        ?us_main_contact_server_registration_scope ),

    ContactFilePathStr = text_utils:ensure_string( ContactFilePath ),

    EmptyTable = table:new(),

    % In this specific case, we consider that the US configuration directory is
    % the current directory:
    %
    BinCfgDir = file_utils:get_bin_current_directory(),

    { ReadUserTable, ReadRoleTable } = read_contact_file( ContactFilePathStr,
        BinCfgDir, _InitUserTable=EmptyTable, _InitRoleTable=EmptyTable,
        SrvState ),

    ReadyState = setAttributes( SrvState, [
        { user_table, ReadUserTable },
        { role_table, ReadRoleTable },
        { contact_files, text_utils:ensure_binary( ContactFilePath ) },
        { execution_context, undefined },
        { config_base_directory, undefined },
        { us_main_config_server_pid,undefined } ] ),

    ?send_notice( ReadyState, "Constructed: " ++ to_string( ReadyState ) ),

    ReadyState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    ?debug_fmt( "Deletion initiated, while state is: ~ts.",
                [ to_string( State ) ] ),

    ?info( "Deleted." ),
    State.



% Method section.


-doc """
Callback triggered, if this server enabled the trapping of EXIT messages,
whenever a linked process terminates.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
        basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StoppedPid, _ExitType=normal ) ->
    ?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
    wooper:const_return();

onWOOPERExitReceived( State, CrashedPid, ExitType ) ->

    % Typically: "Received exit message '{{nocatch,
    %                       {wooper_oneway_failed,<0.44.0>,class_XXX,
    %                           FunName,Arity,Args,AtomCause}}, [...]}"

    % Redundant information yet useful for console outputs:
    ?warning_fmt( "US Contact Directory ~w received and ignored following exit "
                  "message from ~w:~n  ~p", [ self(), CrashedPid, ExitType ] ),

    wooper:const_return().




% Static subsection.


-doc """
Returns the PID of the current, supposedly already-launched, contact directory,
waiting (up to a few seconds, as all US servers are bound to be launched mostly
simultaneously) if needed.

It is better to obtain the PID of a server each time from the naming service
rather than to resolve and store its PID once for all, as, for an increased
robustness, servers may be restarted (hence any stored PID may not reference a
live process anymore).
""".
-spec get_server_pid () -> static_return( contact_directory_pid() ).
get_server_pid() ->

    DirectoryPid = class_USServer:resolve_server_pid(
        _RegName=?us_main_contact_server_registration_name,
        _RegScope=?us_main_contact_server_registration_scope ),

    wooper:return_static( DirectoryPid ).



% Helper section.


-doc """
Loads and applies the relevant configuration settings first from the overall US
configuration file.

As a result, the US configuration file is not fully checked as such (e.g. no
extracting and check that no entry remains; it is the job of the US config
server), we just select the relevant information from it.
""".
-spec load_and_apply_configuration( wooper:state() ) -> wooper:state().
load_and_apply_configuration( State ) ->

    % This contact directory server is not supposed to read more the US
    % configuration file; it should request it to the overall configuration
    % server, about all the extra information it needs, to avoid duplicated,
    % possibly inconsistent reading/interpretation (and in order to declare
    % itself in the same move):
    %
    class_USMainCentralServer:get_server_pid() !
        { getContactSettings, [], self() },

    % No possible interleaving:
    receive

        { wooper_result, { USCfgBinDir, ExecContext, ContactFiles } } ->

            InitTable = table:new(),

            { UserTable, RoleTable } = read_contact_files( ContactFiles,
                USCfgBinDir, _UserTable=InitTable, _RoleTable=InitTable,
                State ),

            setAttributes( State, [
                { user_table, UserTable },
                { role_table, RoleTable },
                { contact_files, ContactFiles },
                { execution_context, ExecContext },
                { config_base_directory, USCfgBinDir } ] )

    end.



-doc "Reads the specified contact file and enriches specified table.".
-spec read_contact_file( any_file_path(), bin_directory_path(), user_table(),
    role_table(), wooper:state() ) -> { user_table(), role_table() }.
read_contact_file( ContactFilePath, USCfgBinDir, UserTable, RoleTable,
                   State ) ->

    AbsContactFilePath = file_utils:ensure_path_is_absolute( ContactFilePath,
                                    _BasePath=USCfgBinDir ),

    ?debug_fmt( "Reading contact file '~ts'.", [ AbsContactFilePath ] ),

    case file_utils:is_existing_file_or_link( AbsContactFilePath ) of

        true ->
            ReadTerms = file_utils:read_etf_file( AbsContactFilePath ),
            add_contacts( ReadTerms, UserTable, RoleTable, State );

        false ->
            ?error_fmt( "Specified contact file '~ts' does not exist; "
                "no contact added.", [ AbsContactFilePath ] ),
            { UserTable, RoleTable }

    end.



-doc """
Adds the specified contact terms (expected to be contact lines) to specified
contact table.
""".
-spec add_contacts( [ term() ], user_table(), role_table(), wooper:state() ) ->
            { user_table(), role_table() }.
add_contacts( _ReadTerms=[], UserTable, RoleTable, _State ) ->
    { UserTable, RoleTable };

% Here a valid contact_line() may be found (the T suffix means "term"):
add_contacts( _ReadTerms=[ Line={ UserIdT, FirstNameT, LastNameT, NicknameT,
        StatusT, CommentT, BirthDateT, LandlineNumberT, MobileNumberT,
        PrimaryEmailAddressT, SecondaryEmailAddressT, PostalAddressT,
        RolesT } | T ],
              UserTable, RoleTable, State ) ->

    case vet_user_id( UserIdT ) of

        invalid ->
            ?error_fmt( "Invalid (hence ignored) contact line:~n  '~p':~n "
                "invalid user identifier ('~p').", [ Line, UserIdT ] ),
            add_contacts( T, UserTable, RoleTable, State );

        UserId ->
            case vet_maybe_string( FirstNameT ) of

                invalid ->
                    ?error_fmt( "Invalid (hence ignored) contact line~n  '~p':"
                        "~ninvalid first name ('~p').", [ Line, FirstNameT ] ),
                    add_contacts( T, UserTable, RoleTable, State );

                MaybeBinFirstName ->
                    case vet_maybe_string( LastNameT ) of

                        invalid ->
                            ?error_fmt( "Invalid (hence ignored) contact "
                                "line~n  '~p':~ninvalid last name ('~p').",
                                [ Line, FirstNameT ] ),
                            add_contacts( T, UserTable, RoleTable, State );

                        undefined ->
                            ?error_fmt( "Invalid (hence ignored) contact "
                                "line~n  '~p':~nlast name must be non-empty.",
                                [ Line ] ),
                            add_contacts( T, UserTable, RoleTable, State );

                        BinLastName ->
                            case vet_maybe_string( NicknameT ) of

                                invalid ->
                                    ?error_fmt( "Invalid (hence ignored) "
                                        "contact line:~n  '~p':~n"
                                        "invalid nickname ('~p').",
                                        [ Line, NicknameT ] ),
                                    add_contacts( T, UserTable, RoleTable,
                                                  State );

                                MaybeBinNickname ->
                                    Settings = #user_settings{
                                        id=UserId,
                                        first_name=MaybeBinFirstName,
                                        last_name=BinLastName,
                                        nickname=MaybeBinNickname },

                                    vet_contacts_first( Line, T, Settings,
                                        UserId, StatusT, CommentT, BirthDateT,
                                        LandlineNumberT, MobileNumberT,
                                        PrimaryEmailAddressT,
                                        SecondaryEmailAddressT, PostalAddressT,
                                        RolesT, UserTable, RoleTable, State )


                            end

                    end

            end

    end;


add_contacts( _ReadTerms=[ InvalidTuple | T ], UserTable, RoleTable, State )
                                            when is_tuple( InvalidTuple ) ->

    ?error_fmt( "Read invalid (hence ignored) contact line "
        "(a tuple with ~B elements instead of 13):~n  ~p",
        [ size( InvalidTuple ), InvalidTuple ] ),

    add_contacts( T, UserTable, RoleTable, State );


add_contacts( _ReadTerms=[ InvalidLine | T ], UserTable, RoleTable, State ) ->

    ?error_fmt( "Read invalid (hence ignored) contact line "
        "(not even a tuple):~n  ~p", [ InvalidLine ] ),

    add_contacts( T, UserTable, RoleTable, State ).



% (helper)
vet_contacts_first( Line, T, Settings, UserId, StatusT, CommentT, BirthDateT,
        LandlineNumberT, MobileNumberT, PrimaryEmailAddressT,
        SecondaryEmailAddressT, PostalAddressT, RolesT, UserTable, RoleTable,
        State ) ->

    case vet_status( StatusT ) of

        invalid ->
            ?error_fmt( "Invalid (hence ignored) contact line:~n  '~p':~n"
                        "invalid status ('~p').", [ Line, StatusT ] ),
            add_contacts( T, UserTable, RoleTable, State );

        Status ->
            case vet_maybe_string( CommentT ) of

                invalid ->
                    ?error_fmt( "Invalid (hence ignored) contact line:~n  "
                        "'~p':~ninvalid comment ('~p').",
                        [ Line, CommentT ] ),
                    add_contacts( T, UserTable, RoleTable, State );

                MaybeBinComment ->
                    NewSettings = Settings#user_settings{
                        status=Status,
                        comment=MaybeBinComment },

                    vet_contacts_third( Line, T, NewSettings, UserId,
                        BirthDateT, LandlineNumberT, MobileNumberT,
                        PrimaryEmailAddressT, SecondaryEmailAddressT,
                        PostalAddressT, RolesT, UserTable, RoleTable, State )

            end

    end.



% (helper)
vet_contacts_third( Line, T, Settings, UserId, BirthDateT, LandlineNumberT,
        MobileNumberT, PrimaryEmailAddressT, SecondaryEmailAddressT,
        PostalAddressT, RolesT, UserTable, RoleTable, State ) ->

    case vet_maybe_date( BirthDateT ) of

        invalid ->
            ?error_fmt( "Invalid (hence ignored) contact line:~n  '~p':~n"
                "invalid birth date ('~p').", [ Line, BirthDateT ] ),
            add_contacts( T, UserTable, RoleTable, State );

        MaybeBirthDate ->
            case vet_phone_number( LandlineNumberT ) of

                invalid ->
                    ?error_fmt( "Invalid (hence ignored) contact line~n  '~p':"
                        "~ninvalid landline number ('~p').",
                        [ Line, LandlineNumberT ] ),
                    add_contacts( T, UserTable, RoleTable, State );

                MaybeLandline ->
                    case vet_phone_number( MobileNumberT ) of

                        invalid ->
                            ?error_fmt( "Invalid (hence ignored) contact "
                                "line:~n  '~p':~ninvalid mobile number ('~p').",
                                [ Line, MobileNumberT ] ),
                            add_contacts( T, UserTable, RoleTable, State );

                        MaybeMobile ->
                            NewSettings = Settings#user_settings{
                                birth_date=MaybeBirthDate,
                                landline_number=MaybeLandline,
                                mobile_number=MaybeMobile },

                            vet_contacts_fourth( Line, T, NewSettings, UserId,
                                PrimaryEmailAddressT, SecondaryEmailAddressT,
                                PostalAddressT, RolesT, UserTable, RoleTable,
                                State )

                    end

            end

    end.



% (helper)
vet_contacts_fourth( Line, T, Settings, UserId, PrimaryEmailAddressT,
        SecondaryEmailAddressT, PostalAddressT, RolesT, UserTable, RoleTable,
        State ) ->

    case vet_email_address( PrimaryEmailAddressT ) of

        invalid ->
            ?error_fmt( "Invalid (hence ignored) contact line:~n  '~p':~n"
                "invalid primary email address identifier ('~p').",
                [ Line, PrimaryEmailAddressT ] ),
            add_contacts( T, UserTable, RoleTable, State );

        MaybePrimEmailAddr ->
            case vet_email_address( SecondaryEmailAddressT ) of

                invalid ->
                    ?error_fmt( "Invalid (hence ignored) contact line:~n  '~p':"
                        "~ninvalid secondary email address identifier ('~p').",
                        [ Line, SecondaryEmailAddressT ] ),
                    add_contacts( T, UserTable, RoleTable, State );

                MaybeSecEmailAddr ->
                    case vet_postal_address( PostalAddressT ) of

                        invalid ->
                            ?error_fmt( "Invalid (hence ignored) contact "
                                "line:~n  '~p':~ninvalid postal address "
                                "('~p').", [ Line, PostalAddressT ] ),
                            add_contacts( T, UserTable, RoleTable, State );

                        MaybePostalAddress ->
                            NewSettings = Settings#user_settings{
                                primary_email_address=MaybePrimEmailAddr,
                                secondary_email_address=MaybeSecEmailAddr,
                                postal_address=MaybePostalAddress },

                            vet_contacts_fifth( Line, T, NewSettings, UserId,
                                RolesT, UserTable, RoleTable, State )

                    end

            end

    end.



% (helper)
vet_contacts_fifth( Line, T, Settings, UserId, RolesT, UserTable, RoleTable,
                    State )->

    case vet_roles( RolesT ) of

        invalid ->
            ?error_fmt( "Invalid (hence ignored) contact line:  '~p':~n"
                "invalid roles ('~p').", [ Line, RolesT ] ),
            add_contacts( T, UserTable, RoleTable, State );

        Roles ->
            NewSettings = Settings#user_settings{ roles=Roles },

            case table:has_entry( UserId, UserTable ) of

                true ->
                    ?error_fmt( "Invalid (hence ignored) contact line:  '~p':~n"
                        "user id #~B already registered, corresponding "
                        "to: ~ts.", [ Line, UserId, user_settings_to_string(
                        table:get_value( UserId, UserTable ) ) ] ),
                    add_contacts( T, UserTable, RoleTable, State );

                false ->
                    NewUserTable =
                        table:add_entry( UserId, NewSettings, UserTable ),

                    NewRoleTable = lists:foldl(
                        fun( R, RT ) ->
                            table:append_to_entry( R, UserId, RT )
                        end,
                        _Acc0=RoleTable,
                        _List=Roles ),

                    add_contacts( T, NewUserTable, NewRoleTable, State )

            end

    end.



-doc "Reads the specified contact files and enriches the specified table.".
-spec read_contact_files( [ any_file_path() ], bin_directory_path(),
        user_table(), role_table(), wooper:state() ) ->
            { user_table(), role_table() }.
read_contact_files( ContactFilePaths, USCfgBinDir, UserTable, RoleTable,
                    State ) ->

    ?debug_fmt( "Reading the following contact files: ~ts.",
                [ text_utils:strings_to_string( ContactFilePaths ) ] ),

    lists:foldl(
        fun( CfgFilePath, { UserTableAcc, RoleTableAcc } ) ->
            read_contact_file( CfgFilePath, USCfgBinDir, UserTableAcc,
                               RoleTableAcc, State )
        end,
        _Acc0={ UserTable, RoleTable },
        _List=ContactFilePaths ).



% Vetting functions.


-doc "Vets the specified term, expected to be a user identifier.".
-spec vet_user_id( term() ) -> 'invalid' | user_id().
vet_user_id( UserId ) when is_integer( UserId ) andalso UserId > 0 ->
    UserId;

vet_user_id( _OtherUserId ) ->
    invalid.



-doc """
Vets the specified term, expected to be a maybe-string. Empty strings are
accepted.
""".
-spec vet_maybe_string( term() ) -> 'invalid' | option( bin_string() ).
vet_maybe_string( MS=undefined ) ->
    MS;

vet_maybe_string( MS ) when is_list( MS ) ->
    case text_utils:is_string( MS ) of

        true ->
            text_utils:string_to_binary( MS );

        false ->
            invalid

    end;

vet_maybe_string( _MS ) ->
    invalid.



-doc "Vets the specified term, expected to be a user status.".
-spec vet_status( term() ) -> 'invalid' | user_status().
vet_status( Status=enabled ) ->
    Status;

vet_status( Status=disabled ) ->
    Status;

vet_status( _OtherStatus ) ->
    invalid.



-doc """
Vets the specified term, expected to be a maybe-postal address. Empty strings
are accepted.
""".
-spec vet_postal_address( term() ) -> 'invalid' | option( bin_string() ).
vet_postal_address( PA=undefined ) ->
    PA;

vet_postal_address( PA ) when is_list( PA ) ->
    case text_utils:is_string( PA ) of

        true ->
            text_utils:string_to_binary( PA );

        false ->
            invalid

    end;

vet_postal_address( _PA ) ->
    invalid.



-doc "Vets the specified term, expected to be a maybe-date.".
-spec vet_maybe_date( term() ) -> 'invalid' | option( date() ).
vet_maybe_date( MD=undefined ) ->
    MD;

vet_maybe_date( UserDate ) ->
    case time_utils:is_user_date( UserDate ) of

        true ->
            time_utils:user_to_canonical_date( UserDate );

        false ->
            invalid

    end.



-doc "Vets the specified term, expected to be a maybe-phone number.".
-spec vet_phone_number( term() ) -> 'invalid' | option( bin_mobile_number() ).
vet_phone_number( MPN=undefined ) ->
    MPN;

vet_phone_number( PN ) when is_list( PN ) ->
    case text_utils:is_string( PN ) of

        true ->
            SpaceLessStr = text_utils:remove_whitespaces( PN ),
            text_utils:string_to_binary( SpaceLessStr );

        false ->
            invalid

    end;

vet_phone_number( _Other ) ->
    invalid.



-doc "Vets the specified term, expected to be a maybe-email address.".
-spec vet_email_address( term() ) -> 'invalid' | option( bin_email_address() ).
vet_email_address( MEA=undefined ) ->
    MEA;

vet_email_address( EA ) when is_list( EA ) ->
    case text_utils:split( EA, _Delimiter=$@ ) of

        [ _User, _FQDN ] ->
            text_utils:string_to_binary( EA );

        _ ->
            invalid

    end;

vet_email_address( _Other ) ->
    invalid.



-doc "Vets the specified term, expected to be a list of roles.".
-spec vet_roles( term() ) -> 'invalid' | [ role() ].
vet_roles( Roles ) ->
    case list_utils:are_atoms( Roles ) of

        true ->
            Roles;

        false ->
            invalid

    end.



% Section related to the US-Main configuration files.


-doc """
Returns the known contact-related keys in the US-Main configuration files.
""".
-spec get_licit_config_keys() -> [ list_table:key() ].
get_licit_config_keys() ->
    [ ?us_main_contact_files_key ].


-doc """
Handles the contact-related entries in the user settings specified in US-Main
configuration files.

Note that the specified state is the one of a US-Main configuration server.
""".
-spec manage_configuration( us_main_config_table(), wooper:state() ) ->
                                        wooper:state().
manage_configuration( ConfigTable, State ) ->

    ContactFiles = case table:lookup_entry( ?us_main_contact_files_key,
                                            ConfigTable ) of

        key_not_found ->
            ?info( "No user-configured contact files." ),
            [];

        { value, Files } when is_list( Files ) ->
            % The contact directory is to make them correctly absolute if
            % necessary:

            BinAbsFiles = text_utils:ensure_binaries( Files ),

            %?info_fmt( "User-configured contact files: ~ts",
            %           [ text_utils:binaries_to_string( BinAbsFiles ) ] ),

            BinAbsFiles;

        { value, InvalidFiles }  ->
            ?error_fmt( "Read invalid user-configured US contact files: '~p'.",
                        [ InvalidFiles ] ),
            throw( { invalid_us_contact_files, InvalidFiles,
                     ?us_main_contact_files_key } )

    end,

    % Not specifically checked at this level, will be done by the contact
    % manager:
    %
    setAttribute( State, contact_files, ContactFiles ).



-doc "Returns a textual description of the specified user settings.".
-spec user_settings_to_string( user_settings() ) -> ustring().
user_settings_to_string( #user_settings{
        id=Id,
        first_name=MaybeFirstName,
        last_name=LastName,
        nickname=MaybeNickname,
        status=Status,
        comment=MaybeComment,
        birth_date=MaybeBirthDate,
        landline_number=MaybeLandlineNumber,
        mobile_number=MaybeMobileNumber,
        primary_email_address=MaybePrimAddr,
        secondary_email_address=MaybeSecAddr,
        postal_address=MaybePostalAddress,
        roles=Roles } ) ->

    % All strings are binary ones.

    FullName = case MaybeFirstName of

        undefined ->
            LastName;

        FirstName ->
            % To clearly denote names:
            text_utils:format( "~ts '~ts'", [ FirstName, LastName ] )

    end,

    NickStr = case MaybeNickname of

        undefined ->
            "";

        Nickname ->
            text_utils:format( ", whose nickname is '~ts'", [ Nickname ] )

    end,

    StatusStr = case Status of

        enabled ->
            "enabled";

        disabled ->
            "disabled"

    end,

    CommentStr = case MaybeComment of

        undefined ->
            "";

        Comment ->
            text_utils:format( ", described as '~ts'", [ Comment ] )

    end,


    BirthStr = case MaybeBirthDate of

        undefined ->
            "";

        BirthDate ->
            text_utils:format( ", born on ~ts",
                [ time_utils:get_textual_date( BirthDate ) ] )

    end,

    LandlineStr = case MaybeLandlineNumber of

        undefined ->
            "";

        LandlineNumber ->
            text_utils:format( ", whose landline number is '~ts'",
                               [ LandlineNumber ] )

    end,

    MobileStr = case MaybeMobileNumber of

        undefined ->
            "";

        MobileNumber ->
            text_utils:format( ", whose mobile number is '~ts'",
                               [ MobileNumber ] )

    end,

    PrimEmailStr = case MaybePrimAddr of

        undefined ->
            "";

        PrimAddr ->
            text_utils:format( ", whose primary email address is '~ts'",
                               [ PrimAddr ] )

    end,


    SecEmailStr = case MaybeSecAddr of

        undefined ->
            "";

        SecAddr ->
            text_utils:format( ", whose secondary email address is '~ts'",
                               [ SecAddr ] )

    end,

    PostStr = case MaybePostalAddress of

        undefined ->
            "";

        PostalAddress ->
            text_utils:format( ", whose postal address is '~ts'",
                               [ PostalAddress ] )

    end,

    RoleStr = case Roles of

        [] ->
            "with no role assigned";

        [ Role ] ->
            text_utils:format( "whose assigned role is '~ts'", [ Role ] );

        _ ->
            text_utils:format( "whose assigned roles are: ~ts",
                [ text_utils:atoms_to_quoted_listed_string( Roles ) ] )

    end,

    text_utils:format( "user '~ts' (id: #~B)~ts, whose status is ~ts~ts~ts"
        "~ts~ts~ts~ts~ts, ~ts",
        [ FullName, Id, NickStr, StatusStr, CommentStr, BirthStr,
          LandlineStr, MobileStr, PrimEmailStr, SecEmailStr, PostStr,
          RoleStr ] ).



-doc "Returns a textual description of the specified role information.".
-spec role_info_to_string( role(), [ user_id() ] ) -> ustring().
role_info_to_string( Role, _UserIds=[] ) ->
    text_utils:format( "role '~ts', not assigned to any user", [ Role ] );

role_info_to_string( Role, [ UserId ] ) ->
    text_utils:format( "role '~ts', assigned to a single user, #~B",
                       [ Role, UserId ] );

role_info_to_string( Role, UserIds ) ->
    text_utils:format( "role '~ts', assigned to ~B users: ~ts",
        [ Role, length( UserIds ),
          text_utils:integer_ids_to_listed_string( lists:sort( UserIds ) ) ] ).



-doc "Returns a textual description of this contact directory.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    % Sorted by increasing IDs:
    SettingsdByIds = [ Settings || { _Id, Settings } <-
        lists:sort( table:enumerate( ?getAttr(user_table) ) ) ],

    UserStr = case SettingsdByIds of

        [] ->
            "no user";

        [ UserSettings ] ->
            "a single user: " ++ user_settings_to_string( UserSettings );

        UserSettingsList ->
            text_utils:format( "~B users: ~ts", [ length( UserSettingsList ),
                text_utils:strings_to_string(
                    [ user_settings_to_string( US )
                        || US <- UserSettingsList ] ) ] )

    end,

    RoleStr = case table:enumerate( ?getAttr(role_table) ) of

        [] ->
            "No role was defined.";

        [ { Role, UserIds } ] ->
            text_utils:format( "A single role was defined overall: ~ts.",
                               [ role_info_to_string( Role, UserIds ) ] );

        RolePairs ->
            text_utils:format( "~B roles were defined overall: ~ts",
                [ length( RolePairs ), text_utils:strings_to_string(
                    [ role_info_to_string( R, UIds )
                        || { R, UIds } <- RolePairs ] ) ] )

    end,

    text_utils:format( "US contact directory registering ~ts~n~ts",
                       [ UserStr, RoleStr ] ).
