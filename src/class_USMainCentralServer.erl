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
% Creation date: Wednesday, September 15, 2021.

-module(class_USMainCentralServer).

-moduledoc """
Singleton central server holding the configuration information, managing the
automated actions, etc. for the **US-Main** framework.
""".


-define( class_description,
		 "Singleton central server holding the configuration information, "
         "managing the automated actions, etc. for the US-Main framework" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USCentralServer ] ).


% This communication gateway is designed to be able to integrate to an OTP
% supervision tree thanks to a supervisor bridge, whose behaviour is directly
% defined in this module. See https://wooper.esperide.org/#otp-guidelines for
% further information.
%
-behaviour(supervisor_bridge).


% User API of the bridge:
-export([ start_link/1 ]).

% Callbacks of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).


% Built-in action requests are not to be explicitly exported (as they are
% requests).


-define( bridge_name, ?MODULE ).



% For the general_main_settings records:
-include("class_USMainCentralServer.hrl").

-doc "General US-Main settings".
-type general_main_settings() :: #general_main_settings{}.



-doc "Checked more precisely as a position() in the home automation server.".
-type user_server_location() ::
	{ LatDegrees :: float(), LongDegrees :: float() }.


-type main_central_server_pid() :: server_pid().


-doc "A table holding US-Main configuration information.".
-type us_main_config_table() :: config_table().


-export_type([ general_main_settings/0, user_server_location/0,
               main_central_server_pid/0, us_main_config_table/0 ]).


% For default_us_main_central_server_registration_name:
-include("us_main_defines.hrl").



% Must be kept consistent with the default_us_main_epmd_port variable in
% us-main-common.sh:
%
-define( default_us_main_epmd_port, 4507 ).


% The default filename (as a binary string) for the US-Main configuration (e.g.
% sensors), to be found from the overall US configuration directory:
%
-define( default_us_main_cfg_filename, <<"us-main.config">> ).

-define( us_main_epmd_port_key, epmd_port ).

% The key to define any specific registration name for this US-Main server:
-define( us_main_central_server_registration_name_key,
		 us_main_central_server_registration_name ).

-define( us_main_username_key, us_main_username ).
-define( us_main_app_base_dir_key, us_main_app_base_dir ).
-define( us_main_data_dir_key, us_main_data_dir ).
-define( us_main_log_dir_key, us_main_log_dir ).


% For us_common_actions_key:
-include("class_USServer.hrl").


% All known, licit (top-level) base keys for the US-Main configuration file:
% (other keys are in their respective server classes)
%
-define( known_us_main_central_config_keys, [ ?us_main_epmd_port_key,
	?us_main_central_server_registration_name_key,
	?us_main_username_key, ?us_main_app_base_dir_key,
	?us_main_data_dir_key, ?us_main_log_dir_key, ?us_actions_key ] ).



% The last-resort environment variable:
-define( us_main_app_env_variable, "US_MAIN_APP_BASE_DIR" ).

% Preferring a default local directory to an absolute one requiring privileges:
%-define( default_data_base_dir, "/var/local/us-main/data" ).
-define( default_data_base_dir, "us-main-data" ).

% The subdirectory of the US-Main application directory in the one designated by
% us_main_data_dir_key:
%
-define( app_subdir, "us-main" ).


-define( default_log_base_dir, "/var/log/universal-server/us-main" ).


% Design notes:
%
% This US-Main central server will ensure that an overall US (i.e. US-Common)
% configuration server is running, either by fetching its PID if already
% existing (which is most probably the case), otherwise by launching it
% accordingly.
%
% In both cases the PID of the overall server will be known, but no link will be
% created between these two, as their life-cycles are mostly independent.
%
% The name of the US-Main configuration file will be obtained from the
% overall US configuration table (see the us_main_config_filename_key defined in
% class_USConfigServer), either as an absolute path or, most preferably, thanks
% to one relative to the overall US configuration directory.



% Implementation notes:
%
% The US-main central server is responsible for the parsing and sharing of all
% US-Main configuration information, and for the management of all automated
% actions - the ones that it provides and the ones implemented by the US-Main
% auxiliary servers.


-include_lib("myriad/include/spawn_utils.hrl").



% The class-specific attributes:
%
% (now, for a better robustness, servers are resolved on the fly, their PIDs are
% not to be stored anymore)
%
-define( class_attributes, [

    { muted_sensor_measurements, user_muted_sensor_points(),
	  "A list expected to contain muted sensor measurement points; "
	  "to be vetted by the sensor manager when it will request it" },

	% Multi-purpose information (not only for home-automation):
	{ server_location, option( user_server_location() ),
	  "the user-specified location of this US-Main server" },

	% Home-automation specific:
	{ home_automation_core_settings, home_automation_core_settings(),
	  "any home automation core settings read (and not specifically checked) "
	  "on behalf of the house automation server" },

	{ contact_files, [ bin_file_path() ], "a list of the known contact files "
	  "(as absolute paths), whence contact information may be read" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Central" ).


% Exported helpers:
-export([ get_execution_target/0 ]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").




% Type shorthands:

-type execution_context() :: basic_utils:execution_context().

-type three_digit_version() :: basic_utils:three_digit_version().

-type ustring() :: text_utils:ustring().

-type bin_file_path() :: file_utils:bin_file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().

-type application_run_context() :: otp_utils:application_run_context().

-type config_table() :: app_facilities:config_table().


%-type user_action_spec() :: us_action:user_action_spec().
-type action_result( T ) :: us_action:action_result( T ).

-type server_pid() :: class_USServer:server_pid().

-type user_muted_sensor_points() ::
	class_USSensorManager:user_muted_sensor_points().

-type home_automation_settings() ::
	class_USHomeAutomationServer:home_automation_settings().



%-type sensor_manager_pid() :: class_USSensorManager:sensor_manager_pid().

%-type position() :: unit_utils:position().

%-type contact_directory_pid() ::
%   class_USContactDirectory:contact_directory_pid().



% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this US-Main central server with an OTP
% supervision tree.


-doc """
Starts and links a supervision bridge for the US-Main central server.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see `us_main_sup:init/1`), hence generally triggered by the application
initialisation.
""".
-spec start_link( application_run_context() ) -> term().
start_link( AppRunContext ) ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the US-Main central server." ),

	supervisor_bridge:start_link( { local, ?bridge_name }, _Module=?MODULE,
                                  _InitArgs=[ AppRunContext ] ).



-doc """
Callback to initialise this supervisor bridge, typically in answer to
`start_link/2` above being executed.
""".
-spec init( list() ) -> { 'ok', pid(), State :: term() }
							| 'ignore' | { 'error', Error :: term() }.
init( _Args=[ AppRunContext ] ) ->

	trace_bridge:info_fmt( "Initialising the US-Main supervisor bridge ~w for "
		"its central server (application run context: ~ts).",
		[ self(), AppRunContext ] ),

	% Not specifically synchronous:
	CtrSrvPid = ?MODULE:new_link( AppRunContext ),

	{ ok, CtrSrvPid, _InitialBridgeState=CtrSrvPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=CtrSrvPid ) when is_pid( CtrSrvPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
		"the central server (reason: ~w, central server: ~w).",
		[ Reason, CtrSrvPid ] ),

	% Synchronicity needed, otherwise a potential race condition exists, leading
	% this process to be killed by its OTP supervisor instead of being normally
	% stopped:
	%
	wooper:delete_synchronously_instance( CtrSrvPid ),

	trace_bridge:debug_fmt( "US-Main central server ~w terminated.",
							[ CtrSrvPid ] ).



-doc """
Constructs the US-Main central server.

`AppRunContext` tells how US-Main is being run.
""".
-spec construct( wooper:state(), application_run_context() ) -> wooper:state().
construct( State, AppRunContext ) ->

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USCentralServer:construct( State, _USAppShortName="main",
        _ServerInit=?trace_categorize("Main central server"), AppRunContext ),

	CfgState = load_and_apply_configuration( SrvState ),

	% Done rather late on purpose, so that the existence of this trace file can
	% be seen as a sign that the initialisation went well (used by
	% start-us-main-{native-build,release}.sh).
	%
	% Now that the log directory is known, we can properly redirect the traces:
    executeConstOneway( CfgState, finaliseTraceSetup ),

	?send_info_fmt( CfgState, "Constructed: ~ts.", [ to_string( CfgState ) ] ),

	CfgState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug( "Deletion initiated." ),

	?info( "Deleted." ),
	State.




% Method section.


-doc """
Returns basic, general main configuration settings (typically for the US-Main
supervisor).
""".
-spec getMainConfigSettings( wooper:state() ) ->
								const_request_return( general_main_settings() ).
getMainConfigSettings( State ) ->

	% (not used currently).

	GenMainSettings = #general_main_settings{},

	?debug_fmt( "Returning the general main configuration settings:~n  ~p",
				[ GenMainSettings ] ),

	wooper:const_return_result( GenMainSettings ).



-doc "Returns suitable sensor settings (typically for the sensor manager).".
-spec getSensorSettings( wooper:state() ) ->
                            const_request_return( user_muted_sensor_points() ).
getSensorSettings( State ) ->
	wooper:const_return_result( ?getAttr(muted_sensor_measurements) ).



-doc """
Returns suitable home automation settings, read from the configuration
(typically on behalf of the home automation server).
""".
-spec getHomeAutomationSettings( wooper:state() ) ->
                        const_request_return( home_automation_settings() ).
getHomeAutomationSettings( State ) ->

	CoreSettingTuple = ?getAttr(home_automation_core_settings),

	FullSettingTuple = list_to_tuple( [ ?getAttr(server_location),
		?getAttr(app_base_directory) | tuple_to_list( CoreSettingTuple ) ] ),

	wooper:const_return_result( FullSettingTuple ).



-doc "Returns suitable contact settings (typically for the contact directory).".
-spec getContactSettings( wooper:state() ) -> const_request_return(
		{ bin_directory_path(), execution_context(), [ bin_file_path() ] } ).
getContactSettings( State ) ->

	ContactSettings = { ?getAttr(config_base_directory),
						?getAttr(execution_context), ?getAttr(contact_files) },

	wooper:const_return_result( ContactSettings ).



-doc "Callback triggered whenever a linked process exits.".
-spec onWOOPERExitReceived( wooper:state(), pid(),
						basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, _StoppedPid, _ExitType=normal ) ->

	% Not even a trace sent for that, as too many of them.
	%
	%?notice_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),

	wooper:const_return();


onWOOPERExitReceived( State, CrashedPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%   {wooper_oneway_failed,<0.44.0>,class_XXX,
	%       FunName,Arity,Args,AtomCause}}, [...]}"

	?error_fmt( "Received and ignored an exit message '~p' from ~w.",
				[ ExitType, CrashedPid ] ),

	wooper:const_return().




% Static section.


-doc "Returns the version of the US application being used.".
-spec get_us_app_version() -> static_return( three_digit_version() ).
get_us_app_version() ->
	wooper:return_static(
		basic_utils:parse_version( get_us_app_version_string() ) ).



-doc "Returns the version of the US application being used, as a string.".
-spec get_us_app_version_string() -> static_return( ustring() ).
get_us_app_version_string() ->
	% As defined (uniquely) in GNUmakevars.inc:
	wooper:return_static( ?us_app_version ).



-doc """
Returns the PID of the current, supposedly already-launched, US-Main central
server, waiting (up to a few seconds, as all US-Main server processes are bound
to be launched mostly simultaneously) if needed.

Typically useful for the various US-Main auxiliary, thematical servers, so that
they can easily access to their configuration information.

It is better to obtain the PID of a server each time from the naming service
rather than to resolve and store its PID once for all, as, for an increased
robustness, servers may be restarted (hence any stored PID may not reference a
live process anymore).
""".
-spec get_server_pid () -> static_return( main_central_server_pid() ).
get_server_pid() ->

	MainCtrPid = class_USServer:resolve_server_pid(
        _RegName=?default_us_main_central_server_registration_name,
        _RegScope=?default_us_main_central_server_registration_scope ),

	wooper:return_static( MainCtrPid ).



-doc """
Creates a mockup of the US-Main central server, notably for the testing of
the other servers.
""".
-spec create_mockup_for_test() -> static_return( server_pid() ).
create_mockup_for_test() ->

	% Clearer than a Y-combinator:
	CfgPid = ?myriad_spawn_link( fun() -> us_main_mockup_srv() end ),

	CfgRegName = ?default_us_main_central_server_registration_name,
	CfgRegScope = ?default_us_main_central_server_registration_scope,

	naming_utils:register_as( CfgPid, CfgRegName, CfgRegScope ),

	trace_bridge:info_fmt( "Created a mock-up US-Main central server ~w, "
		"registered (~ts) as '~ts'.",
		[ CfgPid, CfgRegScope, CfgRegName ] ),


	wooper:return_static( CfgPid ).



us_main_mockup_srv() ->

	%trace_utils:debug( "Mock-up US-Main central server in main loop." ),

	receive

		{ getSensorSettings, [], RequesterPid } ->
			RequesterPid ! { wooper_result, _MutMeasurements=[] },
			us_main_mockup_srv();

		UnexpectedMsg ->
			trace_bridge:error_fmt( "The mock-up US-Main central server "
				"~w received an unexpected (ignored) message: ~p.",
				[ self(), UnexpectedMsg ] ),
			us_main_mockup_srv()

	end.



% Helper section.


-doc """
Loads and applies the relevant configuration settings first from the overall US
configuration file, then from the US-Main specific one.

As a result, the US configuration file is not fully checked as such (e.g. no
extracting and check that no entry remains), we just select the relevant
information from it.
""".
-spec load_and_apply_configuration( wooper:state() ) -> wooper:state().
load_and_apply_configuration( State ) ->

	CfgServerPid = class_USConfigServer:get_us_config_server(
		_CreateIfNeeded=false, State ),

	% This central server is not supposed to read more the US configuration
	% file; it should request it to the overall configuration server, about all
	% the extra information it needs, to avoid duplicated, possibly inconsistent
	% reading/interpretation (and in order to declare itself in the same move):
	%
	CfgServerPid ! { getUSMainRuntimeSettings, [], self() },

	% No possible interleaving:
	receive

		{ wooper_result, { BinCfgDir, ExecContext, MaybeMainCfgFilename } } ->

			StoreState = setAttributes( State, [
				{ execution_context, ExecContext },
				{ config_base_directory, BinCfgDir },
				{ log_directory, ?default_log_base_dir } ] ),

			load_main_config( BinCfgDir, MaybeMainCfgFilename, StoreState )

	end.



-doc """
Loads the US-Main configuration information (that is the corresponding US-Main
configuration file, as identified from the US one), on behalf of the various
services that it offers.
""".
-spec load_main_config( bin_directory_path(), option( bin_file_path() ),
			wooper:state() ) -> wooper:state().
load_main_config( BinCfgBaseDir, _MaybeBinMainCfgFilename=undefined, State ) ->

	DefaultBinMainCfgFilename = ?default_us_main_cfg_filename,

	?info_fmt( "No configuration filename known of the overall US central "
		"server (i.e. none defined in its own configuration file), "
		"hence defaulting to '~ts'.", [ DefaultBinMainCfgFilename ] ),

	load_main_config( BinCfgBaseDir, DefaultBinMainCfgFilename, State );


load_main_config( BinCfgBaseDir, BinMainCfgFilename, State ) ->

	MainCfgFilePath = file_utils:ensure_path_is_absolute( BinMainCfgFilename,
		_BasePath=BinCfgBaseDir ),

	case file_utils:is_existing_file_or_link( MainCfgFilePath ) of

		true ->
			?info_fmt(
                "Reading the US-Main configuration file, found as '~ts'.",
				[ MainCfgFilePath ] );

		false ->
			% Possibly user/group permission issue:
			?error_fmt( "No US-Main configuration file found or accessible "
				"(e.g. symbolic link to an inaccessible file); tried '~ts'.",
				[ MainCfgFilePath ] ),

			throw( { us_main_config_file_not_found,
					 text_utils:binary_to_string( MainCfgFilePath ) } )

	end,

	% Checks that only pairs are found:
	MainCfgTable = table:new_from_unique_entries(
		file_utils:read_etf_file( MainCfgFilePath ) ),

	?debug_fmt( "Read US-Main configuration ~ts",
				[ table:to_string( MainCfgTable ) ] ),

	EpmdState = executeOneway( State, manageEPMDPort,
        [ MainCfgTable, _PortKey=?us_main_epmd_port_key,
          _DefPort=?default_us_main_epmd_port ] ),

	RegState = executeOneway( EpmdState, manageRegistrations,
        [ MainCfgTable, ?default_us_main_central_server_registration_name,
          ?default_us_main_central_server_registration_scope ] ),

	UserState = executeOneway( RegState, manageSystemUser,
        [ MainCfgTable, _UsernameKey=?us_main_username_key ] ),

	AppState = executeOneway( UserState, manageAppBaseDirectories,
        [ MainCfgTable, _BaseDirKey=?us_main_app_base_dir_key,
          _BaseDirEnvVarName=?us_main_app_env_variable ] ),

	DataState = executeOneway( AppState, manageDataDirectory,
        [ MainCfgTable, _DataDirKey=?us_main_data_dir_key,
          _DefaultDataBaseDir=?default_data_base_dir ] ),

	LogState = executeOneway( DataState, manageLogDirectory,
        [ MainCfgTable, _LogDirKey=?us_main_log_dir_key,
          _DefaultLogDir=?default_log_base_dir ] ),

	ContactState = class_USContactDirectory:manage_configuration( MainCfgTable,
																  LogState ),

	SensorState = class_USSensorManager:manage_configuration( MainCfgTable,
															  ContactState ),

	AutomatState = class_USHomeAutomationServer:manage_configuration(
        MainCfgTable, SensorState ),

    % The US servers of this application, federated by this central one:
    SrvClassnames = [ class_USCommunicationGateway, class_USContactDirectory,
                      class_USHomeAutomationServer, class_USSensorManager ],

    ActionState = executeOneway( AutomatState, manageAutomatedActions,
                                 [ MainCfgTable, SrvClassnames ] ),

	FinalState = ActionState,

    % US-Main also has its own key for (non home-automation) actions:
	LicitKeys = ?known_us_main_central_config_keys
		++ class_USContactDirectory:get_licit_config_keys()
		++ class_USSensorManager:get_licit_config_keys()
		% Includes the Oceanic ones:
		++ class_USHomeAutomationServer:get_licit_config_keys(),

	case list_utils:difference( table:keys( MainCfgTable ), LicitKeys ) of

		[] ->
			FinalState;

		UnexpectedKeys ->
			?error_fmt( "Unknown configuration key(s) in '~ts': ~ts~n"
                "Licit ones are: ~ts",
				[ MainCfgFilePath, text_utils:terms_to_string( UnexpectedKeys ),
				  text_utils:terms_to_string( LicitKeys ) ] ),

			throw( { invalid_configuration_keys, UnexpectedKeys,
					 text_utils:binary_to_string( MainCfgFilePath ) } )

	end.




-doc "Built-in help action request.".
-spec help( wooper:state() ) ->
                    const_request_return( action_result( ustring() ) ).
help( State ) ->
    HelpText = "The following actions are supported by the US-Main server:",
    wooper:const_return_result( { success, HelpText } ).


-doc "Returns a textual description of this configuration server.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
    class_USCentralServer:to_string( State ).
