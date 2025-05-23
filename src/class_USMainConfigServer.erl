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

-module(class_USMainConfigServer).

-moduledoc """
Singleton server holding the **configuration information** of the US-Main
framework.
""".


-define( class_description,
		 "Singleton server holding the configuration information of the "
		 "US-Main framework." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% This communication gateway is designed to be able to integrate to an OTP
% supervision tree thanks to a supervisor bridge, whose behaviour is directly
% defined in this module. See https://wooper.esperide.org/#otp-guidelines for
% further information.
%
-behaviour(supervisor_bridge).


% User API of the bridge:
-export([ start_link/2 ]).

% Callbacks of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).

-define( bridge_name, ?MODULE ).



% For the general_main_settings records:
-include("class_USMainConfigServer.hrl").

-doc "General US-Main settings".
-type general_main_settings() :: #general_main_settings{}.



-doc "Checked more precisely as a position() in the home automation server.".
-type user_server_location() ::
	{ LatDegrees :: float(), LongDegrees :: float() }.


-doc "A table holding US-Main configuration information.".
-type us_main_config_table() :: table( atom(), term() ).



% For default_us_main_config_server_registration_name:
-include("us_main_defines.hrl").


-export_type([ general_main_settings/0, user_server_location/0,
			   us_main_config_table/0 ]).


% Must be kept consistent with the default_us_main_epmd_port variable in
% us-main-common.sh:
%
-define( default_us_main_epmd_port, 4507 ).


% The default filename (as a binary string) for the US-Main configuration (e.g.
% sensors), to be found from the overall US configuration directory:
%
-define( default_us_main_cfg_filename, <<"us-main.config">> ).

-define( us_main_epmd_port_key, epmd_port ).

% The default registration name of the US-Main server:
-define( us_main_config_server_registration_name_key,
		 us_main_config_server_registration_name ).

-define( us_main_username_key, us_main_username ).
-define( us_main_app_base_dir_key, us_main_app_base_dir ).
-define( us_main_data_dir_key, us_main_data_dir ).
-define( us_main_log_dir_key, us_main_log_dir ).



% All known, licit (top-level) base keys for the US-Main configuration file:
% (other keys are in their respective server classes)
%
-define( known_us_main_config_keys, [ ?us_main_epmd_port_key,
	?us_main_config_server_registration_name_key,
	?us_main_username_key, ?us_main_app_base_dir_key,
	?us_main_data_dir_key, ?us_main_log_dir_key ] ).



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
% This US-Main configuration server will ensure that an overall US
% (i.e. US-Common) configuration server is running, either by fetching its PID
% if already existing (which is most probably the case), otherwise by launching
% it accordingly.
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
% The US-main configuration server is responsible for the parsing and sharing of
% all US-Main configuration information.


-include_lib("myriad/include/spawn_utils.hrl").


% Type shorthands:

-type execution_context() :: basic_utils:execution_context().
-type three_digit_version() :: basic_utils:three_digit_version().

-type ustring() :: text_utils:ustring().

-type bin_file_path() :: file_utils:bin_file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().

-type supervisor_pid() :: otp_utils:supervisor_pid().
-type application_run_context() :: otp_utils:application_run_context().



-type server_pid() :: class_USServer:server_pid().

-type user_muted_sensor_points() ::
	class_USSensorManager:user_muted_sensor_points().

-type home_automation_settings() ::
	class_USHomeAutomationServer:home_automation_settings().



%-type sensor_manager_pid() :: class_USSensorManager:sensor_manager_pid().

%-type position() :: unit_utils:position().

%-type contact_directory_pid() ::
%   class_USContactDirectory:contact_directory_pid().




% The class-specific attributes:
-define( class_attributes, [

	{ execution_context, execution_context(),
	  "tells whether this server is to run in development or production mode" },

	% As it impacts at least various paths:
	{ app_run_context, application_run_context(),
	  "tells how US-Main is run, natively (using the Ceylan build/run system) "
	  "or as an OTP release" },

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" },

	{ us_main_supervisor_pid, supervisor_pid(),
	  "the PID of the OTP supervisor of US-Main, as defined in us_main_sup" },

	{ contact_directory_pid, option( contact_directory_pid() ),
	  "the PID (if any) of the US-Main server managing the contact directory" },

	{ sensor_manager_pid, option( sensor_manager_pid() ),
	  "the PID (if any) of the US-Main server managing the local sensors" },

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

	{ config_base_directory, bin_directory_path(),
	  "the base directory where all US configuration is to be found "
	  "(not the us_main/priv/conf internal directory)" },

	{ app_base_directory, bin_directory_path(),
	  "the base directory of the US-Main application (the root where "
	  "src, priv, ebin, etc. can be found)" },

	{ conf_directory, bin_directory_path(),
	  "the US-Main internal configuration directory, 'us_main/priv/conf'" },

	{ data_directory, bin_directory_path(),
	  "the directory where the US-Main working data is to be stored "
	  "(typically [...]/us_main/priv/data)" },

	{ log_directory, bin_directory_path(), "the directory where (non-VM) US-Main
	  logs shall be written, notably traces" },

	{ contact_files, [ bin_file_path() ], "a list of the known contact files "
	  "(as absolute paths), whence contact information may be read" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Configuration" ).


% Exported helpers:
-export([ get_execution_target/0 ]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").



% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this US-Main configuration server with an OTP
% supervision tree.


-doc """
Starts and links a supervision bridge for the US-Main configuration server.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see us_main_sup:init/1), hence generally triggered by the application
initialisation.

""".
-spec start_link( supervisor_pid(), application_run_context() ) -> term().
start_link( SupervisorPid, AppRunContext ) ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the communication gateway." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
		_Module=?MODULE, _InitArgs=[ SupervisorPid, AppRunContext ] ).



-doc """
Callback to initialise this supervisor bridge, typically in answer to
start_link/2 above being executed.
""".
-spec init( list() ) -> { 'ok', pid(), State :: term() }
							| 'ignore' | { 'error', Error :: term() }.
init( _Args=[ SupervisorPid, AppRunContext ] ) ->

	trace_bridge:info_fmt( "Initializing the US-Main supervisor bridge ~w for "
		"the configuration server (US-Main OTP root supervisor: ~w; "
		"application run context: ~ts).",
		[ self(), SupervisorPid, AppRunContext ] ),

	% Not specifically synchronous:
	CfgSrvPid = ?MODULE:new_link( SupervisorPid, AppRunContext ),

	{ ok, CfgSrvPid, _InitialBridgeState=CfgSrvPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=CfgSrvPid ) when is_pid( CfgSrvPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
		"the configuration server (reason: ~w, configuration server: ~w).",
		[ Reason, CfgSrvPid ] ),

	% Synchronicity needed, otherwise a potential race condition exists, leading
	% this process to be killed by its OTP supervisor instead of being normally
	% stopped:
	%
	wooper:delete_synchronously_instance( CfgSrvPid ),

	trace_bridge:debug_fmt( "US-Main configuration server ~w terminated.",
							[ CfgSrvPid ] ).



-doc """
Constructs the US-Main configuration server.

SupervisorPid is the PID of the main US-Main OTP supervisor, and AppRunContext
tells how US-Web is being run.
""".
-spec construct( wooper:state(), supervisor_pid(),
				 application_run_context() ) -> wooper:state().
construct( State, SupervisorPid, AppRunContext ) ->

	TraceCateg = ?trace_categorize("Configuration Server"),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State, TraceCateg, _TrapExits=true ),

	% Allows functions provided by lower-level libraries (e.g. LEEC) called
	% directly from this instance process to plug to the same (trace aggregator)
	% bridge, with the same settings:
	%
	class_TraceEmitter:register_bridge( TraceState ),

	?send_info_fmt( TraceState, "Creating a US-Main configuration server, "
		"running ~ts.",
		[ otp_utils:application_run_context_to_string( AppRunContext ) ] ),

	?send_debug_fmt( TraceState, "Running Erlang ~ts, whose ~ts",
		[ system_utils:get_interpreter_version(),
		  code_utils:get_code_path_as_string() ] ),

	?send_debug_fmt( TraceState, "System description: ~ts",
		[ system_utils:get_system_description() ] ),


	% Other attributes set by the next function:
	SupState = setAttributes( TraceState, [
		{ app_run_context, AppRunContext },
		{ us_main_supervisor_pid, SupervisorPid } ] ),

	CfgState = load_and_apply_configuration( SupState ),

	?send_info_fmt( CfgState, "Constructed: ~ts.", [ to_string( CfgState ) ] ),

	% Done rather late on purpose, so that the existence of that file can be
	% seen as a sign that the initialisation went well (used by
	% start-us-main-{native-build,release}.sh).
	%
	% Now that the log directory is known, we can properly redirect the traces.
	% Already a trace emitter:

	LogDirBin = getAttribute( CfgState, log_directory ),

	file_utils:create_directory_if_not_existing( LogDirBin, create_parents ),

	NewBinTraceFilePath = file_utils:bin_join( LogDirBin, "us_main.traces" ),

	?send_debug_fmt( CfgState, "Requesting the renaming of trace file "
					 "to '~ts'.", [ NewBinTraceFilePath ] ),

	getAttribute( CfgState, trace_aggregator_pid ) !
		{ renameTraceFile, NewBinTraceFilePath },

	CfgState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug( "Deletion initiated." ),

	?info( "Deleted." ),
	State.




% Method section.


-doc """
Returns basic, general main configuration settings (typically for the us_main
supervisor).
""".
-spec getMainConfigSettings( wooper:state() ) ->
								const_request_return( general_main_settings() ).
getMainConfigSettings( State ) ->

	% TODO.

	GenMainSettings = #general_main_settings{ },

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
-spec getContactSettings( wooper:state() ) ->
	const_request_return(
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


% Version-related static methods.


-doc "Returns the version of the US-Main library being used.".
-spec get_us_main_version() -> static_return( three_digit_version() ).
get_us_main_version() ->
	wooper:return_static(
		basic_utils:parse_version( get_us_main_version_string() ) ).



-doc "Returns the version of the US-Main library being used, as a string.".
-spec get_us_main_version_string() -> static_return( ustring() ).
get_us_main_version_string() ->
	% As defined (uniquely) in GNUmakevars.inc:
	wooper:return_static( ?us_main_version ).



-doc """
Returns the PID of the US-Main configuration server, waiting (up to a few
seconds, as all US-Main servers are bound to be launched mostly simultaneously)
if needed.

Typically useful for the various other US-Main servers, so that they can easily
access to their configuration information.
""".
-spec get_us_main_config_server() -> static_return( server_pid() ).
get_us_main_config_server() ->

	CfgRegName = ?default_us_main_config_server_registration_name,

	CfgLookupScope = naming_utils:registration_to_look_up_scope(
		?default_us_main_config_server_registration_scope ),

	CfgPid = naming_utils:wait_for_registration_of( CfgRegName,
													CfgLookupScope ),

	%trace_bridge:debug_fmt( "Resolved the US-Main configuration server as ~w, "
	%   "(name: '~ts', look-up scope: ~ts).",
	%   [ CfgPid, CfgRegName, CfgLookupScope ] ),

	wooper:return_static( CfgPid ).



-doc """
Creates a mockup of the US-Main configuration server, notably for the testing of
the other servers.
""".
-spec create_mockup_for_test() -> static_return( server_pid() ).
create_mockup_for_test() ->

	% Clearer than a Y-combinator:
	CfgPid = ?myriad_spawn_link( fun() -> us_main_mockup_srv() end ),

	CfgRegName = ?default_us_main_config_server_registration_name,

	CfgRegScope = ?default_us_main_config_server_registration_scope,

	naming_utils:register_as( CfgPid, CfgRegName, CfgRegScope ),

	trace_bridge:info_fmt( "Created a mock-up US-Main configuration server ~w, "
		"registered (~ts) as '~ts'.",
		[ CfgPid, CfgRegScope, CfgRegName ] ),


	wooper:return_static( CfgPid ).



us_main_mockup_srv() ->

	%trace_utils:debug( "Mock-up US-Main configuration server in main loop." ),

	receive

		{ getSensorSettings, [], RequesterPid } ->

			RequesterPid ! { wooper_result, _MutMeasurements=[] },
			us_main_mockup_srv();

		UnexpectedMsg ->
			trace_bridge:error_fmt( "The mock-up US-Main configuration server "
				"~w received an unexpected (ignored) message: ~p.",
				[ self(), UnexpectedMsg ] ),
			us_main_mockup_srv()

	end.



% Helper section.


-doc """
Loads and applies the relevant configuration settings first from the overall US
configuration file, then from the more main/vhost specific one.

As a result, the US configuration file is not fully checked as such (e.g. no
extracting and check that no entry remains), we just select the relevant
information from it.
""".
-spec load_and_apply_configuration( wooper:state() ) -> wooper:state().
load_and_apply_configuration( State ) ->

	CfgServerPid = class_USConfigServer:get_us_config_server(
		_CreateIfNeeded=false, State ),

	% This main configuration server is not supposed to read more the US
	% configuration file; it should request it to the overall configuration
	% server, about all the extra information it needs, to avoid duplicated,
	% possibly inconsistent reading/interpretation (and in order to declare
	% itself in the same move):
	%
	CfgServerPid ! { getUSMainRuntimeSettings, [], self() },

	% No possible interleaving:
	receive

		{ wooper_result, { BinCfgDir, ExecContext, MaybeMainCfgFilename } } ->

			StoreState = setAttributes( State, [
				{ execution_context, ExecContext },
				{ config_base_directory, BinCfgDir },
				{ log_directory, ?default_log_base_dir },
				{ us_config_server_pid, CfgServerPid } ] ),

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

	?info_fmt( "No configuration filename known of the overall US configuration"
		" server (i.e. none defined in its own configuration file), "
		"hence defaulting to '~ts'.", [ DefaultBinMainCfgFilename ] ),

	load_main_config( BinCfgBaseDir, DefaultBinMainCfgFilename, State );


load_main_config( BinCfgBaseDir, BinMainCfgFilename, State ) ->

	MainCfgFilePath = file_utils:ensure_path_is_absolute( BinMainCfgFilename,
												_BasePath=BinCfgBaseDir ),

	case file_utils:is_existing_file_or_link( MainCfgFilePath ) of

		true ->
			?info_fmt( "Reading US-Main configuration file, found as '~ts'.",
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
		file_utils:read_terms( MainCfgFilePath ) ),

	?debug_fmt( "Read main configuration ~ts",
				[ table:to_string( MainCfgTable ) ] ),

	EpmdState = manage_epmd_port( MainCfgTable, State ),

	RegState = manage_registrations( MainCfgTable, EpmdState ),

	UserState = manage_os_user( MainCfgTable, RegState ),

	AppState = manage_app_base_directories( MainCfgTable, UserState ),

	DataState = manage_data_directory( MainCfgTable, AppState ),

	LogState = manage_log_directory( MainCfgTable, DataState ),

	ContactState = class_USContactDirectory:manage_configuration( MainCfgTable,
																  LogState ),

	SensorState = class_USSensorManager:manage_configuration( MainCfgTable,
															  ContactState ),

	AutomatState = class_USHomeAutomationServer:manage_configuration(
		MainCfgTable, SensorState ),

	FinalState = AutomatState,

	LicitKeys = ?known_us_main_config_keys
		++ class_USContactDirectory:get_licit_config_keys()
		++ class_USSensorManager:get_licit_config_keys()
		% Includes the Oceanic ones:
		++ class_USHomeAutomationServer:get_licit_config_keys(),

	case list_utils:difference( table:keys( MainCfgTable ), LicitKeys ) of

		[] ->
			FinalState;

		UnexpectedKeys ->

			?error_fmt( "Unknown key(s) in '~ts': ~ts~nLicit keys: ~ts",
				[ MainCfgFilePath, text_utils:terms_to_string( UnexpectedKeys ),
				  text_utils:terms_to_string( LicitKeys ) ] ),

			throw( { invalid_configuration_keys, UnexpectedKeys,
					 text_utils:binary_to_string( MainCfgFilePath ) } )

	end.



-doc """
Manages any US-Main level user-configured EPMD port.

The port may be already set at the US overall level, but it can be overridden on
a per-US application basis, as it may be convenient to share one's us.config
between multiple applications (e.g. US-Main and US-Web).
""".
-spec manage_epmd_port( us_main_config_table(), wooper:state() ) ->
										wooper:state().
manage_epmd_port( ConfigTable, State ) ->

	% No simple, integrated way of checking the actual port currently in use:
	{ Port, Origin } = case table:lookup_entry( ?us_main_epmd_port_key,
												ConfigTable ) of

		key_not_found ->
			% No US-Main EPMD port defined, so its default will apply unless a
			% port was explicitly set at the US-level:
			%
			DefaultUSMainEpmdPort = ?default_us_main_epmd_port,

			?info_fmt( "No user-configured EPMD TCP port for US-Main, "
				"proposing its default one, ~B.", [ DefaultUSMainEpmdPort  ] ),

			{ DefaultUSMainEpmdPort, as_default };


		{ value, UserEPMDPort } when is_integer( UserEPMDPort ) ->
			?info_fmt( "Supposing already running using the user-defined "
					   "US-Main EPMD TCP port #~B.", [ UserEPMDPort ] ),

			{ UserEPMDPort, explicit_set };


		{ value, InvalidEPMDPort } ->
			?error_fmt( "Read invalid user-configured US-Main EPMD port: '~p'.",
						[ InvalidEPMDPort ] ),
			throw( { invalid_us_main_epmd_port, InvalidEPMDPort,
					 ?us_main_epmd_port_key } )

	end,

	% For correct information; available by design:
	?getAttr(us_config_server_pid) !
		{ notifyEPMDPort, [ Port, Origin, ?MODULE, self() ] },

	% Const:
	State.



-doc """
Manages any user-configured registration names for this instance, for the
US-Main server and their related services, which may be created here.
""".
-spec manage_registrations( us_main_config_table(), wooper:state() ) ->
									wooper:state().
manage_registrations( _ConfigTable, State ) ->

	% As multiple US-Main may coexist, local name registration is preferred; and
	% overriding such names would be unnecessarily complex.

	CfgRegName = ?default_us_main_config_server_registration_name,

	CfgRegScope = ?default_us_main_config_server_registration_scope,

	naming_utils:register_as( CfgRegName, CfgRegScope ),

	?info_fmt( "This US-Main configuration server was registered as '~ts' "
		"(scope: ~ts).", [ CfgRegName, CfgRegScope ] ),

	setAttributes( State, [
		% Inherited:
		{ registration_name, CfgRegName },
		{ registration_scope, CfgRegScope } ] ).



-doc """
Manages any user-configured specification regarding the (operating-system level)
US-Main user.
""".
-spec manage_os_user( us_main_config_table(), wooper:state() ) ->
									wooper:state().
manage_os_user( ConfigTable, State ) ->

	% Mostly used by start/stop/kill scripts:
	MainUsername = case table:lookup_entry( ?us_main_username_key,
											ConfigTable ) of

		key_not_found ->
			ActualUsername = system_utils:get_user_name(),
			?info_fmt( "No user-configured US-Main operating-system username "
				"set for this server; runtime-detected: '~ts'.",
				[ ActualUsername ] ),
			ActualUsername;

		{ value, Username } when is_list( Username ) ->

			% No overriding expected:
			basic_utils:check_undefined( ?getAttr(username) ),

			case system_utils:get_user_name() of

				Username ->
					?info_fmt( "Using user-configured US-Main operating-system "
						"username '~ts' for this server, which matches "
						"the current runtime user.", [ Username ] ),
					Username;

				OtherUsername ->
					?error_fmt( "The user-configured US-Main operating-system "
						"username '~ts' for this server does not match "
						"the current runtime user, '~ts'.",
						[ Username, OtherUsername ] ),
					throw( { inconsistent_os_us_main_user, OtherUsername,
							 Username, ?us_main_username_key } )

			end

	end,

	setAttribute( State, username,
				  text_utils:string_to_binary( MainUsername ) ).



-doc """
Manages any user-configured application base directory, and sets related
directories.
""".
-spec manage_app_base_directories( us_main_config_table(), wooper:state() ) ->
										wooper:state().
manage_app_base_directories( ConfigTable, State ) ->

	% As opposed to, say, start/stop script, the Erlang code does not care so
	% much about these directories, so warnings, not errors, were issued if
	% not found (the US framework being also launchable thanks to, for example,
	% 'make debug'). We finally opted for a stricter policy, as errors could be
	% induced afterwards.

	AppRunContext = ?getAttr(app_run_context),

	MaybeConfBaseDir = case table:lookup_entry( ?us_main_app_base_dir_key,
												ConfigTable ) of

		key_not_found ->
			undefined;

		{ value, D } when is_list( D ) ->
			?info_fmt( "User-configured US-Main application base directory "
					   "is '~ts'.", [ D ] ),
			D;

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured US-Main application base "
						"directory: '~p'.", [ InvalidDir ] ),
			throw( { invalid_us_main_app_base_directory, InvalidDir,
					 ?us_main_app_base_dir_key, AppRunContext } )

	end,

	MaybeBaseDir = case MaybeConfBaseDir of

		undefined ->
			case system_utils:get_environment_variable(
					?us_main_app_env_variable ) of

				false ->
					undefined;

				% Might be set, yet to an empty string, typically because of
				% US_MAIN_APP_BASE_DIR="${US_MAIN_APP_BASE_DIR}":
				%
				"" ->
					undefined;

				EnvDir ->
					?info_fmt( "No user-configured US-Main application base "
						"directory set in configuration file, using the value "
						"of the '~ts' environment variable: '~ts'.",
						[ ?us_main_app_env_variable, EnvDir ] ),
					EnvDir

			end;

		_ ->
			MaybeConfBaseDir

	end,

	RawBaseDir = case MaybeBaseDir of

		undefined ->
			guess_app_dir( AppRunContext, State );

		_ ->
			MaybeBaseDir

	end,

	BaseDir = file_utils:ensure_path_is_absolute( RawBaseDir ),

	% We check not only that this candidate app directory exists, but also that
	% it is a right one, expecting to have a 'priv' direct subdirectory then:

	MaybeBaseBinDir =
			case file_utils:is_existing_directory_or_link( BaseDir ) of

		true ->
			BinBaseDir = text_utils:string_to_binary( BaseDir ),
			case AppRunContext of

				as_otp_release ->
					% As, if run as a release, it may end with a version (e.g.
					% "us_main-0.0.1"), or a "us_main-latest" symlink thereof,
					% or directly as "us-main":
					%
					case filename:basename( BaseDir ) of

						% From a clone made with our deployment conventions:
						"us_main" ++ _ ->
							?info_fmt( "US-Main (release) application base "
								"directory set to '~ts'.", [ BaseDir ] ),
							BinBaseDir;

						% For a clone made to a default directory (e.g. by CI):
						"us-main" ++ _ ->
							?info_fmt( "US-Main (release) application base "
								"directory set to '~ts'.", [ BaseDir ] ),
							BinBaseDir;

						_Other ->
							%?warning_fmt( "The US-Main application base "
							%  "directory '~ts' does not seem legit (it "
							%  "should end with 'us_main'), thus considering "
							%  "knowing none.", [ BaseDir ] ),
							%undefined
							throw( { incorrect_us_main_app_base_directory,
									 BaseDir, ?us_main_app_base_dir_key,
									 AppRunContext } )

					end;

				as_native ->
					case file_utils:get_last_path_element( BaseDir ) of

						"us_main" ->
							?info_fmt( "US-Main (native) application base "
									   "directory set to '~ts'.", [ BaseDir ] ),
							BinBaseDir;

						_Other ->
							throw( { incorrect_us_main_app_base_directory,
									 BaseDir, ?us_main_app_base_dir_key,
									 AppRunContext } )

					end

			end,

			% Final paranoid check:
			PrivDir = file_utils:join( BinBaseDir, "priv" ),
			case file_utils:is_existing_directory_or_link( PrivDir ) of

				true ->
					BinBaseDir;

				false ->
					?error_fmt( "The determined US-Main application base "
						"directory '~ts' does not have a 'priv' subdirectory.",
						[ BinBaseDir ] ),
					throw( { no_priv_us_main_app_base_directory, BaseDir,
							 ?us_main_app_base_dir_key } )

			end;


		false ->
			%?warning_fmt( "The US-Main application base directory '~ts' does "
			%   "not exist, thus considering knowing none.", [ BaseDir ] ),
			%undefined
			throw( { non_existing_us_main_app_base_directory, BaseDir,
					 ?us_main_app_base_dir_key } )


	end,

	% The internal US-Main directory (see conf_directory) used to be derived
	% from the app base one (as a 'conf' subdirectory thereof), yet because of
	% that it was not included in releases. So instead this 'conf' directory is
	% a subdirectory of 'priv':
	%
	% (for some reason, using this module, although it is listed in us_main.app,
	% results with code:priv_dir/1 in a bad_name exception)
	%
	%TargetMod = ?MODULE,
	%TargetMod = us_main_app,
	TargetMod = us_main_sup,

	ConfBinDir = file_utils:bin_join(
		otp_utils:get_priv_root( TargetMod, _BeSilent=true ), "conf" ),

	% Set in all cases:
	setAttributes( State, [ { app_base_directory, MaybeBaseBinDir },
							{ conf_directory, ConfBinDir } ] ).



-doc "Tries to guess the US-Main application directory.".
guess_app_dir( AppRunContext, State ) ->

	CurrentDir = file_utils:get_current_directory(),

	GuessingDir = case AppRunContext of

		as_otp_release ->
			% In [...]/us_main/_build/default/rel/us_main, and we want the first
			% us_main, so:
			%
			OTPPath = file_utils:normalise_path( file_utils:join(
							[ CurrentDir, "..", "..", "..", ".." ] ) ),

			case file_utils:get_base_path( OTPPath ) of

				"us_main" ->
					% Looks good:
					OTPPath;

				% Not found; another try, if running as a test (from
				% us_main/test):
				%
				_ ->
					file_utils:get_base_path( CurrentDir )

			end;

		as_native ->
			% In the case of a native build, running from us_main/src (covers
			% also the case where a test is being run from us_main/test), so:
			%
			file_utils:get_base_path( CurrentDir )

	end,

	% Was a warning:
	?info_fmt( "No user-configured US-Main application base directory set "
		"(neither in configuration file nor through the '~ts' environment "
		"variable), hence trying to guess it, in a ~ts context, as '~ts'.",
		[ ?us_main_app_env_variable, AppRunContext, GuessingDir ] ),

	GuessingDir.



-doc """
Manages any user-configured data directory to rely on, creating it if necessary.
""".
-spec manage_data_directory( us_main_config_table(), wooper:state() ) ->
									wooper:state().
manage_data_directory( ConfigTable, State ) ->

	BaseDir = case table:lookup_entry( ?us_main_data_dir_key, ConfigTable ) of

		key_not_found ->
			file_utils:ensure_path_is_absolute( ?default_data_base_dir,
				?getAttr(app_base_directory) );

		{ value, D } when is_list( D ) ->
			file_utils:ensure_path_is_absolute( D,
				?getAttr(app_base_directory) );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured data directory: '~p'.",
						[ InvalidDir ] ),
			throw( { invalid_data_directory, InvalidDir,
					 ?us_main_data_dir_key } )

	end,

	file_utils:is_existing_directory( BaseDir ) orelse
		?warning_fmt( "The base data directory '~ts' does not exist, "
					  "creating it.", [ BaseDir ] ),

	% Would lead to inconvenient paths, at least if defined as relative:
	%DataDir = file_utils:join( BaseDir, ?app_subdir ),
	DataDir = BaseDir,

	try

		file_utils:create_directory_if_not_existing( DataDir, create_parents )

	catch

		{ create_directory_failed, _DataDir, eacces } ->

			% Clearer than system_utils:get_user_name_string/0:
			Username = system_utils:get_user_name(),

			?error_fmt( "Unable to create the directory for working data "
				"'~ts': please ensure its parent directory can be written "
				"by user '~ts', or set it to different path thanks to the "
				"'~ts' key.", [ DataDir, Username, ?us_main_data_dir_key ] ),

			throw( { data_directory_creation_failed, DataDir, eacces,
					 Username } );

		E ->
			throw( { data_directory_creation_failed, DataDir, E } )

	end,

	% Enforce security in all cases ("chmod 700"); if it fails here, the
	% combined path/user configuration must be incorrect; however we might not
	% be the owner of that directory (e.g. if the us-main user is different from
	% the us one). So:
	%
	CurrentUserId = system_utils:get_user_id(),

	% If not owned, do nothing:
	file_utils:get_owner_of( DataDir ) =:= CurrentUserId andalso
		file_utils:change_permissions( DataDir,
			[ owner_read, owner_write, owner_execute,
			  group_read, group_write, group_execute ] ),

	BinDataDir = text_utils:ensure_binary( DataDir ),

	setAttribute( State, data_directory, BinDataDir ).



-doc """
Manages any user-configured log directory to rely on, creating it if necessary.
""".
-spec manage_log_directory( us_main_config_table(), wooper:state() ) ->
								wooper:state().
manage_log_directory( ConfigTable, State ) ->

	% Longer paths if defined as relative, yet finally preferred as
	% '/var/log/universal-server/us-main' (rather than
	% '/var/log/universal-server') as it allows separating US-Main from any
	% other US-* services:
	%
	LogDir = case table:lookup_entry( ?us_main_log_dir_key, ConfigTable ) of

		key_not_found ->
			% Bound to require special permissions:
			?default_log_base_dir;

		{ value, D } when is_list( D ) ->
			file_utils:ensure_path_is_absolute( D,
												?getAttr(app_base_directory) );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured log directory: '~p'.",
						[ InvalidDir ] ),
			throw( { invalid_log_directory, InvalidDir, ?us_main_log_dir_key } )

	end,

	file_utils:is_existing_directory( LogDir ) orelse
		begin

			%throw( { non_existing_base_us_web_log_directory, LogDir } )

			?warning_fmt( "The base US-Web log directory '~ts' does not exist, "
						  "creating it.", [ LogDir ] ),

			% As for example the default path would require to create
			% /var/log/universal-server/us-web:
			%
			file_utils:create_directory_if_not_existing( LogDir,
														 create_parents )

		end,

	% Enforce security in all cases ("chmod 700"); if it fails here, the
	% combined path/user configuration must be incorrect; however we might not
	% be the owner of that directory (e.g. if the us-main user is different from
	% the US-Common one).
	%
	% So:
	%
	CurrentUserId = system_utils:get_user_id(),

	% If not owned, does nothing:
	CurrentUserId =:= file_utils:get_owner_of( LogDir ) andalso
		begin

			Perms = [ owner_read, owner_write, owner_execute,
					  group_read, group_write, group_execute ],

			file_utils:change_permissions( LogDir, Perms )

		end,

	BinLogDir = text_utils:ensure_binary( LogDir ),

	setAttribute( State, log_directory, BinLogDir ).



-doc "Returns a textual description of this configuration server.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	text_utils:format( "US-Main configuration ~ts, running ~ts, "
		"running in the ~ts execution context, "
		"knowing US overall configuration server ~w and "
		"OTP supervisor ~w, relying on the '~ts' configuration directory",
		[ class_USServer:to_string( State ),
		  otp_utils:application_run_context_to_string(
			?getAttr(app_run_context ) ), ?getAttr(execution_context),
		  ?getAttr(us_config_server_pid), ?getAttr(us_main_supervisor_pid),
		  ?getAttr(config_base_directory) ] ).
