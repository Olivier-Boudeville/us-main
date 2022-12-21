% Copyright (C) 2021-2022 Olivier Boudeville
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


% @doc Singleton server holding the <b>configuration information</b> of the
% US-Main framework.
%
-module(class_USMainConfigServer).


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

-type general_main_settings() :: #general_main_settings{}.


-type read_muted_sensor_measurements() :: list().
% A list expected to contain muted sensor measurement points, typically as read
% from the sensor monitoring section of the US-Main configuration.
%
% Expected to be specified in the form [user_muted_sensor_measurements()]:
% UserMutedMeasurements = [
%   { { nct6792, isa, "0a20" }, [ "AUXTIN1" ] },
%   { { acpitz, acpi, "0" }, all_points }
%                         ].


% For default_us_main_config_server_registration_name:
-include("us_main_defines.hrl").


-export_type([ general_main_settings/0, read_muted_sensor_measurements/0 ]).



% The default filename (as a binary string) for the US-Main configuration (ex:
% sensors), to be found from the overall US configuration directory:
%
-define( default_us_main_cfg_filename, <<"us-main.config">> ).


% The default registration name of the US-Main server:
-define( us_main_config_server_registration_name_key,
		 us_main_config_server_registration_name ).

-define( us_main_app_base_dir_key, us_main_app_base_dir ).
-define( us_main_log_dir_key, us_main_log_dir ).


% Entries for sensor monitoring:

-define( us_main_sensor_key, sensor_monitoring ).

% For measurement points known to report bogus values:
-define( us_main_muted_sensor_measurements, muted_measurements ).

% Designates all (measurement) points of a given sensor:
-define( us_main_all_points, all_points ).

-define( us_main_contact_files_key, us_contact_files ).


% All known, licit (top-level) keys for the US-Main configuration file:
-define( known_config_keys,
		 [ ?us_main_sensor_key, ?us_main_app_base_dir_key,
		   ?us_main_log_dir_key, ?us_main_contact_files_key ] ).


% The last-resort environment variable:
-define( us_main_app_env_variable, "US_MAIN_APP_BASE_DIR" ).


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


-type us_main_config_table() :: table( atom(), term() ).
% A table holding US-Main configuration information.



% Shorthands:

-type execution_context() :: basic_utils:execution_context().

-type ustring() :: text_utils:ustring().

-type bin_file_path() :: file_utils:bin_file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().

-type supervisor_pid() :: otp_utils:supervisor_pid().
-type application_run_context() :: otp_utils:application_run_context().

-type server_pid() :: class_UniversalServer:server_pid().

%-type sensor_manager_pid() :: class_USSensorManager:sensor_manager_pid().

%-type contact_directory_pid() ::
%		class_USContactDirectory:contact_directory_pid().



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

	%{ us_main_username, basic_utils:user_name(),
	%  "the user (if any) who shall launch the US main application" },

	{ us_main_supervisor_pid, supervisor_pid(),
	  "the PID of the OTP supervisor of US-Main, as defined in us_main_sup" },

	{ contact_directory_pid, maybe( contact_directory_pid() ),
	  "the PID (if any) of the US-Main server managing the contact directory" },

	{ sensor_manager_pid, maybe( sensor_manager_pid() ),
	  "the PID (if any) of the US-Main server managing the local sensors" },

	{ muted_sensor_measurements, read_muted_sensor_measurements(),
	  "A list expected to contain muted sensor measurement points; "
	  "to be vetted by the sensor manager when it will request it" },

	{ config_base_directory, bin_directory_path(),
	  "the base directory where all US configuration is to be found "
	  "(not the us_main/priv/conf internal directory)" },

	{ app_base_directory, bin_directory_path(),
	  "the base directory of the US-Main application (the root where "
	  "src, priv, ebin, etc. can be found)" },

	{ conf_directory, bin_directory_path(),
	  "the US-Main internal configuration directory, 'us_main/priv/conf'" },

	{ log_directory, bin_directory_path(), "the directory where (non-VM) US-Main
	  logs shall be written, notabl traces" },

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


% @doc Starts and links a supervision bridge for the US-Main configuration
% server.
%
% Note: typically spawned as a supervised child of the US-Main root supervisor
% (see us_main_sup:init/1), hence generally triggered by the application
% initialisation.
%
-spec start_link( supervisor_pid(), application_run_context() ) -> term().
start_link( SupervisorPid, AppRunContext ) ->

	% Apparently not displayed in a release context, yet executed:
	trace_bridge:debug( "Starting the US-Main supervisor bridge for "
						"the communication gateway." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
		_Module=?MODULE, _InitArgs=[ SupervisorPid, AppRunContext ] ).



% @doc Callback to initialise this supervisor bridge, typically in answer to
% start_link/2 above being executed.
%
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



% @doc Callback to terminate this supervisor bridge.
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




% @doc Constructs the US-Main configuration server.
%
% SupervisorPid is the PID of the main US-Main OTP supervisor, and AppRunContext
% tells how US-Web is being run.
%
-spec construct( wooper:state(), supervisor_pid(),
				 application_run_context() ) -> wooper:state().
construct( State, SupervisorPid, AppRunContext ) ->

	TraceCateg = ?trace_categorize("Configuration Server"),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State, TraceCateg, _TrapExits=true ),

	% Allows functions provided by lower-level libraries (ex: LEEC) called
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

	?send_info( CfgState, "Constructed: " ++ to_string( CfgState ) ),

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



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug( "Deletion initiated." ),

	?info( "Deleted." ),
	State.




% Method section.


% @doc Returns basic, general main configuration settings (typically for the
% us_main supervisor).
%
-spec getMainConfigSettings( wooper:state() ) ->
								const_request_return( general_main_settings() ).
getMainConfigSettings( State ) ->

	% TODO.

	GenMainSettings = #general_main_settings{ },

	?debug_fmt( "Returning the general main configuration settings:~n  ~p",
				[ GenMainSettings ] ),

	wooper:const_return_result( GenMainSettings ).



% @doc Returns suitable contact settings (typically for the contact directory).
-spec getContactSettings( wooper:state() ) ->
			const_request_return( { bin_directory_path(), execution_context(),
									[ bin_file_path() ] } ).
getContactSettings( State ) ->

	ContactSettings = { ?getAttr(config_base_directory),
						?getAttr(execution_context), ?getAttr(contact_files) },

	wooper:const_return_result( ContactSettings ).



% @doc Returns suitable sensor settings (typically for the sensor manager).
-spec getSensorSettings( wooper:state() ) ->
			const_request_return( read_muted_sensor_measurements() ).
getSensorSettings( State ) ->
	wooper:const_return_result( ?getAttr(muted_sensor_measurements) ).



% @doc Callback triggered whenever a linked process exits.
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






% Helper section.


% @doc Loads and applies the relevant configuration settings first from the
% overall US configuration file, then from the more main/vhost specific one.
%
% As a result, the US configuration file is not fully checked as such (ex: no
% extracting and check that no entry remains), we just select the relevant
% information from it.
%
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



% @doc Loads the US-Main configuration information (that is the corresponding
% US-Main configuration file, as identified from the US one), on behalf of the
% various services that it offers.
%
-spec load_main_config( bin_directory_path(), maybe( bin_file_path() ),
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
				"(ex: symbolic link to an inaccessible file); tried '~ts'.",
				[ MainCfgFilePath ] ),

			throw( { us_main_config_file_not_found,
					 text_utils:binary_to_string( MainCfgFilePath ) } )

	end,

	% Checks that only pairs are found:
	MainCfgTable = table:new_from_unique_entries(
					file_utils:read_terms( MainCfgFilePath ) ),

	?debug_fmt( "Read main configuration ~ts",
				[ table:to_string( MainCfgTable ) ] ),

	RegState = manage_registrations( MainCfgTable, State ),

	AppState = manage_app_base_directories( MainCfgTable, RegState ),

	LogState = manage_log_directory( MainCfgTable, AppState ),

	ContactState = manage_contacts( MainCfgTable, LogState ),

	SensorState = manage_sensors( MainCfgTable, ContactState ),

	FinalState = SensorState,

	LicitKeys = ?known_config_keys,

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





% Helper section.



% @doc Manages any user-configured registration names for this instance, for the
% US-Main server and their related services, which may be created here.
%
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



% @doc Manages any user-configured application base directory, and sets related
% directories.
%
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
					% As, if run as a release, it may end with a version (ex:
					% "us_main-0.0.1"), or a "us_main-latest" symlink thereof,
					% or directly as "us-main":
					%
					case filename:basename( BaseDir ) of

						% From a clone made with our deployment conventions:
						"us_main" ++ _ ->
							?info_fmt( "US-Main (release) application base "
								"directory set to '~ts'.", [ BaseDir ] ),
							BinBaseDir;

						% For a clone made to a default directory (ex: by CI):
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



% @doc Tries to guess the US-Main application directory.
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




% @doc Manages any user-configured log directory to rely on, creating it if
% necessary.
%
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
	% be the owner of that directory (ex: if the us-web user is different from
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



% @doc Manages any user-configured contact files.
-spec manage_contacts( us_main_config_table(), wooper:state() ) ->
			wooper:state().
manage_contacts( ConfigTable, State ) ->

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



% @doc Manages any user settings regarding sensors.
-spec manage_sensors( us_main_config_table(), wooper:state() ) ->
										wooper:state().
manage_sensors( ConfigTable, State ) ->

	MutedMeasurements = case table:lookup_entry( ?us_main_sensor_key,
												 ConfigTable ) of

		key_not_found ->
			?info( "No user settings regarding sensors." ),
			[];


		{ value, SensorSettings } when is_list( SensorSettings ) ->

			{ MutMeasurements, ShrunkSensorSettings } =
				list_table:extract_entry_with_default(
					?us_main_muted_sensor_measurements, _DefPoints=[],
					SensorSettings ),

			%?debug_fmt( "Muted sensor measurement points read as:~n ~p",
			%            [ MutMeasurements ] ),

			is_list( MutMeasurements ) orelse
				begin

					?error_fmt( "Invalid muted sensor measurement points "
						"specified (must be a list): ~p",
						[ MutMeasurements ] ),

					throw( { invalid_muted_sensor_measurement_points,
						MutMeasurements, ?us_main_muted_sensor_measurements } )

				end,

			ShrunkSensorSettings =:= [] orelse
				begin

					?error_fmt( "Unexpected extra sensor settings: ~p.",
								[ ShrunkSensorSettings ] ),

					throw( { extra_sensor_settings, ShrunkSensorSettings,
							 ?us_main_muted_sensor_measurements } )

				end,

			% Kept verbatim, will be vetted by the sensor manager:
			MutMeasurements;


		{ value, InvalidSensorSettings }  ->

			?error_fmt( "Read invalid user settings regarding sensors: '~p'.",
						[ InvalidSensorSettings ] ),

			throw( { invalid_us_sensor_settings, InvalidSensorSettings,
					 ?us_main_sensor_key } )

	end,

	% Not specifically checked at this level, will be done by the sensor
	% manager:
	%
	setAttribute( State, muted_sensor_measurements, MutedMeasurements ).



% Static section.


% @doc Returns the PID of the US-Main configuration server, waiting (up to a few
% seconds, as all US-Main servers are bound to be launched mostly
% simultaneously) if needed.
%
% Typically useful for the various other US-Main servers, so that they can
% easily access to their configuration information.
%
-spec get_us_main_config_server() -> static_return( server_pid() ).
get_us_main_config_server() ->

	Scope = naming_utils:registration_to_look_up_scope(
				?default_us_main_config_server_registration_scope ),

	CfgPid = naming_utils:wait_for_registration_of(
				?default_us_main_config_server_registration_name, Scope ),

	wooper:return_static( CfgPid ).




% @doc Returns a textual description of this configuration server.
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
