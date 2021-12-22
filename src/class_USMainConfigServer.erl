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



% For the general_main_settings records:
-include("class_USMainConfigServer.hrl").

-type general_main_settings() :: #general_main_settings{}.


% For default_us_main_config_server_registration_name:
-include("us_main_defines.hrl").


-export_type([ general_main_settings/0 ]).



% The default filename (as a binary string) for the US-Main configuration (ex:
% sensors), to be found from the overall US configuration directory:
%
-define( default_us_main_cfg_filename, <<"us-main.config">> ).


% The default registration name of the US-Main server:
-define( us_main_config_server_registration_name_key,
		 us_main_config_server_registration_name ).


-define( us_main_sensor_key, sensor_monitoring ).

-define( us_main_muted_points_key, muted_measurement_points ).
-define( us_main_all_points, all_points ).

-define( us_main_contact_files_key, us_contact_files ).


% All known, licit (top-level) keys for the US-Main configuration file:
-define( known_config_keys, [ ?us_main_sensor_key,
							  ?us_main_contact_files_key ] ).



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


% To silence attribute-only types:
-export_type([  ]).


% Shorthands:

%-type error_reason() :: basic_utils:error_reason().

-type ustring() :: text_utils:ustring().
%-type any_string() :: text_utils:any_string().

%-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
%-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
%-type any_directory_path() :: file_utils:any_directory_path().

%-type registration_name() :: naming_utils:registration_name().
%-type registration_scope() :: naming_utils:registration_scope().

-type supervisor_pid() :: otp_utils:supervisor_pid().
-type application_run_context() :: otp_utils:application_run_context().

-type server_pid() :: class_UniversalServer:server_pid().



% The class-specific attributes:
-define( class_attributes, [

	{ execution_context, basic_utils:execution_context(),
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

	{ config_base_directory, bin_directory_path(),
	  "the base directory where all US configuration is to be found "
	  "(not the us_main/priv/conf internal directory)" },

	%{ app_base_directory, bin_directory_path(),
	%  "the base directory of the US-Main application (the root where "
	%  "src, priv, ebin, etc. can be found)" },

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

	NewBinTraceFilePath = file_utils:bin_join(
		getAttribute( CfgState, log_directory ), "us_main.traces" ),

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

	GenMainSettings = #general_main_settings{ },

	?debug_fmt( "Returning the general main configuration settings:~n  ~p",
				[ GenMainSettings ] ),

	wooper:const_return_result( GenMainSettings ).






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
	%						{wooper_oneway_failed,<0.44.0>,class_XXX,
	%							FunName,Arity,Args,AtomCause}}, [...]}"

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
	CfgServerPid ! { getMainRuntimeSettings, [], self() },

	% No possible interleaving:
	receive

		{ wooper_result, { BinCfgDir, ExecContext, MaybeMainCfgFilename } } ->

			StoreState = setAttributes( State, [
				{ execution_context, ExecContext },
				{ nitrogen_roots, [] },
				{ meta_main_settings, undefined },
				{ config_base_directory, BinCfgDir },
				{ us_config_server_pid, CfgServerPid },
				{ log_analysis_settings, undefined } ] ),

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
			?error_fmt( "No main configuration file found or accessible "
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

	ContactState = manage_contacts( MainCfgTable, RegState ),

	LicitKeys = ?known_config_keys,

	case list_utils:difference( table:keys( MainCfgTable ), LicitKeys ) of

		[] ->
			ContactState;

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

	% Not specifically checked:
	setAttribute( State, contact_files, ContactFiles ).




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
