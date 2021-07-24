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
% Creation date: Saturday, June 5, 2021.


% @doc US server in charge of the <b>monitoring of the sensors available on a
% local host</b> (notably the temperatures, chassis intrusion status and the
% operation of fans), and of reporting any abnormal situation.
%
-module(class_USSensorManager).


-define( class_description, "US server in charge of the monitoring of the
sensors available on a local host (notably the temperatures and the operation of
fans), and of reporting any abnormal situation" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").



% Design notes:
%
% We rely here directly on the US infrastructure (as opposed to US-Web, which
% may run autonomously): the configuration file, scheduler, etc. of US-Common
% are used here.


% Two phases shall be considered:
%
%  - an initial pass through all measurement points of all sensors, in order to
%  initialise properly them, based on a per-category exhaustive filtering
%
%  - regular update passes, just picking the enabled points, updating their
%  status and possibly reporting any detected issue



% Implementation notes:

% Regarding sensors, refer to the Technical Manual of the Universal Server
% (http://us-main.esperide.org).

% Note that map_hashtable instances are explicitly mentioned instead of tables
% as this is the actual type returned by the json_utils.


% Section about JSON data being read.
%
% Types defined from here correspond to elements read from the JSON output of
% the 'sensors' tool.


-type json_read_value() :: float().
% To designate values read from JSON keys.


-type json_point_map() :: map_hashtable:map_hashtable( measurement_point_name(),
													   point_attribute_map() ).
% Table associating, to a measurement point (ex: `<<"Core 0">>'), a table of the
% associated attributes (i.e. a point_attribute_map/0).


-type json_point_entry() :: { measurement_point_name(), point_attribute_map() }.
% An entry of a json_point_map/0.


-type json_sensor_entry() :: { raw_sensor_id(), json_point_map() }.
% An entry corresponding to the top-level information read from JSON, in a
% corresponding map.


-type json_triple() :: { measurement_point_name(),
						 measurement_point_description(), json_read_value() }.
% Defined just for convenience.




% Section about sensors.


-type raw_sensor_type() :: ustring().
% The reported type for a sensor, like "coretemp", "acpitz", "nvme", etc.; many
% exist as they correspond to different chips (ex: "nct6792") on motherboards.


-type atom_sensor_type() :: atom().
% Atom-based version of a raw sensor type (ex: 'coretemp').


-type sensor_category() :: 'cpu'
						 | 'cpu_socket' % Often less reliable than 'cpu'.
						 | 'motherboard'
						 | 'ram'
						 | 'disk'
						 | 'gpu'
						 | 'unknown'.
% A higher-level (potentially less precise), clearer (US-assigned) category for
% a sensor, deriving from a raw sensor type.


-type sensor_interface() ::
		'isa' | 'acpi' | 'pci' | 'virtual' | 'unknown' | atom().
% The reported hardware interface for a sensor.


-type sensor_number() :: bin_string().
% Kept as a (binary) string (even if is an hexadecimal value), for clarity.
% Ex: `<<"0a20">>'.


-type raw_sensor_id() :: bin_string().
% Full identifier of a sensor, directly as read from JSON; ex:
% `<<"coretemp-isa-0000">>', `<<"nct6792-isa-0a20">>', `<<"nvme-pci-0200">>',
% etc.


-type sensor_id() ::
		{ atom_sensor_type(), sensor_interface(), sensor_number() }.
% Full identifier of a sensor, directly deriving from a raw one.
% Ex: `{coretemp, isa, <<"0a20">>}'.


-type sensor_table() :: table( raw_sensor_id(), sensor_info() ).
% Table registering all information regarding all local sensors; useful when
% parsing a global (JSON) sensor report.




% Section about measurement points.


-type measurement_point_name() :: bin_string().
% The (raw) name of a measurement point (ex: `<<"temp1">>' or `<<"Core 0">>') of
% a sensor (which may provide multiple points), as read from JSON.


-type measurement_point_description() :: bin_string().
% A description of a measurement point that is more user-friendly than its raw
% name.


-type point_status() :: 'enabled' | 'disabled'.
% The enable status of a measurement point. Those that reports bogus values
% shall be disabled.


-type point_attribute() :: bin_string().
% The name of an attribute at a measurement point, as read from JSON.

-type point_value() :: json_read_value().
% The value associated to a read JSON key, e.g. a key that is an attribute at a
% measurement point.


-type point_attribute_map() ::
		map_hashtable:map_hashtable( point_attribute(), point_value() ).
% Table associating, to an attribute (ex: `<<"temp1_crit">>') of a measurement
% point, a value (ex: 105.0).


%-type point_attribute_entry() :: { point_attribute(), point_value() }.
% An entry of a point_attribute_map/0.




% Section about temperature sensors.

% Read from a sensor on your CPU: Core temperature.
%
% Read from sensors on the motherboard:
% - CPUTIN (CPU temperature index): CPU temperature
% - AUXTIN (auxiliary temperature index): power supply temperature sensor
% - SYSTIN (system temperature index): motherboard temperature

% Settings: HYST (hysteresis; this is the value that you want an alarm to turn
% off. For example, if your alarm temperature is 80C, set your HYST value to
% stop the alarm when the temperature falls to 75C.)


-type temperature_description() :: ustring().
% The description of a temperature (ex: "alarm").


-type range_description() :: ustring().
% The description of a temperature range (ex: "bogus").


% By decreasing temperature:
-type temp_alert_state() :: 'critical_high'
						  | 'alarm_high'
						  | 'nominal'
						  | 'alarm_low'
						  | 'critical_low'.
% The temperature-related alert state of a measurement point. Allows to report
% issues once, rather than repeatedly.


-record( temperature_data, {

	% The name of the attribute to read from the JSON table in order to
	% determine the current temperature of the associated point:
	%
	% (ex: <<"temp9_input">>)
	%
	input_attribute :: point_attribute(),

	% The most user-friendly description of the associated measurement point:
	description :: bin_string(),

	% The enable status of the associated measurement point:
	status = 'enabled' :: point_status(),

	% The temperature-related alert state of this sensor:
	alert_state = 'nominal' :: temp_alert_state(),

	% The timestamp of the beginning of the current alert state:
	alert_timestamp :: timestamp(),

	% The current temperature reading:
	current :: celsius(),

	% The lowest temperature measured since start (hence an actual measurement,
	% not a setting):
	%
	min_reached :: celsius(),

	% The highest temperature measured since start (hence an actual measurement,
	% not a setting):
	%
	max_reached :: celsius(),


	% To compute the average temperature:

	% The sum of all (vetted) temperatures measured since start:
	avg_sum = 0.0 :: celsius(),

	% The number of all (vetted) temperature measurements since start:
	avg_count = 0 :: count(),



	% We ensure that all thresholds are set, even if the sensor does not report
	% corresponding values. We consider that thresholds shall be strictly
	% exceeded to change severity.
	%
	% Of course following inequalities shall hold:
	%   crit_low < alarm_low < alarm_high < crit_high
	%
	% and under nominal conditions, the current measured temperature shall be in
	% [alarm_low; alarm_high].

	% The minimum allowed temperature, below which we consider that an alarm
	% shall be raised (it is typically set higher than any crit_low):
	%
	% (not the most useful of indicators; generally is not reported, yet we set
	% it to a sensible value in all cases)
	%
	alarm_low = undefined :: celsius(),


	% The maximum allowed temperature, above which we consider that an alarm
	% shall be raised (it is typically set lower than any crit_high):
	%
	alarm_high = undefined :: celsius(),



	% The built-in critical low temperature (i.e. the temperature below which
	% serious problems are bound to happen), as reported by the sensor (if any
	% is defined):
	%
	% (not the most useful of indicators; generally is not reported, yet we set
	% it to a sensible value in all cases)
	%
	crit_low = undefined :: celsius(),


	% The built-in critical high temperature (i.e. the temperature above which
	% serious problems are bound to happen), as reported by the sensor (if any
	% is defined, otherwise it is estimated by ourselves):
	%
	crit_high = undefined :: celsius() } ).


-type temperature_data() :: #temperature_data{}.
% Stores information about the temperature measured by a given measurement point
% of a sensor.




% Section regarding intrusion sensors.


-type intrusion_detected_status() :: boolean().
% A status regarding the detection of a chassis intrusion: false if nothing
% detected, true if an intrusion apparently happened.


-record( intrusion_data, {

	% The name of the attribute to read from the JSON table in order to
	% determine the current intrusion status of the associated point:
	%
	% (ex: <<"intrusion0">>)
	%
	input_attribute :: point_attribute(),

	% The most user-friendly description of the associated measurement point:
	description :: bin_string(),

	% The status of the associated measurement point:
	status = 'enabled' :: point_status(),

	% The current intrusion reading:
	intrusion_reported :: intrusion_detected_status(),

	% The timestamp of the beginning of the current intrusion status:
	intrusion_timestamp :: timestamp(),

	% Tells whether the motherboard is to beep in case of intrusion:
	beep_on_intrusion = false :: boolean() } ).


-type intrusion_data() :: #intrusion_data{}.
% Stores information about any chassis intrusion detected by a given measurement
% point of a sensor.


% For a point P, intrusionP_alarm tells whether a chassis intrusion was detected
% (if equal to 1.0) or not (if equal to 0.0). Bogus values may be reported (we
% suppose that when this manager launches no intrusion already occurred), so
% only transitions from "normal" (an intrusion_detected_status of false) to
% "alarm" (an intrusion_detected_status of true) are scrutinised here.


-export_type([ raw_sensor_type/0, atom_sensor_type/0,
			   sensor_category/0, sensor_interface/0, sensor_number/0,
			   raw_sensor_id/0, sensor_id/0 ]).


-type point_data() :: temperature_data() | intrusion_data().
% The data that is associated to a measurement point.


-type points_data_table() :: table( measurement_point_name(), point_data() ).
% A table associating, for a given sensor, to each of its measurement points,
% its data (ex: regarding temperature, intrusion, etc.).


-record( sensor_info, {

	% Duplicated here for convenience (often is an associated key):
	raw_id :: raw_sensor_id(),

	% Identifier of this sensor.
	id :: sensor_id(),

	% Higher-level category of this sensor:
	category :: sensor_category(),

	% All data (of all sorts; and if any, as the category of this sensor must be
	% known to be interpreted) monitored by that sensor:
	%
	data :: maybe( points_data_table() ) } ).


-type sensor_info() :: #sensor_info{}.
% All information regarding a known sensor.



% Section for temperature defines.


% A temperature value below this threshold denotes a bogus measure, resulting in
% the corresponding measurement point to be disabled:
%
-define( low_bogus_temperature_threshold, 5.0 ).


% A temperature value above this threshold denotes a bogus measure (ex: 127°C),
% resulting in the corresponding measurement point to be disabled:
%
-define( high_bogus_temperature_threshold, 125.0 ).



% Should none be returned by a sensor (as celsius()):
-define( default_critical_high_temperature, 95.0 ).


% Should none be returned by a sensor (as celsius()):
-define( default_critical_low_temperature, 2.0 ).




% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type indentation_level() :: indentation_level().

-type file_path() :: file_utils:file_path().

-type string_json() :: json_utils:string_json().
-type decoded_json() :: json_utils:decoded_json().
-type parser_state() :: json_utils:parser_state().

-type celsius() :: unit_utils:celsius().

-type timestamp() :: time_utils:timestamp().


%-type scheduler_pid() :: class_USScheduler:scheduler_pid().
%-type task_id() :: class_USScheduler:task_id().



% The class-specific attributes:
-define( class_attributes, [

	{ sensor_exec_pair, system_utils:execution_pair(),
	  "to run the sensor tool conveniently" },

	{ sensor_monitoring, boolean(),
	  "tells whether sensor monitoring is enabled" },

	{ parser_state, parser_state(), "state of the JSON parser in use" },

	{ sensor_table, sensor_table(),
	  "table registering all information regarding detected local sensors" },

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" },

	{ us_scheduler_pid, scheduler_pid(), "the PID of the main US scheduler" },

	{ task_id, task_id(),
	  "the scheduling identifier of the sensor-polling task" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Sensors" ).


% Exported helpers:
-export([ intrusion_status_to_string/1 ]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").


% In unit_utils:ms_duration():
%
% (hence 5 minutes here) :
%-define( default_sensor_poll_periodicity, 1000 * 60 * 5 ).

% (hence 10 seconds here) :
-define( default_sensor_poll_periodicity, 1000 * 10 ).



% @doc Constructs a sensor manager, for the local host.
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	ServerTraceName = text_utils:format( "Sensor manager for ~ts",
										 [ net_utils:localhost() ] ),

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize(ServerTraceName),
		?us_main_sensor_server_registration_name,
		?us_main_sensor_server_registration_scope ),

	InitSensorState = case init_sensors( SrvState ) of

		{ _SensorEnabled=true, InitState } ->

			?send_info( InitState, "Performing a first, direct, relevant "
				"sensor update before scheduling it periodically." ),

			UpdatedSensorState = update_sensor_data( InitState ),

			init_polling( ?default_sensor_poll_periodicity,
						  UpdatedSensorState );

		{ _SensorEnabled=false, InitState } ->
			?notice( "No sensor enabled, no polling scheduled." ),
			InitState

	end,

	?send_info( InitSensorState,
				"Constructed: " ++ to_string( InitSensorState ) ),

	InitSensorState.



% @doc Constructs a minimal sensor manager in order to test whether the sensor
% JSON output stored in the specified file can be propertly interpreted.
%
% Such a file is typically obtained thanks to:
%      $ sensors --no-adapter -j > my_sensor_output.txt
%
% Useful to support sensors from third-party computers (transmitting such a file
% just suffice to update this module accordingly).
%
-spec construct( wooper:state(), file_path() ) -> wooper:state().
construct( State, SensorOutputFilePath ) ->

	ServerTraceName = text_utils:format(
		"Sensor manager for data from file ~ts", [ SensorOutputFilePath ] ),

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize(ServerTraceName),
		?us_main_sensor_server_registration_name,
		?us_main_sensor_server_registration_scope ),

	% Internal state of the JSON parser:
	ParserState = initialise_json_support( SrvState ),

	InitState = setAttributes( SrvState, [
					{ sensor_monitoring, false },
					{ sensor_exec_pair, get_sensor_execution_pair( State ) },
					{ parser_state, ParserState } ] ),

	ReadState =
		parse_sensor_output_from_file( SensorOutputFilePath, InitState ),

	UpdatedSensorState = update_sensor_data( ReadState ),

	?send_info( ReadState, "Constructed: " ++ to_string( UpdatedSensorState ) ),

	ReadState.



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug_fmt( "Deletion initiated, while state is: ~ts.",
				[ to_string( State ) ] ),

	?info( "Deleted." ),
	State.



% Method section.


% @doc Reads the local sensors.
%
% Typically triggered periodically by the scheduler.
%
-spec readSensors( wooper:state() ) -> oneway_return().
readSensors( State ) ->

	?debug( "Reading sensors now." ),

	ReadState = update_sensor_data( State ),

	wooper:return_state( ReadState ).




% Helper section.


% @doc Initialises the sensor management.
-spec init_sensors( wooper:state() ) ->
						{ SensorEnabled :: boolean(), wooper:state() }.
init_sensors( State ) ->

	CheckedParserState = initialise_json_support( State ),

	ReadyState = setAttributes( State, [
		{ sensor_exec_pair, get_sensor_execution_pair( State ) },
		{ parser_state, CheckedParserState } ] ),

	initialise_sensor_data( ReadyState ).



% @doc Returns how 'sensors' shall be run.
-spec get_sensor_execution_pair( wooper:state() ) ->
										system_utils:execution_pair().
get_sensor_execution_pair( State ) ->

	SensorExec = "sensors",

	SensorExecPath = case executable_utils:lookup_executable( SensorExec ) of

		false ->

			PathVar =
				system_utils:get_environment_variable_for_executable_lookup(),

			?error_fmt( "Tool for sensor monitoring, '~ts', not found. "
				"Please check whether it is installed and available from "
				"the user path (in '~ts'), which is ~ts.",
				[ SensorExec, PathVar,
				  system_utils:get_environment_variable( PathVar ) ] ),

			throw( { sensor_tool_not_found, SensorExec } );

		SensorPath ->
			SensorPath

	end,

	SensorArgs = [ <<"--no-adapter">>, _JSONOutput= <<"-j">> ],

	ExecPair = { text_utils:string_to_binary( SensorExecPath ), SensorArgs },

	%?debug_fmt( "Exec pair for sensor reading:~n ~p.", [ ExecPair ] ),

	ExecPair.



% @doc Returns a proper initial state of the JSON parser.
-spec initialise_json_support( wooper:state() ) -> parser_state().
initialise_json_support( State ) ->

	% We will interpret the JSON outputs of sensors, so:
	case json_utils:is_parser_available() of

		true ->
			ok;

		false ->
			?error( "No JSON parser found available. "
					 ++ system_utils:get_json_unavailability_hint() ),
			throw( no_json_backend )

	end,

	ParserState = json_utils:start_parser(),

	BackendName = json_utils:get_parser_backend_name( ParserState ),

	CheckedParserState = json_utils:check_parser_operational( ParserState ),

	?info_fmt( "JSON parser successfully started (based on a ~ts back-end), "
			   "and tested.", [ BackendName ] ),

	CheckedParserState.



% @doc Initialises and integrates the first data (ex: temperatures and fan
% speeds) read from sensors.
%
-spec initialise_sensor_data( wooper:state() ) ->
								{ SensorEnabled :: boolean(), wooper:state() }.
initialise_sensor_data( State ) ->

	case fetch_sensor_data( State ) of

		{ ok, CmdOutput } ->
			{ true, parse_initial_sensor_output( CmdOutput, State ) };

		% If initialisation detects that no sensor is available, this is not a
		% showstopper anymore, as in some contexts (ex: continuous integration)
		% this might happen and should not crash such a manager:
		%
		error ->
			?warning( "Not able to initialise sensor data, not activating "
					  "their monitoring." ),
			{ false, setAttribute( State, sensor_monitoring, false ) }

	end.



% @doc Returns the output of the corresponding sensor command (may crash on
% systematic, non-recoverable errors).
%
-spec fetch_sensor_data( wooper:state() ) -> fallible( ustring() ).
fetch_sensor_data( State ) ->

	{ ExecPath, ExecArgs } = ?getAttr(sensor_exec_pair),

	case system_utils:run_executable( ExecPath, ExecArgs ) of

		{ _ReturnCode=0, CmdOutput  } ->
			?debug_fmt( "Raw (JSON) command output read from sensors: '~ts'.",
						[ CmdOutput ] ),
			{ ok, CmdOutput };

		% Message typically starts with 'No sensors found!...":
		{ _ReturnCode=1, ErrorOutput } ->
			?error_fmt( "Error when running '~ts' with arguments ~p: '~ts' "
				"(exit code: 1); interpreting that as no sensor being found.",
				[ ExecPath, ExecArgs, ErrorOutput ] ),

			% Do not crash in this case anymore:
			%throw( { sensor_read_failed, ErrorOutput } )
			error;

		{ ReturnCode, ErrorOutput  } ->
			?emergency_fmt( "Error when running '~ts' with arguments ~p: '~ts' "
				"(exit code: ~B).",
				[ ExecPath, ExecArgs, ErrorOutput, ReturnCode ] ),

			throw( { sensor_read_failed, ErrorOutput } )

	end.



% @doc Parses the specified sensor JSON output, returns a state with a
% corresponding initial sensor table.
%
-spec parse_initial_sensor_output( string_json(), wooper:state() ) ->
										wooper:state().
parse_initial_sensor_output( SensorJsonStr, State ) ->

	DecodedMap = decode_sensor_json( SensorJsonStr, State ),

	parse_initial_sensors( map_hashtable:enumerate( DecodedMap ),
						   _SensorTable=table:new(), State ).



% @doc Parses in turn the specified initial sensor JSON entries.
parse_initial_sensors( _SensorPairs=[], SensorTable, State ) ->
	setAttributes( State, [ { sensor_table, SensorTable },
							{ sensor_monitoring, true } ] );

parse_initial_sensors( _SensorPairs=[ { RawSensorIdBinStr, SensorJSON } | T ],
					   SensorTable, State ) ->

	% Having for example "nct6779-isa-0a00":
	RawSensorIdStr = text_utils:binary_to_string( RawSensorIdBinStr ),

	case text_utils:split( RawSensorIdStr, _Delimiters=[ $- ] ) of

		[ RawSensorType, InterfStr, NumStr ] ->

			{ AtomSensorType, SensorCategory } =
					case categorise_sensor( RawSensorType ) of

				UnknownP={ _AtomSensorType, _Categ=unknown } ->
					?warning_fmt( "Unable to categorise sensor '~ts'.",
								  [ RawSensorType ] ),
					UnknownP;

				P ->
					P

			end,

			Interf = case get_interface( InterfStr ) of

				unknown ->
					?warning_fmt( "Unknown interface '~ts' found for sensor "
						"'~ts'.", [ InterfStr, RawSensorIdBinStr ] ),
					unknown;

				I ->
					I

			end,

			SensorId = { AtomSensorType, Interf,
						 text_utils:string_to_binary( NumStr ) },

			MaybeSensorData = parse_sensor_data( SensorJSON, SensorCategory,
												 SensorId, State ),

			SensorInfo = #sensor_info{ raw_id=RawSensorIdBinStr,
									   id=SensorId,
									   category=SensorCategory,
									   data=MaybeSensorData },

			%?debug_fmt( "For sensor ~ts, we have following information:~n  ~p",
			%            [ RawSensorIdBinStr, SensorInfo ] ),

			% Ensures of uniqueness of key:
			NewSensorTable = table:add_new_entry( RawSensorIdBinStr, SensorInfo,
												  SensorTable ),

			parse_initial_sensors( T, NewSensorTable, State );


		_Other ->
			?error_fmt( "Unable to interpret raw sensor identifier '~ts', "
				"ignoring this sensor as a whole from now.",
				[ RawSensorIdStr ] ),

			parse_initial_sensors( T, SensorTable, State  )

	end.




% @doc Returns a pair to categorise specified sensor, deriving from its raw
% type.
%
-spec categorise_sensor( raw_sensor_type() ) ->
								{ atom_sensor_type(), sensor_category() }.
categorise_sensor( _RawSensorType="coretemp" ) ->
	{ coretemp, cpu };

categorise_sensor( _RawSensorType="acpitz" ) ->
	{ acpitz, cpu_socket };

categorise_sensor( _RawSensorType="nvme" ) ->
	{ nvme, disk };

categorise_sensor( RawSensorType="nct" ++ _ ) ->
	{ text_utils:string_to_atom( RawSensorType ), motherboard };

categorise_sensor( RawSensorType ) ->
	{ text_utils:string_to_atom( RawSensorType ), unknown }.



% @doc Returns the sensor interface corresponding to the specified string.
-spec get_interface( ustring() ) -> sensor_interface().
get_interface( _InterfStr="isa" ) ->
	isa;

get_interface( _InterfStr="acpi" ) ->
	acpi;

get_interface( _InterfStr="pci" ) ->
	pci;

get_interface( _InterfStr="virtual" ) ->
	virtual;

get_interface( _InterfStr ) ->
	unknown.



% @doc Parses specified JSON information regarding specified sensor, in order to
% detect initially the various measurement points that it supports, and returns
% the corresponding data table.
%
-spec parse_sensor_data( decoded_json(), sensor_category(), sensor_id(),
						 wooper:state() ) -> points_data_table().
parse_sensor_data( SensorJSON, _SensorCateg=cpu_socket, SensorId, State ) ->

	%?debug_fmt( "JSON to parse for ~ts for cpu_socket :~n ~p",
	%			 [ sensor_id_to_string( SensorId ), SensorJSON ] ),

	{ TempJSONTriples, OtherJSONTriples } =
		filter_cpu_socket_json( SensorJSON ),

	%?debug_fmt( "For cpu_socket: TempJSONTriples: ~p~nOtherJSONTriples: ~p",
	%			 [ TempJSONTriples, OtherJSONTriples ] ),

	case OtherJSONTriples of

		[] ->
			ok;

		_ ->
			?warning_fmt( "Following ~B CPU-socket measurement points "
				"could not be categorised: ~ts.",
				[ length( OtherJSONTriples ), text_utils:strings_to_string(
					[ json_triple_to_string( JT )
								|| JT <- OtherJSONTriples ] ) ] )

	end,

	register_temperature_points( TempJSONTriples, _EmptyDataTable=table:new(),
								 SensorId, State );



% Mostly the same as cpu_socket:
parse_sensor_data( SensorJSON, _SensorCateg=cpu, SensorId, State ) ->

	%?debug_fmt( "JSON to parse for ~ts for cpu:~n ~p",
	%			 [ sensor_id_to_string( SensorId ), SensorJSON ] ),

	{ TempJSONTriples, OtherJSONTriples } = filter_cpu_json( SensorJSON ),

	%?debug_fmt( "For cpu: TempJSONTriples: ~p~nOtherJSONTriples: ~p",
	%			 [ TempJSONTriples, OtherJSONTriples ] ),

	case OtherJSONTriples of

		[] ->
			ok;

		_ ->
			?warning_fmt( "Following ~B CPU measurements points "
				"could not be categorised: ~ts.",
				[ length( OtherJSONTriples ), text_utils:strings_to_string(
					[ json_triple_to_string( JT )
								|| JT <- OtherJSONTriples ] ) ] )

	end,

	register_temperature_points( TempJSONTriples, _EmptyDataTable=table:new(),
								 SensorId, State );


parse_sensor_data( SensorJSON, _SensorCateg=motherboard, SensorId, State ) ->

	%?debug_fmt( "JSON to parse for ~ts for motherboard:~n ~p",
	%			 [ sensor_id_to_string( SensorId ), SensorJSON ] ),

	{ TempJSONTriples, _FanJSONTriples, IntrusionJSONTriples,
	  OtherJSONTriples } = filter_motherboard_json( SensorJSON ),

	%?debug_fmt( "TempJSONTriples: ~p~nFanJSONTriples: ~p~n"
	%   "IntrusionJSONTriples: ~p~nOtherJSONTriples: ~p",
	%   [ TempJSONTriples, FanJSONTriples, IntrusionJSONTriples,
	%     OtherJSONTriples ] ),

	case OtherJSONTriples of

		[] ->
			ok;

		_ ->
			?warning_fmt( "Following ~B motherboard measurements points "
				"could not be categorised: ~ts.",
				[ length( OtherJSONTriples ), text_utils:strings_to_string(
					[ json_triple_to_string( JT )
								|| JT <- OtherJSONTriples ] ) ] )

	end,

	TempDataTable = register_temperature_points( TempJSONTriples,
								_EmptyDataTable=table:new(), SensorId, State ),

	register_intrusion_points( IntrusionJSONTriples, TempDataTable, SensorId,
							   State );


parse_sensor_data( SensorJSON, _SensorCateg=disk, SensorId, State ) ->

	?debug_fmt( "JSON to parse for ~ts for disk:~n ~p",
				[ sensor_id_to_string( SensorId ), SensorJSON ] ),

	{ TempJSONTriples, OtherJSONTriples } = filter_disk_json( SensorJSON ),

	%?debug_fmt( "TempJSONTriples: ~p~nOtherJSONTriples: ~p",
	%            [ TempJSONTriples, OtherJSONTriples ] ),

	case OtherJSONTriples of

		[] ->
			ok;

		_ ->
			?warning_fmt( "Following ~B disk measurements points "
				"could not be categorised: ~ts.",
				[ length( OtherJSONTriples ), text_utils:strings_to_string(
					[ json_triple_to_string( JT )
								|| JT <- OtherJSONTriples ] ) ] )

	end,

	register_temperature_points( TempJSONTriples, _EmptyDataTable=table:new(),
								 SensorId, State );


parse_sensor_data( SensorJSON, SensorCateg, SensorId, State ) ->

	?error_fmt( "Ignoring JSON for ~ts of unsupported category ~ts:~n ~p",
		[ sensor_id_to_string( SensorId ), SensorCateg, SensorJSON ] ),

	undefined.



% @doc Registers specified temperature points in specified data table, for the
% initialisation of specified sensor, from specified JSON content.
%
% Only point triples that are already filtered for temperature are expected,
% thus no need to accumulate unexpected points.
%
-spec register_temperature_points( [ json_triple() ], points_data_table(),
			sensor_id(), wooper:state() ) -> points_data_table().
register_temperature_points( _PointTriples=[], DataTable, _SensorId, _State ) ->
	DataTable;


% Ex: BinPointName = <<"temp1">>, BinDesc=<<"Some description">> and
% PointValueMap = #{<<"temp1_crit">> => 105.0, <<"temp1_input">> => 27.8}.
%
register_temperature_points(
		_PointTriples=[ { BinPointName, BinDesc, PointValueMap } | T ],
		DataTable, SensorId, State ) ->

	?debug_fmt( "Registering temperature point '~ts' (~ts) with:~n ~p",
				[ BinPointName, BinDesc, PointValueMap ] ),

	InitPointData = create_temperature_data( PointValueMap, BinPointName,
											 BinDesc, SensorId, State ),

	% First update done globally directly from the constructor.

	% Uniqueness checked:
	NewDataTable =
		table:add_new_entry( BinPointName, InitPointData, DataTable ),

	register_temperature_points( T, NewDataTable, SensorId, State ).



% @doc Returns a new temperature data initialised from the specified JSON
% content, ready to be updated with the future next current temperatures.
%
-spec create_temperature_data( point_attribute_map(), measurement_point_name(),
		measurement_point_description(), sensor_id(), wooper:state() ) ->
									temperature_data().
create_temperature_data( PointValueMap, BinPointName, BinDesc, SensorId,
						 State ) ->

	% We check all attributes in turn; no need to accumulate here.
	% Here the current temperature has not been taken into account yet:
	init_temp_point( _TempEntries=map_hashtable:enumerate( PointValueMap ),
		BinPointName, #temperature_data{
						description=BinDesc,
						% Already the case: alert_state=nominal;
						alert_timestamp=time_utils:get_timestamp() },
		SensorId, State ).



% (helper)
% End of recursion, here with no current temperature reading:
init_temp_point( _TempEntries=[], BinPointName,
		TempData=#temperature_data{ current=undefined }, SensorId, State ) ->

	?error_fmt( "For temperature measurement point '~ts' of ~ts, no current "
		"temperature was reported ; disabling this point.",
		[ BinPointName, sensor_id_to_string( SensorId ) ] ),

	TempData#temperature_data{ status=disabled };


% End of recursion, here with an expected current temperature reading:
init_temp_point( _TempEntries=[], _BinPointName,
				 TempData=#temperature_data{ current=CurrentTemp,
											 crit_low=MaybeCritLowTemp,
											 alarm_low=MaybeMinTemp,
											 crit_high=MaybeCritHighTemp,
											 alarm_high=MaybeMaxTemp,
											 min_reached=MaybeReached,
											 max_reached=MaybeReachedMax },
				 _SensorId, _State ) ->

	type_utils:check_float( CurrentTemp ),

	% First, tackle low temperatures (not really of interest generally):
	CritLowTemp = case MaybeCritLowTemp of

		undefined ->
			case MaybeReached of

				undefined ->
					?default_critical_low_temperature;

				ReportedMin ->
					% Hazardous will negative temperatures:
					%min( 0.9*ReportedMin, ReportedMin - 15.0 )
					ReportedMin - 15.0

			end;

		CLTemp ->
			CLTemp

	end,

	AlarmLowTemp = case MaybeMinTemp of

		undefined ->
			% Let's establish it then from the critical temperature:
			%max( CritLowTemp + 12.0, 0.9 * CritLowTemp );
			min( CritLowTemp + 10.0, 8.0 );

		AlLowTemp ->
			AlLowTemp

	end,

	% Now the same for max:
	CritHighTemp = case MaybeCritHighTemp of

		undefined ->
			case MaybeReachedMax of

				undefined ->
					?default_critical_high_temperature;

				ReportedMax ->
					max( 1.1*ReportedMax, ReportedMax + 15.0 )

			end;

		CHTemp ->
			CHTemp

	end,

	AlarmHighTemp = case MaybeMaxTemp of

		undefined ->
			% Let's establish it then from the critical temperature:
			max( CritHighTemp - 12.0, 0.9 * CritHighTemp );

		AlHighTemp ->
			AlHighTemp

	end,

	% All constant information then set:
	ReadyTempData = TempData#temperature_data{ crit_low=CritLowTemp,
											   alarm_low=AlarmLowTemp,
											   crit_high=CritHighTemp,
											   alarm_high=AlarmHighTemp,
											   min_reached=CurrentTemp,
											   max_reached=CurrentTemp },

	%?debug_fmt( "Adding temperature point '~ts' of ~ts.",
	%            [ BinPointName, sensor_id_to_string( SensorId ) ] ),

	% We used to perform the first usual reading here, now done in the final
	% part of the constructor.

	check_temperature_data( ReadyTempData );


% Ex: AttrNameBin = <<"temp1_input">>, AttrValue = 44.85:
init_temp_point( _TempEntries=[ { AttrNameBin, AttrValue } | T ], BinPointName,
				 TempData, SensorId, State ) ->

	% Done in all cases afterwards: type_utils:check_float( AttrValue ),

	AttrName = text_utils:binary_to_string( AttrNameBin ),

	% We thought tables to be all like: #{<<"temp1">> =>
	%    #{<<"temp1_crit">> => 105.0, <<"temp1_input">> => 27.8},

	% (i.e. with attributes of a temperature_data being prefixed by their point
	% name, like 'temp1' here)
	%
	% however it is not always true, ex in the cpu category we have:
	% #{<<"Core 0">> =>
	%       #{<<"temp2_crit">> => 100.0,
	%         <<"temp2_crit_alarm">> => 0.0,...
	%
	% So we just drop (and not check the prefix):
	%case text_utils:split_after_prefix( _Prefix=BinPointName, AttrName ) of
	%
	%	no_prefix ->
	%		?error_fmt( "Attribute '~ts' not prefixed with the name of "
	%			"the current measurement point '~ts', thus ignored.",
	%			[ AttrName, BinPointName ] ),
	%		init_temp_point( T, BinPointName, TempData, SensorId, PointValueMap,
	%						 State );

	Separator = $_,

	case text_utils:split( AttrName, _Delimiters=[ Separator ] ) of

		[ _WithoutSepElem ] ->

			?error_fmt( "For temperature measurement point '~ts' of ~ts, "
				"attribute '~ts' cannot be stripped of a relevant prefix; "
				"ignoring it.",
				[ BinPointName, sensor_id_to_string( SensorId ), AttrName ] ),

			init_temp_point( T, BinPointName, TempData, SensorId, State );

		% Just remove the prefix:
		[ _AnyPrefix | OtherElems ] ->
			case text_utils:join( Separator, OtherElems ) of

				"input" ->

					% Just record the full attribute name once for all:
					%
					% (value considered afterwards, as all post-init updates)
					%
					case is_float( AttrValue ) of

						true ->
							SetTempData = TempData#temperature_data{
											input_attribute=AttrNameBin,
											current=AttrValue },

							% To be taken into account once all other attributes
							% (especially the static ones) have been processed:
							%
							init_temp_point( T, BinPointName, SetTempData,
											 SensorId, State );

						false ->

							?error_fmt( "Non-float value associated to "
								"temperature attribute '~ts' (got '~p'); "
								"skipping that information.",
								[ AttrName, AttrValue ] ),

							init_temp_point( T, BinPointName, TempData,
											 SensorId, State )

					end;


				Suffix="crit" ->
					NewTempData = init_for_crit( AttrValue, TempData, Suffix,
											BinPointName, SensorId, State ),
					init_temp_point( T, BinPointName, NewTempData, SensorId,
									 State );


				% Interpreted as a synonym of "crit":
				Suffix="crit_alarm" ->
					NewTempData = init_for_crit( AttrValue, TempData, Suffix,
											BinPointName, SensorId, State ),
					init_temp_point( T, BinPointName, NewTempData, SensorId,
									 State );


				% Unsurprisingly, nothing like crit/crit_alarm for *low*
				% temperatures.

				% Just during this initialisation, we (ab)use the 'min_reached'
				% field in order to record the minimum allowed temperature as
				% reported by the chip (not corresponding to the "real"
				% min_reached, which is the lowest temperature *actually*
				% measured):
				%
				Suffix="min" ->
					case vet_temperature( AttrValue, Suffix, BinPointName,
										  SensorId, State ) of

						false ->
							% Just ignored then:
							init_temp_point( T, BinPointName, TempData,
											 SensorId, State );

						true ->
							NewTempData = TempData#temperature_data{
											min_reached=AttrValue },

							init_temp_point( T, BinPointName, NewTempData,
											 SensorId, State )

					end;

				% Same as for "min" just above:
				Suffix="max" ->
					case vet_temperature( AttrValue, Suffix, BinPointName,
										  SensorId, State ) of

						false ->
							% Just ignored then:
							init_temp_point( T, BinPointName, TempData,
											 SensorId, State );

						true ->
							NewTempData = TempData#temperature_data{
											max_reached=AttrValue },

							init_temp_point( T, BinPointName, NewTempData,
											 SensorId, State )

					end;

				Suffix="alarm" ->
					% First ensure that this alarm has not a bogus value:
					case vet_temperature( AttrValue, Suffix, BinPointName,
										  SensorId, State ) of

						false ->
							% Just ignored then:
							init_temp_point( T, BinPointName, TempData,
											 SensorId, State );

						true ->
							NewHigh =
									case TempData#temperature_data.alarm_high of

								undefined ->
									AttrValue;

								AlarmHigh ->
									Min = min( AlarmHigh, AttrValue ),
									?warning_fmt( "Alarm high was already set "
										"to ~ts for temperature measurement "
										"point ~ts of ~ts, whereas alarm found "
										"as ~ts; set as ~ts.",
									 [ unit_utils:temperature_to_string(
											AlarmHigh ), BinPointName,
									   sensor_id_to_string( SensorId ),
									   unit_utils:temperature_to_string(
											AttrValue ),
									   unit_utils:temperature_to_string(
											Min ) ] ),
									Min

							end,

							NewTempData =
								TempData#temperature_data{ alarm_high=NewHigh },

							init_temp_point( T, BinPointName, NewTempData,
											 SensorId, State )

					end;


				Suffix when Suffix == "beep" orelse Suffix == "type"
						orelse Suffix == "offset" orelse Suffix == "max_hyst" ->

					%?debug_fmt( "Attribute '~ts' belongs to the ignored ones.",
					%			 [ Suffix ] ),

					init_temp_point( T, BinPointName, TempData, SensorId,
									 State );

				_OtherSuffix ->

					?warning_fmt( "Unknown suffix for attribute '~ts' of "
						"temperature measurement point ~ts of ~ts; "
						"ignoring it.",
						[ AttrName, BinPointName,
						  sensor_id_to_string( SensorId ) ] ),

					init_temp_point( T, BinPointName, TempData, SensorId,
									 State )

			end

	end.



% @doc Initialises the specified measurement point with specified critical
% temperature.
%
init_for_crit( AttrValue, TempData, Suffix, BinPointName, SensorId, State ) ->

	case vet_temperature( AttrValue, Suffix, BinPointName, SensorId, State ) of

		false ->
			% Just ignored then:
			TempData;

		true ->
			NewCritHigh = case TempData#temperature_data.crit_high of

				undefined ->
					AttrValue;

				CritHigh ->
					Min = min( CritHigh, AttrValue ),
					?warning_fmt( "Critical high was already set to ~ts for "
						"measurement point ~ts of ~ts, whereas ~ts found "
						"as ~ts; set as ~ts.",
						[ unit_utils:temperature_to_string( CritHigh ),
						  BinPointName, sensor_id_to_string( SensorId ),
						  Suffix, unit_utils:temperature_to_string( AttrValue ),
						  unit_utils:temperature_to_string( Min ) ] ),
					Min

			end,

			TempData#temperature_data{ crit_high=NewCritHigh }

	end.



% @doc Registers specified intrusion points in specified data table, for the
% initialisation of specified sensor, from specified JSON content.
%
% Only point triples that are already filtered for intrusion are expected,
% thus no need to accumulate unexpected points.
%
-spec register_intrusion_points( [ json_triple() ], points_data_table(),
			sensor_id(), wooper:state() ) -> points_data_table().
register_intrusion_points( _PointTriples=[], DataTable, _SensorId, _State ) ->
	DataTable;


% Ex: BinPointName = <<"intrusion0">>, BinDesc=<<"Some description">> and
% PointValueMap = #{<<"intrusion0_alarm">> => 1.0,
%                   <<"intrusion0_beep">> => 0.0}.
%
register_intrusion_points(
		_PointTriples=[ { BinPointName, BinDesc, PointValueMap } | T ],
		DataTable, SensorId, State ) ->

	?debug_fmt( "Registering intrusion point '~ts' (~ts) with:~n ~p",
				[ BinPointName, BinDesc, PointValueMap ] ),

	InitPointData = create_intrusion_data( PointValueMap, BinPointName,
										   BinDesc, SensorId, State ),

	% Uniqueness checked:
	NewDataTable =
		table:add_new_entry( BinPointName, InitPointData, DataTable ),

	register_intrusion_points( T, NewDataTable, SensorId, State ).



% @doc Returns a new intrusion data initialised from the specified JSON content,
% ready to be updated with the future next current intrusion notifications.
%
-spec create_intrusion_data( point_attribute_map(), measurement_point_name(),
		measurement_point_description(), sensor_id(), wooper:state() ) ->
									intrusion_data().
create_intrusion_data( PointValueMap, BinPointName, BinDesc, SensorId,
					   State ) ->

	% We check all attributes in turn; no need to accumulate here.
	% Here the current intrusion status has not been taken into account yet:
	init_intrus_point( _IntrusEntries=map_hashtable:enumerate( PointValueMap ),
		BinPointName, #intrusion_data{
						description=BinDesc,
						intrusion_timestamp=time_utils:get_timestamp() },
		SensorId, State ).



% (helper)
% End of recursion, here with no intrusion reading:
init_intrus_point( _IntrusEntries=[], BinPointName,
		IntrusData=#intrusion_data{ intrusion_reported=undefined }, SensorId,
		State ) ->

	?error_fmt( "For intrusion measurement point '~ts' of ~ts, no intrusion "
		"status was reported; disabling this point.",
		[ BinPointName, sensor_id_to_string( SensorId ) ] ),

	IntrusData=#intrusion_data{ input_attribute=BinPointName,
								status=disabled };


% End of recursion, here with an expected intrusion reading:
init_intrus_point( _IntrusEntries=[], BinPointName, IntrusData, _SensorId,
				   _State ) ->

	% No specific final consistency update to be performed here.

	%?debug_fmt( "Adding intrusion point '~ts' of ~ts.",
	%            [ BinPointName, sensor_id_to_string( SensorId ) ] ),

	% We used to perform the first usual reading here, now done in the final
	% part of the constructor.

	check_intrusion_data(
		IntrusData#intrusion_data{ input_attribute=BinPointName } );


% Ex: AttrNameBin = <<"intrusion0_alarm">>, AttrValue = 1.0:
init_intrus_point( _IntrusEntries=[ { AttrNameBin, AttrValue } | T ],
				   BinPointName, IntrusData, SensorId, State ) ->

	AttrName = text_utils:binary_to_string( AttrNameBin ),
	Separator = $_,

	case text_utils:split( AttrName, _Delimiters=[ Separator ] ) of

		[ _WithoutSepElem ] ->

			?error_fmt( "For intrusion measurement point '~ts' of ~ts, "
				"attribute '~ts' cannot be stripped of a relevant prefix; "
				"ignoring it.",
				[ BinPointName, sensor_id_to_string( SensorId ), AttrName ] ),

			init_intrus_point( T, BinPointName, IntrusData, SensorId, State );

		% Just remove the prefix:
		[ _AnyPrefix | OtherElems ] ->

			type_utils:check_float( AttrValue ),

			Now = time_utils:get_timestamp(),

			case text_utils:join( Separator, OtherElems ) of

				"alarm" ->

					case AttrValue of

						% Normal case, no intrusion initially:
						0.0 ->
							SetIntrusData = IntrusData#intrusion_data{
								intrusion_reported=false,
								intrusion_timestamp=Now },

							init_intrus_point( T, BinPointName, SetIntrusData,
											   SensorId, State );

						1.0 ->

							?warning_fmt( "For intrusion measurement point "
								"'~ts' of ~ts, attribute '~ts' already "
								"reports initially an intrusion; interpreting "
								"it as a bogus value and disabling that point.",
								[ BinPointName, sensor_id_to_string( SensorId ),
								  AttrName ] ),

							DisIntrusData =
								IntrusData#intrusion_data{
									status=disabled,
									intrusion_reported=true,
									intrusion_timestamp=Now },

							init_intrus_point( T, BinPointName, DisIntrusData,
											   SensorId, State );

						_Other ->
							?error_fmt( "For intrusion measurement point '~ts' "
								"of ~ts, attribute '~ts' reports an unexpected "
								"value ('~p'); disabling that point.",
								[ BinPointName, sensor_id_to_string( SensorId ),
								  AttrName, AttrValue ] ),

							DisIntrusData =
								IntrusData#intrusion_data{
									status=disabled,
									intrusion_reported=false,
									intrusion_timestamp=Now },

							init_intrus_point( T, BinPointName, DisIntrusData,
											   SensorId, State )

					end;


				"beep" ->
					case AttrValue of

						0.0 ->
							SetIntrusData = IntrusData#intrusion_data{
												beep_on_intrusion=false },

							init_intrus_point( T, BinPointName, SetIntrusData,
											   SensorId, State );


						1.0 ->
							SetIntrusData = IntrusData#intrusion_data{
												beep_on_intrusion=true },

							init_intrus_point( T, BinPointName, SetIntrusData,
											   SensorId, State );


						_Other ->
							?warning_fmt( "For intrusion measurement point "
								"'~ts' of ~ts, ignoring unexpected value "
								"('~p') reported by attribute '~ts'.",
								[ BinPointName, sensor_id_to_string( SensorId ),
								  AttrValue, AttrName ] ),

							init_intrus_point( T, BinPointName, IntrusData,
											   SensorId, State )

					end;

				_OtherSuffix ->

					?warning_fmt( "Unknown suffix for attribute '~ts' of "
						"intrusion measurement point ~ts of ~ts; ignoring it.",
						[ AttrName, BinPointName,
						  sensor_id_to_string( SensorId ) ] ),

					init_intrus_point( T, BinPointName, IntrusData, SensorId,
									   State )

			end

	end.



% @doc Returns a term corresponding to specified JSON text.
-spec decode_sensor_json( string_json(), wooper:state() ) -> decoded_json().
decode_sensor_json( SensorJsonStr, State ) ->

	ParserState = ?getAttr(parser_state),

	DecodedMap = json_utils:from_json( SensorJsonStr, ParserState ),

	?debug_fmt( "Top-level JSON-decoded map for sensors: ~ts",
				[ map_hashtable:to_string( DecodedMap ) ] ),

	DecodedMap.



% @doc Updates the sensor data (ex: temperatures and fan speeds), as read from
% sensors.
%
update_sensor_data( State ) ->

	case fetch_sensor_data( State ) of

		{ ok, CmdOutput } ->
			update_from_sensor_output( CmdOutput, State );

		{ error, ErrorOutput } ->
			?alert_fmt( "Failed to read sensor data: '~ts'.", [ ErrorOutput ] ),
			State

	end.



% @doc Parses the specified sensor JSON output, returns a state with an updated
% sensor table.
%
-spec update_from_sensor_output( string_json(), wooper:state() ) ->
										wooper:state().
update_from_sensor_output( SensorJsonStr, State ) ->

	JSONDecodedMap = decode_sensor_json( SensorJsonStr, State ),

	?debug_fmt( "Updating sensors from '~p'.", [ JSONDecodedMap ] ),

	update_active_sensors( map_hashtable:enumerate( JSONDecodedMap ),
						   ?getAttr(sensor_table), State ).



% @doc Updates all active sensors based on the specified JSON-decoded map.
%
% We iterate through the JSON content rather than through the known sensors
% (more logical); we just update the sensor table.
%
-spec update_active_sensors( [ json_sensor_entry() ], sensor_table(),
							 wooper:state() ) -> wooper:state().
update_active_sensors( _JSONDecodedEntries=[], SensorTable, State ) ->
	setAttribute( State, sensor_table, SensorTable );

update_active_sensors(
		_JSONDecodedEntries=[ { RawSensorId, JSONPointMap } | T ],
		SensorTable, State ) ->

	% Update each sensor according to its category:
	MaybeNewSInfo = case table:lookup_entry( RawSensorId, SensorTable ) of

		% No data managed for this sensor:
		{ value, SI=#sensor_info{ id=SensorId, data=undefined } } ->
			?debug_fmt( "For ~ts, no data is to be stored, "
				"so not taking into account its measurement points in ~p.",
				[ sensor_id_to_string( SensorId ), JSONPointMap ] ),
			SI;


		% From here we have actual sensor data to update. Now we do not need to
		% rely on the sensor category to select the right kind of data:
		%
		{ value, SI=#sensor_info{ id=SensorId,
								  data=PointsDataTable } } ->

			NewPointsDataTable = update_data_table( PointsDataTable,
				map_hashtable:enumerate( JSONPointMap ), SensorId, State ),

			SI#sensor_info{ data=NewPointsDataTable };


		key_not_found ->
			?error_fmt( "Got data for unknown sensor whose raw identifier "
				"is '~ts': ~p (ignoring these measurement points).",
				[ RawSensorId, JSONPointMap ] ),
			undefined

	end,

	NewSensorTable = case MaybeNewSInfo of

		undefined ->
			SensorTable;

		NewSInfo ->
			table:add_entry( RawSensorId, NewSInfo, SensorTable )

	end,

	update_active_sensors( T, NewSensorTable, State ).



% @doc Updates, for the specified sensor, its specified data table of points of
% all sorts (temperature, intrusion, fan, etc.) from the specified JSON point
% entries.
%
-spec update_data_table( points_data_table(), [ json_point_entry() ],
						 sensor_id(), wooper:state() ) -> points_data_table().
update_data_table( PointsDataTable, _PointEntries=[], _SensorId, _State ) ->
	PointsDataTable;

update_data_table( PointsDataTable,
		_PointEntries=[ { PointNameBin, PointAttrMap } | T ],
		SensorId, State ) ->

	% Cannot be factored more between clauses:
	NewPointsDataTable =
			case table:lookup_entry( PointNameBin, PointsDataTable ) of

		{ value, #temperature_data{ status=disabled } } ->

			?debug_fmt( "(temperature measurement point '~ts' is disabled)",
						[ PointNameBin ] ),

			% No change then:
			PointsDataTable;


		{ value, TempData=#temperature_data{ input_attribute=InputAttrName,
											 description=Desc,
											 % Superfluous:
											 status=enabled,
											 min_reached=MinTemp,
											 max_reached=MaxTemp,
											 avg_sum=AvgSum,
											 avg_count=AvgCount } } ->

			case map_hashtable:lookup_entry( InputAttrName, PointAttrMap ) of

				{ value, CurrentTemp } ->

					NewTempData = case vet_temperature( CurrentTemp, Desc,
							PointNameBin, SensorId, State ) of

						true ->
							NewMinTemp = min( MinTemp, CurrentTemp ),
							NewMaxTemp = max( MaxTemp, CurrentTemp ),
							NewAvgSum = AvgSum + CurrentTemp,
							NewAvgCount = AvgCount + 1,

							SyncTempData = TempData#temperature_data{
											current=CurrentTemp,
											min_reached=NewMinTemp,
											max_reached=NewMaxTemp,
											avg_sum=NewAvgSum,
											avg_count=NewAvgCount },

							% Returns new temperature_data:
							examine_temperature( PointNameBin, CurrentTemp,
								SyncTempData, SensorId, State );

						false ->
							?warning_fmt( "Read for ~ts, measurement "
								"point '~ts', a value considered invalid "
								"for input temperature attribute '~ts': ~p "
								"(ignoring it).",
								[ sensor_id_to_string( SensorId ), PointNameBin,
								  InputAttrName, CurrentTemp ] ),

							TempData

					end,

					table:add_entry( PointNameBin, NewTempData,
									 PointsDataTable );


				% Attribute not anymore in JSON output (never expected to
				% happen):
				%
				key_not_found ->

					?error_fmt( "Could not read temperature attribute '~ts' "
						"of measurement point '~ts' regarding ~ts; "
						"skipping the update of this point.",
						[ InputAttrName, PointNameBin,
						  sensor_id_to_string( SensorId ) ] ),

					PointsDataTable

			end;


		{ value, #intrusion_data{ status=disabled } } ->

			?debug_fmt( "(intrusion measurement point '~ts' is disabled)",
						[ PointNameBin ] ),

			% No change then:
			PointsDataTable;


		{ value, IntrusData=#intrusion_data{ input_attribute=InputAttrName,
											 % Superfluous:
											 status=enabled } } ->

			case map_hashtable:lookup_entry( InputAttrName, PointAttrMap ) of

				{ value, CurrentIntrusStatus } ->

					% Returns new intrusion_data:
					NewIntrusData = examine_intrusion_status( PointNameBin,
						CurrentIntrusStatus, IntrusData, SensorId, State ),

					table:add_entry( PointNameBin, NewIntrusData,
									 PointsDataTable );

				% Attribute not anymore in JSON output (never expected to
				% happen):
				%
				key_not_found ->

					?error_fmt( "Could not read intrusion attribute '~ts' "
						"of measurement point '~ts' regarding ~ts; "
						"skipping the update of this point.",
						[ InputAttrName, PointNameBin,
						  sensor_id_to_string( SensorId ) ] ),

					PointsDataTable

			end;

		% Perfectly normal, as many points (ex: "in7") have no interest:
		key_not_found ->

			% Convenient to catch non-interpreted entries:
			?error_fmt( "No entry in points table found for "
				"measurement point '~ts' of ~ts; skipping this point.",
				[ PointNameBin, sensor_id_to_string( SensorId ) ] ),

			PointsDataTable

	end,

	update_data_table( NewPointsDataTable, T, SensorId, State ).



% @doc Examines the specified reported, current temperature and takes any
% appropriate action.
%
% (sensor expected to be enabled)
%
-spec examine_temperature( measurement_point_name(), celsius(),
		temperature_data(), sensor_id(), wooper:state() ) -> temperature_data().
examine_temperature( PointNameBin, CurrentTemp,
		TempData=#temperature_data{ status=enabled,
									alert_state=AlertState,
									alarm_low=AlarmLow,
									alarm_high=AlarmHigh },
		SensorId, State )
  when CurrentTemp >= AlarmLow andalso CurrentTemp =< AlarmHigh ->

	% In nominal state now; clearing any past alert state:
	case AlertState of

		nominal ->
			TempData;

		PrevAlertState ->

			?notice_fmt( "Temperature of ~ts back to normal (~ts) "
				"(from ~ts) at measurement point ~ts.",
				[ sensor_id_to_string( SensorId ),
				  unit_utils:temperature_to_string( CurrentTemp ),
				  PrevAlertState, PointNameBin ] ),

			Now = time_utils:get_timestamp(),

			TempData#temperature_data{ alert_state=nominal,
									   alert_timestamp=Now }

	end;


examine_temperature( PointNameBin, CurrentTemp,
		TempData=#temperature_data{ alert_state=AlertState,
									alarm_high=AlarmHigh,
									crit_high=CritHigh },
		SensorId, State ) when CurrentTemp > AlarmHigh ->

	% Too hot:
	case CurrentTemp > CritHigh of

		% Total panic, as we are critical_high here:
		true ->
			case AlertState of

				critical_high ->
					% Nothing new, must have already been notified...
					TempData;

				_ ->
					?emergency_fmt( "Temperature of ~ts exceeds the "
						"critical high threshold (~ts) at measurement "
						"point ~ts, which reported ~ts. Check urgently "
						"fans and room temperature.",
						[ sensor_id_to_string( SensorId ),
						  unit_utils:temperature_to_string( CritHigh ),
						  PointNameBin,
						  unit_utils:temperature_to_string( CurrentTemp ) ] ),

					Now = time_utils:get_timestamp(),

					TempData#temperature_data{ alert_state=critical_high,
											   alert_timestamp=Now }

			end;

		% Still a very serious worry, as we are alarm_high here:
		false ->

			% We react to all state changes, aggravations like "improvements":
			case AlertState of

				alarm_high ->
					% Nothing new, must have already been notified...
					TempData;

				_ ->
					?alert_fmt( "Temperature of ~ts exceeds the "
						"alarm high threshold (~ts) at measurement "
						"point ~ts, which reported ~ts (critical high "
						"threshold of ~ts not reached yet). "
						"Check fans and room temperature.",
						[ sensor_id_to_string( SensorId ),
						  unit_utils:temperature_to_string( AlarmHigh ),
						  PointNameBin,
						  unit_utils:temperature_to_string( CurrentTemp ),
						  unit_utils:temperature_to_string( CritHigh ) ] ),

					Now = time_utils:get_timestamp(),

					TempData#temperature_data{ alert_state=alarm_high,
											   alert_timestamp=Now }

			end

	end;


examine_temperature( PointNameBin, CurrentTemp,
		TempData=#temperature_data{ alert_state=AlertState,
									alarm_low=AlarmLow,
									crit_low=CritLow },
		% Implicit, but better safe than sorry:
		SensorId, State ) when CurrentTemp < AlarmLow ->

	% Too cold (!):
	case CurrentTemp < CritLow of

		% Total unlikely panic, as we are critical_low here:
		true ->
			case AlertState of

				critical_low ->
					% Nothing new, must have already been notified...
					TempData;

				_ ->
					?emergency_fmt( "Temperature of ~ts went "
						"below the critical low threshold (~ts) "
						"at measurement point ~ts, which reported ~ts. "
						"Check urgently room temperature.",
						[ sensor_id_to_string( SensorId ),
						  unit_utils:temperature_to_string( CritLow ),
						  PointNameBin,
						  unit_utils:temperature_to_string( CurrentTemp ) ] ),

					Now = time_utils:get_timestamp(),

					TempData#temperature_data{ alert_state=critical_low,
											   alert_timestamp=Now }

			end;

		% Still a very serious worry apparently, as we are alarm_low here:
		false ->
			case AlertState of

				alarm_low ->
					% Nothing new, must have already been notified...
					TempData;

				_ ->
					?alert_fmt( "Temperature of ~ts went below "
						"the alarm low threshold (~ts) at measurement "
						"point ~ts, which reported ~ts (critical low threshold "
						"of ~ts not reached yet). Check room temperature.",
						[ sensor_id_to_string( SensorId ),
						  unit_utils:temperature_to_string( AlarmLow ),
						  PointNameBin,
						  unit_utils:temperature_to_string( CurrentTemp ),
						  unit_utils:temperature_to_string( CritLow ) ] ),

					Now = time_utils:get_timestamp(),

					TempData#temperature_data{ alert_state=alarm_low,
											   alert_timestamp=Now }

			end

	end.



% @doc Examines the specified reported, current intrusion status and takes any
% appropriate action.
%
% (sensor expected to be enabled)
%
-spec examine_intrusion_status( measurement_point_name(),
		intrusion_detected_status(), intrusion_data(), sensor_id(),
		wooper:state() ) -> intrusion_data().
% No intrusion ever:
examine_intrusion_status( _PointNameBin, _CurrentIntrusStatus=false,
		IntrusData=#intrusion_data{ status=enabled,
									intrusion_reported=false },
		_SensorId, _State ) ->
	IntrusData;

% Intrusion still "ongoing":
examine_intrusion_status( _PointNameBin, _CurrentIntrusStatus=true,
		IntrusData=#intrusion_data{ status=enabled,
									intrusion_reported=true },
		_SensorId, _State ) ->
	% As already reported by design:
	IntrusData;

% New intrusion detected:
examine_intrusion_status( PointNameBin, _CurrentIntrusStatus=true,
		IntrusData=#intrusion_data{ status=enabled,
									intrusion_reported=false },
		SensorId, State ) ->

	?emergency_fmt( "Intrusion just detected by ~ts at measurement point ~ts.",
		[ sensor_id_to_string( SensorId ), PointNameBin ] ),

	IntrusData#intrusion_data{ intrusion_reported=true,
							   intrusion_timestamp=time_utils:get_timestamp() };


% Intrusion disappeared (not expected to happen):
examine_intrusion_status( PointNameBin, _CurrentIntrusStatus=false,
		IntrusData=#intrusion_data{ status=enabled,
									intrusion_reported=true },
		SensorId, State ) ->

	?warning_fmt( "Intrusion  detected by ~ts at measurement point ~ts just "
		"disappeared.", [ sensor_id_to_string( SensorId ), PointNameBin ] ),

	% Not changing our view:
	IntrusData.



% Section for the filtering of the attributes of a measurement point, according
% to the category to which the corresponding sensor belongs.


% @doc Filters the specified JSON content corresponding to a CPU socket.
-spec filter_cpu_socket_json( decoded_json() ) ->
									{ [ json_triple() ], [ json_triple() ] }.
filter_cpu_socket_json( SensorJSON ) ->

	% Plain strings used (for matching), as we cannot leave the name of the
	% measurement points as binaries: if having "tempN_input", N can have one
	% digit or more (ex: <<"temp10_input">>), and non-last segments must of
	% course be of fixed length.
	%
	BasicTriples = [ { text_utils:binary_to_string( BinStr ), BinStr, V }
				|| { BinStr, V } <- map_hashtable:enumerate( SensorJSON ) ],

	filter_cpu_socket_json( BasicTriples, _TempAcc=[], _OtherAccc=[] ).



% (helper)
filter_cpu_socket_json( _BasicTriples=[], TempAcc, OtherAcc ) ->
	% Original order is clearer (ex: for core numbers):
	{ lists:reverse( TempAcc ), OtherAcc };

filter_cpu_socket_json(
		_BasicTriples=[ { _Name="temp" ++ Num, BinPointName, V } | T ],
		TempAcc, OtherAcc ) ->

	Desc = integrate_any_number( "CPU socket temperature sensor", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_cpu_socket_json( T, [ JSONTriple | TempAcc ], OtherAcc );



% Ignored entries are typically:
filter_cpu_socket_json( _BasicTriples=[ { Name, BinPointName, V } | T ],
						TempAcc, OtherAcc ) ->

	Desc = text_utils:bin_format( "uncategorised CPU socket sensor '~ts'",
								  [ Name ] ),

	JSONTriple = { BinPointName, Desc, V },

	filter_cpu_socket_json( T, TempAcc, [ JSONTriple | OtherAcc ] ).



% @doc Filters the specified JSON content corresponding to a CPU.
-spec filter_cpu_json( decoded_json() ) ->
									{ [ json_triple() ], [ json_triple() ] }.
filter_cpu_json( SensorJSON ) ->

	BasicTriples = [ { text_utils:binary_to_string( BinStr ), BinStr, V }
				|| { BinStr, V } <- map_hashtable:enumerate( SensorJSON ) ],

	filter_cpu_json( BasicTriples, _TempAcc=[], _OtherAccc=[] ).


% (helper)
filter_cpu_json( _BasicTriples=[], TempAcc, OtherAcc ) ->
	% Original order is clearer (ex: for core numbers):
	{ lists:reverse( TempAcc ), OtherAcc };

filter_cpu_json(
		_BasicTriples=[ { _Name="Package id " ++ Num, BinPointName, V } | T ],
		TempAcc, OtherAcc ) ->

	Desc = integrate_any_number( "CPU temperature sensor for package", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_cpu_json( T, [ JSONTriple | TempAcc ], OtherAcc );


filter_cpu_json(
		_BasicTriples=[ { _Name="Core " ++ Num, BinPointName, V } | T ],
		TempAcc, OtherAcc ) ->

	Desc = integrate_any_number( "CPU temperature sensor for core", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_cpu_json( T, [ JSONTriple | TempAcc ], OtherAcc );


% Ignoring "in*", expected to be voltages:
filter_cpu_json(
		_BasicTriples=[ { _Name="in" ++ _, _BinPointName, _V } | T ],
		TempAcc, OtherAcc ) ->

	filter_cpu_json( T, TempAcc, OtherAcc );


% Ignored entries are typically:
filter_cpu_json( _BasicTriples=[ { Name, BinPointName, V } | T ],
						TempAcc, OtherAcc ) ->

	Desc = text_utils:bin_format( "uncategorised CPU socket sensor '~ts'",
								  [ Name ] ),

	JSONTriple = { BinPointName, Desc, V },

	filter_cpu_json( T, TempAcc, [ JSONTriple | OtherAcc ] ).




% @doc Filters te JSON content corresponding to a motherboard.
-spec filter_motherboard_json( decoded_json() ) ->
	 { [ json_triple() ], [ json_triple() ], [ json_triple() ],
	   [ json_triple() ] }.
filter_motherboard_json( SensorJSON ) ->

	BasicTriples = [ { text_utils:binary_to_string( BinStr ), BinStr, V }
				|| { BinStr, V } <- map_hashtable:enumerate( SensorJSON ) ],

	filter_motherboard_json( BasicTriples, _TempAcc=[], _FanAcc=[],
							 _IntrusionAcc=[], _OtherAcc=[] ).


% (helper)
filter_motherboard_json( _BasicTriples=[], TempAcc, FanAcc, IntrusionAcc,
						 OtherAcc ) ->
	% Original order is clearer:
	{ lists:reverse( TempAcc ), lists:reverse( FanAcc ),
	  lists:reverse( IntrusionAcc ), OtherAcc };

% On-CPU temperature sensors, see
% https://en.wikipedia.org/wiki/Platform_Environment_Control_Interface:
%
filter_motherboard_json(
		_BasicTriples=[ { _Name="PECI Agent " ++ Num, BinPointName, V } | T ],
		TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number( "on-CPU temperature sensor", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
							 OtherAcc );

% Motherboard CPU temperature index:
filter_motherboard_json( _BasicTriples=[
		{ _Name="CPUTIN" ++ Num, BinPointName, V } | T ], TempAcc,
						 FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number( "motherboard-level CPU temperature sensor",
								 Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
							 OtherAcc );

% Temperature sensor for the Platform Control Hub (PCH) chipset:
filter_motherboard_json( _BasicTriples=[
		{ _Name="PCH_CHIP_CPU_MAX_TEMP" ++ Num, BinPointName, V } | T ],
						 TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number(
			"platform control hub chipset max temperature sensor", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
							 OtherAcc );


% Temperature sensor for the Platform Control Hub (PCH) chipset:
filter_motherboard_json( _BasicTriples=[
		{ _Name="PCH_CHIP_TEMP" ++ Num, BinPointName, V } | T ],
						 TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number( "chipset temperature sensor", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
							 OtherAcc );


% Motherboard temperature sensor:
filter_motherboard_json( _BasicTriples=[
		{ _Name="SYSTIN" ++ Num, BinPointName, V } | T ], TempAcc,
						 FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number( "motherboard temperature sensor", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
							 OtherAcc );


% Power supply temperature sensor:
filter_motherboard_json( _BasicTriples=[
		{ _Name="AUXTIN" ++ Num, BinPointName, V } | T ], TempAcc,
						 FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number( "power supply temperature sensor", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
							 OtherAcc );


% Fans known of the motherboard:
filter_motherboard_json( _BasicTriples=[
		{ _Name="fan" ++ Num, BinPointName, V } | T ], TempAcc,
						 FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number( "motherboard-controlled fan", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, TempAcc, [ JSONTriple | FanAcc ], IntrusionAcc,
							 OtherAcc );


% Chassis intrusion sensors:
filter_motherboard_json( _BasicTriples=[
		{ _Name="intrusion" ++ Num, BinPointName, V } | T ],
						 TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = integrate_any_number( "motherboard intrusion detector", Num ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, TempAcc, FanAcc, [ JSONTriple | IntrusionAcc ],
							 OtherAcc );


% Explicitly ignored entries are typically:
%  - "in*" (ex: "in1" - but not "intrusion1") [voltage? unassigned input port?]
%  - Vcore, Vbat, AVCC, 3VSB, +3.3V
%  - beep_enable
%  - PCH_* (Platform Control Hub) such as PCH_CPU_TEMP
%
filter_motherboard_json( _BasicTriples=[
		{ _Name="in" ++ _, _BinPointName, _V } | T ],
						 TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->
	filter_motherboard_json( T, TempAcc, FanAcc, IntrusionAcc, OtherAcc );

filter_motherboard_json( _BasicTriples=[ { IgnName, _BinPointName, _V } | T ],
		TempAcc, FanAcc, IntrusionAcc, OtherAcc ) when IgnName == "Vcore"
			orelse IgnName == "Vbat" orelse IgnName == "AVCC"
			orelse IgnName == "3VSB" orelse IgnName == "+3.3V" ->
	filter_motherboard_json( T, TempAcc, FanAcc, IntrusionAcc, OtherAcc );

filter_motherboard_json( _BasicTriples=[
		{ _Name="beep_enable", _BinPointName, _V } | T ],
						 TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->
	filter_motherboard_json( T, TempAcc, FanAcc, IntrusionAcc, OtherAcc );

filter_motherboard_json( _BasicTriples=[
		{ _Name="PCH_CPU_TEMP" ++ _, _BinPointName, _V } | T ],
						 TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->
	filter_motherboard_json( T, TempAcc, FanAcc, IntrusionAcc, OtherAcc );


% Ignored entries are typically:
filter_motherboard_json( _BasicTriples=[ { Name, BinPointName, V } | T ],
						 TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->

	Desc = text_utils:bin_format( "uncategorised motherboard sensor '~ts'",
								  [ Name ] ),

	JSONTriple = { BinPointName, Desc, V },

	filter_motherboard_json( T, TempAcc, FanAcc, IntrusionAcc,
							 [ JSONTriple | OtherAcc ] ).



% @doc Filters the specified JSON content corresponding to a disk.
-spec filter_disk_json( decoded_json() ) ->
									{ [ json_triple() ], [ json_triple() ] }.
filter_disk_json( SensorJSON ) ->

	BasicTriples = [ { text_utils:binary_to_string( BinStr ), BinStr, V }
				|| { BinStr, V } <- map_hashtable:enumerate( SensorJSON ) ],

	filter_disk_json( BasicTriples, _TempAcc=[], _OtherAccc=[] ).


% (helper)
filter_disk_json( _BasicTriples=[], TempAcc, OtherAcc ) ->
	% Original order may be clearer:
	{ lists:reverse( TempAcc ), OtherAcc };

filter_disk_json( _BasicTriples=[ { Name, BinPointName, V } | T ],
						TempAcc, OtherAcc ) ->

	Desc = text_utils:bin_format( "uncategorised disk sensor '~ts'", [ Name ] ),

	JSONTriple = { BinPointName, Desc, V },

	filter_disk_json( T, [ JSONTriple | TempAcc ], OtherAcc ).



% @doc Integrates to the specified base name of a measurement point a
% corresponding number (if any).
%
-spec integrate_any_number( ustring(), ustring() ) -> bin_string().
integrate_any_number( BaseStr, _Num="" ) ->
	text_utils:string_to_binary( BaseStr );

% If like "_1" in <<"temp_1">>:
integrate_any_number( BaseStr, [ $_ | NumStr ] ) ->
	text_utils:bin_format( "~ts #~ts", [ BaseStr, NumStr ] );

% If like "1" in <<"temp_1">>:
integrate_any_number( BaseStr, NumStr ) ->
	%text_utils:bin_format( "~ts designated by ~ts", [ BaseStr, NumStr ] ).
	text_utils:bin_format( "~ts #~ts", [ BaseStr, NumStr ] ).



% @doc Inits the polling of the sensors.
-spec init_polling( class_USScheduler:periodicity(), wooper:state() ) ->
							wooper:state().
init_polling( SensorPollPeriodicity, State ) ->

	SchedulerPid = case class_USScheduler:get_main_scheduler() of

		undefined ->
			?critical( "The US main scheduler has not been found, "
					   "whereas it is necessary for sensor polling." ),
			throw( us_scheduler_not_found );

		SchedPid ->
			SchedPid

	end,

	SchedulerPid ! { registerTask,
					[ _TaskCmd=readSensors, SensorPollPeriodicity ], self() },

	receive

		{ wooper_result, { task_registered, TaskId } } ->
			setAttributes( State, [ { us_scheduler_pid, SchedulerPid },
									{ task_id, TaskId } ] )

	end.



% @doc Vets specified temperature regarding bogus range.
-spec vet_temperature( celsius(), temperature_description(),
		measurement_point_name(), sensor_id(), wooper:state() ) -> boolean().
vet_temperature( Temp, TempDesc, BinPointName, SensorId, State ) ->
	vet_temperature( Temp, TempDesc, _Min=?low_bogus_temperature_threshold,
		_Max=?high_bogus_temperature_threshold, "bogus", BinPointName,
		SensorId, State ).



% @doc Vets specified temperature regarding specified range.
-spec vet_temperature( celsius(), temperature_description(), celsius(),
		celsius(), range_description(), measurement_point_name(), sensor_id(),
		wooper:state() ) -> boolean().
vet_temperature( Temp, TempDesc, _Min, _Max, _RangeDesc, BinPointName, SensorId,
				 State ) when not is_float( Temp ) ->
	?error_fmt( "For measurement point ~ts of ~ts, the ~ts temperature is "
		"reported as '~p', which is not a float; "
		"so this value is to be ignored.",
		[ BinPointName, sensor_id_to_string( SensorId ), TempDesc, Temp ] ),
	false;

% From here Temp is a float:
vet_temperature( Temp, TempDesc, Min, _Max, RangeDesc, BinPointName, SensorId,
				 State ) when Temp < Min ->
	?warning_fmt( "For measurement point ~ts of ~ts, the ~ts temperature is "
		"reported as ~ts, i.e. below the low ~ts threshold (~ts), "
		"so this value is to be ignored.",
		[ BinPointName, sensor_id_to_string( SensorId ), TempDesc,
		  unit_utils:temperature_to_string( Temp ), RangeDesc,
		  unit_utils:temperature_to_string( Min ) ] ),
	false;

vet_temperature( Temp, TempDesc, _Min, Max, RangeDesc, BinPointName, SensorId,
				 State ) when Temp > Max ->
	?warning_fmt( "For measurement point ~ts of ~ts, the ~ts temperature is "
		"reported as ~ts, i.e. above the high ~ts threshold (~ts), "
		"so this value is to be ignored.",
		[ BinPointName, sensor_id_to_string( SensorId ), TempDesc,
		  unit_utils:temperature_to_string( Temp ), RangeDesc,
		  unit_utils:temperature_to_string( Max ) ] ),
	false;

vet_temperature( _Temp, _TempDesc, _Min, _Max, _RangeDesc, _BinPointName,
				 _SensorId, _State ) ->
	true.



% @doc Checks that the static information of specified temperature data is
% legit, and returns it.
%
-spec check_temperature_data( temperature_data() ) -> temperature_data().
check_temperature_data( TempData=#temperature_data{ status=disabled } ) ->
	TempData;

check_temperature_data( TempData=#temperature_data{
									input_attribute=InputAttr,
									description=Desc,
									status=enabled,
									alert_state=AlertState,
									alert_timestamp=AlertTimestamp,
									crit_low=CritLowTemp,
									alarm_low=AlarmLowTemp,
									crit_high=CritHighTemp,
									alarm_high=AlarmHighTemp } ) ->

	type_utils:check_binaries( [ InputAttr, Desc ] ),

	basic_utils:check_all_defined( [ AlertState, AlertState, AlertTimestamp ] ),

	OrderedTemps = [ CritLowTemp, AlarmLowTemp, AlarmHighTemp, CritHighTemp ],
	type_utils:check_floats( OrderedTemps ),
	list_utils:check_strictly_ascending( OrderedTemps ),

	TempData.



% @doc Checks that the static information of specified intrusion data is legit,
% and returns it.
%
-spec check_intrusion_data( intrusion_data() ) -> intrusion_data().
check_intrusion_data( IntrusData=#intrusion_data{ status=disabled } ) ->
	IntrusData;

check_intrusion_data( IntrusData=#intrusion_data{
									input_attribute=InputAttr,
									description=Desc,
									status=enabled,
									intrusion_reported=IntrusStatus,
									intrusion_timestamp=IntrusTimestamp,
									beep_on_intrusion=DoBeep } ) ->

	type_utils:check_binaries( [ InputAttr, Desc ] ),

	basic_utils:check_all_defined( [ IntrusStatus, IntrusTimestamp, DoBeep ] ),

	IntrusData.



% @doc Parses the sensor JSON output stored in the specified file, returns a
% state with a corresponding initial sensor table.
%
-spec parse_sensor_output_from_file( file_path(), wooper:state() ) ->
											wooper:state().
parse_sensor_output_from_file( OutputFilePath, State ) ->

	case file_utils:is_existing_file_or_link( OutputFilePath ) of

		true ->
			ok;

		false ->
			?error_fmt( "The file '~ts' from which sensor output is to be read "
				"does not exist (current directory: '~ts').",
				[ OutputFilePath, file_utils:get_current_directory() ] ),
			throw( { sensor_output_file_not_found, OutputFilePath } )

	end,

	% Better integrated than if using json_utils:from_json_file/1:
	BinJson = file_utils:read_whole( OutputFilePath ),

	parse_initial_sensor_output( BinJson, State ).



% @doc Returns a textual description of the specified temperature alert state.
-spec temp_alert_state_to_string( temp_alert_state() ) -> ustring().
temp_alert_state_to_string( _AlertState=critical_high ) ->
	"critical high temperature";

temp_alert_state_to_string( _AlertState=alarm_high ) ->
	"alarm high temperature";

temp_alert_state_to_string( _AlertState=nominal ) ->
	"nominal temperature";

temp_alert_state_to_string( _AlertState=alarm_low ) ->
	"alarm low temperature";

temp_alert_state_to_string( _AlertState=critical_low ) ->
	"critical low temperature".



% @doc Returns a textual description of the specified measurement data.
-spec point_data_to_string( point_data() ) -> ustring().
point_data_to_string( #temperature_data{ input_attribute=InputAttr,
										 description=Desc,
										 status=disabled } ) ->
	text_utils:format( "which is currently disabled (whereas read "
		"temperature attribute would be '~ts' for ~ts)", [ InputAttr, Desc ] );

point_data_to_string( #temperature_data{ avg_count=0 } ) ->
	"with no (plausible) temperature reported yet";

point_data_to_string( #temperature_data{ input_attribute=InputAttr,
										 description=Desc,
										 % Implicit: status=enabled,
										 alert_state=AlertState,
										 alert_timestamp=AlertTimestamp,
										 current=Current,
										 min_reached=Min,
										 max_reached=Max,
										 avg_sum=AvgSum,
										 avg_count=AvgCount,
										 alarm_low=MaybeAlarmLow,
										 alarm_high=MaybeAlarmHigh,
										 crit_low=MaybeCritLow,
										 crit_high=MaybeCritHigh } ) ->

	Now = time_utils:get_timestamp(),

	AlertStr = text_utils:format( "this point has been in ~ts state for ~ts",
		[ temp_alert_state_to_string( AlertState ),
		  time_utils:get_textual_duration( AlertTimestamp, Now ) ] ),

	% Count is non-null by design:
	Avg = AvgSum / AvgCount,

	[ CurrentTempStr | OtherTempStrs ] =
		[ unit_utils:maybe_temperature_to_string( T )
			|| T <- [ Current, Min, Max, Avg, MaybeAlarmLow, MaybeCritLow,
					  MaybeAlarmHigh, MaybeCritHigh ] ],

	text_utils:format( "whose current temperature (read from the '~ts' "
		"attribute, for ~ts) is ~ts; ~ts "
		"(regarding measurements: min=~ts, max=~ts, average=~ts; "
		"regarding thresholds: alarm low=~ts, critical low=~ts, "
		"alarm high=~ts, critical high=~ts)",
		[ InputAttr, Desc, CurrentTempStr, AlertStr ] ++ OtherTempStrs );


point_data_to_string( #intrusion_data{ input_attribute=InputAttr,
									   description=Desc,
									   status=disabled } ) ->
	text_utils:format( "which is currently disabled (whereas read "
		"intrusion attribute would be '~ts' for ~ts)", [ InputAttr, Desc ] );


point_data_to_string( #intrusion_data{
								input_attribute=InputAttr,
								description=Desc,
								% Implicit: status=enabled,
								intrusion_reported=false } ) ->
	text_utils:format( "which does not report any intrusion (as read from "
		"the '~ts' attribute, for ~ts)", [ InputAttr, Desc ] );


point_data_to_string( #intrusion_data{
								input_attribute=InputAttr,
								description=Desc,
								% Implicit: status=enabled,
								intrusion_reported=true,
								intrusion_timestamp=IntrusTimestamp,
								beep_on_intrusion=DoBeep } ) ->

	Now = time_utils:get_timestamp(),

	text_utils:format( "which reports an intrusion that happened ~ts ago "
		"(as read from the '~ts' attribute, for ~ts; beep on intrusion: ~ts)",
		[ time_utils:get_textual_duration( IntrusTimestamp, Now ), InputAttr,
		  Desc, DoBeep ] ).



% @doc Returns a textual description of the specified measurement point table.
-spec measurement_points_to_string( points_data_table() ) -> ustring().
measurement_points_to_string( PointsDataTable ) ->
	measurement_points_to_string( PointsDataTable, _IndentationLevel=0 ).


% @doc Returns a textual description of the specified measurement point table.
-spec measurement_points_to_string( points_data_table(),
									indentation_level() ) -> ustring().
measurement_points_to_string( PointsDataTable, IndentationLevel ) ->

	case table:enumerate( PointsDataTable ) of

		[] ->
			"no measurement point registered";

		[ { PointName, PointData } ] ->
			text_utils:format(
				"a single measurement point registered: ~ts, ~ts",
				[ PointName, point_data_to_string( PointData ) ] );

		MesurePairs ->
			text_utils:format( "~B measurement points registered: ~ts",
				[ length( MesurePairs ),
				  text_utils:strings_to_enumerated_string(
					[ text_utils:format( "point '~ts', ~ts",
						[ PN, point_data_to_string( TD ) ] )
						  || { PN, TD } <- MesurePairs ], IndentationLevel ) ] )

	end.



% @doc Returns a textual description of the specified intrusion status.
-spec intrusion_status_to_string( temp_alert_state() ) -> ustring().
intrusion_status_to_string( _IntrusStatus=true ) ->
	"intrusion detected";

intrusion_status_to_string( _IntrusStatus=false ) ->
	"no intrusion detected".



% @doc Returns a textual description of the specified sensor identifier.
-spec sensor_id_to_string( sensor_id() ) -> ustring().
sensor_id_to_string( { AtomSensorType, SensorInterface, SensorNumber } ) ->
	text_utils:format( "sensor of type ~ts, on interface ~ts, "
		"whose number is ~ts",
		[ AtomSensorType, SensorInterface, SensorNumber ] ).



% @doc Returns a textual description of the specified sensor information.
-spec sensor_info_to_string( sensor_info() ) -> ustring().
sensor_info_to_string( #sensor_info{ raw_id=RawIdBinStr,
									 id={ _RawType, Interface, _NumBinStr },
									 category=Categ,
									 data=MaybePointsDataTable } ) ->

	DataStr = case MaybePointsDataTable of

		undefined ->
			"with no data";

		PointsDataTable ->
			"with " ++ measurement_points_to_string( PointsDataTable )

	end,

	text_utils:format( "~ts sensor on interface ~ts (raw identifier: '~ts'), "
		"~ts", [ Categ, Interface, RawIdBinStr, DataStr ] ).



% @doc Returns a textual description of the specified JSON triple.
-spec json_triple_to_string( json_triple() ) -> ustring().
json_triple_to_string( { BinPointName, BinDesc, PointValueMap } ) ->
	text_utils:format( "measurement point '~ts' (~ts) whose value map is ~p",
					   [ BinPointName, BinDesc, PointValueMap ] ).



% @doc Returns a textual description of the specified sensor table.
-spec sensor_table_to_string( sensor_table() ) -> ustring().
sensor_table_to_string( SensorTable ) ->

	case table:values( SensorTable ) of

		[] ->
			"no sensor";

		[ SensorInfo ] ->
			text_utils:format( "a single sensor, ~ts",
							   [ sensor_info_to_string( SensorInfo ) ] );

		SensorInfos ->
			text_utils:format( "~B sensors: ~ts",
				[ length( SensorInfos ), text_utils:strings_to_string(
					[ sensor_info_to_string( SI ) || SI <- SensorInfos ] ) ] )

	end.



% @doc Returns a textual description of this manager.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	TrackStr = case ?getAttr(sensor_monitoring) of

		true ->
			text_utils:format( "tracking ~ts",
				[ sensor_table_to_string( ?getAttr(sensor_table) ) ] );

		false ->
			"with no sensor tracking enabled"

	end,

	text_utils:format( "US sensor manager for '~ts', ~ts",
					   [ net_utils:localhost(), TrackStr ] ).
