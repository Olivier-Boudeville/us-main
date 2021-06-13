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
% local host</b> (notably the temperatures and the operation of fans), and of
% reporting any abnormal situation.
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




% Implementation notes:


% Regarding sensors:
%
% The monitoring done by this server relies on the 'sensors' executable
% (typically /usr/bin/sensors, obtained generally from a package of the same
% name, relying on lm-sensors, see https://github.com/lm-sensors/lm-sensors).
% Install the 'i2c-tools' package as well for DIMM information (see R2 below).

% The 'sensors-detect' script must have been run once by root beforehand (select
% only the default, safer options, by hitting Enter repeatedly or simply use its
% '--auto' option), in order to configure sensors. Configuration is typically
% stored in /etc/sensors3.conf.
%
% To have information regarding a given sensor, psensor may be used: open the
% preferences of the sensor (click on its name in the main window, and select
% the menu item 'Preferences'), and look at the 'Chip' field.  See
% https://wpitchoune.net/psensor/faq.html for more information.

% Sensors is reporting values found in the Linux virtual file system directory:
% /sys/class/thermal/thermal_zone*/{temp,type}.
%
% Examples:
%
% - package id 0 is your (first) CPU
%
% - dell_smm-virtual-0 is your CPU fan, managed by your system firmware
%
% - acpitz-virtual-0 (ACPI Thermal Zone) is the temperature sensor near/on your
% CPU socket; this sensor can be unreliable
%
% - coretemp-isa-0000 measures the temperature of the specific cores

% Use also: paste <(cat /sys/class/thermal/thermal_zone*/type) <(cat
% /sys/class/thermal/thermal_zone*/temp) | column -s $'\t' -t | sed
% 's/...$/.0°C/' for thermal information


% See also:

% - R1: https://askubuntu.com/questions/843231/what-is-the-meaning-of-the-output-of-the-command-sensors
% - R2: https://wiki.archlinux.org/title/lm_sensors
% - R3: https://www.linux.com/topic/desktop/advanced-lm-sensors-tips-and-tricks-linux-0/
% - R4: https://www.linux.com/training-tutorials/jazz-lm-sensors-graphics-and-notifications-0/
% - R5: https://wpitchoune.net/psensor/


% Read from a sensor on your CPU:  Core temperature.
%
% Read from sensors on the motherboard:
% - CPUTIN (CPU temperature index): CPU temperature
% - AUXTIN (auxiliary temperature index): power supply temperature sensor
% - SYSTIN (system temperature index): motherboard temperature


% Settings: HYST (hysteresis; this is the value that you want an alarm to turn
% off. For example, if your alarm temperature is 80C, set your HYST value to
% stop the alarm when the temperature falls to 75C.)


% More generally temperatures may be monitored regarding:
% - the motherboard and CPU sensors, thanks to lm-sensors
% - Hard Disk Drives, thanks to hddtemp, libatasmart, udisks2 or smartmontools
% - DIMM Temperature sensors (see R2)
% - GPU, thanks to XNVCtrl for NVidia ones, or ADL SDK for ATI ones
%
% Can also be monitored:
% - the rotation speed of the fans, thanks to lm-sensors
%
% Refer to R5 for further details.


% Note that map_hashtables are explicitly mentioned instead of tables as this is
% the actual type returned by the json_utils.



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
% Ex: <<"0a20">>.


-type raw_sensor_id() :: bin_string().
% Full identifier of a sensor, directly as read; ex: <<"coretemp-isa-0000">>,
% <<"nct6792-isa-0a20">>, <<"nct6779-isa-0a00">>, <<"nvme-pci-0200">>, etc.


-type sensor_id() ::
		{ atom_sensor_type(), sensor_interface(), sensor_number() }.
% Full identifier of a sensor, directly deriving from a raw one.


-export_type([ raw_sensor_type/0, atom_sensor_type/0,
			   sensor_category/0, sensor_interface/0, sensor_number/0,
			   raw_sensor_id/0, sensor_id/0 ]).


-record( sensor_info, {


	% Duplicated here for convenience (often is an associated key):
	raw_id :: raw_sensor_id(),


	% Identifier of this sensor.
	id :: sensor_id(),


	% Higher-level category of this sensor:
	category :: sensor_category(),


	% The data stored regarding that sensor, which depends on its category:
	%
	%  - for the cpu_socket category, a temperature_points() table is stored
	%
	data :: maybe( sensor_data() )

} ).

-type sensor_info() :: #sensor_info{}.
% All information regarding a known sensor.


-type sensor_data() :: temperature_points().
% The various data corresponding to various kinds of sensors.


-record( temperature_data, {

	% The current temperature reading:
	current :: celsius(),

	% The lowest temperature measured since start:
	min :: celsius(),

	% The highest temperature measured since start:
	max :: celsius(),


	% To compute the average temperature:

	% The sum of all temperatures measured since start:
	avg_sum = 0.0 :: celsius(),

	% The number of all temperature measurements since start:
	avg_count = 0 :: count(),


	% The built-in critical high temperature (i.e. the temperature above which
	% serious problems are bound to happen), as reported by the sensor (if any
	% is defined):
	%
	crit_high = undefined :: maybe( celsius() ),

	% crit_low would be mostly meaningless.


	% The temperature above which we consider an alarm shall be raised (it is
	% typically set lower than any crit_high):
	%
	alarm_high = undefined :: maybe( celsius() )

	% alarm_low would be mostly meaningless.

} ).

-type temperature_data() :: #temperature_data{}.
% Stores information about the temperature measured by a given measurement point
% of a sensor.



% Should none be returned by a sensor (as celsius()):
-define( default_critical_high_temperature, 95.0 ).



-type measurement_point_name() :: bin_string().
% The name of a measurement point (ex: <<"temp1">>) of a sensor (which may
% provide multiple points).


-type temperature_points() ::
		table( measurement_point_name(), temperature_data() ).
% A table associating, to a temperature measurement point, its data.


-type sensor_table() :: table( raw_sensor_id(), sensor_info() ).
% Table registering all information regarding all local sensors; useful when
% parsing a global (JSON) sensor report.


-type point_attribute() :: bin_string().
% An attribute of a measurement point, as read from JSON (ex: <<"temp1_crit">>).

-type point_value() :: celsius().
% The value obtained at a given measurement point, as read from JSON (ex:
% 105.0).


-type point_value_map() ::
		map_hashtable:map_hashtable( point_attribute(), point_value() ).
% Table associating, to an attribute (ex: <<"temp1_crit">>) of a measurement
% point, a value (ex: 105.0).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type indentation_level() :: indentation_level().

-type string_json() :: json_utils:string_json().
-type decoded_json() :: json_utils:decoded_json().

-type celsius() :: unit_utils:celsius().

%-type scheduler_pid() :: class_USScheduler:scheduler_pid().



% The class-specific attributes:
-define( class_attributes, [

	{ sensor_exec_pair, system_utils:execution_pair(),
	  "to run the sensor tool conveniently" },

	{ parser_state, parser_state(), "state of the JSON parser in use" },

	{ sensor_table, sensor_table(),
	  "table registering all information regarding detected local sensors" },

	{ us_config_server_pid, server_pid(),
	  "the PID of the overall US configuration server" }

] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Sensors" ).



% Exported helpers:
-export([ temperature_points_to_string/1, temperature_points_to_string/2,
		  temperature_data_to_string/1 ]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").




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

	InitState = init_sensors( SrvState ),

	?send_info( InitState, "Constructed: " ++ to_string( InitState ) ),

	InitState.



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug_fmt( "Deletion initiated, while state is: ~ts.",
				[ to_string( State ) ] ),

	?info( "Deleted." ),
	State.




% Method section.


% Helper section.


% @doc Initialises the sensor management.
-spec init_sensors( wooper:state() ) -> wooper:state().
init_sensors( State ) ->

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

	?debug_fmt( "Exec pair for sensor reading:~n ~p.", [ ExecPair ] ),

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

	ReadyState = setAttributes( State, [
		{ sensor_exec_pair, ExecPair },
		{ parser_state, CheckedParserState } ] ),

	initialise_sensor_data( ReadyState ).



% @doc Initialises and integrates the first data (ex: temperatures and fan
% speeds) read from sensors.
%
initialise_sensor_data( State ) ->

	case fetch_sensor_data( State ) of

		{ ok, CmdOutput } ->
			parse_initial_sensor_output( CmdOutput, State );

		% If it happens at initialisation, this is a showstopper:
		{ error, ErrorOutput } ->
			throw( { cannot_read_sensors, ErrorOutput } )

	end.



% Returns the output of the corresponding sensor command.
-spec fetch_sensor_data( wooper:state() ) -> fallible( ustring() ).
fetch_sensor_data( State ) ->

	{ ExecPath, ExecArgs } = ?getAttr(sensor_exec_pair),

	case system_utils:run_executable( ExecPath, ExecArgs ) of

		{ _ReturnCode=0, CmdOutput  } ->
			?debug_fmt( "Read from sensors: '~ts'.", [ CmdOutput ] ),
			{ ok, CmdOutput };

		{ ReturnCode,ErrorOutput  } ->
			?error_fmt( "Error when running '~ts' with arguments ~p: '~ts' "
				"(exit code: ~B).",
				[ ExecPath, ExecArgs, ErrorOutput, ReturnCode ] ),

			% Do not crash:
			%throw( { sensor_read_failed, ErrorOutput } )
			{ error, ErrorOutput }

	end.



% @doc Parses the specified sensor JSON output, returns a state with an initial
% sensor table.
%
-spec parse_initial_sensor_output( string_json(), wooper:state() ) ->
										wooper:state().
parse_initial_sensor_output( SensorJsonStr, State ) ->

	ParserState = ?getAttr(parser_state),

	DecodedMap = json_utils:from_json( SensorJsonStr, ParserState ),

	?debug_fmt( "Top-level decoded map for sensors: ~ts",
				[ map_hashtable:to_string( DecodedMap ) ] ),

	parse_initial_sensors( map_hashtable:enumerate( DecodedMap ),
						   _SensorTable=table:new(), State ).



% @doc Parses in turn the specified initial sensor JSON entries.
parse_initial_sensors( _SensorPairs=[], SensorTable, State ) ->
	setAttribute( State, sensor_table, SensorTable );

parse_initial_sensors( _SensorPairs=[ { RawSensorIdBinStr, SensorJSON } | T ],
					   SensorTable, State ) ->

	% To have for example "nct6779-isa-0a00":
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



% @doc Parses specified JSON information regarding a given sensor.
-spec parse_sensor_data( decoded_json(), sensor_category(), sensor_id(),
						 wooper:state() ) -> maybe( sensor_data() ).
parse_sensor_data( SensorJSON, _SensorCateg=cpu_socket, SensorId, State ) ->

	?debug_fmt( "JSON to parse for ~ts for cpu_socket :~n ~p",
				[ sensor_id_to_string( SensorId ), SensorJSON ] ),

	init_temperature_points( SensorJSON, SensorId, State );


parse_sensor_data( SensorJSON, SensorCateg, SensorId, State ) ->
	?debug_fmt( "Ignoring JSON for ~ts of unsupported category ~ts:~n ~p",
				[ sensor_id_to_string( SensorId ), SensorCateg, SensorJSON ] ),
	undefined.



% @doc Initialises a table of temperature points, for a given sensor, from
% specified JSON content.
%
-spec init_temperature_points( decoded_json(), sensor_id(), wooper:state() ) ->
										temperature_points().
init_temperature_points( SensorJSON, SensorId, State ) ->
	PointPairs = map_hashtable:enumerate( SensorJSON ),
	init_temperature_points( PointPairs, _TempPointTable=table:new(),
							 SensorId, State ).


% (helper)
init_temperature_points( _PointPairs=[], TempPointTable, _SensorId, _State ) ->
	TempPointTable;

% Ex: PointName = <<"temp1">> and PointValues = #{<<"temp1_crit">> => 105.0,
% <<"temp1_input">> => 27.8}.
%
init_temperature_points( _PointPairs=[ { PointName, PointValueMap } | T ],
						 TempPointTable, SensorId, State ) ->

	InitPointData =
		init_temperature_data( PointValueMap, PointName, SensorId, State ),

	% First iteration made directly:
	DirectPointData = update_temperature_data( PointName, PointValueMap,
											   InitPointData, SensorId, State ),

	% Uniqueness checked:
	NewTempPointTable =
		table:add_new_entry( PointName, DirectPointData, TempPointTable ),

	init_temperature_points( T, NewTempPointTable, SensorId, State ).



% @doc Returns a new temperature data initialised from specified JSON content,
% ready to be updated with the future next current temperatures.
%
-spec init_temperature_data( point_value_map(), measurement_point_name(),
			sensor_id(), wooper:state() ) -> temperature_data().
init_temperature_data( PointValueMap, PointName, SensorId, State ) ->

	% Here the current temperature has not been taken into account yet:
	init_temp_point( _TempEntries=map_hashtable:enumerate( PointValueMap ),
		text_utils:binary_to_string( PointName ), #temperature_data{},
		SensorId, PointValueMap, State ).




% (helper)
init_temp_point( _TempEntries=[], PointNameStr,
				 TempData=#temperature_data{ crit_high=MaybeCritHighTemp },
				 SensorId, PointValueMap, State ) ->

	CritHighTemp = case MaybeCritHighTemp of

		undefined ->
			?default_critical_high_temperature;

		_ ->
			MaybeCritHighTemp

	end,

	AlarmHighTemp = max( CritHighTemp - 15.0, 0.9 * CritHighTemp ),

	% All constant information then set:
	ReadyTempData = TempData#temperature_data{ crit_high=CritHighTemp,
											   alarm_high=AlarmHighTemp },

	% Now performs the first usual reading:
	update_temperature_data( text_utils:string_to_binary( PointNameStr ),
							 PointValueMap, ReadyTempData, SensorId, State );


% Ex: AttrNameBin = <<"temp1_input">>, AttrValue = 44.85:
init_temp_point( _TempEntries=[ { AttrNameBin, AttrValue } | T ], PointNameStr,
				 TempData, SensorId, PointValueMap, State ) ->

	AttrName = text_utils:binary_to_string( AttrNameBin ),

	case text_utils:split_after_prefix( _Prefix=PointNameStr, AttrName ) of

		no_prefix ->
			?error_fmt( "Attribute '~ts' not prefixed with the name of "
				"the current measurement point '~ts', thus ignored.",
				[ AttrName, PointNameStr ] ),
			init_temp_point( T, PointNameStr, TempData, SensorId, PointValueMap,
							 State );

		"_input" ->
			% To be taken into account once all other attributes (especially the
			% static ones) have been processed:
			%
			init_temp_point( T, PointNameStr, TempData, SensorId, PointValueMap,
							 State );

		"_crit" ->
			NewTempData = TempData#temperature_data{ crit_high=AttrValue },
			init_temp_point( T, PointNameStr, NewTempData, SensorId,
							 PointValueMap, State );

		"_" ++ _AttrSuffix ->
			?warning_fmt( "Unknown suffix for attribute '~ts', which is "
				"thus ignored.", [ AttrName ] ),
			init_temp_point( T, PointNameStr, TempData, SensorId, PointValueMap,
							 State )

	end.



% @doc Updates the specified temperature data based on the current temperature
% read from specified JSON-originated map.
%
-spec update_temperature_data( measurement_point_name(), point_value_map(),
		temperature_data(), sensor_id(), wooper:state() ) -> temperature_data().
update_temperature_data( PointName, PointValueMap, TempData, SensorId,
						 State ) ->

	CurrentTempKey = text_utils:bin_format( "~ts_input", [ PointName ] ),

	case map_hashtable:lookup_entry( CurrentTempKey, PointValueMap ) of

		key_not_found ->
			?warning_fmt( "No '~ts' key found for ~ts; ignoring the "
				"corresponding measurement point.",
				[ CurrentTempKey, sensor_id_to_string( SensorId ) ] ),
			TempData;


		{ value, CurrentTemp } ->

			MaybeMin = TempData#temperature_data.min,

			NewMin = case MaybeMin of

				undefined ->
					CurrentTemp;

				_ ->
					min( CurrentTemp, MaybeMin )

			end,

			MaybeMax = TempData#temperature_data.max,

			NewMax = case MaybeMax of

				undefined ->
					CurrentTemp;

				_ ->
					max( CurrentTemp, MaybeMax )

			end,

			NewAvgSum = TempData#temperature_data.avg_sum + CurrentTemp,
			NewAvgCount = TempData#temperature_data.avg_count + 1,

			CritHigh = TempData#temperature_data.crit_high,

			case CritHigh =/= undefined andalso CurrentTemp > CritHigh of

				true ->
					?emergency_fmt( "For ~ts, current temperature, ~ts, "
						"exceeds the critical one (~ts).",
						[ sensor_id_to_string( SensorId ),
						  unit_utils:temperature_to_string( CurrentTemp ),
						  unit_utils:temperature_to_string( CritHigh ) ] );

				% As we expect AlarmHigh < CritHigh:
				false ->

					AlarmHigh = TempData#temperature_data.alarm_high,

					case AlarmHigh =/= undefined
							andalso CurrentTemp > AlarmHigh of

						true ->
							?alert_fmt( "For ~ts, current temperature, ~ts, "
								"exceeds the alarm one (~ts).",
								[ sensor_id_to_string( SensorId ),
								  unit_utils:temperature_to_string(
									CurrentTemp ),
								  unit_utils:temperature_to_string( AlarmHigh )
								] );

						false ->
							ok

					end

			end,

			TempData#temperature_data{ current=CurrentTemp,
									   min=NewMin,
									   max=NewMax,
									   avg_sum=NewAvgSum,
									   avg_count=NewAvgCount }

	end.



% @doc Returns a textual description of the specified temperature data.
-spec temperature_data_to_string( temperature_data() ) -> ustring().
temperature_data_to_string( #temperature_data{ avg_count=0 } ) ->
	"with no temperature reported yet";

temperature_data_to_string( #temperature_data{ current=Current,
											   min=Min,
											   max=Max,
											   avg_sum=AvgSum,
											   avg_count=AvgCount,
											   crit_high=MaybeCritHigh,
											   alarm_high=MaybeAlarmHigh } ) ->

	% Count is non-null by design:
	Avg = AvgSum / AvgCount,

	TempStrs = [ unit_utils:maybe_temperature_to_string( T )
		|| T <- [ Current, Min, Max, Avg, MaybeAlarmHigh, MaybeCritHigh ] ],

	text_utils:format( "whose current temperature is ~ts "
		"(min: ~ts, max: ~ts, average: ~ts; "
		"thresholds: alarm: ~ts, critical: ~ts)",
		TempStrs ).



% @doc Returns a textual description of the specified temperature point table.
-spec temperature_points_to_string( temperature_points() ) -> ustring().
temperature_points_to_string( TempPointTable ) ->
	temperature_points_to_string( TempPointTable, _IndentationLevel=0 ).


% @doc Returns a textual description of the specified temperature point table.
-spec temperature_points_to_string( temperature_points(),
									indentation_level() ) -> ustring().
temperature_points_to_string( TempPointTable, IndentationLevel ) ->

	case table:enumerate( TempPointTable ) of

		[] ->
			"no measurement point registered";

		[ { PointName, TempData } ] ->
			text_utils:format(
				"a single measurement point registered: ~ts, ~ts",
				[ PointName, temperature_data_to_string( TempData ) ] );

		MesurePairs ->
			text_utils:format( "~B measurement points registered: ~ts",
				[ length( MesurePairs ),
				  text_utils:strings_to_enumerated_string(
					[ text_utils:format( "point '~ts', ~ts",
						[ PN, temperature_data_to_string( TD ) ] )
					  || { PN, TD } <- MesurePairs ], IndentationLevel ) ] )

	end.



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
									 data=MaybeData } ) ->

	DataStr = case MaybeData of

		undefined ->
			"with no data";

		Data ->
			"with " ++ case Categ of

				cpu_socket ->
					temperature_points_to_string( Data, _IndentationLevel=1 );

				_ ->
					"sensor-specific data"

					   end

	end,

	text_utils:format( "~ts sensor on interface ~ts (raw identifier: '~ts'), "
		"~ts", [ Categ, Interface, RawIdBinStr, DataStr ] ).



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
	text_utils:format( "US sensor manager for '~ts', tracking ~ts",
		[ net_utils:localhost(),
		  sensor_table_to_string( ?getAttr(sensor_table) ) ] ).
