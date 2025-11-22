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
% Creation date: Saturday, June 5, 2021.

-module(class_USSensorManager).

-moduledoc """
US server in charge of the **monitoring of the sensors available on a local
host** (notably the temperatures, chassis intrusion status and the operation of
fans), and of reporting any abnormal situation.
""".



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
% - an initial pass through all measurement points of all sensors, in order to
% initialise properly them, based on a per-category exhaustive filtering
% - regular update passes, just picking the enabled points, updating their
% status and possibly reporting any detected issue



% Implementation notes:

% Regarding sensors, refer to the Technical Manual of the Universal Server
% (http://us-main.esperide.org).
%
% Many sensors look like poorly-designed random generators.
%
% Initially we registered all sensors and points, even muted ones. It was overly
% complex and mostly useless. Now we drop muted ones, so unknown reported ones
% are just ignored (anyway the set of sensors never changes).


% This sensor manager is designed to be able to integrate to an OTP supervision
% tree thanks to a supervisor bridge, whose behaviour is directly defined in
% this module. See https://wooper.esperide.org/#otp-guidelines for further
% information.
%
-behaviour(supervisor_bridge).

% User API of the bridge:
-export([ start_link/0 ]).


% Callbacks of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).

-define( bridge_name, ?MODULE ).



% Entries in the US-configuration files for sensor monitoring:

-define( us_main_sensor_key, sensor_monitoring ).



% For measurement points known to report bogus values:
-define( us_main_muted_sensor_measurements, muted_measurements ).

% Designates all (measurement) points of a given sensor:
-define( us_main_all_points, all_points ).




% Note that map_hashtable instances are explicitly mentioned instead of tables,
% as this is the actual type returned by the json_utils module (as opposed to
% the 'table' WOOPER-native type).


% Section about JSON data being read.
%
% Types defined from here correspond to elements read from the JSON output of
% the 'sensors' tool.

% Canonical sensor order enforced here: temperature, fan, then chassis
% intrusion.




-doc "PID of a sensor manager.".
-type sensor_manager_pid() :: class_USServer:server_pid().



-doc "To designate values read from JSON keys.".
-type json_read_value() :: float().



-doc """
Table associating, to a measurement point (e.g. `<<"Core 0">>`), a table of the
corresponding attributes (that is a `point_attribute_map/0`).
""".
-type json_point_map() :: map_hashtable( measurement_point_name(),
                                         point_attribute_map() ).



-doc "An entry of a json_point_map/0.".
-type json_point_entry() :: { measurement_point_name(), point_attribute_map() }.



-doc """
An entry corresponding to the top-level information read from JSON, in a
corresponding map.
""".
-type json_sensor_entry() :: { raw_sensor_id(), json_point_map() }.



-doc "Defined just for convenience.".
-type json_triple() :: { measurement_point_name(),
    measurement_point_description(), point_attribute_map() }.




% Section about sensors.


-doc """
The reported type for a sensor, like "coretemp", "k10temp", "acpitz", "nvme",
"radeon", etc.; many exist as they correspond to different chips (e.g.
"nct6792") on motherboards.
""".
-type raw_sensor_type() :: ustring().



-doc "Atom-based version of a raw sensor type (e.g. `coretemp`).".
-type atom_sensor_type() :: atom().



-doc """
A higher-level (potentially less precise), clearer (US-assigned) category for a
sensor, deriving from a raw sensor type.
""".
-type sensor_category() ::
    'cpu' % Also includes APU (e.g. Atom+Radeon)
  | 'cpu_socket' % Often less reliable than 'cpu'.
  | 'motherboard'
  | 'chipset' % Like pch_skylake-virtual-*.
  | 'ram'
  | 'disk'
  | 'battery'
  | 'gpu'
  | 'network' % Network interface, like the iwlwifi wireless adapter.
  | 'bus' % Typically USB
  | 'unknown'.



-doc "The reported hardware interface for a sensor.".
-type sensor_interface() ::
    'isa' | 'acpi' | 'pci' | 'virtual' | 'unknown' | atom().



-doc """
For example "0a20", in `{nct6792, isa, "0a20"}`.
""".
-type user_sensor_number() :: ustring().



-doc """
Kept as a (binary) string (even if is an hexadecimal value), for clarity.

For example `<<"0a20">>`.
""".
-type sensor_number() :: bin_string().



-doc """
Full identifier of a sensor, directly as read from JSON; e.g.
`<<"coretemp-isa-0000">>`, `<<"nct6792-isa-0a20">>`, `<<"nvme-pci-0200">>`,
`<<"radeon-pci-0008">>`, etc.; structure is
`<<"RawSensorType-SensorInterface-SensorNumber">>`.
""".
-type raw_sensor_id() :: bin_string().



-doc """
Table registering all information regarding all local sensors; useful when
parsing a global (JSON) sensor report.
""".
-type sensor_table() :: table( raw_sensor_id(), sensor_info() ).



-doc """
Full internal identifier of a sensor, directly deriving from a raw one.

For example `{coretemp, isa, <<"0a20">>}`.
""".
-type sensor_id() ::
    { atom_sensor_type(), sensor_interface(), sensor_number() }.




% Section about measurement points.


-doc """
The (raw) name of a measurement point (e.g. '<<"temp1">>' or '<<"Core 0">>') of
a sensor (which may provide multiple points), as read from JSON.
""".
-type measurement_point_name() :: bin_string().



-doc """
A description of a measurement point that is more user-friendly than its raw
name.
""".
-type measurement_point_description() :: bin_string().



-doc """
The enable status of a measurement point. Those that report bogus values shall
be disabled.
""".
-type point_status() :: 'enabled' | 'disabled'.



-doc "The name of an attribute at a measurement point, as read from JSON.".
-type point_attribute() :: bin_string().



-doc """
The value associated to a read JSON key, e.g. a key that is an attribute at a
measurement point.
""".
-type point_value() :: json_read_value().



-doc """
Table associating, to an attribute (e.g. `<<"temp1_crit">>`) of a measurement
point, a value (e.g. 105.0).
""".
-type point_attribute_map() ::
    map_hashtable( point_attribute(), point_value() ).



%-doc "An entry of a point_attribute_map/0.".
%-type point_attribute_entry() :: { point_attribute(), point_value() }.



-doc """
User specification of a sensor whose measurements may be muted.

For example `{nct6792, isa, "0a20"}`.
""".
-type user_muted_sensor_spec() ::
    { atom_sensor_type(), sensor_interface(), user_sensor_number() }.


-doc """
A measurement point specified in the configuration (e.g. "AUXTIN1").
""".
-type user_specified_point() :: ustring().



-doc """
User specification of measurement points to be muted for a given sensor (they
shall not be taken into account, typically because they are known to report
bogus values).

At least currently, this mostly applies to temperature sensors.
""".
-type user_muted_points() :: [ user_specified_point() ]
                           | 'all_points'. % of a given sensor


-doc """
A list expected to contain muted sensor measurement points, typically as read
from the sensor monitoring section of the US-Main configuration.

Expected to be specified in the form [user_muted_sensor_measurements()]:
```
UserMutedMeasurements = [
   { { nct6792, isa, "0a20" }, [ "AUXTIN1" ] },
   { { acpitz, acpi, "0" }, all_points } ].
```
""".
-type user_muted_sensor_points() :: list().


% Commented-out, as strangely considered as redefining the previous -doc
% attribute.
%
% -doc """
% User specification of sensor measurements to be muted.

% At least currently, this mostly applies to temperature sensors.

% Corresponds to the legit settings expected as
% class_USMainCentralServer:user_muted_sensor_points(); may be for example:
% ```
% [{nct6792, isa, "0a20"}, ["AUXTIN1"]},
%  {acpitz, acpi, "0"}, all_points}].
% ```
% """.
-type user_muted_sensor_measurements() ::
    % Previously was [{user_muted_sensor_spec(), user_muted_points()}], yet the
    % user may have specified a sensor spec more than once, so:
    table( user_muted_sensor_spec(), user_muted_points() ).



-doc """
Specification of measurement points (as binaries) to be muted for a given sensor
(they shall not be taken into account, typically because they are known to
report bogus values).

For example `[<<"AUXTIN1">>]` or all_points.

At least currently, this mostly applies to temperature sensors.
""".
-type muted_points() :: [ measurement_point_name() ]
                      | 'all_points'. % of a given sensor



-doc """
Specification of sensor measurements to be muted.

At least currently, this mostly applies to temperature sensors.

Corresponds to the legit settings expected as `user_muted_sensor_points/0`; may
be for example:
```
[{nct6792, isa, <<"0a20">>}, [<<"AUXTIN1">>]},
 {acpitz, acpi, <<"0">>}, all_points}]
```.
""".
-type muted_sensor_measurements() :: [ { sensor_id(), muted_points() } ].




% Section about temperature sensors.

% Read from a sensor on your CPU: Core temperature.
%
% Read from sensors on the motherboard:
% - CPUTIN (CPU temperature index): CPU temperature
% - AUXTIN (auxiliary temperature index): power supply temperature sensor
% - SYSTIN (system temperature index): motherboard temperature

% Settings: HYST (hysteresis; this is the value at which you want an alarm to
% turn off. For example, if your alarm temperature is 80°C, set your HYST value
% to stop the alarm when the temperature falls to 75°C.)


-doc """
The description of a temperature (e.g. "alarm").
""".
-type temperature_description() :: ustring().


-doc """
The description of a temperature range (e.g. "bogus").
""".
-type range_description() :: ustring().



% By decreasing temperature:
-doc """
The temperature-related alert state of a measurement point. Allows to report
issues once, rather than repeatedly.
""".
-type temp_alert_state() :: 'critical_high'
                          | 'alarm_high'
                          | 'nominal'
                          | 'alarm_low'
                          | 'critical_low'.


% Stores information about the temperature measured by a given measurement point
% of a sensor.
%
-record( temperature_data, {

    % The name of the attribute to read from the JSON table in order to
    % determine the current temperature of the associated point:
    %
    % (e.g. <<"temp9_input">>)
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
    % exceeded in order to change severity.
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



-doc """
Stores information about the temperature measured by a given measurement point
of a sensor.
""".
-type temperature_data() :: #temperature_data{}.




% Section regarding fan sensors.


-doc "The detected type of a given fan.".
-type fan_type() :: 'fixed_speed' % Fans are rotating at a fixed speed,
                                  % regardless of temperature
                  | 'pwm' % Pulse-Width modulation (controlled fan speed)
                  | 'unknown'.



-doc """
The (floating-point) number of pulses generated per revolution of the fan
(typically 2).
""".
-type fan_pulse() :: option( float() ).



-doc "The state condition of a given fan.".
-type fan_state() :: 'nominal'
                   | 'inactive' % If a fan is reported as non-operational
                                % (possibly not even existing)
                   | 'dysfunctional' % If a fan is reported as out of order
                   | 'insufficient_speed' % If a fan spins too slowly
                   | 'excessive_speed' % If a fan spins too fast
                   | 'unknown'.


-record( fan_data, {

    % The name of the attribute to read from the JSON table in order to
    % determine the current fan speed of the associated point:
    %
    % (e.g. <<"fan2_input">>)
    %
    input_attribute :: point_attribute(),

    % The most user-friendly description of the associated measurement point:
    description :: bin_string(),

    % The enable status of the associated measurement point:
    status = 'enabled' :: point_status(),

    % The determined fan type:
    type = 'unknown' :: fan_type(),

    % Tells how many of pulses are generated per revolution of the fan:
    pulses = 'undefined' :: option( fan_pulse() ),

    % Tells whether this fan was ever detected as moving:
    % (useless; see min_reached field)
    %
    %ever_spinned = 'false' :: boolean(),

    % The determined state of this fan:
    state = 'unknown' :: fan_state(),

    % The timestamp of the last time this fan was detected as spinning:
    last_spin_timestamp = undefined :: option( timestamp() ),

    % The current fan speed (if any):
    current :: option( rpm() ),

    % The lowest fan speed (if any) measured since start:
    min_reached :: option( rpm() ),

    % The highest fan speed (if any) measured since start:
    max_reached :: option( rpm() ),


    % To compute the average fan speed:

    % The sum of all (vetted) fan speeds measured since start:
    avg_sum = 0.0 :: option( rpm() ),

    % The number of all (vetted) fan measurements since start:
    avg_count = 0 :: option( count() ),


    % Tells, if defined, what is the speed threshold below which an alarm shall
    % be raised regarding that fan (e.g. a fixed-speed fan that would halt for
    % any reason).
    %
    alarm_low = undefined :: option( rpm() ),


    % Tells, if defined, what is the speed threshold above which an alarm shall
    % be raised regarding that fan (e.g. a PWM fan having to speed because of
    % excessive heat).
    %
    alarm_high = undefined :: option( rpm() ),


    % Tells whether the motherboard is to beep in case of (presumably) alarm:
    beep_on_alarm = false :: boolean() } ).


-doc """
Stores information about the fan measured by a given measurement point of a
sensor.
""".
-type fan_data() :: #fan_data{}.




% Section regarding intrusion sensors.


-doc """
A status regarding the detection of a chassis intrusion: false if nothing
detected, true if an intrusion apparently happened.
""".
-type intrusion_detected_status() :: boolean().


-record( intrusion_data, {

    % The name of the attribute to read from the JSON table in order to
    % determine the current intrusion status of the associated point:
    %
    % (e.g. <<"intrusion0">>)
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


-doc """
Stores information about any chassis intrusion detected by a given measurement
point of a sensor.
""".
-type intrusion_data() :: #intrusion_data{}.


% For a point P, intrusionP_alarm tells whether a chassis intrusion was detected
% (if equal to 1.0) or not (if equal to 0.0). Bogus values may be reported (we
% suppose that when this manager launches no intrusion already occurred), so
% only transitions from "normal" (an intrusion_detected_status of false) to
% "alarm" (an intrusion_detected_status of true) are scrutinised here.


-export_type([ user_muted_sensor_points/0,
               user_muted_sensor_measurements/0,
               raw_sensor_type/0, atom_sensor_type/0,
               sensor_category/0, sensor_interface/0, sensor_number/0,
               raw_sensor_id/0, sensor_id/0 ]).


-doc "The data that is associated to a measurement point.".
-type point_data() :: temperature_data() | intrusion_data().



-doc """
A table associating, for a given sensor, to each of its measurement points, its
data (e.g. regarding temperature, intrusion, etc.).
""".
-type points_data_table() :: table( measurement_point_name(), point_data() ).


% All information regarding a known sensor.
-record( sensor_info, {

    % Duplicated here for convenience (often is an associated key):
    raw_id :: raw_sensor_id(),

    % Identifier of this sensor.
    id :: sensor_id(),

    % Higher-level category of this sensor:
    category :: sensor_category(),

    % All data (of all sorts; and if any, as the category of this sensor must be
    % known in order to be interpreted) monitored by that sensor:
    %
    data :: option( points_data_table() ) } ).


-doc "All information regarding a known sensor.".
-type sensor_info() :: #sensor_info{}.



% Section for temperature defines.



% An initial temperature value below this threshold denotes a bogus measure,
% resulting in the corresponding measurement point to be disabled from start.
%
% We consider that, when this sensor manager starts, the local computer is
% running under norminal condition, and that thus any worryingly low or high
% reported temperature denotes a bogus sensor.
%
% So for example a sensor reporting 7°C at manager start-up is deemed bogus
% rather than being a real problem; afterwards this temperature means a problem
% (and not a bogus sensor).
%
-define( low_bogus_temperature_initial_threshold, 8.0 ).


% A temperature value below this threshold denotes a bogus measure, resulting in
% the corresponding measurement point to be disabled (at any time after
% initialisation):
%
-define( low_bogus_temperature_threshold, 5.0 ).



% A temperature value above this threshold denotes a bogus measure (e.g. 127°C),
% resulting in the corresponding measurement point to be disabled:
%
% We consider that, when this sensor manager starts, the local computer is
% running under norminal condition, and that thus any worryingly low or high
% reported temperature denotes a bogus sensor.
%
% So for example a sensor reporting 82°C at manager start-up is deemed bogus
% rather than being a real problem; afterwards this temperature means a problem
% (and not a bogus sensor).
%
-define( high_bogus_temperature_initial_threshold, 75.0 ).


% A temperature value above this threshold denotes a bogus measure (e.g. 127°C),
% resulting in the corresponding measurement point to be disabled (at any time
% after initialisation):
%
-define( high_bogus_temperature_threshold, 125.0 ).



% Should none be returned by a sensor (as celsius()):
-define( default_critical_high_temperature, 95.0 ).


% Should none be returned by a sensor (as celsius()):
-define( default_critical_low_temperature, 2.0 ).



% The class-specific attributes:
%
% (no more us_config_server_pid, scheduler_pid, comm_gateway_pid, etc. as, for
% an increased robustness, servers shall be resolved on the fly)
%
-define( class_attributes, [

    { sensor_exec_pair, system_utils:execution_pair(),
      "to run the sensor tool conveniently" },

    { sensor_monitoring, boolean(),
      "tells whether sensor monitoring is enabled" },

    { parser_state, parser_state(), "state of the JSON parser in use" },

    { sensor_table, sensor_table(),
      "table registering all information regarding detected local sensors" },

    { task_id, task_id(),
      "the scheduling identifier of the sensor-polling task" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.HostSensors" ).


% Exported helpers:
-export([ get_licit_config_keys/0, manage_configuration/2,
          intrusion_status_to_string/1 ]).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").


% User periodicity, hence in seconds:

% Nominal one:
-define( default_sensor_poll_periodicity, 30 ).

% For testing:
%-define( default_sensor_poll_periodicity, 5 ).


% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type indentation_level() :: indentation_level().

-type file_path() :: file_utils:file_path().

-type string_json() :: json_utils:string_json().
-type decoded_json() :: json_utils:decoded_json().
-type parser_state() :: json_utils:parser_state().

-type celsius() :: unit_utils:celsius().
-type rpm() :: unit_utils:rpm().

-type timestamp() :: time_utils:timestamp().

-type map_hashtable( K, V ) :: map_hashtable:map_hashtable( K, V ).

% In state definition:
%-type scheduler_pid() :: class_USScheduler:scheduler_pid().
%-type task_id() :: class_USScheduler:task_id().

-type us_main_config_table() ::
    class_USMainCentralServer:us_main_config_table().




% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this sensor manager with an OTP supervision
% tree.


-doc """
Starts and links a supervision bridge for the sensor management.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see `us_main_sup:init/1`), hence generally triggered by the application
initialisation.
""".
-spec start_link() -> term().
start_link() ->

    % Apparently not displayed in a release context, yet executed:
    trace_bridge:debug( "Starting the US-Main supervisor bridge for "
                        "the sensor management." ),

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
                           "the sensor management.", [ self() ] ),

    % Not specifically synchronous:
    SensorManagerPid = ?MODULE:new_link(),

    { ok, SensorManagerPid, _InitialBridgeState=SensorManagerPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=SensorManagerPid )
                                when is_pid( SensorManagerPid ) ->

    trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
        "the sensor management (reason: ~w, sensor manager: ~w).",
        [ Reason, SensorManagerPid ] ),

    % Synchronicity needed, otherwise a potential race condition exists, leading
    % this process to be killed by its OTP supervisor instead of being normally
    % stopped:
    %
    wooper:delete_synchronously_instance( SensorManagerPid ),

    trace_bridge:debug_fmt( "US-Main sensor manager ~w terminated.",
                           [ SensorManagerPid ] ).




% Actual implementation of the sensor manager.


-doc "Constructs a sensor manager, for the local host.".
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

            PollPeriod = ?default_sensor_poll_periodicity,

            ?send_info_fmt( InitState, "Performing a first, direct, relevant "
                "sensor update before scheduling it periodically "
                "(every ~B seconds).", [ PollPeriod ] ),

            UpdatedSensorState = update_sensor_data( InitState ),

            init_polling( PollPeriod, UpdatedSensorState );

        { _SensorEnabled=false, InitState } ->
            ?send_notice( InitState,
                          "No sensor enabled, no polling scheduled." ),
            setAttribute( InitState, task_id, undefined )

    end,

    ?send_notice_fmt( InitSensorState, "Constructed: ~ts",
                      [ to_string( InitSensorState ) ] ),

    InitSensorState.



-doc """
Constructs a minimal sensor manager in order to test whether the sensor JSON
output stored in the specified file can be propertly interpreted.

Such a file is typically obtained thanks to `$ sensors --no-adapter -j >
my_sensor_output.txt`.

Useful to support sensors from third-party computers (transmitting such a file
just suffice to update this module accordingly). Applies only for testing.
""".
-spec construct( wooper:state(), file_path() ) -> wooper:state().
construct( State, SensorOutputFilePath ) ->

    ServerTraceName = text_utils:format(
        "Sensor manager for data from file ~ts", [ SensorOutputFilePath ] ),

    % First the direct mother classes, then this class-specific actions:
    SrvState = class_USServer:construct( State,
        ?trace_categorize(ServerTraceName),
        ?us_main_sensor_server_registration_name,
        ?us_main_sensor_server_registration_scope ),

    % Internal state of the JSON parser (*not* a WOOPER state):
    JSONParserState = initialise_json_support( SrvState ),

    % Mostly bogus manager having some bogus (undefined) attributes:
    % (no, state management is fine)
    InitState = setAttributes( SrvState, [
        { sensor_monitoring, false },
        { sensor_exec_pair, get_sensor_execution_pair( SrvState ) },
        { parser_state, JSONParserState },
        { task_id, undefined } ] ),

    ReadState =
        parse_sensor_output_from_file( SensorOutputFilePath, InitState ),

    ?send_notice_fmt( ReadState, "After the initial reading of file '~ts': ~ts",
                      [ SensorOutputFilePath, to_string( ReadState ) ] ),

    % Not wanted, as we would read updated data not from file but from local
    % host instead, and thus detect unexpected sensors:
    %
    %UpdatedSensorState = update_sensor_data( ReadState ),
    UpdatedSensorState = ReadState,

    % Similar trace already sent:
    %?send_notice( UpdatedSensorState,
    %              "Constructed: " ++ to_string( UpdatedSensorState ) ),

    UpdatedSensorState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    ?debug_fmt( "Deletion initiated, while state is: ~ts.",
                [ to_string( State ) ] ),

    ?info( "Deleted." ),
    State.



% Method section.


-doc """
Reads the local sensors.

Typically triggered periodically by the scheduler.
""".
-spec readSensors( wooper:state() ) -> oneway_return().
readSensors( State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
                           ?notice( "Reading sensors now." ) ),

    ReadState = update_sensor_data( State ),

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "New sensor state after update: ~ts",
                    [ to_string( State ) ] ) ),

    wooper:return_state( ReadState ).



-doc """
Callback triggered, if this server enabled the trapping of EXIT messages,
whenever a linked process terminates.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
        basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, _StoppedPid, _ExitType=normal ) ->
    % Useless to trace, triggered when executing 'sensors' for example:
    %?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
    wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

    % Typically: "Received exit message '{{nocatch,
    %   {wooper_oneway_failed,<0.44.0>,class_XXX,
    %      FunName,Arity,Args,AtomCause}}, [...]}"

    % Redundant information yet useful for console outputs:
    ?warning_fmt( "US Sensor Manager ~w received and ignored following exit "
                  "message from ~w:~n  ~p", [ self(), CrashPid, ExitType ] ),

    wooper:const_return().




% Static subsection.


-doc """
Returns the PID of the current, supposedly already-launched, sensor manager,
waiting (up to a few seconds, as all US servers are bound to be launched mostly
simultaneously) if needed.

It is better to obtain the PID of a server each time from the naming service
rather than to resolve and store its PID once for all, as, for an increased
robustness, servers may be restarted (hence any stored PID may not reference a
live process anymore).
""".
-spec get_server_pid () -> static_return( sensor_manager_pid() ).
get_server_pid() ->

    ManagerPid = class_USServer:resolve_server_pid(
        _RegName=?us_main_sensor_server_registration_name,
        _RegScope=?us_main_sensor_server_registration_scope ),

    wooper:return_static( ManagerPid ).




% Helper section.


-doc "Initialises the sensor management.".
-spec init_sensors( wooper:state() ) ->
                        { SensorEnabled :: boolean(), wooper:state() }.
init_sensors( State ) ->

    CheckedParserState = initialise_json_support( State ),

    ReadyState = setAttributes( State, [
        { sensor_exec_pair, get_sensor_execution_pair( State ) },
        { parser_state, CheckedParserState } ] ),

    initialise_sensor_data( ReadyState ).



-doc "Returns how `sensors` shall be run.".
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

    % Removing error messages such as: "ERROR: Can't get value of subfeature
    % tempx_input: Can't read":
    %
    SensorArgs = [ <<"--no-adapter">>, _JSONOutput= <<"-j">> ],

    ExecPair = { text_utils:string_to_binary( SensorExecPath ), SensorArgs },

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "Exec pair for sensor reading:~n ~p.", [ ExecPair ] ) ),

    ExecPair.



-doc "Returns a proper initial state of the JSON parser.".
-spec initialise_json_support( wooper:state() ) -> parser_state().
initialise_json_support( State ) ->

    % We will interpret the JSON outputs of sensors, so:
    json_utils:is_parser_available() orelse
        begin

            ?error( "No JSON parser found available. "
                        ++ system_utils:get_json_unavailability_hint() ),

            throw( no_json_backend )

        end,

    ParserState = json_utils:start_parser(),

    BackendName = json_utils:get_parser_backend_name( ParserState ),

    CheckedParserState = json_utils:check_parser_operational( ParserState ),

    ?info_fmt( "JSON parser successfully started (based on the '~ts' backend), "
               "and tested.", [ BackendName ] ),

    CheckedParserState.



-doc """
Initialises and integrates the first data (e.g. temperatures and fan speeds)
read from sensors.
""".
-spec initialise_sensor_data( wooper:state() ) ->
                                { SensorEnabled :: boolean(), wooper:state() }.
initialise_sensor_data( State ) ->

    case fetch_sensor_data( State ) of

        { ok, CmdOutput } ->
            { true, parse_initial_sensor_output( CmdOutput, State ) };

        % If initialisation detects that no sensor is available, this is not a
        % showstopper anymore, as in some contexts (e.g. continuous integration)
        % this might happen and should not crash such a manager:
        %
        { error, ErrorOutput } ->
            ?warning_fmt( "Not able to initialise sensor data ('~ts'), "
                          "not activating their monitoring.", [ ErrorOutput ] ),
            { false, setAttribute( State, sensor_monitoring, false ) }

    end.



-doc """
Returns the output of the corresponding sensor command (may crash on systematic,
non-recoverable errors).
""".
-spec fetch_sensor_data( wooper:state() ) -> fallible( ustring() ).
fetch_sensor_data( State ) ->

    { ExecPath, ExecArgs } = ?getAttr(sensor_exec_pair),

    Environment = system_utils:get_standard_environment(),

    % Not our default options, as we do not want that error output such as
    % "ERROR: Can't get value of subfeature tempx_input: Can't read" are mixed
    % up with normal outputs; so no stderr_to_stdout:
    %
    PortOptions = [ stream, exit_status, use_stdio, eof ],

    case system_utils:run_executable( ExecPath, ExecArgs, Environment,
            _MaybeWorkingDir=undefined, PortOptions ) of

        { _ReturnCode=0, CmdOutput } ->

            cond_utils:if_defined( us_main_debug_execution,
                ?debug_fmt(
                    "Raw (JSON) command output read from sensors: '~ts'.",
                    [ CmdOutput ] ) ),

            { ok, CmdOutput };

        % Message typically starts with 'No sensors found!...":
        { _ReturnCode=1, ErrorOutput } ->
            ?error_fmt( "Error when running '~ts' with arguments ~p: '~ts' "
                "(exit code: 1); interpreting that as no sensor being found.",
                [ ExecPath, ExecArgs, ErrorOutput ] ),

            % Do not crash in this case anymore:
            %throw( { sensor_read_failed, ErrorOutput } )
            { error, ErrorOutput };

        { ReturnCode, ErrorOutput  } ->
            ?emergency_fmt( "Error when running '~ts' with arguments ~p: '~ts' "
                "(exit code: ~B).",
                [ ExecPath, ExecArgs, ErrorOutput, ReturnCode ] ),

            throw( { sensor_read_failed, ErrorOutput } )

    end.



-doc """
Parses the specified sensor JSON output, returns a state with a corresponding
initial sensor table.
""".
-spec parse_initial_sensor_output( string_json(), wooper:state() ) ->
                                        wooper:state().
parse_initial_sensor_output( SensorJsonStr, State ) ->

    DecodedMap = decode_sensor_json( SensorJsonStr, State ),

    % At about the last possible moment:
    USMainCtrSrvPid = class_USMainCentralServer:get_server_pid(),

    % Blocking; beware of not creating deadlocks that way:
    USMainCtrSrvPid ! { getSensorSettings, [], self() },

    ReadMutedMeasurements = receive

        % Already checked by the US-Main configuration server to be a list:
        { wooper_result, MutMeasurements } when is_list( MutMeasurements ) ->
            MutMeasurements

    end,

    MutedMeasurementTable =
        vet_muted_sensor_points( ReadMutedMeasurements, State ),

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "Muted measurements:~n ~p.", [ ReadMutedMeasurements ] ) ),

    parse_initial_sensors( map_hashtable:enumerate( DecodedMap ),
        _SensorTable=table:new(), MutedMeasurementTable, State ).



-doc "Vets the user-configured measurements to be muted.".
-spec vet_muted_sensor_points( user_muted_sensor_points(),
                               wooper:state() ) -> muted_sensor_measurements().
vet_muted_sensor_points( ReadMutedMeasurements, State ) ->
    vet_muted_sensor_points( ReadMutedMeasurements, _AccTable=table:new(),
                             State ).

vet_muted_sensor_points( _ReadMutedMeasurements=[], AccTable, _State ) ->
    % No order matters:
    AccTable;

vet_muted_sensor_points( _ReadMutedMeasurements=[
        { ReadSensorId={ SensorType, SensorInterface, SensorNumber },
          UserPoints } | T ] , AccTable, State ) ->

    is_atom( SensorType ) orelse
        begin

            ?error_fmt( "Invalid sensor type ('~p' - not an atom) "
                "in muted measurement sensor identifier ~p.",
                [ SensorType, ReadSensorId ] ),

            throw( { invalid_muted_sensor_type, SensorType } )

        end,

    is_atom( SensorInterface ) orelse
        begin
            ?error_fmt( "Invalid sensor interface ('~p' - not an atom) "
                "in muted measurement sensor identifier ~p.",
                [ SensorInterface, ReadSensorId ] ),

            throw( { invalid_muted_sensor_interface, SensorInterface } )
        end,

    BinSensorNumber = case text_utils:is_string( SensorNumber ) of

        true ->
            text_utils:string_to_binary( SensorNumber );

        _False ->
            ?error_fmt( "Invalid sensor number ('~p' - not a string) "
                "in muted measurement sensor identifier ~p.",
                [ SensorNumber, ReadSensorId ] ),

            throw( { invalid_muted_sensor_number, SensorNumber } )

    end,

    Points = vet_user_points( UserPoints, State ),

    SensorId = { SensorType, SensorInterface, BinSensorNumber },

    NewAccTable = table:append_list_to_entry( _K=SensorId, _Elems=Points,
                                              AccTable ),

    vet_muted_sensor_points( T, NewAccTable, State );


vet_muted_sensor_points( _ReadMutedMeasurements=[ Other | _T ] , _AccTable,
                         State ) ->

    ?error_fmt( "Invalid user-specified sensor measurements to be muted "
                "('~p' - not a triplet).", [ Other ] ),

    throw( { invalid_muted_sensor_measurements, Other } ).



-doc "Vets the user-specified measurement points to be muted.".
-spec vet_user_points( any(), wooper:state() ) -> muted_points().
vet_user_points( _UserPoints=all_points, _State ) ->
    all_points;

vet_user_points( UserPoints, _State ) when is_list( UserPoints ) ->
    [ text_utils:string_to_binary( UP ) || UP <- UserPoints ];

vet_user_points( Other, State ) ->

    ?error_fmt( "Invalid user-specified sensor measurement points to be muted "
                "('~p').", [ Other ] ),

    throw( { invalid_muted_sensor_measurement_points, Other } ).



-doc "Parses in turn the specified initial sensor JSON entries.".
parse_initial_sensors( _SensorPairs=[], SensorTable, MutedMeasurementTable,
                       State ) ->

    case table:is_empty( MutedMeasurementTable ) of

        true ->
            setAttributes( State, [ { sensor_table, SensorTable },
                                    { sensor_monitoring, true } ] );

        false ->
            MutedEntries = table:enumerate( MutedMeasurementTable ),

            ?error_fmt( "Not all muted sensor points were found during "
                "the initial sensor look-up; remaining:~n ~p.",
                [ MutedEntries ] ),

            throw( { unknown_muted_sensor_points, MutedEntries } )

    end;

parse_initial_sensors( _SensorPairs=[ { RawSensorIdBinStr, SensorJSON } | T ],
                       SensorTable, MutedMeasurementTable, State ) ->

    % Having for example "nct6779-isa-0a00":
    RawSensorIdStr = text_utils:binary_to_string( RawSensorIdBinStr ),

    case text_utils:split( RawSensorIdStr, _Delimiter=$- ) of

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

            % Get any muted points (e.g. ["AUXTIN1"] or 'all_points') for this
            % sensor:
            %
            { MutedPoints, ShrunkMutedMeasTable } =
                table:extract_entry_with_default( _K=SensorId,
                    _DefaultValue=[], MutedMeasurementTable ),

            MaybeSensorData = parse_initial_sensor_data( SensorJSON,
                SensorCategory, SensorId, MutedPoints, State ),

            SensorInfo = #sensor_info{ raw_id=RawSensorIdBinStr,
                                       id=SensorId,
                                       category=SensorCategory,
                                       data=MaybeSensorData },

            ?debug_fmt( "For sensor ~ts, we have following information:~n  ~p",
                        [ RawSensorIdBinStr, SensorInfo ] ),

            % Ensures uniqueness of keys:
            NewSensorTable = table:add_new_entry( RawSensorIdBinStr, SensorInfo,
                                                  SensorTable ),

            parse_initial_sensors( T, NewSensorTable, ShrunkMutedMeasTable,
                                   State );


        _Other ->
            ?error_fmt( "Unable to interpret raw sensor identifier '~ts', "
                "ignoring this sensor as a whole from now.",
                [ RawSensorIdStr ] ),

            parse_initial_sensors( T, SensorTable, MutedMeasurementTable,
                                   State  )

    end.



-doc """
Returns a pair to categorise specified sensor, deriving from its raw type.
""".
-spec categorise_sensor( raw_sensor_type() ) ->
                                { atom_sensor_type(), sensor_category() }.
% Internal temperature sensor of Intel Family:
categorise_sensor( _RawSensorType="coretemp" ) ->
    { coretemp, cpu };

% Internal temperature sensor of AMD Family 10h/11h/12h/14h/15h/16h processors:
categorise_sensor( _RawSensorType="k10temp" ) ->
    { coretemp, cpu };

categorise_sensor( _RawSensorType="acpitz" ) ->
    { acpitz, cpu_socket };

categorise_sensor( _RawSensorType="nvme" ) ->
    { nvme, disk };

categorise_sensor( RawSensorType="nct" ++ _ ) ->
    { text_utils:string_to_atom( RawSensorType ), motherboard };

% For example "thinkpad-isa-0000"
categorise_sensor( RawSensorType="thinkpad" ++ _ ) ->
    { text_utils:string_to_atom( RawSensorType ), motherboard };

% For example "pch_skylake-virtual-0"
categorise_sensor( RawSensorType="pch_" ++ _ ) ->
    { text_utils:string_to_atom( RawSensorType ), chipset };

categorise_sensor( RawSensorType="BAT" ++ _Number ) ->
    { text_utils:string_to_atom( RawSensorType ), battery };

% Probably an APU:
categorise_sensor( _RawSensorType="radeon" ) ->
    % No real gpu-specific information found in JSON yet:
    { radeon, cpu };

% For example "iwlwifi_1-virtual-0"
categorise_sensor( RawSensorType="iwlwifi" ++ _ ) ->
    { text_utils:string_to_atom( RawSensorType ), network };

% For example "ucsi_source_psy_USBC000:001-isa-0000" (for USB-C)
categorise_sensor( RawSensorType ) ->
    { text_utils:string_to_atom( RawSensorType ), bus }.



-doc "Returns the sensor interface corresponding to the specified string.".
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



-doc """
Parses the specified JSON information regarding specified sensor, in order to
detect initially the various measurement points that it supports, and returns
the corresponding data table.
""".
-spec parse_initial_sensor_data( decoded_json(), sensor_category(), sensor_id(),
            muted_points(), wooper:state() ) -> points_data_table().
parse_initial_sensor_data( SensorJSON, _SensorCateg=cpu_socket, SensorId,
                           MutedPoints, State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "JSON to parse for ~ts for cpu_socket "
            "(with muted points ~p): ~n ~p",
            [ sensor_id_to_string( SensorId ), MutedPoints, SensorJSON ] ) ),

    { TempJSONTriples, OtherJSONTriples } =
        filter_cpu_socket_json( SensorJSON ),

    %debug_fmt( "For cpu_socket: TempJSONTriples: ~p~nOtherJSONTriples: ~p",
    %           [ TempJSONTriples, OtherJSONTriples ] ),

    OtherJSONTriples =:= [] orelse
        ?warning_fmt( "Following ~B CPU-socket measurement points "
            "could not be categorised for '~ts': ~ts.",
            [ length( OtherJSONTriples ), sensor_id_to_string( SensorId ),
              text_utils:strings_to_string( [ json_triple_to_string( JT )
                            || JT <- OtherJSONTriples ] ) ] ),

    register_temperature_points( TempJSONTriples, _EmptyDataTable=table:new(),
                                 SensorId, MutedPoints, State );


% Mostly the same as cpu_socket:
parse_initial_sensor_data( SensorJSON, _SensorCateg=cpu, SensorId, MutedPoints,
                           State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "JSON to parse for ~ts for cpu:~n ~p",
                    [ sensor_id_to_string( SensorId ), SensorJSON ] ) ),

    { TempJSONTriples, OtherJSONTriples } = filter_cpu_json( SensorJSON ),

    %?debug_fmt( "For cpu: TempJSONTriples: ~p~nOtherJSONTriples: ~p",
    %            [ TempJSONTriples, OtherJSONTriples ] ),

    OtherJSONTriples =:= [] orelse
        ?warning_fmt( "Following ~B CPU measurements points "
            "could not be categorised for '~ts': ~ts.",
            [ length( OtherJSONTriples ), sensor_id_to_string( SensorId ),
              text_utils:strings_to_string(
                [ json_triple_to_string( JT )
                            || JT <- OtherJSONTriples ] ) ] ),

    register_temperature_points( TempJSONTriples, _EmptyDataTable=table:new(),
                                 SensorId, MutedPoints, State );


parse_initial_sensor_data( SensorJSON, _SensorCateg=motherboard, SensorId,
                           MutedPoints, State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "JSON to parse for ~ts for motherboard:~n ~p"
            "~n(whereas muted points are ~p).",
            [ sensor_id_to_string( SensorId ), SensorJSON, MutedPoints ] ) ),

    { TempJSONTriples, FanJSONTriples, IntrusionJSONTriples,
      OtherJSONTriples } = filter_motherboard_json( SensorJSON ),

    %?debug_fmt( "TempJSONTriples: ~p~nFanJSONTriples: ~p~n"
    %   "IntrusionJSONTriples: ~p~nOtherJSONTriples: ~p",
    %   [ TempJSONTriples, FanJSONTriples, IntrusionJSONTriples,
    %     OtherJSONTriples ] ),

    OtherJSONTriples =:= [] orelse
        ?warning_fmt( "Following ~B motherboard measurements points "
            "could not be categorised for '~ts': ~ts.",
            [ length( OtherJSONTriples ), sensor_id_to_string( SensorId ),
              text_utils:strings_to_string(
                [ json_triple_to_string( JT )
                            || JT <- OtherJSONTriples ] ) ] ),

    TempDataTable = register_temperature_points( TempJSONTriples,
        _EmptyDataTable=table:new(), SensorId, MutedPoints, State ),

    FanDataTable = register_fan_points( FanJSONTriples, TempDataTable,
                                        SensorId, MutedPoints, State ),

    register_intrusion_points( IntrusionJSONTriples, FanDataTable, SensorId,
                               MutedPoints, State );


parse_initial_sensor_data( SensorJSON, _SensorCateg=disk, SensorId, MutedPoints,
                           State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "JSON to parse for ~ts for disk:~n ~p",
                    [ sensor_id_to_string( SensorId ), SensorJSON ] ) ),

    { TempJSONTriples, OtherJSONTriples } = filter_disk_json( SensorJSON ),

    %?debug_fmt( "TempJSONTriples: ~p~nOtherJSONTriples: ~p",
    %            [ TempJSONTriples, OtherJSONTriples ] ),

    OtherJSONTriples =:= [] orelse
        ?warning_fmt( "Following ~B disk measurements points "
            "could not be categorised for '~ts': ~ts.",
            [ length( OtherJSONTriples ), sensor_id_to_string( SensorId ),
              text_utils:strings_to_string(
                [ json_triple_to_string( JT )
                            || JT <- OtherJSONTriples ] ) ] ),

    register_temperature_points( TempJSONTriples, _EmptyDataTable=table:new(),
                                 SensorId, MutedPoints, State );

parse_initial_sensor_data( SensorJSON, _SensorCateg=chipset, SensorId,
                           MutedPoints, State ) ->

    % Autonomous chipset.
    % For example "pch_skylake-virtual-0":{
    %  "temp1":{ "temp1_input": 30.000
    %  }
    %},

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "JSON to parse for ~ts for chipset:~n ~p",
                    [ sensor_id_to_string( SensorId ), SensorJSON ] ) ),

    { TempJSONTriples, OtherJSONTriples } = filter_chipset_json( SensorJSON ),

    %?debug_fmt( "TempJSONTriples: ~p~nOtherJSONTriples: ~p",
    %            [ TempJSONTriples, OtherJSONTriples ] ),

    OtherJSONTriples =:= [] orelse
        ?warning_fmt( "Following ~B chipset measurements points "
            "could not be categorised for '~ts': ~ts.",
            [ length( OtherJSONTriples ), sensor_id_to_string( SensorId ),
              text_utils:strings_to_string(
                [ json_triple_to_string( JT )
                            || JT <- OtherJSONTriples ] ) ] ),

    register_temperature_points( TempJSONTriples, _EmptyDataTable=table:new(),
                                 SensorId, MutedPoints, State );


parse_initial_sensor_data( SensorJSON, _SensorCateg=battery, SensorId,
                           _MutedPoints, State ) ->

    % Batteries are boring.
    % For example <<"BAT0-acpi-0">>:
    %  #{<<"curr1">> => #{<<"curr1_input">> => 3.476},
    %                          <<"in0">> => #{<<"in0_input">> => 12.417}}

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "No relevant JSON expected to parse for ~ts "
            "for battery:~n ~p",
            [ sensor_id_to_string( SensorId ), SensorJSON ] ),
        basic_utils:ignore_unused( [ SensorJSON, SensorId, State ] ) ),

    undefined;

parse_initial_sensor_data( SensorJSON, _SensorCateg=network, SensorId,
                           _MutedPoints, State ) ->

    % Network interfaces are boring.
    % For example <<"iwlwifi_1-virtual-0">>: #{<<"temp1">> => #{}}

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "No relevant JSON expected to parse for ~ts for network "
            "interface:~n ~p",
            [ sensor_id_to_string( SensorId ), SensorJSON ] ),
        basic_utils:ignore_unused( [ SensorJSON, SensorId, State ] ) ),

    undefined;

parse_initial_sensor_data( SensorJSON, _SensorCateg=bus, SensorId,
                           _MutedPoints, State ) ->

    % Buses are boring.
    % For example "ucsi_source_psy_USBC000:001-isa-0000":{
    %  "in0":{
    %     "in0_input": 0.000,
    %     "in0_min": 0.000,
    %     "in0_max": 0.000
    %  },
    %  "curr1":{
    %     "curr1_input": 0.000,
    %     "curr1_max": 0.000
    %  }
    %}


    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "No relevant JSON expected to parse for ~ts for bus :~n ~p",
                    [ sensor_id_to_string( SensorId ), SensorJSON ] ),
        basic_utils:ignore_unused( [ SensorJSON, SensorId, State ] ) ),

    undefined;

parse_initial_sensor_data( SensorJSON, SensorCateg, SensorId,
                           _MutedPoints, State ) ->

    ?error_fmt( "Ignoring JSON for ~ts of unsupported category ~ts:~n ~p",
        [ sensor_id_to_string( SensorId ), SensorCateg, SensorJSON ] ),

    undefined.



% Temperature section.


-doc """
Registers the specified temperature points in the specified data table, for the
initialisation of the specified sensor, from the specified JSON content.

Only point triplets that are already filtered for temperature are expected, thus
no need to accumulate unexpected points.
""".
-spec register_temperature_points( [ json_triple() ], points_data_table(),
        sensor_id(), muted_points(), wooper:state() ) -> points_data_table().
register_temperature_points( _PointTriples, DataTable, _SensorId,
                             _MutedPoints=all_points, _State ) ->
    DataTable;

% Normal end of recursion:
register_temperature_points( _PointTriples=[], DataTable, _SensorId,
                             _MutedPoints=[], _State ) ->
    DataTable;

% Remaining muted points:
register_temperature_points( _PointTriples=[], DataTable, SensorId,
                             MutedPoints, State ) ->

    % This is not an error at all, as non-temperature points (e.g. intrusion
    % ones) may have to be muted as well:
    %
    ?debug_fmt( "For ~ts, following ~B muted measurements were not retained "
        "in terms of temperature: ~ts.",
        [ sensor_id_to_string( SensorId ), length( MutedPoints ),
          text_utils:binaries_to_listed_string( MutedPoints ) ] ),

    DataTable;


% For example BinPointName = <<"temp1">>, BinDesc=<<"Some description">> and
% PointValueMap = #{<<"temp1_crit">> => 105.0, <<"temp1_input">> => 27.8}.
%
register_temperature_points(
        _PointTriples=[ { BinPointName, BinDesc, PointValueMap } | T ],
        DataTable, SensorId, MutedPoints, State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "Considering the registration of temperature point "
            "'~ts' (~ts) with:~n ~p~n(muted points: ~p)",
            [ BinPointName, BinDesc, PointValueMap, MutedPoints ] ) ),

    % The case MutedPoints=all_points has already been intercepted:
    { NewDataTable, NewMutedPoints } =
            case list_utils:extract_element_if_existing( BinPointName,
                                                         MutedPoints ) of

        % Hence not muted:
        false ->
            InitPointData = create_temperature_data( PointValueMap,
                BinPointName, BinDesc, SensorId, State ),

            % Uniqueness checked:
            { table:add_new_entry( BinPointName, InitPointData, DataTable ),
              MutedPoints };

        ShrunkMutedPoints ->
            ?info_fmt( "Point '~ts' of sensor ~ts belongs to the muted "
                "ones, hence is excluded.",
                [ BinPointName, sensor_id_to_string( SensorId ) ] ),
            { DataTable, ShrunkMutedPoints }

    end,

    % First update done globally directly from the constructor.

    register_temperature_points( T, NewDataTable, SensorId, NewMutedPoints,
                                 State ).



-doc """
Returns a new temperature data initialised from the specified JSON content for
the specified measurement point, ready to be updated with the future next
current temperatures (knowing it is not muted, as now muted points are filtered
upfront).
""".
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

    ?error_fmt( "For temperature measurement point '~ts' of ~ts, no valid "
        "current temperature was reported; disabling this point.",
        [ BinPointName, sensor_id_to_string( SensorId ) ] ),

    TempData#temperature_data{ status=disabled };


% End of recursion, here with an expected, defined, current temperature reading:
init_temp_point( _TempEntries=[], _BinPointName,
        TempData=#temperature_data{ current=CurrentTemp,
                                    crit_low=MaybeCritLowTemp,
                                    alarm_low=MaybeMinTemp,
                                    crit_high=MaybeCritHighTemp,
                                    alarm_high=MaybeMaxTemp,
                                    min_reached=MaybeReachedMin,
                                    max_reached=MaybeReachedMax },
        _SensorId, _State ) ->

    type_utils:check_float( CurrentTemp ),

    % First, tackle low temperatures (not really of interest generally):
    CritLowTemp = case MaybeCritLowTemp of

        undefined ->

            case MaybeReachedMin of

                undefined ->
                    ?default_critical_low_temperature;

                ReportedMin ->
                    % Hazardous with negative temperatures:
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
    ReadyTempData = TempData#temperature_data{
        crit_low=CritLowTemp,
        alarm_low=AlarmLowTemp,
        crit_high=CritHighTemp,
        alarm_high=AlarmHighTemp,
        % End of field abuse for min/max:
        min_reached=CurrentTemp,
        max_reached=CurrentTemp,
        % Allows to start with actual info:
        avg_sum=CurrentTemp,
        avg_count=1 },

    %?debug_fmt( "Adding initial temperature point '~ts' of ~ts:~n  ~ts",
    %   [ BinPointName, sensor_id_to_string( SensorId ),
    %     point_data_to_string( ReadyTempData ) ] ),
    %
    % We used to perform the first usual reading here; it is now done in the
    % final part of the constructor.

    check_temperature_data( ReadyTempData );


% For example AttrNameBin = <<"temp1_input">>, AttrValue = 44.85:
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
    %   no_prefix ->
    %       ?error_fmt( "Attribute '~ts' not prefixed with the name of "
    %           "the current measurement point '~ts', thus ignored.",
    %           [ AttrName, BinPointName ] ),
    %       init_temp_point( T, BinPointName, TempData, SensorId, PointValueMap,
    %                        State );

    % Used more than once:
    Separator = $_,

    case text_utils:split( AttrName, Separator ) of

        [ _WithoutSepElem ] ->

            ?error_fmt( "For temperature measurement point '~ts' of ~ts, "
                "attribute '~ts' cannot be stripped of a relevant prefix; "
                "ignoring it.",
                [ BinPointName, sensor_id_to_string( SensorId ), AttrName ] ),

            init_temp_point( T, BinPointName, TempData, SensorId, State );

        % Just remove the prefix:
        [ _AnyPrefix | OtherElems ] ->
            NewTempData = case text_utils:join( Separator, OtherElems ) of

                "input" ->

                    % Just record the full attribute name once for all:
                    %
                    % (value considered afterwards, as all post-init updates)
                    %
                    case vet_initial_temperature( AttrValue, AttrName,
                            BinPointName, SensorId, State ) of

                        true ->
                            % To be taken into account once all other attributes
                            % (especially the static ones) have been processed:
                            %
                            TempData#temperature_data{
                                input_attribute=AttrNameBin,
                                current=AttrValue };

                        _False ->

                            % Happens very often and duplicates a more general
                            % trace:
                            %
                            cond_utils:if_defined( us_main_debug_sensors,
                                ?error_fmt( "Invalid value associated to "
                                    "temperature attribute '~ts' (got '~p'); "
                                    "skipping that information.",
                                    [ AttrName, AttrValue ] ) ),

                            TempData#temperature_data{
                                input_attribute=AttrNameBin }

                    end;


                Suffix="crit" ->
                    init_for_crit( AttrValue, TempData, Suffix, BinPointName,
                                   SensorId, State );


                % Interpreted as a synonym of "crit":
                Suffix="crit_alarm" ->
                    init_for_crit( AttrValue, TempData, Suffix, BinPointName,
                                   SensorId, State );


                % Unsurprisingly, nothing like crit/crit_alarm for *low*
                % temperatures.

                % Just during this initialisation, we (ab)use the 'min_reached'
                % field in order to record the minimum allowed temperature as
                % reported by the chip (not corresponding to the "real"
                % min_reached, which is the lowest temperature *actually*
                % measured):
                %
                Suffix="min" ->
                    case vet_initial_temperature( AttrValue, Suffix,
                            BinPointName, SensorId, State ) of

                        false ->
                            % Just ignored then:
                            TempData;

                        _True ->
                            TempData#temperature_data{ min_reached=AttrValue }

                    end;

                % Same as for "min" just above:
                Suffix="max" ->
                    case vet_initial_temperature( AttrValue, Suffix,
                            BinPointName, SensorId, State ) of

                        false ->
                            % Just ignored then:
                            TempData;

                        _True ->
                            TempData#temperature_data{ max_reached=AttrValue }

                    end;

                Suffix="alarm" ->
                    % First ensure that this alarm has not a bogus value:
                    case vet_initial_temperature( AttrValue, Suffix,
                            BinPointName, SensorId, State ) of

                        false ->
                            % Just ignored then:
                            TempData;

                        _True ->
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

                            TempData#temperature_data{ alarm_high=NewHigh }

                    end;


                Suffix when Suffix == "beep" orelse Suffix == "type"
                        orelse Suffix == "offset" orelse Suffix == "max_hyst"
                        orelse Suffix == "crit_hyst" ->
                    %?debug_fmt( "Attribute '~ts' belongs to the ignored ones.",
                    %            [ Suffix ] ),
                    TempData;


                _OtherSuffix ->

                    ?warning_fmt( "Unknown suffix for attribute '~ts' of "
                        "temperature measurement point ~ts of ~ts; "
                        "ignoring it.",
                        [ AttrName, BinPointName,
                          sensor_id_to_string( SensorId ) ] ),

                    TempData

        end,

        init_temp_point( T, BinPointName, NewTempData, SensorId, State )

    end.



-doc """
Initialises the specified measurement point with the specified critical
temperature.
""".
init_for_crit( AttrValue, TempData, Suffix, BinPointName, SensorId, State ) ->

    case vet_initial_temperature( AttrValue, Suffix, BinPointName, SensorId,
                                  State ) of

        false ->
            % Just ignored then:
            TempData;

        _True ->
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




% Fan section.


-doc """
Registers the specified fan points in the specified data table, for the
initialisation of the specified sensor, from the specified JSON content.

Only point triplets that are already filtered for fans are expected, thus no
need to accumulate unexpected points.
""".
-spec register_fan_points( [ json_triple() ], points_data_table(),
    sensor_id(), muted_points(), wooper:state() ) -> points_data_table().
register_fan_points( _PointTriples, DataTable, _SensorId,
                     _MutedPoints=all_points, _State ) ->
    DataTable;

% Normal end of recursion:
register_fan_points( _PointTriples=[], DataTable, _SensorId, _MutedPoints=[],
                     _State ) ->
    DataTable;

% Remaining muted points:
register_fan_points( _PointTriples=[], DataTable, SensorId, MutedPoints,
                     State ) ->

    % This is not an error at all, as non-fan points (e.g. intrusion ones) may
    % have to be muted as well:
    %
    ?debug_fmt( "For ~ts, following ~B muted measurements were not retained "
        "in terms of fans: ~ts.",
        [ sensor_id_to_string( SensorId ), length( MutedPoints ),
          text_utils:binaries_to_listed_string( MutedPoints ) ] ),

    DataTable;


% For example "fan2":{ "fan2_input": 1048.000, "fan2_min": 0.000,
%              "fan2_alarm": 0.000, "fan2_beep": 0.000, "fan2_pulses": 2.000 },
register_fan_points(
        _PointTriples=[ { BinPointName, BinDesc, PointValueMap } | T ],
        DataTable, SensorId, MutedPoints, State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "Considering the registration of fan point "
            "'~ts' (~ts) with:~n ~p~n(muted points: ~p)",
            [ BinPointName, BinDesc, PointValueMap, MutedPoints ] ) ),

    % The case MutedPoints=all_points has already been intercepted:
    { NewDataTable, NewMutedPoints } =
            case list_utils:extract_element_if_existing( BinPointName,
                                                         MutedPoints ) of

        % Hence not muted:
        false ->
            InitPointData = create_fan_data( PointValueMap, BinPointName,
                BinDesc, SensorId, State ),

            % Uniqueness checked:
            { table:add_new_entry( BinPointName, InitPointData, DataTable ),
              MutedPoints };

        ShrunkMutedPoints ->
            ?info_fmt( "Point '~ts' of sensor ~ts belongs to the muted "
                "ones, hence is excluded.",
                [ BinPointName, sensor_id_to_string( SensorId ) ] ),
            { DataTable, ShrunkMutedPoints }

    end,

    % First update done globally directly from the constructor.

    register_fan_points( T, NewDataTable, SensorId, NewMutedPoints, State ).



-doc """
Returns a new fan data initialised from the specified JSON content, ready to be
updated with the future next current fan notifications.
""".
-spec create_fan_data( point_attribute_map(), measurement_point_name(),
        measurement_point_description(), sensor_id(), wooper:state() ) ->
                                    fan_data().
create_fan_data( PointValueMap, BinPointName, BinDesc, SensorId, State ) ->

    % We check all attributes in turn; no need to accumulate here.

    % Here the current fan status has not been taken into account yet:
    init_fan_point( _FanEntries=map_hashtable:enumerate( PointValueMap ),
        BinPointName, #fan_data{ description=BinDesc }, SensorId, State ).



% (helper)
% End of recursion, here with no fan reading:
init_fan_point( _FanEntries=[], BinPointName,
        FanData=#fan_data{ current=undefined }, SensorId, State ) ->

    ?error_fmt( "For fan measurement point '~ts' of ~ts, no fan "
        "speed was reported; disabling this point.",
        [ BinPointName, sensor_id_to_string( SensorId ) ] ),

    FanData#fan_data{ status=disabled };


% End of recursion, here with no specified min speed setting:
init_fan_point( _FanEntries=[], _BinPointName,
                FanData=#fan_data{ alarm_low=undefined }, _SensorId, _State ) ->

    % No specific final consistency update to be performed here.

    %?debug_fmt( "Adding fan point '~ts' of ~ts.",
    %            [ BinPointName, sensor_id_to_string( SensorId ) ] ),

    check_fan_data( FanData );


% End of recursion, here with a specified min speed setting:
init_fan_point( _FanEntries=[], BinPointName,
                FanData=#fan_data{ current=CurrentSpeed,
                                   alarm_low=AlarmLowSpeed },
                % Eliminates the case of null AlarmLowSpeed:
                SensorId, State ) when CurrentSpeed < AlarmLowSpeed ->

    ?error_fmt( "For fan measurement point '~ts' of ~ts, the initial fan "
        "speed, ~ts, is lower than the alarm low one (~ts).",
        [ BinPointName, sensor_id_to_string( SensorId ),
          unit_utils:rpm_to_string( CurrentSpeed ),
          unit_utils:rpm_to_string( AlarmLowSpeed ) ] ),

    check_fan_data( FanData );


init_fan_point( _FanEntries=[], _BinPointName, FanData, _SensorId, _State  ) ->
    check_fan_data( FanData );


% Example of AttrNameBin/AttrValue entries for a "fan2" point:
%   "fan2_input": 1048.000,
%   "fan2_min": 0.000,
%   "fan2_alarm": 0.000,
%   "fan2_beep": 0.000,
%   "fan2_pulses": 2.000
%
init_fan_point( _FanEntries=[ { AttrNameBin, AttrValue } | T ], BinPointName,
                FanData, SensorId, State ) ->

    AttrName = text_utils:binary_to_string( AttrNameBin ),

    % Used more than once:
    Separator = $_,

    case text_utils:split( AttrName, Separator ) of

        [ _WithoutSepElem ] ->

            ?error_fmt( "For fan measurement point '~ts' of ~ts, "
                "attribute '~ts' cannot be stripped of a relevant prefix; "
                "ignoring it.",
                [ BinPointName, sensor_id_to_string( SensorId ), AttrName ] ),

            init_fan_point( T, BinPointName, FanData, SensorId, State );

        % Just remove the prefix:
        [ _AnyPrefix | OtherElems ] ->

            type_utils:check_float( AttrValue ),

            Now = time_utils:get_timestamp(),

            NewFanData = case text_utils:join( Separator, OtherElems ) of

                % Current speed:
                "input" ->

                    BinInputAttr = text_utils:string_to_binary( AttrName ),

                    Speed = AttrValue,

                    case math_utils:is_null( Speed ) of

                        true ->
                            ZeroSpeed = 0.0,
                            FanData#fan_data{ input_attribute=BinInputAttr,
                                              current=ZeroSpeed,
                                              % Supposingly normal:
                                              %state=unknown,
                                              state=nominal,
                                              min_reached=ZeroSpeed,
                                              max_reached=ZeroSpeed,
                                              avg_sum=ZeroSpeed,
                                              avg_count=1 };

                        _False ->
                            Now = time_utils:get_timestamp(),
                            FanData#fan_data{ input_attribute=BinInputAttr,
                                              current=Speed,
                                              state=nominal,
                                              last_spin_timestamp=Now,
                                              min_reached=Speed,
                                              max_reached=Speed,
                                              avg_sum=Speed,
                                              avg_count=1 }

                    end;


                % Supposing this attribute tells us the minimum speed allowed:
                "min" ->
                    % Checks against current speed later:
                    FanData#fan_data{ alarm_low=AttrValue };


                % Not knowing what to do with such an attribute, supposedly
                % telling an alarm was raised:
                %
                "alarm" ->

                    case math_utils:is_null( AttrValue ) of

                        % Normal case, no alarm initially:
                        true ->
                            % Nothing special done:
                            FanData;

                        _False ->
                            case AttrValue of

                                1.0 ->
                                    ?warning_fmt( "For fan measurement point "
                                        "'~ts' of ~ts, attribute '~ts' already "
                                        "reports initially an alarm.",
                                        [ BinPointName,
                                          sensor_id_to_string( SensorId ),
                                          AttrName ] ),

                                    % Nothing special done either:
                                    FanData;

                                _Other ->
                                    ?error_fmt( "For fan measurement point "
                                        "'~ts' of ~ts, attribute '~ts' reports "
                                        "an unexpected alarm value ('~p'); "
                                        "disabling that point.",
                                        [ BinPointName,
                                          sensor_id_to_string( SensorId ),
                                          AttrName, AttrValue ] ),

                                    FanData#fan_data{ status=disabled }

                            end

                    end;


                "beep" ->
                    case math_utils:is_null( AttrValue ) of

                        true ->
                            FanData#fan_data{ beep_on_alarm=false };

                        _False ->
                            case AttrValue of

                                1.0 ->
                                    FanData#fan_data{ beep_on_alarm=true };

                                _Other ->
                                    ?warning_fmt( "For fan measurement point "
                                        "'~ts' of ~ts, ignoring unexpected "
                                        "beep value ('~p') reported by "
                                        "attribute '~ts'.",
                                        [ BinPointName,
                                          sensor_id_to_string( SensorId ),
                                          AttrValue, AttrName ] ),

                                    % Nothing special done either:
                                    FanData

                            end

                    end;


                "pulses" ->
                    FanData#fan_data{ pulses=AttrValue };


                _OtherSuffix ->

                    ?warning_fmt( "Unknown suffix for attribute '~ts' of "
                        "fan measurement point ~ts of ~ts; ignoring it.",
                        [ AttrName, BinPointName,
                          sensor_id_to_string( SensorId ) ] ),

                    FanData

            end,

            init_fan_point( T, BinPointName, NewFanData, SensorId, State )

    end.



-doc """
Registers the specified intrusion points in the specified data table, for the
initialisation of the specified sensor, from the specified JSON content.

Only point triples that are already filtered for intrusion are expected, thus no
need to accumulate unexpected points.
""".
-spec register_intrusion_points( [ json_triple() ], points_data_table(),
    sensor_id(), muted_points(), wooper:state() ) -> points_data_table().
register_intrusion_points( _PointTriples, DataTable, _SensorId,
                           _MutedPoints=all_points, _State ) ->
    DataTable;

% Normal end of recursion:
register_intrusion_points( _PointTriples=[], DataTable, _SensorId,
                           _MutedPoints=[], _State ) ->
    DataTable;

% Remaining muted points:
register_intrusion_points( _PointTriples=[], DataTable, SensorId, MutedPoints,
                           State ) ->

    % This is not an error at all, as non-intrusion points (e.g. temperature
    % ones) may have to be muted as well:
    %
    ?debug_fmt( "For ~ts, following ~B muted measurements were not retained "
        "in terms of intrusion: ~ts.",
        [ sensor_id_to_string( SensorId ), length( MutedPoints ),
          text_utils:binaries_to_listed_string( MutedPoints ) ] ),

    DataTable;


% For example BinPointName = <<"intrusion0">>,
% BinDesc=<<"Some description">> and PointValueMap =
%    #{<<"intrusion0_alarm">> => 1.0,
%      <<"intrusion0_beep">> => 0.0}.
%
register_intrusion_points(
        _PointTriples=[ { BinPointName, BinDesc, PointValueMap } | T ],
        DataTable, SensorId, MutedPoints, State ) ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "Considering the registration of intrusion point "
            "'~ts' (~ts) with:~n ~p~n(muted points: ~p)",
            [ BinPointName, BinDesc, PointValueMap, MutedPoints ] ) ),

    % The case MutedPoints=all_points has already been intercepted:
    { NewDataTable, NewMutedPoints } =
            case list_utils:extract_element_if_existing( BinPointName,
                                                         MutedPoints ) of

        % Hence not muted:
        false ->
            InitPointData = create_intrusion_data( PointValueMap,
                BinPointName, BinDesc, SensorId, State ),

            % Uniqueness checked:
            { table:add_new_entry( BinPointName, InitPointData, DataTable ),
              MutedPoints };

        ShrunkMutedPoints ->
            ?info_fmt( "Point '~ts' of sensor ~ts belongs to the muted "
                "ones, hence is excluded.",
                [ BinPointName, sensor_id_to_string( SensorId ) ] ),
            { DataTable, ShrunkMutedPoints }

    end,

    % First update done globally directly from the constructor.

    register_intrusion_points( T, NewDataTable, SensorId, NewMutedPoints,
                               State ).



-doc """
Returns a new intrusion data initialised from the specified JSON content for the
specified measurement point, ready to be updated with the future next current
intrusion notifications (knowing it is not muted, as now muted points are
filtered upfront).
""".
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

    IntrusData#intrusion_data{ input_attribute=BinPointName,
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


% For example AttrNameBin = <<"intrusion0_alarm">>, AttrValue = 1.0:
init_intrus_point( _IntrusEntries=[ { AttrNameBin, AttrValue } | T ],
                   BinPointName, IntrusData, SensorId, State ) ->

    AttrName = text_utils:binary_to_string( AttrNameBin ),

    % Used more than once:
    Separator = $ ,

    case text_utils:split( AttrName, Separator ) of

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

                    case math_utils:is_null( AttrValue ) of

                        % Normal case, no intrusion initially:
                        true ->
                            SetIntrusData = IntrusData#intrusion_data{
                                intrusion_reported=false,
                                intrusion_timestamp=Now },

                            init_intrus_point( T, BinPointName, SetIntrusData,
                                               SensorId, State );

                        _False ->
                            case AttrValue of

                                1.0 ->
                                    ?warning_fmt( "For intrusion measurement "
                                        "point '~ts' of ~ts, attribute '~ts' "
                                        "already reports initially an "
                                        "intrusion; interpreting it as a "
                                        "bogus value and disabling that point.",
                                        [ BinPointName,
                                          sensor_id_to_string( SensorId ),
                                          AttrName ] ),

                                    DisIntrusData =
                                        IntrusData#intrusion_data{
                                            status=disabled,
                                            intrusion_reported=true,
                                            intrusion_timestamp=Now },

                                    init_intrus_point( T, BinPointName,
                                        DisIntrusData, SensorId, State );

                                _Other ->
                                    ?error_fmt( "For intrusion measurement "
                                        "point '~ts' of ~ts, attribute '~ts' "
                                        "reports an unexpected value ('~p'); "
                                        "disabling that point.",
                                        [ BinPointName,
                                          sensor_id_to_string( SensorId ),
                                          AttrName, AttrValue ] ),

                                    DisIntrusData = IntrusData#intrusion_data{
                                        status=disabled,
                                        intrusion_reported=false,
                                        intrusion_timestamp=Now },

                                    init_intrus_point( T, BinPointName,
                                        DisIntrusData, SensorId, State )

                            end

                    end;


                "beep" ->
                    case math_utils:is_null( AttrValue ) of

                        true ->
                            SetIntrusData = IntrusData#intrusion_data{
                                beep_on_intrusion=false },

                            init_intrus_point( T, BinPointName, SetIntrusData,
                                               SensorId, State );

                        _False ->
                            case AttrValue of

                                1.0 ->
                                    SetIntrusData = IntrusData#intrusion_data{
                                        beep_on_intrusion=true },

                                    init_intrus_point( T, BinPointName,
                                        SetIntrusData, SensorId, State );

                                _Other ->
                                    ?warning_fmt( "For intrusion measurement "
                                        "point '~ts' of ~ts, ignoring "
                                        "unexpected value ('~p') reported by "
                                        "attribute '~ts'.",
                                        [ BinPointName,
                                          sensor_id_to_string( SensorId ),
                                          AttrValue, AttrName ] ),

                                    init_intrus_point( T, BinPointName,
                                        IntrusData, SensorId, State )

                            end

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



-doc "Returns a term corresponding to specified JSON text.".
-spec decode_sensor_json( string_json(), wooper:state() ) -> decoded_json().
decode_sensor_json( SensorJsonStr, State ) ->

    ParserState = ?getAttr(parser_state),

    DecodedMap = json_utils:from_json( SensorJsonStr, ParserState ),

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "Top-level JSON-decoded map for sensors: ~ts",
                    [ map_hashtable:to_string( DecodedMap ) ] ) ),

    DecodedMap.



-doc """
Updates the sensor data (e.g. temperatures and fan speeds), as read from
sensors.
""".
update_sensor_data( State ) ->

    case fetch_sensor_data( State ) of

        { ok, CmdOutput } ->
            update_from_sensor_output( CmdOutput, State );

        { error, ErrorOutput } ->
            ?alert_fmt( "Failed to read sensor data: '~ts'.", [ ErrorOutput ] ),
            State

    end.



-doc """
Parses the specified sensor JSON output, returns a state with an updated sensor
table.
""".
-spec update_from_sensor_output( string_json(), wooper:state() ) ->
                                        wooper:state().
update_from_sensor_output( SensorJsonStr, State ) ->

    JSONDecodedMap = decode_sensor_json( SensorJsonStr, State ),

    % Quite convenient:
    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "Updating sensors from '~p'.", [ JSONDecodedMap ] ) ),

    update_active_sensors( map_hashtable:enumerate( JSONDecodedMap ),
                           ?getAttr(sensor_table), State ).



-doc """
Updates all active sensors based on the specified JSON-decoded map.

We iterate through the JSON content rather than through the known sensors (more
logical); we just update the sensor table.
""".
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
                "so not taking into account its measurement points in:~n  ~p.",
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
                "is '~ts':~n  ~p (ignoring these measurement points).",
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



-doc """
Updates, for the specified sensor, its specified data table of points of all
sorts (temperature, intrusion, fan, etc.) from the specified JSON point entries.
""".
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
            cond_utils:if_defined( us_main_debug_sensors,
                ?debug_fmt( "(temperature measurement point '~ts' is disabled)",
                            [ PointNameBin ] ) ),

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

                    NewTempData = case vet_runtime_temperature( CurrentTemp,
                            Desc, PointNameBin, SensorId, State ) of

                        true ->
                            % Status 'enabled' implies these fields are set:
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

                        _False ->
                            % Was a non-disabling warning:
                            ?error_fmt( "Read for ~ts, temperature "
                                "measurement point '~ts', a value considered "
                                "invalid for input temperature attribute "
                                "'~ts': ~p (ignoring this measurement point "
                                "from now on).",
                                [ sensor_id_to_string( SensorId ), PointNameBin,
                                  InputAttrName, CurrentTemp ] ),

                            % Previously: TempData

                            % Stricter now: we just disable any reporting at
                            % least once a bogus value (temperature here):
                            %
                            TempData#temperature_data{ status=disabled }

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


        { value, #fan_data{ status=disabled } } ->
            cond_utils:if_defined( us_main_debug_sensors,
                ?debug_fmt( "(fan measurement point '~ts' is disabled)",
                            [ PointNameBin ] ) ),

            % No change then:
            PointsDataTable;


        { value, FanData=#fan_data{ input_attribute=InputAttrName,
                                    description=Desc,
                                    % Superfluous:
                                    status=enabled,
                                    last_spin_timestamp=MaybeLastSpinTimestamp,
                                    min_reached=MinReachedSpeed,
                                    max_reached=MaxReachedSpeed,
                                    avg_sum=AvgSum,
                                    avg_count=AvgCount } } ->

            case map_hashtable:lookup_entry( InputAttrName, PointAttrMap ) of

                { value, CurrentSpeed } ->

                    NewFanData = case vet_fan_speed( CurrentSpeed, Desc,
                                    PointNameBin, SensorId, State ) of

                        true ->
                            % Status 'enabled' implies these fields are set:
                            NewMinSpeed = min( MinReachedSpeed, CurrentSpeed ),
                            NewMaxSpeed = max( MaxReachedSpeed, CurrentSpeed ),
                            NewAvgSum = AvgSum + CurrentSpeed,
                            NewAvgCount = AvgCount + 1,

                            NewLastSpinTimestamp = case
                                    math_utils:is_null( CurrentSpeed ) of

                                true ->
                                    MaybeLastSpinTimestamp;

                                _False ->
                                    time_utils:get_timestamp()

                            end,

                            SyncFanData = FanData#fan_data{
                                last_spin_timestamp=NewLastSpinTimestamp,
                                current=CurrentSpeed,
                                min_reached=NewMinSpeed,
                                max_reached=NewMaxSpeed,
                                avg_sum=NewAvgSum,
                                avg_count=NewAvgCount },

                            % Returns new fan_data:
                            examine_fan_speed( PointNameBin, CurrentSpeed,
                                               SyncFanData, SensorId, State );

                        _False ->
                            % If wanting to be stricter, to be promoted to a
                            % disabling error:
                            %
                            ?warning_fmt( "Read for ~ts, fan measurement "
                                "point '~ts', a value considered invalid "
                                "for input fan attribute '~ts': ~p "
                                "(ignoring it).",
                                [ sensor_id_to_string( SensorId ), PointNameBin,
                                  InputAttrName, CurrentSpeed ] ),

                            FanData
                            %FanData#fan_data{ status=disabled }

                    end,

                    table:add_entry( PointNameBin, NewFanData,
                                     PointsDataTable );


                % Attribute not anymore in JSON output (never expected to
                % happen):
                %
                key_not_found ->

                    ?error_fmt( "Could not read fan attribute '~ts' "
                        "of measurement point '~ts' regarding ~ts; "
                        "skipping the update of this point.",
                        [ InputAttrName, PointNameBin,
                          sensor_id_to_string( SensorId ) ] ),

                    PointsDataTable

            end;


        { value, #intrusion_data{ status=disabled } } ->
            cond_utils:if_defined( us_main_debug_sensors,
                ?debug_fmt( "(intrusion measurement point '~ts' is disabled)",
                            [ PointNameBin ] ) ),

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

        % Perfectly normal, as many points (e.g. "in7") have no interest:
        key_not_found ->

            % Convenient to catch non-interpreted entries (otherwise fully
            % normal):
            %
            cond_utils:if_defined( us_main_debug_sensors,
                ?debug_fmt( "(no entry in points table found for "
                    "measurement point '~ts' of ~ts; skipping this point)",
                    [ PointNameBin, sensor_id_to_string( SensorId ) ] ) ),

            PointsDataTable

    end,

    update_data_table( NewPointsDataTable, T, SensorId, State ).



-doc """
Examines the specified reported, current (runtime) temperature and takes any
appropriate action.

(sensor expected to be enabled)
""".
-spec examine_temperature( measurement_point_name(), celsius(),
        temperature_data(), sensor_id(), wooper:state() ) -> temperature_data().
examine_temperature( PointNameBin, CurrentTemp,
        TempData=#temperature_data{ %status=enabled,
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
            ?notice_fmt( "Temperature of ~ts back to normal (~ts; "
                "from ~ts) at measurement point ~ts.",
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
        _False ->

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
                        "Check fans and room temperature.~n~ts~n~ts~n~ts",
                        [ sensor_id_to_string( SensorId ),
                          unit_utils:temperature_to_string( AlarmHigh ),
                          PointNameBin,
                          unit_utils:temperature_to_string( CurrentTemp ),
                          unit_utils:temperature_to_string( CritHigh ),
                          system_utils:report_main_system_metrics(),
                          system_utils:interpret_top_cumulated_cpu_processes(),
                          % Even if vaguely related:
                          system_utils:interpret_top_memory_using_processes()
                        ] ),

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
        _False ->
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



-doc """
Examines the specified reported, current fan speed and takes any appropriate
action.

(sensor expected to be enabled)
""".
-spec examine_fan_speed( measurement_point_name(), rpm(), fan_data(),
                         sensor_id(), wooper:state() ) -> fan_data().
examine_fan_speed( PointNameBin, CurrentSpeed,
        FanData=#fan_data{ %status=enabled,
                           type=FanType,
                           state=FanState,
                           alarm_low=MaybeAlarmLowSpeed,
                           alarm_high=MaybeAlarmHighSpeed },
        SensorId, State ) ->

    case get_fan_state( CurrentSpeed, FanType, MaybeAlarmLowSpeed,
                        MaybeAlarmHighSpeed ) of

        % Nothing changed:
        FanState ->
            FanData;

        % Thus always different; is among a subset of fan_state():
        NewFanState ->
            case { FanState, NewFanState } of

                { nominal, insufficient_speed } ->
                    ?emergency_fmt( "Fan speed monitored by ~ts at measurement "
                        "point ~ts is now ~ts, and just went below the alarm "
                        "low threshold (~ts), "
                        "whereas this fan is not known being a PWM one.",
                        [ sensor_id_to_string( SensorId ), PointNameBin,
                          unit_utils:rpm_to_string( CurrentSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmLowSpeed ) ] );

                { nominal, excessive_speed } ->
                    ?alert_fmt( "Fan speed monitored by ~ts at measurement "
                        "point ~ts is now ~ts, and just exceeded the alarm "
                        "high threshold (~ts); check that the cause is not "
                        "a skyrocketing temperature.~n~ts~n~ts~n~ts",
                        [ sensor_id_to_string( SensorId ), PointNameBin,
                          unit_utils:rpm_to_string( CurrentSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmLowSpeed ),
                          system_utils:interpret_top_instant_cpu_processes(),
                          system_utils:interpret_top_cumulated_cpu_processes(),
                          % Even if vaguely related:
                          system_utils:interpret_top_memory_using_processes()
                        ] );

                { insufficient_speed, nominal } ->
                    ?warning_fmt( "Fan speed monitored by ~ts at measurement "
                        "point ~ts is now ~ts, and just went back above "
                        "the alarm low threshold (~ts); this fan thus returned "
                        "to nominal state.",
                        [ sensor_id_to_string( SensorId ), PointNameBin,
                          unit_utils:rpm_to_string( CurrentSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmLowSpeed ) ] );

                { excessive_speed, nominal } ->
                    ?warning_fmt( "Fan speed monitored by ~ts at measurement "
                        "point ~ts is now ~ts, and just went back below "
                        "the alarm high threshold (~ts); this fan thus "
                        "returned to nominal state.",
                        [ sensor_id_to_string( SensorId ), PointNameBin,
                          unit_utils:rpm_to_string( CurrentSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmHighSpeed ) ] );


                % Unlikely cases:
                { insufficient_speed, excessive_speed } ->
                    ?emergency_fmt( "Fan speed monitored by ~ts at "
                        "measurement point ~ts is now ~ts, and just went "
                        "from below the alarm low threshold (~ts) "
                        "to above the alarm high threshold (~ts).",
                        [ sensor_id_to_string( SensorId ), PointNameBin,
                          unit_utils:rpm_to_string( CurrentSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmLowSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmHighSpeed ) ] );

                { excessive_speed, insufficient_speed } ->
                    ?emergency_fmt( "Fan speed monitored by ~ts at "
                        "measurement point ~ts is now ~ts, and just went "
                        "from above the alarm high threshold (~ts) "
                        "to below the alarm low threshold (~ts).",
                        [ sensor_id_to_string( SensorId ), PointNameBin,
                          unit_utils:rpm_to_string( CurrentSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmHighSpeed ),
                          unit_utils:rpm_to_string( MaybeAlarmLowSpeed ) ] )

            end,

            FanData#fan_data{ state=NewFanState }

    end.



-doc "Returns the (new) fan state corresponding to specified parameters.".
-spec get_fan_state( rpm(), fan_type(), option( rpm() ), option( rpm() ) ) ->
                            fan_state().
% Later, if fan types are detected, more fan states may be returned:
get_fan_state( CurrentSpeed, _FanType=fixed_speed, AlarmLowSpeed,
               _MaybeAlarmHighSpeed ) when is_float( AlarmLowSpeed )
                                        andalso CurrentSpeed < AlarmLowSpeed ->
    insufficient_speed;

get_fan_state( CurrentSpeed, _FanType=fixed_speed, _AlarmLowSpeed,
               AlarmHighSpeed ) when is_float( AlarmHighSpeed )
                                        andalso CurrentSpeed > AlarmHighSpeed ->
    excessive_speed;

get_fan_state( _CurrentSpeed, _FanType=fixed_speed, _AlarmLowSpeed,
               _AlarmHighSpeed ) ->
    % Includes undefined speed:
    nominal;


% No abnormal low speed if being PWM (e.g. just stopping after a temperature
% drop):
%
get_fan_state( CurrentSpeed, _FanType=pwm, _AlarmLowSpeed, AlarmHighSpeed )
        when is_float( AlarmHighSpeed ) andalso CurrentSpeed > AlarmHighSpeed ->
    excessive_speed;

get_fan_state( _CurrentSpeed, _FanType=pwm, _AlarmLowSpeed, _AlarmHighSpeed ) ->
    nominal;


% Currently like fixed_speed:
get_fan_state( CurrentSpeed, _FanType=unknown, AlarmLowSpeed,
               _MaybeAlarmHighSpeed ) when is_float( AlarmLowSpeed )
                                        andalso CurrentSpeed < AlarmLowSpeed ->
    insufficient_speed;

get_fan_state( CurrentSpeed, _FanType=unknown, _AlarmLowSpeed,
               AlarmHighSpeed ) when is_float( AlarmHighSpeed )
                                     andalso CurrentSpeed > AlarmHighSpeed ->
    excessive_speed;

get_fan_state( _CurrentSpeed, _FanType=unknown, _AlarmLowSpeed,
               _AlarmHighSpeed ) ->
    % Includes undefined speed:
    nominal.



-doc """
Examines the specified reported, current intrusion status and takes any
appropriate action.

(sensor expected to be enabled)
""".
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


-doc "Filters the specified JSON content corresponding to a CPU socket.".
-spec filter_cpu_socket_json( decoded_json() ) ->
                                    { [ json_triple() ], [ json_triple() ] }.
filter_cpu_socket_json( SensorJSON ) ->

    % Plain strings used (for matching), as we cannot leave the name of the
    % measurement points as binaries: if having "tempN_input", N can have one
    % digit or more (e.g. <<"temp10_input">>), and non-last segments must of
    % course be of fixed length.
    %
    BasicTriples = [ { text_utils:binary_to_string( BinStr ), BinStr, V }
        || { BinStr, V } <- map_hashtable:enumerate( SensorJSON ) ],

    filter_cpu_socket_json( BasicTriples, _TempAcc=[], _OtherAccc=[] ).



% (helper)
filter_cpu_socket_json( _BasicTriples=[], TempAcc, OtherAcc ) ->
    % Original order is clearer (e.g. for core numbers):
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



-doc "Filters the specified JSON content corresponding to a CPU.".
-spec filter_cpu_json( decoded_json() ) ->
                                    { [ json_triple() ], [ json_triple() ] }.
filter_cpu_json( SensorJSON ) ->

    BasicTriples = [ { text_utils:binary_to_string( BinStr ), BinStr, V }
                || { BinStr, V } <- map_hashtable:enumerate( SensorJSON ) ],

    filter_cpu_json( BasicTriples, _TempAcc=[], _OtherAccc=[] ).


% (helper)
filter_cpu_json( _BasicTriples=[], TempAcc, OtherAcc ) ->
    % Original order is clearer (e.g. for core numbers):
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


% For example <<"k10temp-pci-00c3">>: #{<<"temp1">> =>...
filter_cpu_json(
        _BasicTriples=[ { _Name="temp" ++ Num, BinPointName, V } | T ],
        TempAcc, OtherAcc ) ->

    Desc = integrate_any_number( "CPU temperature sensor", Num ),

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

    Desc = text_utils:bin_format( "uncategorised CPU sensor '~ts'", [ Name ] ),

    JSONTriple = { BinPointName, Desc, V },

    filter_cpu_json( T, TempAcc, [ JSONTriple | OtherAcc ] ).




-doc "Filters te JSON content corresponding to a motherboard.".
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


% Other motherboard CPU temperature index:
filter_motherboard_json( _BasicTriples=[
        { _Name="CPU" ++ Num, BinPointName, V } | T ], TempAcc,
                         FanAcc, IntrusionAcc, OtherAcc ) ->

    Desc = integrate_any_number( "motherboard CPU temperature sensor", Num ),

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

% Any (unclassified) temperature sensor:
filter_motherboard_json( _BasicTriples=[
        { _Name="temp" ++ Num, BinPointName, V } | T ], TempAcc,
                         FanAcc, IntrusionAcc, OtherAcc ) ->

    Desc = integrate_any_number( "temperature sensor", Num ),

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

% PWM fans are dropped, as these entries just correspond to commands (inputs set
% by motherboard), not measurements (outputs).
%
% Entry example:
%    "pwm3": {
%      "pwm3": 76.500000,
%      "pwm3_enable": 5.000000,
%      "pwm3_mode": 1.000000
%    },
%
filter_motherboard_json( _BasicTriples=[
        { _Name="pwm" ++ _Num, _BinPointName, _V } | T ], TempAcc,
                         FanAcc, IntrusionAcc, OtherAcc ) ->
    filter_motherboard_json( T, TempAcc, FanAcc, IntrusionAcc, OtherAcc );


% Motherboard-level GPU temperature index (typically for a laptop):
filter_motherboard_json( _BasicTriples=[
        { _Name="GPU" ++ Num, BinPointName, V } | T ], TempAcc,
                         FanAcc, IntrusionAcc, OtherAcc ) ->

    Desc = integrate_any_number( "motherboard-level GPU temperature sensor",
                                 Num ),

    JSONTriple = { BinPointName, Desc, V },

    filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
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
%  - "in*" (e.g. "in1" - but not "intrusion1") [voltage? unassigned input port?]
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


% For example PCH_CHIP_CPU_MAX_TEMP, PCH_CHIP_TEMP, PCH_CPU_TEMP (temperatures;
% yet often returned as being 0°C):
%
filter_motherboard_json( _BasicTriples=[
        { Name="PCH_" ++ _, BinPointName, V } | T ],
                         TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->

    Desc = "chipset temperature sensor " ++ Name,

    JSONTriple = { BinPointName, Desc, V },

    filter_motherboard_json( T, [ JSONTriple | TempAcc ], FanAcc, IntrusionAcc,
                             OtherAcc );


% Ignored entries are typically:
filter_motherboard_json( _BasicTriples=[ { Name, BinPointName, V } | T ],
                         TempAcc, FanAcc, IntrusionAcc, OtherAcc ) ->

    Desc = text_utils:bin_format( "uncategorised motherboard sensor '~ts'",
                                  [ Name ] ),

    JSONTriple = { BinPointName, Desc, V },

    filter_motherboard_json( T, TempAcc, FanAcc, IntrusionAcc,
                             [ JSONTriple | OtherAcc ] ).



-doc "Filters the specified JSON content corresponding to a disk.".
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



-doc "Filters the specified JSON content corresponding to a chipset.".
-spec filter_chipset_json( decoded_json() ) ->
                                    { [ json_triple() ], [ json_triple() ] }.
filter_chipset_json( SensorJSON ) ->

    BasicTriples = [ { text_utils:binary_to_string( BinStr ), BinStr, V }
                || { BinStr, V } <- map_hashtable:enumerate( SensorJSON ) ],

    filter_chipset_json( BasicTriples, _TempAcc=[], _OtherAccc=[] ).


% (helper)
filter_chipset_json( _BasicTriples=[], TempAcc, OtherAcc ) ->
    % Original order may be clearer:
    { lists:reverse( TempAcc ), OtherAcc };

filter_chipset_json( _BasicTriples=[ { Name, BinPointName, V } | T ],
                     TempAcc, OtherAcc ) ->

    Desc = text_utils:bin_format( "uncategorised chipset sensor '~ts'",
                                  [ Name ] ),

    JSONTriple = { BinPointName, Desc, V },

    filter_chipset_json( T, [ JSONTriple | TempAcc ], OtherAcc ).



-doc """
Integrates to the specified base name of a measurement point a corresponding
number (if any).
""".
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



-doc "Inits the polling of the sensors.".
-spec init_polling( class_USScheduler:user_periodicity(), wooper:state() ) ->
                            wooper:state().
init_polling( SensorPollPeriodicity, State ) ->

    ?debug_fmt( "Planning a sensor measurement each ~ts.",
        [ time_utils:duration_to_string( 1000 * SensorPollPeriodicity ) ] ),

    SchedulerPid = class_USScheduler:get_server_pid(),

    SchedulerPid ! { registerTask,
                    [ _TaskCmd=readSensors, SensorPollPeriodicity ], self() },

    receive

        { wooper_result, { task_registered, TaskId } } ->
            ?debug_fmt( "Sensor polling task registered, as identifier #~B.",
                        [ TaskId ] ),
            setAttribute( State, task_id, TaskId )

    end.



-doc """
Vets the specified initial (stricter) temperature regarding bogus range.
""".
-spec vet_initial_temperature( celsius(), temperature_description(),
        measurement_point_name(), sensor_id(), wooper:state() ) -> boolean().
vet_initial_temperature( Temp, TempDesc, BinPointName, SensorId, State ) ->
    vet_temperature( Temp, TempDesc,
        _Min=?low_bogus_temperature_initial_threshold,
        _Max=?high_bogus_temperature_initial_threshold, _RangeDesc="bogus",
        BinPointName, SensorId, State ).



-doc "Vets the specified runtime temperature regarding the bogus range.".
-spec vet_runtime_temperature( celsius(), temperature_description(),
        measurement_point_name(), sensor_id(), wooper:state() ) -> boolean().
vet_runtime_temperature( Temp, TempDesc, BinPointName, SensorId, State ) ->
    vet_temperature( Temp, TempDesc, _Min=?low_bogus_temperature_threshold,
        _Max=?high_bogus_temperature_threshold, _RangeDesc="bogus",
        BinPointName, SensorId, State ).



-doc """
Vets the specified temperature regarding the specified range.

Note that this check is applied to all kinds of temperatures (e.g. crit ones -
not only the measured ones), so no need to raise too serious alerts here (could
be misleading).
""".
-spec vet_temperature( celsius(), temperature_description(), celsius(),
        celsius(), range_description(), measurement_point_name(), sensor_id(),
        wooper:state() ) -> boolean().
vet_temperature( Temp, TempDesc, _Min, _Max, _RangeDesc, BinPointName, SensorId,
                 State ) when not is_float( Temp ) ->

    ?error_fmt( "For temperature measurement point ~ts of ~ts, "
        "the ~ts temperature is reported as '~p', which is not a float; "
        "this value is thus ignored.",
        [ BinPointName, sensor_id_to_string( SensorId ), TempDesc, Temp ] ),

    false;


% From here Temp is a float:
vet_temperature( Temp, TempDesc, Min, _Max, RangeDesc, BinPointName, SensorId,
                 State ) when Temp < Min ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "For temperature measurement point ~ts of ~ts, "
            "the ~ts temperature is reported as ~ts, i.e. below the "
            "low ~ts threshold (~ts), so this value is to be ignored.",
            [ BinPointName, sensor_id_to_string( SensorId ), TempDesc,
              unit_utils:temperature_to_string( Temp ), RangeDesc,
              unit_utils:temperature_to_string( Min ) ] ),
        basic_utils:ignore_unused(
            [ TempDesc, RangeDesc, BinPointName, SensorId, State ] ) ),

    false;

vet_temperature( Temp, TempDesc, _Min, Max, RangeDesc, BinPointName, SensorId,
                 State ) when Temp > Max ->

    cond_utils:if_defined( us_main_debug_sensors,
        ?debug_fmt( "For temperature measurement point ~ts of ~ts, "
            "the ~ts temperature is reported as ~ts, i.e. above the "
            "high ~ts threshold (~ts), so this value is to be ignored.",
            [ BinPointName, sensor_id_to_string( SensorId ), TempDesc,
              unit_utils:temperature_to_string( Temp ), RangeDesc,
              unit_utils:temperature_to_string( Max ) ] ),
        basic_utils:ignore_unused(
            [ TempDesc, Max, RangeDesc, BinPointName, SensorId, State ] ) ),

    false;

vet_temperature( _Temp, _TempDesc, _Min, _Max, _RangeDesc, _BinPointName,
                 _SensorId, _State ) ->
    true.



-doc "Vets specified temperature regarding specified range.".
-spec vet_fan_speed( rpm(), bin_string(), measurement_point_name(), sensor_id(),
                     wooper:state() ) -> boolean().
vet_fan_speed( Speed, _Desc, _PointNameBin, _SensorId, _State )
                                    when is_float( Speed ) ->
                                    % Useless: andalso Speed >= 0.0 ->
    true;

vet_fan_speed( _Speed, _Desc, _PointNameBin, _SensorId, _State )  ->
    false.



-doc """
Checks that the static information of specified temperature data is legit, and
returns it.
""".
-spec check_temperature_data( temperature_data() ) -> temperature_data().
check_temperature_data( TempData=#temperature_data{ status=disabled } ) ->
    TempData;

check_temperature_data( TempData=#temperature_data{
                                    input_attribute=InputAttr,
                                    description=Desc,
                                    status=enabled, % Check
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



-doc """
Checks that the static information of specified fan data is legit, and returns
it.
""".
-spec check_fan_data( fan_data() ) -> fan_data().
check_fan_data( FanData=#fan_data{ status=disabled } ) ->
    FanData;

check_fan_data( FanData=#fan_data{ input_attribute=InputAttr,
                                   description=Desc,
                                   status=enabled, % Check
                                   %type=Type,
                                   pulses=MaybePulses,
                                   %state=FanState,
                                   %last_spin_timestamp=LastSpinTimestamp,
                                   current=CurrentSpeed,
                                   min_reached=MinReachedSpeed,
                                   max_reached=MaxReachedSpeed,
                                   alarm_low=MaybeAlarmLow,
                                   alarm_high=MaybeAlarmHigh } ) ->

    type_utils:check_binaries( [ InputAttr, Desc ] ),

    type_utils:check_maybe_float( MaybePulses ),

    type_utils:check_floats(
        [ CurrentSpeed, MinReachedSpeed, MaxReachedSpeed ] ),

    type_utils:check_maybe_floats( [ MaybeAlarmLow, MaybeAlarmHigh ] ),

    FanData.



-doc """
Checks that the static information of specified intrusion data is legit, and
returns it.
""".
-spec check_intrusion_data( intrusion_data() ) -> intrusion_data().
check_intrusion_data( IntrusData=#intrusion_data{ status=disabled } ) ->
    IntrusData;

check_intrusion_data( IntrusData=#intrusion_data{
                                    input_attribute=InputAttr,
                                    description=Desc,
                                    status=enabled, % Check
                                    intrusion_reported=IntrusStatus,
                                    intrusion_timestamp=IntrusTimestamp,
                                    beep_on_intrusion=DoBeep } ) ->

    type_utils:check_binaries( [ InputAttr, Desc ] ),

    basic_utils:check_all_defined( [ IntrusStatus, IntrusTimestamp, DoBeep ] ),

    IntrusData.



-doc """
Parses the sensor JSON output stored in the specified file, returns a state with
a corresponding initial sensor table.
""".
-spec parse_sensor_output_from_file( file_path(), wooper:state() ) ->
                                            wooper:state().
parse_sensor_output_from_file( OutputFilePath, State ) ->

    file_utils:is_existing_file_or_link( OutputFilePath ) orelse
        begin

            ?error_fmt( "The file '~ts' from which sensor output is to be read "
                "does not exist (current directory: '~ts').",
                [ OutputFilePath, file_utils:get_current_directory() ] ),

            throw( { sensor_output_file_not_found, OutputFilePath } )

        end,

    % Better integrated than if using json_utils:from_json_file/1:
    BinJson = file_utils:read_whole( OutputFilePath ),

    parse_initial_sensor_output( BinJson, State ).



% Section related to the US-Main configuration files.


-doc """
Returns the known sensor-related keys in the US-Main configuration files.
""".
-spec get_licit_config_keys() -> [ list_table:key() ].
get_licit_config_keys() ->
    [ ?us_main_sensor_key ].


-doc """
Handles the sensor-related entries in the user settings specified in US-Main
configuration files.

Note that the specified state is the one of a US-Main configuration server.
""".
-spec manage_configuration( us_main_config_table(), wooper:state() ) ->
                                        wooper:state().
manage_configuration( ConfigTable, State ) ->

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






% Section for conversions to strings.


-doc "Returns a textual description of the specified temperature alert state.".
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



-doc "Returns a textual description of the specified fan state.".
-spec fan_state_to_string( fan_state() ) -> ustring().
fan_state_to_string( _State=nominal ) ->
    "nominal";

fan_state_to_string( _State=inactive ) ->
    "inactive";

fan_state_to_string( _State=dysfunctional ) ->
    "dysfunctional";

fan_state_to_string( _State=insufficient_speed ) ->
    "insufficient-speed";


fan_state_to_string( _State=excessive_speed ) ->
    "excessive-speed";

fan_state_to_string( _State=unknown ) ->
    "unknown".



-doc "Returns a textual description of the specified fan type.".
-spec fan_type_to_string( fan_type() ) -> ustring().
fan_type_to_string( _Type=fixed_speed ) ->
    "fixed-speed";

fan_type_to_string( _Type=pwm ) ->
    "PWM speed";

fan_type_to_string( _Type=unknown ) ->
    "unknown".



-doc "Returns a textual description of the specified measurement data.".
-spec point_data_to_string( point_data() ) -> ustring().
% First, temperature-related clauses:
point_data_to_string( #temperature_data{ input_attribute=InputAttr,
                                         description=Desc,
                                         status=disabled } ) ->
    text_utils:format( "a currently disabled temperature point (whereas read "
        "attribute would be '~ts' for ~ts)", [ InputAttr, Desc ] );

point_data_to_string( #temperature_data{ avg_count=0 } ) ->
    "a temperature point with no (plausible) measurement reported yet";

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

    text_utils:format( "a temperature point currently reporting ~ts "
        "(as read from the '~ts' attribute, for ~ts); ~ts "
        "(regarding measurements: min=~ts, max=~ts, average=~ts; "
        "regarding thresholds: alarm low=~ts, critical low=~ts, "
        "alarm high=~ts, critical high=~ts)",
        [ CurrentTempStr, InputAttr, Desc, AlertStr ] ++ OtherTempStrs );


% Then fan-related clauses:
point_data_to_string( #fan_data{ input_attribute=InputAttr,
                                 description=Desc,
                                 status=disabled } ) ->
    text_utils:format( "a currently disabled fan monitoring point "
        "(whereas read attribute would be '~ts' for ~ts)",
        [ InputAttr, Desc ] );


point_data_to_string( #fan_data{ input_attribute=InputAttr,
                                 description=Desc,
                                 % Implicit: status=enabled,
                                 type=FanType,
                                 pulses=MaybePulses,
                                 state=FanState,
                                 last_spin_timestamp=MaybeLastSpinTimestamp,
                                 current=CurrentSpeed,
                                 min_reached=MinSpeed,
                                 max_reached=MaxSpeed,
                                 avg_sum=AvgSum,
                                 avg_count=AvgCount,
                                 alarm_low=MaybeAlarmLow,
                                 alarm_high=MaybeAlarmHigh,
                                 beep_on_alarm=BeepOnAlarm } ) ->

    SpeedStr = "whose fan " ++ case MaybeLastSpinTimestamp of

        undefined ->
            "was never detected spinning";

        LastSpinTimestamp ->
            case math_utils:is_null( CurrentSpeed ) of

                true ->
                    Now = time_utils:get_timestamp(),
                    % Implied: not currently spinning.
                    "last spinned " ++ time_utils:get_textual_duration(
                        LastSpinTimestamp, Now ) ++ " ago";

                _False ->
                    text_utils:format( "is currently spinning at ~ts",
                        [ unit_utils:rpm_to_string( CurrentSpeed ) ] )

            end

    end ++ text_utils:format( " (as read, from the '~ts' attribute)",
                              [ InputAttr ] ) ,

    PulseStr = case MaybePulses of

        undefined ->
            "an unknown number of";

        Pulses ->
            text_utils:float_to_string( Pulses, [ { decimals, 1 }, compact ] )

    end ++ " generated pulses per revolution",

    DescStr = text_utils:format( "this fan, ~ts, is of type ~ts and "
        "is in ~ts state (with ~ts; beep on alarm: ~ts)",
        [ Desc, fan_type_to_string( FanType ), fan_state_to_string( FanState ),
          PulseStr, BeepOnAlarm ] ),

    % Count is non-null by design:
    Avg = AvgSum / AvgCount,

    SpeedStrs = [ unit_utils:maybe_rpm_to_string( S )
        || S <- [ MinSpeed, MaxSpeed, Avg, MaybeAlarmLow, MaybeAlarmHigh ] ],

    text_utils:format( "a fan monitoring point ~ts; ~ts; "
        "regarding measurements: min=~ts, max=~ts, average=~ts; "
        "regarding thresholds: alarm low=~ts, alarm high=~ts",
        [ SpeedStr, DescStr ] ++ SpeedStrs );


% Then intrusion clauses:
point_data_to_string( #intrusion_data{ input_attribute=InputAttr,
                                       description=Desc,
                                       status=disabled } ) ->
    text_utils:format( "a currently disabled intrusion detection point "
        "(whereas read attribute would be '~ts' for ~ts)",
        [ InputAttr, Desc ] );


point_data_to_string( #intrusion_data{
                                input_attribute=InputAttr,
                                description=Desc,
                                % Implicit: status=enabled,
                                intrusion_reported=false } ) ->
    text_utils:format( "an intrusion detection point that currently "
        "does not report any intrusion "
        "(as read from the '~ts' attribute, for ~ts)",
        [ InputAttr, Desc ] );


point_data_to_string( #intrusion_data{
                                input_attribute=InputAttr,
                                description=Desc,
                                % Implicit: status=enabled,
                                intrusion_reported=true,
                                intrusion_timestamp=IntrusTimestamp,
                                beep_on_intrusion=DoBeep } ) ->

    Now = time_utils:get_timestamp(),

    text_utils:format( "an intrusion detection point reporting that "
        "an intrusion happened ~ts ago (as read from the '~ts' attribute, "
        "for ~ts; beep on intrusion: ~ts)",
        [ time_utils:get_textual_duration( IntrusTimestamp, Now ), InputAttr,
          Desc, DoBeep ] ).



-doc "Returns a textual description of the specified measurement point table.".
-spec measurement_points_to_string( points_data_table() ) -> ustring().
measurement_points_to_string( PointsDataTable ) ->
    measurement_points_to_string( PointsDataTable, _IndentationLevel=0 ).



-doc "Returns a textual description of the specified measurement point table.".
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
                    [ text_utils:format( "'~ts', ~ts",
                        [ PN, point_data_to_string( TD ) ] )
                            || { PN, TD } <- MesurePairs ],
                    IndentationLevel ) ] )

    end.



-doc """
Returns a textual description of the specified intrusion detection status.
""".
-spec intrusion_status_to_string( intrusion_detected_status() ) -> ustring().
intrusion_status_to_string( _IntrusStatus=true ) ->
    "intrusion detected";

intrusion_status_to_string( _IntrusStatus=false ) ->
    "no intrusion detected".



-doc "Returns a textual description of the specified sensor identifier.".
-spec sensor_id_to_string( sensor_id() ) -> ustring().
sensor_id_to_string( { AtomSensorType, SensorInterface, SensorNumber } ) ->
    text_utils:format( "sensor of type ~ts, on interface ~ts, "
        "whose number is ~ts",
        [ AtomSensorType, SensorInterface, SensorNumber ] ).



-doc "Returns a textual description of the specified sensor information.".
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



-doc "Returns a textual description of the specified JSON triple.".
-spec json_triple_to_string( json_triple() ) -> ustring().
json_triple_to_string( { BinPointName, BinDesc, PointValueMap } ) ->
    text_utils:format( "measurement point '~ts' (~ts) "
        "whose value map is:~n  ~p", [ BinPointName, BinDesc, PointValueMap ] ).



-doc "Returns a textual description of the specified sensor table.".
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
                [ length( SensorInfos ), text_utils:strings_to_spaced_string(
                    [ sensor_info_to_string( SI ) || SI <- SensorInfos ] ) ] )

    end.



-doc "Returns a textual description of this manager.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    TrackStr = case ?getAttr(sensor_monitoring) of

        true ->
            text_utils:format( "tracking ~ts",
                [ sensor_table_to_string( ?getAttr(sensor_table) ) ] );

        _False ->
            "with no sensor tracking enabled"

    end,

    % No more scheduler_pid:
    SchedStr = case ?getAttr(task_id) of

        undefined ->
            "not known of the US main scheduler";

        TaskId ->
            text_utils:format( "known of the US main scheduler as task #~B",
                               [ TaskId ] )

    end,

    text_utils:format( "US sensor manager for '~ts', ~ts; ~ts",
        [ net_utils:localhost(), TrackStr, SchedStr ] ).
