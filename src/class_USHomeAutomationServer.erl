% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: Wednesday, November 23, 2022.

-module(class_USHomeAutomationServer).

-moduledoc """
US server in charge of **providing home automation services**, based on Enocean,
thanks to Ceylan-Oceanic.
""".


-define( class_description, "US server in charge of providing home "
         "automation services, based on Enocean, thanks to Ceylan-Oceanic" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% For settings regarding name registration:
-include("us_main_defines.hrl").

% For the event records:
-include_lib("oceanic/include/oceanic.hrl").


% Design notes:
%
% We rely here on Ceylan-Oceanic (https://oceanic.esperide.org/), which itself
% relies on our fork of erlang-serial
% (https://github.com/Olivier-Boudeville/erlang-serial).



% This home automation server is designed to be able to integrate to an OTP
% supervision tree thanks to a supervisor bridge, whose behaviour is directly
% defined in this module. See https://wooper.esperide.org/#otp-guidelines for
% further information.
%
-behaviour(supervisor_bridge).

% User API of the bridge:
-export([ start_link/0 ]).


% Callbacks of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).


% Exported helpers:
-export([ get_licit_config_keys/0, manage_configuration/2 ]).



-define( bridge_name, ?MODULE ).



-doc "US-Server for home automation.".
-type home_automation_server_pid() :: class_USServer:server_pid().


% For defines:
-include_lib("myriad/include/utils/time_utils.hrl").


% The default mean duration, in seconds, of a period of lighting:
% (15 minutes)
%
-define( default_mean_light_duration, 15*60 ).


% The default mean duration, in seconds, of an interruption of lighting:
-define( default_mean_no_light_duration, 12 ).



-doc "Describes a start/stop logical moment to simulate presence.".
-type presence_milestone() ::
    time().  % A time in the day
  % Actually these two are transparently managed by the server, if smart
  % lighting is enabled:
  %
  %| 'dawn'  % First light (if any) of the day
  %| 'dusk'. % Last light (if any) of the day



-doc """
A time slot during which a presence shall be simulated

The start milestone is included, the stop one is excluded.
""".
-type presence_slot() ::
    { Start :: presence_milestone(), Stop :: presence_milestone() }.



-doc """
An intra-day general program regarding a presence to simulate.

Corresponds to chronologically-ordered intra-day (from midnight to midnight)
time slots, or a constant policy, during which a presence shall be simulated.

If slots are used, at least one shall be defined (otherwise one of the
`constant_*` atoms shall be used).
""".
-type presence_program() :: [ presence_slot() ] % User-specified
                          | 'default_program'
                          | 'constant_presence'
                          | 'constant_absence'.



-doc """
Describes the (lighting) activity to be operated during a period of simulated
presence.

So tells whether, during a period of simulated presence, lighting shall be, at
random times, stopped and restarted after a random duration, to better simulate
a local presence (otherwise constant lighting happens):
""".
-type random_activity_settings() ::
    'true' % hence defaults
   | canon_random_activity_settings().



-doc """
Canonical description of the (lighting) activity to be operated during a period
of simulated presence.
""".
-type canon_random_activity_settings() ::

    'false'

    % Implies that random activity is enabled:
  | { % The mean duration, in seconds, of a period of lighting:
      MeanLightDuration :: second_duration(),

      % The mean duration, in seconds, of an interruption of lighting:
      MeanNoLightDuration :: second_duration() }.




-doc """
User-specified settings regarding the presence simulations to run.
""".
% A record would not be appropriate for user-level specifications:
-type presence_simulation_user_settings() :: [ psc_sim_user_setting() ].


-doc """
User-level configuration for a given presence simulation.

Note that the specified actuator trigger events correspond to the switching on
of that actuator (generally it is a smart plug) by a (generally double) rocker,
and it is supposed that the recipocal operation (switching it off the actuator)
is obtained by pushing the other button of the rocker.
""".
-type psc_sim_user_setting() ::
    { presence_program(),

      % Not necessarily canonicalised:
      TargetedPscActuators :: [ emitted_event_spec() ],

      % Tells whether lighting shall be switched off during a presence slot when
      % the light of day should be available (provided that the position of the
      % server is known):
      %
      SmartLighting :: boolean(),

      random_activity_settings() }

    % Random activity enabled, with default settings:
  | { presence_program(), TargetedPscActuators :: [ emitted_event_spec() ],
      SmartLighting :: boolean() }

    % Smart lighting and random activity enabled, with default settings:
  | { presence_program(), TargetedPscActuators :: [ emitted_event_spec() ] }.



-doc """
The time for dawn and dusk (if any - think to the extreme latitudes), at a given
(implicit) location and date.

Dawn and dusk are defined as actual transitions between darkness/daylight.
""".
-type celestial_timing() ::
    { Dawn :: option( time() ), Dusk :: option( time() ) }
  | 'constant_daylight' | 'constant_darkness'.



-doc """
The time for dawn and dusk (if any - think to the extreme latitudes), at a given
(implicit) location, for the specified date.
""".
-type celestial_info() :: { date(), celestial_timing() }.


-doc "Device operations, as extended by US-Main to take presence into account.".
-type extended_device_operation() :: oceanic:device_operation()
                                   | 'switch_on_if_at_home'
                                   | 'switch_off_if_at_home'
                                   | 'switch_on_if_away'
                                   | 'switch_off_if_away'.


-doc """
A table storing information regarding the scheduled periodical tasks on devices,
typically as triggered by actions.
""".
-type task_table() :: table( device_designator(), task_id() ).


-export_type([ home_automation_server_pid/0,
               home_automation_core_settings/0, home_automation_settings/0,
               presence_milestone/0, presence_slot/0, presence_program/0,
               random_activity_settings/0, presence_simulation_user_settings/0,
               psc_sim_user_setting/0,
               celestial_timing/0, celestial_info/0,
               extended_device_operation/0 ]).


% 15 minutes, in seconds:
%-define( default_alarm_duration, 15 * 60 ).

% For testing, 15 seconds:
-define( default_alarm_duration, 15 ).


% Type silencing:
-export_type([ telegram_info/0, device_splitter_table/0, task_table/0 ]).


% Local types:


-doc "Internal identifier of a registered presence simulation instance.".
-type presence_sim_id() :: count().



-doc "A pair of telegrams corresponding to a press/release transaction.".
-type telegram_pair() :: { Press :: telegram(), Release :: telegram() }.



-doc """
Information regarding a planned (presence) task, to be able to check/control it.
""".
-type task_info() :: { task_id(), time() }.



-doc """
An hour in the day or duration, as a floating-point value (e.g. 12.5 for 30
minutes past noon).
""".
-type decimal_hour() :: float().



-doc """
Time equation table, to correct sunrise/sunset times based on day rank.

A simpler indexed list would have been sufficient.

See also [https://en.wikipedia.org/wiki/Equation_of_time].
""".
-type time_equation_table() :: table( day_in_the_year(), decimal_hour() ).


% Originally in
% https://www.astrolabe-science.fr/wp-content/uploads/2023/02/EdTimcce.csv.
%
-define( time_equation_data_file, "time_equation_data.csv" ).





% Internal information regarding an instance of presence simulation:
-record( presence_simulation, {

    % The identifier of this presence simulation:
    id :: presence_sim_id(),

    % Tells whether this presence simulation is enabled:
    %
    % (as it may be disabled, for example when somebody is at home)
    %
    enabled = true :: boolean(),

    % Tells whether the presence simulation is currently activated, typically if
    % lighting is currently on or off (allows avoiding the sending of
    % unnecessary switching orders).
    %
    % Note that we consider that this server is the sole controller of the
    % target actuator; if not (e.g. should the user be able to switch on/off a
    % given lamp thanks to a physical button), we may:
    %  - either force states (setting them explicitly regardless of their
    %  inferred status), and do that more frequently (e.g. not jumping over
    %  milestones known to have no effect)
    %  - or, better, listen to Oceanic events for notifications about state
    %  changes, and update our knowledge according to unexpected ones
    %
    % Setting it by default to false, to force any needed activation, should it
    % be not set by mistake:
    %
    activated = false :: boolean(),


    % Specifies the target actuators to be reached in order to simulate an
    % actual presence (typically a smart plug controlling a lamp):
    %
    % (possibly using a broadcast address, typically if no EURID is known for
    % it)
    %
    actuator_event_specs :: [ canon_emitted_event_spec() ],


    % The general daily program of this presence simulation, possibly as a
    % chronologically-ordered intra-day (from midnight to midnight) list of
    % presence slots:
    %
    program :: presence_program(),


    % The next planned action (if any) that shall happen:
    % Not used, as relying now on a "stateless" algorithm.
    %next_action :: option( { timestamp(), presence_action() } ),


    % Tells whether lighting shall be switched off during a presence slot when
    % the light of day should be available (provided that the geographical
    % position of the server is known):
    %
    smart_lighting = 'true' :: boolean(),


    % Tells whether, during a period of simulated presence, lighting shall be,
    % at random times, stopped and restarted after a random duration, to better
    % simulate a local presence (otherwise constant lighting happens):
    %
    random_activity = 'false' : canon_random_activity_settings(),


    % The identifier and planned time of the currently-pending scheduling task
    % (if any) declared to manage that presence during the current day:
    %
    presence_task_info :: option( task_info() ) } ).



-doc """
Internal information regarding an instance of presence simulation.

Counterpart of the user-level `psc_sim_setting/0`.
""".
-type presence_simulation() :: #presence_simulation{}.



-doc "A table keeping track of the known presence simulations.".
-type presence_table() :: table( presence_sim_id(), presence_simulation() ).



-doc """
A (higher-level) status of a device, based on its type, as known by this server.
""".
-type device_status() :: thermometer_status()
                       | thermo_hygro_sensor_status()
                       | motion_detector_status()
                       | motion_detector_with_illumination_status()
                       | opening_detector_status()
                       | push_button_status()
                       | double_rocker_status()
                       | in_wall_module_status()
                       | smart_plug_status().


-doc "State of a thermometer.".
-type thermometer_status() :: temperature().


-doc "State of a thermo-hygro sensor.".
-type thermo_hygro_sensor_status() :: { temperature(), relative_humidity() }.


-doc "State of a standard motion detector.".
-type motion_detector_status() :: { MotionDetected :: boolean(),
    MaybeVoltage :: option( unit_utils:volts() ) }.


-doc "State of a motion detector with illumination.".
-type motion_detector_with_illumination_status() ::
    { MotionDetected :: boolean(), MaybeVoltage :: option( unit_utils:volts() ),
      MaybeIlluminance :: option( unit_utils:lux() ) }.



-doc "State of an opening detector (single contact).".
% Rather than 'opened' | 'closed':
-type opening_detector_status() :: oceanic:contact_status().


-doc "State of a push button.".
-type push_button_status() :: button_state().


-doc "State of the 4 buttons of a double rocker.".
-type double_rocker_status() :: { AI :: button_state(), AO :: button_state(),
                                  BI :: button_state(), BO :: button_state() }.


-doc "State of an in-wall module.".
-type in_wall_module_status() :: tuploid().


-doc "State of a smart plug.".
% Mostly by decreasing order of interest:
-type smart_plug_status() :: { power_report(),
                               PowerFailureDetected :: boolean(),
                               SwitchedOffDueToOvercurrent :: boolean(),
                               hardware_status(),
                               LocalControlEnabled :: boolean() }.



-doc "Information about telegram(s) already prepared for a given device.".
-type telegram_info() ::

    % Just a single telegram, for a single event:
    telegram()

    % A pair of telegrams (e.g. for press/release events):
  | telegram_pair()

    % For two actions (e.g. activation/deactivation):
  | { telegram_pair(), telegram_pair() }.



% Internal information regarding a known (Enocean) device:
%
% (note: if modifying fields, update list_devices_ordered/1 accordingly)
%
-record( device_state, {

    % The identifier of this device:
    eurid :: eurid(),

    % For example: <<"Opening detector of the front door">>:
    name :: device_name(),

    % Any user-defined short device name:
    short_name :: option( device_short_name() ),

    % Any splitter available to designate that device, from any short name
    % otherwise its EURID (the full name not being relevant for that):
    %
    splitter :: option( splitter() ),

    % Any determined higher-level type for this device:
    type :: option( device_type() ),

    % The set of the EEP identifiers already detected for this device
    % (e.g. 'double_rocker_multipress'):
    %
    eep_ids = set_utils:new() :: set_utils:set( eep_id() ),

    % Records the initial event (if any) received regarding this device (a
    % configured one being first seen, a taught-in one, or one not known a
    % priori but discovered):
    %
    initial_event :: option( device_event() ),

    % Records the last event (if any) received regarding this device:
    last_event :: option( device_event() ),

    % Records any last time this device was seen by Oceanic (separate from
    % last_event, as for example a "device being lost" message is not a device
    % event as such):
    %
    last_seen :: option( timestamp() ),

    % Records any last reported availability status of this device: online,
    % lost, unreachable.
    %
    % (device might be as well only configured)
    %
    availability :: option( availability_status() ),

    % Any currently known status for this device:
    current_status :: option( device_status() ),

    % Tells whether this device (e.g. a smart plug) has been taught to this
    % gateway, i.e. if a teach-in procedure apparently succeeded with no further
    % teach-out (therefore this gateway is expected to be registered to this
    % device):
    %
    taught = false :: boolean()

    % Any pre-forged telegram(s) of interest:
    %telegram_info :: option( telegram_info() ),

    % The acknowledgements of commands that are currently waited for, for this
    % device:
    %
    % (better to integrate at the Oceanic level)
    %
    %waited_acks = [] :: [ waited_ack_info() ]

} ).


-doc "Internal information regarding a known (Enocean) device.".
-type device_state() :: #device_state{}.



-doc """
A table recording the information regarding the Enocean devices known by this
automation server, notably in order to detect state transitions and trigger
events.
""".
-type device_table() :: table( eurid(), device_state() ).


-doc """
A table able to translate a resolved device splitter into the EURID of the
corresponding device.
""".
-type device_splitter_table() :: table( bin_string(), eurid() ).


-doc "Core settings gathered for the home automation server.".
-type home_automation_core_settings() :: {
    AlarmTriggerListenEvSpecs :: [ canon_listened_event_spec() ],
    AlarmActuatorEmitEvSpecs :: [ canon_emitted_event_spec() ],
    PscTriggerListenEvSpecs :: [ canon_listened_event_spec() ],
    presence_simulation_user_settings(),
    HAActionSpecs :: [ user_action_spec() ], oceanic_settings() }.



-doc """
Full settings gathered regarding the home automation server.
""".
-type home_automation_settings() :: {

    ServerLocation :: option( user_server_location() ),
    BinAppBaseDirectoryPath :: bin_directory_path(),

    % Same as home_automation_core_settings():
    AlarmTriggerListenEvSpecs :: [ canon_listened_event_spec() ],
    AlarmActuatorEmitEvSpecs :: [ canon_emitted_event_spec() ],
    PscTriggerListenEvSpecs :: [ canon_listened_event_spec() ],
    presence_simulation_user_settings(),
    HAActionSpecs :: [ user_action_spec() ],
    oceanic_settings() }.



% To define the actual location of a US-Main server.
%
% The value associated to this key has for type
% [class_USMainCentralServer:user_server_location()].
%
-define( us_main_server_location_key, server_location ).


% Designates the specification of events that are listened to by this server in
% order to decide to switch the alarm on/off.
%
% Typically describes state change events from opening detectors, from push
% buttons or double rockers for manual control, etc. (one may refer to
% oceanic:device_state_change_spec()).
%
% The value associated to this key has for type [oceanic:listened_event_spec()].
%
-define( us_main_alarm_triggers_key, alarm_triggers ).


% The specification of events that are to be emitted to by this server in order
% to actually trigger alarm actuators, typically to switch alarms on/off (one
% may refer to oceanic:emitted_event_spec()).
%
% In practice, typically targets smart plugs that power sirens, and describes
% actions from pseudo-devices (emulated by this server), typically push buttons
% or double rockers, that the target actuator already learnt.
%
% The value associated to this key has for type [oceanic:emitted_event_spec()].
%
-define( us_main_alarm_actuators_key, alarm_actuators ).



% Designates the specification of events that are listened to by this server in
% order to decide to switch the current presence status between somebody or
% nobody at home.
%
% Typically describes state change events from push buttons or double rockers
% (one may refer to oceanic:device_state_change_spec()).
%
% The value associated to this key has for type [oceanic:listened_event_spec()].
%
-define( us_main_presence_triggers_key, presence_switching_triggers ).


% The specification of events that are to be emitted to by this server in order
% to actually switch presence actuators (one may refer to
% oceanic:emitted_event_spec()).
%
% In practice, typically targets smart plugs that power lights, and describes
% actions from pseudo-devices (emulated by this server), typically push buttons
% or double rockers, that the target actuator already learnt.
%
% The value associated to this key has for type [oceanic:emitted_event_spec()].
%
-define( us_main_presence_actuators_key, presence_switching_actuators ).


% The other settings for presence simulation, including notably its program.
%
% The value associated to this key has for type
% presence_simulation_user_settings().
%
-define( us_main_presence_settings_key, presence_simulation_settings ).


% The settings of the automated actions that shall be supported for home
% automation:
%
-define( us_main_home_automation_actions_key, home_automation_action_specs ).

% All known, licit (top-level) keys for the Oceanic configuration information
% (preferred to be read directly from the US-Main configuration file rather than
% from a separate Myriad preferences file):
%
-define( supported_oceanic_config_keys,
         [ oceanic_emitter, oceanic_devices, oceanic_jamming_threshold ] ).



% The class-specific attributes:
-define( class_attributes, [

    { oc_srv_pid, option( oceanic_server_pid() ),
      "the PID of the Oceanic server (if any can exist) used by this server; "
      "it is stored rather than fetched from the naming service as this server "
      "manages its life-cycle" },

    { oc_periodic_restart, boolean(), "tells whether Oceanic shall be "
      "periodically restarted, in order to overcome any risk of freeze of "
      "the USB-based serial interface" },

    { oc_src_eurid, option( eurid() ),
      "the source identifier (if any) to specify when generating emitted "
      "packets; this ought to be the EURID of the local Enocean gateway, "
      "as determined by Oceanic, otherwise telegrams are bound to be "
      "rejected" },

    { app_base_directory, bin_directory_path(),
      "the base directory of the US-Main application (the root where "
      "src, priv, ebin, etc. can be found)" },

    { alarm_inhibited, boolean(),
      "tells whether the alarm is inhibited, permanently, if somebody is at "
      "home - thus no opening matters - or temporarily, so that one has a "
      "chance of leaving home without triggering the alarm)" },

    { alarm_triggered, boolean(), "tells whether the alarm is currently "
      "activated (i.e. whether a siren is expected to be roaring)" },


    { alarm_trigger_specs, [ canon_listened_event_spec() ],
      "the specifications of the events that can be received from devices "
      "(typically push buttons or double rockers) that may be used to directly "
      "switch on/off the alarm" },

    { alarm_actuator_specs, [ canon_emitted_event_spec() ],
      "the specifications of the events that can be emitted to "
      "actuators (typically smart plugs powering sirens) that shall be "
      "triggered when an actual alarm is activated" },


    { alarm_duration, seconds(),
      "the base duration of an alarm, once triggered; can be shortened "
      "by alarm-inhibiting devices" },

    { alarm_stop_task_id, option( task_id() ),
      "the identifier of a scheduler task (if any) whose purpose is to "
      "stop an alarm after a fixed duration once triggered" },


    { actual_presence, boolean(), "tells whether there is someone at home; "
      "in this case, no specific lighting or alarm shall apply" },

    % Better defined separately from presence_table, as the program shall
    % remain, whereas presence simulation service may be regularly
    % enabled/disabled; different also from actual_presence:
    %
    { presence_simulation_enabled, boolean(),
      "tells whether the presence simulation service is currently enabled" },

    { presence_table, presence_table(),
      "registery of the presence simulations, recording all their settings" },

    { next_presence_id, presence_sim_id(),
      "the next presence identifier that will be assigned" },

    { time_equation_table, option( time_equation_table() ),
      "a table (if any) allowing to correct sunrise/sunset times, for smart "
      "lighting" },

    { midnight_task_id, option( task_id() ),
      "the identifier of any task to be triggered, if the presence simulation is
      activated, each day at midnight to determine and update the activity of
      the " "presence simulations for the next day (and to ensure that any "
      "potential switching discrepancy does not linger)" },

    { oceanic_monitor_task_id, option( task_id() ),
      "the identifier of any task periodically triggered in order to monitor "
      "Oceanic, notably to detect any freeze of the serial port and/or to "
      "restart the serial interface)" },

    { server_location, option( position() ),
      "the (geographic) location, as a position, of this US-Main server" },

    { presence_switching_trigger_specs, [ canon_listened_event_spec() ],
      "the specifications of the events that can be received from devices "
      "(typically push buttons or double rockers) that may be used to directly "
      "switch on/off the presence status" },

    { presence_switching_device_desc, option( bin_string() ),
      "a textual description of the presence switching devices (if any)" },

    { celestial_info, option( celestial_info() ),
      "any precomputed dawn/dusk time, for the current day" },

    { task_table, task_table(),
      "a table storing information regarding the current, action-based "
      "scheduled periodical tasks" },

    { device_table, device_table(),
      "the table recording the current state of devices, notably to "
      "detect their state transitions" },

    { device_spell_tree, option( spell_tree() ),
      "the spell tree resolving device prefixes into their designators" },

    { dev_splitter_table, device_splitter_table(),
      "a table keeping track of the splitters of the devices that can be "
      "user-designated (typically actuators)" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Main.Home Automation" ).


% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").




% Implementation notes:


% Regarding presence simulation:
%
% The various presence periods in a slot may overlap (typically because the time
% of dusk and dawn varies in the course of the year); anyway the presence will
% be simulated in all cases with no interruption.
%
% A robust mode of operation has been retained, with which states are enforced
% (e.g smart plug is on) rather than transitions (e.g. toggling smart plug
% "blindly" on->off or off->on).


% Regarding the computation of the time in the day of dawn and dusk:
%
% The duration of a given day depends on the date and latitude of the location
% of interest; the actual moments for dawn and dusk depend also on longitude.
%
% More information (in French):
% https://www.astrolabe-science.fr/duree-du-jour-et-latitude/. Many thanks to
% David Alberto for the sharing.


% Regarding scheduling:
%
% Each presence simulation is planned from the current event to (only) the next.
% As soon there is at least one presence simulation, an additional overall
% update (daily, at midnight) is scheduled, so that the programs for this new
% day are established.


% Regarding the Enocean actuators:
%
% We suppose that these devices already learnt the USB gateway being used by
% Oceanic. This server will act upon each of these actuators as if it was a
% double-rocker switch (e.g. not two single-contact buttons), whose 'on' button
% is button_ao, and whose 'off' button is button_ai.



% TO-DO (by decreasing priorities):
%
% - support safe commands, with waited acks and retries (to do in Oceanic, and
% to integrate here)
% - make control-us-main.sh commands unconditional (regardless of the current
% state supposed by US-Main), typicall to start/stop lighting, alarm
% - check that no decoding freeze happens (blocking accumulating chunk), and add
% an automatic unblocking mechanism (forget longer/remaining chunks)
% - have alarms triggers all lighting
% - auto-stop alarm (e.g. 15 minutes after being activated)
% - auto-enable alarm after a user-specified delay (e.g. 8 minutes after
% reporting that leaving home)
% - support inquiry and control by SMS (with Ceylan-Mobile)
% - support movement detectors
% - add audio output if away and suspicions
% - support *multiple* presence actuators

% - ensure all Oceanic logs are reported in US-Main traces
% - possibly: allow testing the availability of Oceanic/serial from
% control-us-main.sh, via 'report'



% Type shorthands:

-type count() :: basic_utils:count().
-type user_data() :: basic_utils:user_data().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type trace_format() :: text_utils:trace_format().
-type trace_values() :: text_utils:trace_values().

-type splitter() :: spell_tree:splitter().
-type spell_tree() :: spell_tree:spell_tree().

-type device_path() :: file_utils:device_path().
-type bin_directory_path() :: file_utils:bin_directory_path().

-type tuploid() :: type_utils:tuploid().

-type bytes_per_second() :: system_utils:bytes_per_second().

-type date() :: time_utils:date().
-type time() :: time_utils:time().
-type timestamp() :: time_utils:timestamp().
-type second_duration() :: time_utils:second_duration().
-type day_in_the_year() :: time_utils:day_in_the_year().

-type extended_timestamp() :: unit_utils:extended_timestamp().
-type milliseconds() :: unit_utils:milliseconds().
-type relative_humidity() :: unit_utils:relative_humidity().

-type lookup_info() :: naming_utils:lookup_info().

-type trace_severity() :: trace_utils:trace_severity().
-type trace_message() :: trace_utils: trace_message().


-type position() :: unit_utils:position().
-type declination() :: unit_utils:declination().
-type radians() :: unit_utils:radians().
-type temperature() :: unit_utils: temperature().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().
-type user_periodicity() :: class_USScheduler:user_periodicity().
-type schedule_count() ::  class_USScheduler:schedule_count().
-type task_id() :: class_USScheduler:task_id().


-type oceanic_server_pid() :: oceanic:oceanic_server_pid().
-type device_name() :: oceanic:device_name().
-type device_short_name() :: oceanic:device_short_name().
-type device_type() :: oceanic:device_type().
-type user_device_designator()  :: oceanic:user_device_designator().
-type device_designator()  :: oceanic:device_designator().
-type device_description() :: oceanic:device_description().
-type device_event() :: oceanic:device_event().
-type unresolved_device_event() :: oceanic:unresolved_device_event().
-type device_event_type() :: oceanic:device_event_type().
-type device_operation() :: oceanic:device_operation().
-type back_online_info() :: oceanic:back_online_info().
-type eurid_string() :: oceanic:eurid_string().
-type eurid() :: oceanic:eurid().
-type telegram() :: oceanic:telegram().
-type eep_id() :: oceanic:eep_id().
-type button_state() :: oceanic:button_state().
-type power_report() :: oceanic:power_report().
-type hardware_status() :: oceanic:hardware_status().
-type availability_status() :: oceanic:availability_status().
-type device_state_info() :: oceanic:device_state_info().

-type canon_listened_event_spec() :: oceanic:canon_listened_event_spec().
-type canon_emitted_event_spec() :: oceanic:canon_emitted_event_spec().
-type emitted_event_spec() :: oceanic:emitted_event_spec().
-type oceanic_settings() :: oceanic:oceanic_settings().

-type us_main_config_table() ::
    class_USMainCentralServer:us_main_config_table().

-type user_server_location() ::
    class_USMainCentralServer:user_server_location().

-type user_action_spec() :: us_action:user_action_spec().
-type user_action_specs() :: us_action:user_action_specs().
%-type action_outcome() :: us_action:action_outcome().
%-type action_outcome( ReturnT ) :: us_action:action_outcome( ReturnT ).



% Implementation of the supervisor_bridge behaviour, for the intermediate
% process allowing to interface this home automation server with an OTP
% supervision tree.


-doc """
Starts and links a supervision bridge for the home automation system.

Note: typically spawned as a supervised child of the US-Main root supervisor
(see `us_main_sup:init/1`), hence generally triggered by the application
initialisation.
""".
-spec start_link() -> term().
start_link() ->

    % Apparently not displayed in a release context, yet executed:
    trace_bridge:debug( "Starting the US-Main supervisor bridge for "
                        "the home automation system." ),

    % Call next init/1:
    supervisor_bridge:start_link( { local, ?bridge_name }, _Module=?MODULE,
                                  _InitArgs=[] ).



-doc """
Callback to initialise this supervisor bridge, typically in answer to
`start_link/0` above being executed.
""".
-spec init( list() ) -> { 'ok', pid(), State :: term() } | 'ignore'
                      | { 'error', Error :: term() }.
init( _Args=[] ) ->

    trace_bridge:info_fmt( "Initialising the US-Main supervisor bridge ~w for "
                           "the home automation system.", [ self() ] ),

    % Not specifically synchronous:
    HomeAutomSrvPid = ?MODULE:new_link(),

    { ok, HomeAutomSrvPid, _InitialBridgeState=HomeAutomSrvPid }.



-doc "Callback to terminate this supervisor bridge.".
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _BridgeState=HomeAutomSrvPid )
                                when is_pid( HomeAutomSrvPid ) ->

    trace_bridge:info_fmt( "Terminating the US-Main supervisor bridge for "
        "the home automation system (reason: ~w, "
        "home automation server: ~w).", [ Reason, HomeAutomSrvPid ] ),

    % Synchronicity needed, otherwise a potential race condition exists, leading
    % this process to be killed by its OTP supervisor instead of being normally
    % stopped:
    %
    wooper:delete_synchronously_instance( HomeAutomSrvPid ),

    trace_bridge:debug_fmt( "US-Main home automation server ~w terminated.",
                            [ HomeAutomSrvPid ] ).




% Actual implementation of the home automation server.


-doc """
Constructs an home automation server, based on the default, local TTY allocated
to the USB Enocean gateway, whose base identifier will be used as source EURID
for telegram sendings, and not performing presence simulation.

This is the actual constructor being called first, by the OTP supervision logic.
""".
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
    construct( State, _TtyPath=oceanic:get_default_tty_path() ).



-doc """
Constructs an home automation server, based on the specified local TTY allocated
to the USB Enocean gateway, base identifier will be used as source EURID
for telegram sendings, and not performing presence simulation.
""".
-spec construct( wooper:state(), device_path() ) -> wooper:state().
construct( State, TtyPath ) ->
    % Undefined and empty list have not exactly the same semantics here:
    construct( State, TtyPath, _MaybePscSimUserSettings=undefined ).



-doc """
Constructs an home automation server, based on the specified local TTY allocated
to the USB Enocean gateway.

Unless PscSimUserSettings is the empty list, presence simulation will be
performed, specified as a complete list of presence user settings; as usual, the
source used for the sent telegrams will be the base identifier of the gateway.
""".
-spec construct( wooper:state(), device_path(),
        option( presence_simulation_user_settings() ) ) -> wooper:state().
construct( State, TtyPath, MaybePscSimUserSettings ) ->
    construct( State, TtyPath, MaybePscSimUserSettings,
               _MaybeSourceEuridStr=undefined ).



-doc """
Constructs an home automation server, based on the specified local TTY allocated
to the USB Enocean gateway.

Unless PscSimUserSettings is the empty list, presence simulation will be
performed, specified as a complete list of presence user settings; any specified
source identifier will be used for the sent telegrams (in general this should be
the base identifier of the gateway, which is the default).

(only actual constructor)
""".
-spec construct( wooper:state(), device_path(),
        option( presence_simulation_user_settings() ),
                        option( eurid_string() ) ) -> wooper:state().
construct( State, TtyPath, MaybePscSimUserSettings, MaybeSourceEuridStr ) ->

    ServerTraceName = "Home automation server",

    % First the direct mother classes, then this class-specific actions:
    SrvState = class_USServer:construct( State,
        ?trace_categorize(ServerTraceName),
        ?us_main_home_automation_server_registration_name,
        ?us_main_home_automation_server_registration_scope ),

    % Common to all home-automation services; beware of blocking calls:
    class_USMainCentralServer:get_server_pid() !
        { getHomeAutomationSettings, [], self() },

    % Do not start Oceanic if it is bound to fail:
    { MaybeOcSrvPid, MaybeSrcEurid } = case oceanic:is_available( TtyPath ) of

        { true, _SerialRootDir } ->
            OcPid = oceanic:start_link( TtyPath, [ _EventListenerPid=self() ] ),

            % Wanting that the traces emitted by Oceanic are collected in our
            % Ceylan-Traces system, rather than being buried just in, typically,
            % /opt/universal-server/us_main-latest/us_main/log/erlang.log.*
            % files:
            %
            OCBridgeSpec = trace_bridge:get_bridge_spec(
                _TraceEmitterName="Oceanic", _TraceCategory="Server",
                _BridgePid=class_TraceAggregator:get_aggregator() ),

            OcPid ! { registerTraceBridge, OCBridgeSpec },

            SrcEurid = case MaybeSourceEuridStr of

                undefined ->
                    _BaseEurid=oceanic:get_oceanic_eurid( OcPid );

                % No spoofing can be easily done, if emitting with a bogus
                % source EURID expect telegrams not to be processed or even
                % emitted:
                %
                SourceEuridStr ->
                    oceanic_text:string_to_eurid( SourceEuridStr )

            end,

            { OcPid, SrcEurid };

        { false, ReasonStr, ErrorTerm } ->
            % An home automation server is created by the US-Main server
            % unconditionally (the US-Main configuration file is not yet located
            % and parsed), so the message must not suggest it was or was not
            % requested.
            %
            % No house automation can be done then (newline needed, otherwise
            % bad formatting):
            %
            ?send_warning_fmt( SrvState,
                "No Oceanic support will be available: ~ts~n"
                "(error term: ~p).", [ ReasonStr, ErrorTerm ] ),

            { undefined, undefined }

    end,


    % Interleaved getHomeAutomationSettings call; most elements already
    % canonicalised, except MaybeConfPscSimUSettings:
    %
    { MaybeUserSrvLoc, BinAppBaseDirectoryPath,
      AlarmTriggerListenEvSpecs, AlarmActuatorEmitEvSpecs,
      PscTriggerListenEvSpecs, ConfPscSimUSettings, UserActSpecs,
      OcSettings } = receive

        { wooper_result, HomeAutoMatSettings } ->
            HomeAutoMatSettings

    end,

    % First possible moment to read all oceanic_* configuration keys, and before
    % the definition of alarm and all, which would otherwise log unknown
    % devices:
    %
    MaybeOcSrvPid =:= undefined orelse
        begin

            % Here we send to Oceanic the settings that are read as a whole from
            % the US-Main configuration file (this includes all configured
            % devices):
            %
            oceanic:add_configuration_settings( OcSettings, MaybeOcSrvPid ),

            % US-Main has also to know the devices that are initially configured
            % in Oceanic, otherwise we would not be able to act upon for example
            % a smart plug, unless it sent for some reason a telegram first.
            %
            % Rather than having US-Main process in parallel of Oceanic the same
            % data, we request them once it processed them:
            %
            % (oneway, for asynchronicity; will trigger back a call to
            % notifyDeviceStateInfos/2)
            %
            MaybeOcSrvPid ! { getAllDeviceStateInfo, self() }

        end,

    AlarmState = init_alarm( AlarmTriggerListenEvSpecs,
        AlarmActuatorEmitEvSpecs, MaybeOcSrvPid, SrvState ),

    % Any constructor-level settings (usually not specified) will take priority
    % over in-configuration ones:
    %
    RetainedPscSimUSettings = case MaybePscSimUserSettings of

        % Most likely case here, the (in-file) configuration applies:
        undefined ->
            case ConfPscSimUSettings of

                [] ->
                    send_psc_trace( info, "Neither construction-level nor "
                        "configuration-level presence simulation settings "
                        "defined, no presence simulation will be done.",
                                    AlarmState );

                ConfPscSimUSettings->
                    send_psc_trace_fmt( info, "Configuration-level presence "
                        "simulation settings will apply "
                        "(as no construction-level ones were defined):~n ~p",
                        [ ConfPscSimUSettings ], AlarmState )

            end,

            ConfPscSimUSettings;


        % Here constructor-level settings will apply:
        PscSimUserSettings when is_list( PscSimUserSettings ) ->
            case ConfPscSimUSettings of

                [] ->
                    send_psc_trace_fmt( info, "Construction-level presence "
                        "simulation settings will apply (and no "
                        "configuration-level were defined):~n ~p",
                        [ PscSimUserSettings ], AlarmState );

                ConfPscSimUSettings ->
                    send_psc_trace_fmt( info, "The construction-specified "
                        "presence simulation settings (~p) will take "
                        "precedence over the configuration-specified "
                        "ones (~p).",
                        [ PscSimUserSettings, ConfPscSimUSettings ],
                        AlarmState )

            end,

            ConfPscSimUSettings;


        Other ->
            throw( { invalid_constructor_presence_settings, Other } )

    end,

    ActionState = init_automated_actions( UserActSpecs, AlarmState ),


    % Geographical location:
    MaybeSrvLoc = case MaybeUserSrvLoc of

        undefined ->
            undefined;

        % Already checked as floats by the US-Main configuration server; these
        % are degrees:
        %
        _UserSrvLoc={ Lat, _Long } when Lat > 90.0 orelse Lat < -90.0 ->
            throw( { invalid_latitude, Lat } );

        _UserSrvLoc={ _Lat, Long } when Long > 180.0 orelse Long < -180.0 ->
            throw( { invalid_longitude, Long } );

        UserSrvLoc ->
            UserSrvLoc

    end,

    % PscTriggerListenEvSpecs already canonicalised:
    PscSwitchBinDesc = case PscTriggerListenEvSpecs of

        [] ->
            <<"no presence-switching device is defined">>;

        _ ->
            case MaybeOcSrvPid of

                % Suspect:
                undefined ->
                    text_utils:bin_format( "the presence-switching "
                        "devices defined are: ~ts", [
                            oceanic_text:canon_listened_event_specs_to_string(
                                PscTriggerListenEvSpecs ) ] );

                OcSrvPid ->
                    text_utils:bin_format( "~B presence-switching devices "
                        "are defined: ~ts", [ length( PscTriggerListenEvSpecs ),
                            oceanic_text:canon_listened_event_specs_to_string(
                                PscTriggerListenEvSpecs, OcSrvPid ) ] )

            end

    end,

    EmptyTable = table:new(),

    MoreCompleState = setAttributes( ActionState, [
        { oc_srv_pid, MaybeOcSrvPid },

        %{ oc_periodic_restart, true },
        { oc_periodic_restart, false },

        { oc_src_eurid, MaybeSrcEurid },
        { app_base_directory, BinAppBaseDirectoryPath },

        % Expecting to be launching this server while being at home:
        { actual_presence, true },
        % If ever needing to force an initial away status:
        %{ actual_presence, false },

        { device_table, EmptyTable },
        { device_spell_tree, undefined },
        { dev_splitter_table, EmptyTable } ] ),

    { InitPscTable, NextPscId, MaybeTimeEqTable, MaybeMidnightTaskId } =
        init_presence_simulation( RetainedPscSimUSettings, MaybeOcSrvPid,
                                  MoreCompleState ),

    MaybeOcMonTaskId = init_oceanic_monitor( MaybeOcSrvPid, MoreCompleState ),

    InitialPscEnabled = not table:is_empty( InitPscTable ),

    SetState = setAttributes( MoreCompleState, [
        { presence_simulation_enabled, InitialPscEnabled },
        { presence_table, InitPscTable },
        { next_presence_id, NextPscId },
        { time_equation_table, MaybeTimeEqTable },
        { midnight_task_id, MaybeMidnightTaskId },
        { oceanic_monitor_task_id, MaybeOcMonTaskId },
        { server_location, MaybeSrvLoc },
        { presence_switching_trigger_specs, PscTriggerListenEvSpecs },
        { presence_switching_device_desc, PscSwitchBinDesc },
        { celestial_info, undefined },
        { task_table, table:new() } ] ),

    ApplyState = apply_presence_simulation( SetState ),

    ?send_notice_fmt( ApplyState, "Constructed: ~ts",
                      [ to_string( ApplyState ) ] ),

    ApplyState.



-doc "Initialises the alarm system.".
-spec init_alarm( [ canon_listened_event_spec() ],
        [ canon_emitted_event_spec() ], option( oceanic_server_pid() ),
        wooper:state() ) -> wooper:state().
init_alarm( _AlarmTriggerListenEvSpecs, _AlarmActuatorEmitEvSpecs,
            _MaybeOcSrvPid=undefined, State ) ->

    send_alarm_trace( info, "No Oceanic support available, no alarm managed.",
                      State ),

    setAttributes( State, [
        { alarm_inhibited, true },
        { alarm_triggered, false },
        { alarm_trigger_specs, [] },
        { alarm_actuator_specs, [] },
        { alarm_duration, ?default_alarm_duration },
        { alarm_stop_task_id, undefined } ] );

init_alarm( AlarmTriggerListenEvSpecs, AlarmActuatorEmitEvSpecs, _OcSrvPid,
            State ) ->

    % Second, if alarm trigger buttons are set, their references shall be stored
    % as such:
    %
    setAttributes( State, [
        { alarm_inhibited, true },

        % Initially quiet:
        { alarm_triggered, false },

        % Already vetted:
        { alarm_trigger_specs, AlarmTriggerListenEvSpecs },
        { alarm_actuator_specs, AlarmActuatorEmitEvSpecs },

        { alarm_duration, ?default_alarm_duration },
        { alarm_stop_task_id, undefined } ] ).



-doc "Initialises the overall presence simulation.".
-spec init_presence_simulation( presence_simulation_user_settings(),
            option( oceanic_server_pid() ), wooper:state() ) ->
        { presence_table(), presence_sim_id(), option( time_equation_table() ),
          option( task_id() ) }.
init_presence_simulation( _PscSimUSettings=[], _MaybeOcSrvPid, State ) ->

    send_psc_trace( info, "No presence simulation wanted, nothing done.",
                    State ),

    { _EmptyPscTable=table:new(), _NextPscId=1, _TimeEqTableNeeded=false,
      _MaybeMidTaskId=undefined };


init_presence_simulation( PscSimUSettings, _MaybeOcSrvPid=undefined, State ) ->
    send_psc_trace_fmt( error, "No Oceanic support available, the requested "
        "presence simulation (~p) cannot be performed.",
        [ PscSimUSettings ], State ),

    throw( { no_presence_simulation, no_oceanic } );


init_presence_simulation( PscSimUSettings, OcSrvPid, State ) ->

    ?getAttr(oc_src_eurid) =/= undefined orelse
        begin
            send_psc_trace_fmt( error, "No base gateway EURID available, "
                "the requested presence simulation (~p) cannot be performed.",
                [ PscSimUSettings ], State ),

            throw( { no_presence_simulation, no_base_eurid } )
        end,

    % Already reported:
    %send_psc_trace_fmt( debug, "Initialising presence simulation, "
    %   "from following settings:~n ~p.", [ PscSimUSettings ], State ),

    init_presence_simulation( PscSimUSettings, OcSrvPid, _PscTable=table:new(),
                              _NextPscId=1, _TimeEqTableNeeded=false, State ).



% (helper)
%
% First canonicalising the program:
init_presence_simulation( [ { _PscProg=default_program, TargetedPscActuators,
                              SmartLighting, RandActSettings } | T ],
                          OcSrvPid, PscTable, NextPscId, TimeEqTableNeeded,
                          State ) ->

    send_psc_trace( info, "Applying a default presence simulation program.",
                    State ),

    % Principle: no useless lighting during the expected presence slots, which
    % were initially:
    %  - in the morning: from 7:30 AM to 8:30 AM
    %  - in the evening: from 6:30:00 PM to 11:45 PM

    % TMorningStart = { 7, 30, 0 },
    % TMorningStop = { 8, 30, 0 },

    % TEveningStart = { 18, 30, 00 },
    % TEveningStop = { 23, 45, 00 },

    % PscSlots = [ { TMorningStart, TMorningStop },
    %              { TEveningStart, TEveningStop } ],

    % Now, default settings are safer: if not at home, simulating all day long
    % (unless there is daylight, supposing smart lighting to be enabled); so, in
    % logical terms, from 7:00 to around 00:30, translating to:

    % From the first second of that day (midnight):
    TEndOfNightStart = { 0, 0, 1 },

    % To a little after midnight:
    TEndOfNightStop = { 0, 31, 15 },

    % Time to wake up:
    TDayStart = { 7, 0, 0 },

    % In most countries bound to be stopped before, due to daylight:
    TDayStop = { 23, 59, 59 },

    PscSlots = [ { TEndOfNightStart, TEndOfNightStop },
                 { TDayStart, TDayStop } ],

    ExpandedPscSimUSetting = { PscSlots, TargetedPscActuators, SmartLighting,
                               RandActSettings },

    % Branch then to the general rule:
    init_presence_simulation( [ ExpandedPscSimUSetting | T ], OcSrvPid,
        PscTable, NextPscId, TimeEqTableNeeded, State );


% The "actual" clauses now.

% The only final, exit point:
init_presence_simulation( _PresenceSimSettings=[], _OcSrvPid, PscTable,
                          NextPscId, TimeEqTableNeeded, State ) ->

    % Schedules a periodic midnight presence update iff necessary:
    MaybeMidTaskId = case table:is_empty( PscTable ) of

        true ->
            send_psc_trace( warning, "Presence simulation enabled, yet "
                "no specific one requested.", State ),
            undefined;

        false ->
            TomorrowDate = time_utils:get_date_after( date(), _DaysOffset=1 ),

            % Define from time_utils.hrl:
            NextMidnightTimestamp = { TomorrowDate, ?first_time },

            send_psc_trace_fmt( debug, "Registering a daily presence "
                "program update task (every midnight), starting from ~ts.",
                [ time_utils:timestamp_to_string( NextMidnightTimestamp ) ],
                State ),

            % Every day:
            DHMSPeriodicity = { _D=1, _H=0, _M=0, _S=0 },

            class_USScheduler:get_server_pid() ! { registerTask,
                [ _CmdMsg=updatePresencePrograms,
                  _StartTime=NextMidnightTimestamp, DHMSPeriodicity,
                  _Count=unlimited ], self() },

            receive

                { wooper_result, { task_registered, MidTaskId } } ->

                    send_psc_trace_fmt( debug, "Midnight presence program "
                        "update task #~B defined.", [ MidTaskId ], State ),

                    MidTaskId

            end

    end,

    MaybeTimeEqTable = case TimeEqTableNeeded of

        true ->
            TimeTableCsvPath = file_utils:join( [ ?getAttr(app_base_directory),
                "priv", "data", ?time_equation_data_file ] ),

            file_utils:is_existing_file_or_link( TimeTableCsvPath ) orelse
                throw( { time_equation_data_file_not_found,
                         TimeTableCsvPath } ),

            { Rows, _RowCount=365, _FieldCount=2 } =
                csv_utils:read_file( TimeTableCsvPath, _Separator=$\t ),

            RowsAsNums = [ { text_utils:string_to_integer( DayRankStr ),
                             text_utils:string_to_float( CorrStr ) } ||
                { DayRankStr, CorrStr } <- Rows ],

            table:new( RowsAsNums );

        false ->
            undefined

    end,

    { PscTable, NextPscId, MaybeTimeEqTable, MaybeMidTaskId };


% Main, full clause:
init_presence_simulation( _PresenceSimSettings=[
        { PscProg, TargetedPscActuators, SmartLighting, RandActSettings } | T ],
                          OcSrvPid, PscTable, NextPscId,
                          TimeEqTableNeeded, State ) ->

    % We check the program, not taking into account here dawn/dusk, as they
    % change each day (and 'default_program' must have already been translated):
    %
    % We finally prefer not translating all programs into slots, as special
    % cases ares easier to manage afterwards:
    %
    VetProgram = vet_program( PscProg ),

    VetTargetedPscActuators = oceanic:canonicalise_emitted_event_specs(
        TargetedPscActuators ),

    DoSmartLighting = vet_smart_lighting( SmartLighting ),

    { ActualProgram, RandActivity } =
            case vet_random_activity( RandActSettings ) of

        % Hence randomised:
        RA={ MLightDur, MNoLightDur } ->

            RandProgram = randomise_program( VetProgram, MLightDur, MNoLightDur,
                                             State ),

            send_psc_trace_fmt( debug,
                "Original program ~ts~nResulting randomised program ~ts",
                [ program_to_string( PscProg ),
                  program_to_string( RandProgram ) ], State ),

            % Extra safety, to ensure that generated programs are legit:
            %RandProgram;
            { vet_program( RandProgram ), RA };


        % Hence taken verbatim:
        RA=false ->
            send_psc_trace_fmt( debug, "Vetted verbatim program ~ts",
                [ program_to_string( VetProgram ) ], State ),

            { vet_program( VetProgram ), RA }

    end,

    PscSim = #presence_simulation{
        id=NextPscId,
        enabled=true,

        % If ever that presence actuator was already running, prior to our
        % launch (it will be deactivated immediately, see below):
        %
        activated=true,

        actuator_event_specs=VetTargetedPscActuators,
        program=ActualProgram,
        smart_lighting=DoSmartLighting,
        random_activity=RandActivity },

    % We used to start from no light in all cases (regardless of the initial
    % state), yet this is a natural byproduct of the initial applying of
    % presence simulations, hence the following is commented out:
    %
    %send_psc_trace( info, "Ensuring that initially no lighting is done.",
    %                State ),
    %
    %UnlitPscSim = ensure_not_lighting( PscSim, _IsActivated=true, State ),

    NewPscTable = table:add_new_entry( _K=NextPscId, PscSim, PscTable ),

    send_psc_trace_fmt( info, "Registered a new presence simulation: ~ts",
        [ presence_simulation_to_string( PscSim ) ], State ),

    NewTimeEqTableNeeded = DoSmartLighting orelse TimeEqTableNeeded,

    init_presence_simulation( T, OcSrvPid, NewPscTable, NextPscId+1,
                              NewTimeEqTableNeeded, State );


% No random activity specified:
init_presence_simulation( _PresenceSimSettings=[
        { PscProg, TargetedPscActuators, SmartLighting } | T ],
                          OcSrvPid, PscTable, NextPscId,
                          TimeEqTableNeeded, State ) ->
    init_presence_simulation( _PSimSettings=[
        { PscProg, TargetedPscActuators, SmartLighting, _RandAct=true } | T ],
                              OcSrvPid, PscTable, NextPscId,
                              TimeEqTableNeeded, State );


% No smart lighting specified either:
init_presence_simulation( _PresenceSimSettings=[
        { PscProg, TargetedPscActuators } | T ],
                          OcSrvPid, PscTable, NextPscId,
                          TimeEqTableNeeded, State ) ->
    init_presence_simulation( _PSimSettings=[
        { PscProg, TargetedPscActuators, _SmartLighting=true } | T ],
                              OcSrvPid, PscTable, NextPscId,
                              TimeEqTableNeeded, State );

init_presence_simulation( _PresenceSimSettings=[ Other | _T ], _OcSrvPid,
                          _PscTable, _NextPscId, _TimeEqTableNeeded, _State ) ->
    throw( { invalid_presence_setting, Other } );


init_presence_simulation( _PresenceSimSettings=Other, _OcSrvPid, _PscTable,
                          _NextPscId, _TimeEqTableNeeded, _State ) ->
    throw( { invalid_presence_settings, Other } ).



-doc "Vets the specified user program.".
-spec vet_program( user_data() ) -> presence_program().
vet_program( _PresenceProgram=constant_presence ) ->
    constant_presence;

vet_program( _PresenceProgram=constant_absence ) ->
    constant_absence;

% Then expecting slots:
vet_program( PresenceProgram ) ->
    vet_program( PresenceProgram, _MaybeLastStop=undefined, _AccSlots=[] ).



% (helper)
vet_program( _PresenceProgram=[], _MaybeLastStop, _AccSlots=[] ) ->
    % At least one slot required:
    throw( empty_slot_list );

vet_program( _PresenceProgram=[], _MaybeLastStop, AccSlots ) ->
    lists:reverse( AccSlots );


vet_program( _PresenceProgram=[ Slot={ StartMilestone, StopMilestone } | T ],
             MaybeLastStop, AccSlots ) ->

    time_utils:is_time( StartMilestone ) orelse
        throw( { invalid_slot_start_milestone, StartMilestone } ),

    time_utils:is_time( StopMilestone ) orelse
        throw( { invalid_slot_stop_milestone, StopMilestone } ),

    StartMilestone < StopMilestone orelse
        throw( { inconsistent_slot_milestones, StartMilestone,
                 StopMilestone } ),

    % As Erlang term order tells 'undefined' is lower than all tuples:
    MaybeLastStop =:= undefined orelse
        ( MaybeLastStop < StartMilestone orelse
          throw( { interleaved_slots, Slot, MaybeLastStop } ) ),

    vet_program( T, StopMilestone, [ Slot | AccSlots ] );


vet_program( _PresenceProgram=[ Other | _T ], _MaybeLastStop, _AccSlots ) ->
    throw( { invalid_presence_slot, Other } );

vet_program( Other, _MaybeLastStop, _AccSlots ) ->
    throw( { invalid_presence_program, Other } ).



-doc "Vets the specified smart lighting setting.".
-spec vet_smart_lighting( user_data() ) -> boolean().
vet_smart_lighting( true ) ->
    true;

vet_smart_lighting( false ) ->
    false;

vet_smart_lighting( Other ) ->
    throw( { invalid_presence_smart_lighting_setting, Other } ).



-doc "Vets the specified random activity settings.".
-spec vet_random_activity( user_data() ) -> canon_random_activity_settings().
vet_random_activity( false ) ->
    false;

vet_random_activity( true ) ->
    { ?default_mean_light_duration, ?default_mean_no_light_duration };

vet_random_activity( S={ MeanLightDuration, MeanNoLightDuration } )
        when is_integer( MeanLightDuration ) andalso MeanLightDuration > 0
             andalso is_integer( MeanNoLightDuration )
                 andalso MeanNoLightDuration > 0 ->
    S;

vet_random_activity( Other ) ->
    throw( { invalid_presence_random_activity_setting, Other } ).




-doc "Adds random interruptions to the specified program.".
-spec randomise_program( presence_program(), second_duration(),
            second_duration(), wooper:state() ) -> presence_program().
randomise_program( _PscProgram=constant_presence,
                   MeanLightDuration, MeanNoLightDuration, State ) ->
    % Converted only in the case of random lighting:
    SingleSlots = [ { { 0, 0, 0 }, { 23, 59, 59 } } ],
    randomise_program( SingleSlots, MeanLightDuration, MeanNoLightDuration,
                       State );

randomise_program( PscProgram=constant_absence, _MeanLightDuration,
                   _MeanNoLightDuration, _State ) ->
    PscProgram;

randomise_program( Slots, MeanLightDuration, MeanNoLightDuration, State ) ->

    StdDevLightDur = MeanLightDuration div 2,

    % No less than 8 seconds:
    MinNoLightDur = max( 8, MeanNoLightDuration div 2 ),

    % Max symmetric of Min around Mean:
    MaxNoLightDur = MeanNoLightDuration + 2*(MeanNoLightDuration-MinNoLightDur),

    send_psc_trace_fmt( info, "Randomising presence slots, with, for lighting, "
        "a Gaussian law of mean ~w seconds (~ts) and "
        "standard deviation ~w seconds (~ts) and, for non-lighting, "
        "a uniform law in [~w,~w] seconds.",
        [ MeanLightDuration,
          time_utils:duration_to_string( 1000 * MeanLightDuration ),
          StdDevLightDur,
          time_utils:duration_to_string( 1000 * StdDevLightDur ),
          MinNoLightDur, MaxNoLightDur ], State ),

    % Sanity checks:

    MeanLightDuration > 0 orelse
        throw( { invalid_mean_light_duration, MeanLightDuration } ),

    MinNoLightDur > 0 orelse
        throw( { invalid_min_no_light_duration, MinNoLightDur } ),

    MaxNoLightDur > MinNoLightDur orelse
        throw( { invalid_max_no_light_duration, MaxNoLightDur } ),

    randomise_slots( Slots, MeanLightDuration, StdDevLightDur,
                     MinNoLightDur, MaxNoLightDur, _Acc=[] ).



% (helper)
randomise_slots( _Slots=[], _MeanLightDuration, _StdDevLightDur,
                   _MinNoLightDur, _MaxNoLightDur, Acc ) ->
    lists:reverse( Acc );

% Initially, dawn/dusk are not resolved as actual times, so let's leave them:
% (they are not presence milestones anymore)
% randomise_slots( _Slots=[ H={ _Start=dawn, Stop } | T ],
%                    MeanLightDuration, MeanNoLightDuration, Acc ) ->
%   randomise_slots( T, [ H | T ] );
%
% randomise_slots( _Slots=[ H={ Start, _Stop=dusk } | T ],
%                    MeanLightDuration, MeanNoLightDuration, Acc ) ->
%   randomise_slots( T, [ H | T ] );

% Replacing each slot by as many sub-slots that fit:
randomise_slots( _Slots=[ { Start, Stop } | T ], MeanLightDuration,
                   StdDevLightDur, MinNoLightDur, MaxNoLightDur, Acc ) ->
    % We replace a single overall slot by as many we can fit:
    Duration = time_utils:get_intertime_duration( Start, Stop ),

    RevSubSlots = draw_subslots( Start, Duration, MeanLightDuration,
        StdDevLightDur, MinNoLightDur, MaxNoLightDur, _AccSlots=[] ),

    randomise_slots( T, MeanLightDuration, StdDevLightDur, MinNoLightDur,
                     MaxNoLightDur, RevSubSlots ++ Acc ).



% We try here to insert a light slot and a no-light slot in the remaining
% duration - provided that their duration fit.
%
% (helper)
draw_subslots( Start, Duration, MeanLightDuration, StdDevLightDur,
               MinNoLightDur, MaxNoLightDur, AccSlots ) ->

    % Incremented to be strictly positive:
    LightDur = random_utils:get_positive_integer_gaussian_value(
        MeanLightDuration, StdDevLightDur ) + 1,

    NoLightDur = random_utils:get_uniform_value( MinNoLightDur,
                                                 MaxNoLightDur ),

    SubDur = LightDur + NoLightDur,

    NewDuration = Duration - SubDur,

    case NewDuration > 0 of

        true ->
            % Then we can insert at least one on/off subslot:
            NextStop = time_utils:offset_time( Start, LightDur ),
            NewAccSlots = [ { Start, NextStop } | AccSlots ],
            NewStart = time_utils:offset_time( Start, SubDur ),
            draw_subslots( NewStart, NewDuration, MeanLightDuration,
                StdDevLightDur, MinNoLightDur, MaxNoLightDur, NewAccSlots );

        false ->
            % Then with a last one bridging the gap, we stop introducing
            % subslots:
            %
            Stop = time_utils:offset_time( Start, Duration ),
            [ { Start, Stop } | AccSlots ]

    end.



-doc """
Applies, from the current moment, the intra-day presence program.

Defined for reuse (at creation or afterwards, if toggling this service as a
whole, updating presence simulations, etc.).
""".
-spec apply_presence_simulation( wooper:state() ) -> wooper:state().
apply_presence_simulation( State ) ->

    case ?getAttr(presence_simulation_enabled) of

        true ->
            case ?getAttr(actual_presence) of

                true ->
                    cond_utils:if_defined( us_main_debug_presence_simulation,
                        send_psc_trace( notice, "Presence simulation is "
                            "enabled, but none is applied, as somebody is "
                            "expected to be at home.", State ) ),
                    % Ensure that no lighting remains:
                    ensure_not_any_lighting( State );

                false ->
                    PscTable = ?getAttr(presence_table),

                    case table:values( PscTable ) of

                        [] ->
                            cond_utils:if_defined(
                                us_main_debug_presence_simulation,
                                send_psc_trace( debug, "Presence simulation is "
                                    "enabled, but none is applied: if nobody "
                                    "is expected to be at home, no presence "
                                    "simulation is registered.", State ) ),
                            % No lighting expected to shutdown.
                            State;

                        PscSims ->
                            update_presence_simulations( PscSims, State )

                    end


            end;

        false ->
            % Ensure that no lighting remains:
            ensure_not_any_lighting( State )

    end.



-doc """
Updates, in turn and if necessary, from scratch, the specified presence
simulations.

Defined for reuse. Prefer calling the parent, more integrated
`apply_presence_simulation/1` function instead, which is to check first that
presence simuation is wanted and that nobody is at home.
""".
-spec update_presence_simulations( [ presence_simulation() ],
                                   wooper:state() ) -> wooper:state().
update_presence_simulations( PscSims, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Updating ~B presence "
            "simulation(s) now (supposedly nobody is at home).",
            [ length( PscSims ) ], State ) ),

    % Keep only relevant information (if any):
    CleanedMaybeCelestialInfo = case ?getAttr(celestial_info) of

        undefined ->
            undefined;

        CelestialInfo={ Date, _Timing } ->
            case erlang:date() of

                Date ->
                    CelestialInfo;

                % Outdated:
                _ ->
                    undefined

            end

    end,

    update_presence_simulations( PscSims, _CurrentTime=erlang:time(),
        CleanedMaybeCelestialInfo, _EmptyTable=table:new(), State ).



-doc """
Updates, in turn and if necessary, from scratch, the specified presence
simulations.

If defined, the celestial times are expected to be correct here.
""".
-spec update_presence_simulations( [ presence_simulation() ], time(),
        option( celestial_info() ), presence_table(), wooper:state() ) ->
            wooper:state().
% No more presence simulation:
update_presence_simulations( _PscSims=[], _CurrentTime, MaybeCelestialInfo,
                             PscTable, State ) ->
    setAttributes( State, [ { presence_table, PscTable },
                            { celestial_info, MaybeCelestialInfo } ] );

% Disabled presence:
update_presence_simulations( _PscSims=[
        PscSim=#presence_simulation{ id=Id,
                                     enabled=false } | T ], CurrentTime,
                             MaybeCelestialInfo, PscTable, State ) ->

    % Nothing to be done if disabled:
    NewPscTable = table:add_new_entry( _K=Id, PscSim, PscTable ),

    update_presence_simulations( T, CurrentTime, MaybeCelestialInfo,
                                 NewPscTable, State );

% Thus enabled:
update_presence_simulations(
        _PscSims=[ PscSim=#presence_simulation{ id=Id } | T ], CurrentTime,
        MaybeCelestialInfo, PscTable, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( info, "Managing presence simulation ~ts",
            [ presence_simulation_to_string( PscSim ) ], State ) ),

    % Probably not a maybe-CelestialInfo anymore:
    { StartedPscSim, CelestialInfo } = manage_presence_simulation( PscSim,
        CurrentTime, MaybeCelestialInfo, State ),

    NewPscTable = table:add_new_entry( _K=Id, StartedPscSim, PscTable ),

    update_presence_simulations( T, CurrentTime, CelestialInfo, NewPscTable,
                                 State ).



-doc """
Manages the specified presence simulation from scratch (callable at any time),
covering all cases, and acting accordingly iff necessary.

A large function, but defined only once.
""".
-spec manage_presence_simulation( presence_simulation(), time(),
        option( celestial_info() ), wooper:state() ) ->
            { presence_simulation(), option( celestial_info() ) }.
% Not enabled:
manage_presence_simulation( PscSim=#presence_simulation{
        enabled=false,
        activated=IsActivated,
        presence_task_info=MaybePscTaskInfo },
                            _CurrentTime, MaybeCelestialInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace( debug, "Simulated presence not enabled, "
            "hence not lighting, and no planned presence transition.",
            State ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),

    NoPlanPscSim = ensure_no_planned_presence_transition( UnlitPscSim,
                                                          MaybePscTaskInfo ),

    { NoPlanPscSim, MaybeCelestialInfo };


% From here, 'enabled' expected to be true.
%
% Splitting per program now, starting with always on, first with no smart
% lighting:
manage_presence_simulation( PscSim=#presence_simulation{
        activated=IsActivated,
        program=constant_presence,
        smart_lighting=false,
        presence_task_info=MaybePscTaskInfo },
                            _CurrentTime, MaybeCelestialInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace( debug, "Enabled, no smart lighting requested, "
            "simulating a constant presence, hence always-on lighting "
            "and no planned presence transition.",
            State ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),

    NoPlanPscSim = ensure_no_planned_presence_transition( LitPscSim,
                                                          MaybePscTaskInfo ),

    { NoPlanPscSim, MaybeCelestialInfo };

% Now still with always on, but with smart lighting:
manage_presence_simulation( PscSim=#presence_simulation{
        activated=IsActivated,
        program=constant_presence,
        smart_lighting=true,
        presence_task_info=MaybePscTaskInfo },
                            CurrentTime, MaybeCelestialInfo, State ) ->

    case get_celestial_info( MaybeCelestialInfo, State ) of

        CI={ _TodayDate, constant_daylight } ->
            % Naturally lighted, no lighting then:
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace( debug, "Enabled, smart lighting requested, "
                    "simulating a constant presence yet in constant daylight,"
                    "hence not lighting and no planned presence transition.",
                    State ) ),

            UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
            NoPlanPscSim = ensure_no_planned_presence_transition( UnlitPscSim,
                MaybePscTaskInfo ),
            { NoPlanPscSim, CI };


        CI={ _TodayDate, constant_darkness } ->
            % No natural lighting, so lighting needed:
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace( debug, "Enabled, smart lighting requested, "
                    "simulating a constant presence and in constant darkness,"
                    "hence lighting with no planned presence transition.",
                    State ) ),

            LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
            NoPlanPscSim = ensure_no_planned_presence_transition( LitPscSim,
                MaybePscTaskInfo ),
            { NoPlanPscSim, CI };


        CI={ _TodayDate, { MaybeDawnTime, MaybeDuskTime } } ->

            NewPscSim = case MaybeDawnTime of

                undefined ->
                    % No dawn, thus supposing constant darkness, hence lighting
                    % needed and no switching off planned:

                    cond_utils:if_defined( us_main_debug_presence_simulation,
                        send_psc_trace( debug, "Enabled, smart lighting "
                            "requested, simulating a constant presence, "
                            "no dawn thus in constant darkness, "
                            "hence lighting with no planned presence "
                            "transition.", State ) ),

                    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
                    ensure_no_planned_presence_transition( LitPscSim,
                                                           MaybePscTaskInfo );

                DawnTime when CurrentTime > DawnTime ->
                    % We are after the dawn; but before or after any dusk?
                    case MaybeDuskTime of

                        undefined ->
                            % No dusk, hence with light at least until the end
                            % of day:
                            %
                            cond_utils:if_defined(
                                us_main_debug_presence_simulation,
                                send_psc_trace( debug, "Enabled, smart lighting"
                                    " requested, simulating a constant "
                                    "presence, being after dawn and with no "
                                    "dusk, thus with light at least until "
                                    "the end of the day, hence not lighting and"
                                    " no planned presence transition.",
                                    State ) ),

                            UnlitPscSim = ensure_not_lighting( PscSim,
                                IsActivated, State ),

                            ensure_no_planned_presence_transition( UnlitPscSim,
                                MaybePscTaskInfo );


                        DuskTime when CurrentTime > DuskTime ->
                            % Already after dusk; lighting needed from now and
                            % till the end of day:
                            %
                            cond_utils:if_defined(
                                us_main_debug_presence_simulation,
                                send_psc_trace( debug, "Enabled, smart lighting"
                                    " requested, simulating a constant "
                                    "presence, already after dusk, hence "
                                    "lighting needed from now and till "
                                    "the end of the day.", State ) ),

                            LitPscSim = ensure_lighting( PscSim, IsActivated,
                                                         State ),

                            ensure_no_planned_presence_transition(
                                LitPscSim, MaybePscTaskInfo );


                        % Hence CurrentTime <= DuskTime:
                        DuskTime ->
                            % Between dawn and dusk, so no lighting needed until
                            % dusk:
                            %
                            cond_utils:if_defined(
                                us_main_debug_presence_simulation,
                                send_psc_trace( debug, "Enabled, smart lighting"
                                    " requested, simulating a constant "
                                    "presence, between dawn and dusk, so no "
                                    "lighting needed until dusk.", State ) ),

                            UnlitPscSim = ensure_not_lighting( PscSim,
                                IsActivated, State ),

                            ensure_planned_presence_transition( UnlitPscSim,
                                DuskTime, MaybePscTaskInfo, State )

                    end

            end,

            { NewPscSim, CI }

    end;


% Now always off, the easiest case:
manage_presence_simulation( PscSim=#presence_simulation{
        activated=IsActivated,
        program=constant_absence,
        % Does not matter: smart_lighting
        presence_task_info=MaybePscTaskInfo },
                            _CurrentTime, MaybeCelestialInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace( debug, "Enabled, yet in constant-absence simulation "
            "program, hence not lighting and not planning any presence "
            "transition.",
            State ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),

    NoPlanPscSim = ensure_no_planned_presence_transition( UnlitPscSim,
                                                          MaybePscTaskInfo ),

    { NoPlanPscSim, MaybeCelestialInfo };


% Now a slot-based program; the most complex case, yet with no smart lighting
% first:
%
manage_presence_simulation( PscSim=#presence_simulation{
        activated=IsActivated,
        program=Slots,
        smart_lighting=false,
        presence_task_info=MaybePscTaskInfo },
                            CurrentTime, MaybeCelestialInfo, State ) ->

    NewPscSim = case get_programmed_presence( Slots, CurrentTime ) of

        % For good, today:
        always_present ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace( debug, "Enabled, no smart lighting requested, "
                    "simulating a constant presence from now, hence lighting "
                    "and not planning any presence transition.", State ) ),

            LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
            ensure_no_planned_presence_transition( LitPscSim,
                                                   MaybePscTaskInfo );

        always_absent ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace( debug, "Enabled, no smart lighting requested "
                    "simulating a constant absence from now, hence "
                    "not lighting and not planning any presence transition.",
                    State ) ),

            UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
            ensure_no_planned_presence_transition( UnlitPscSim,
                                                   MaybePscTaskInfo );

        { present_until, AbsStartTime } ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Enabled, no smart lighting "
                    "requested, simulating a presence until ~ts, hence "
                    "lighting and planning a presence transition then.",
                    [ time_utils:time_to_string( AbsStartTime ) ],
                    State ) ),

            LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
            ensure_planned_presence_transition( LitPscSim, AbsStartTime,
                                                MaybePscTaskInfo, State );

        { absent_until, PresStartTime } ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Enabled, no smart lighting "
                    "requested, simulating an absence until ~ts, hence "
                    "not lighting and planning a presence transition then.",
                    [ time_utils:time_to_string( PresStartTime ) ],
                    State ) ),

            UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
            ensure_planned_presence_transition( UnlitPscSim, PresStartTime,
                                                MaybePscTaskInfo, State )

    end,

    { NewPscSim, MaybeCelestialInfo };


% The real most complex case: slots with smart lighting.
manage_presence_simulation( PscSim=#presence_simulation{
        activated=IsActivated,
        program=Slots,
        % Here smart_lighting is true
        presence_task_info=MaybePscTaskInfo },
                            CurrentTime, MaybeCelestialInfo, State ) ->

    CelestialInfo = { _Date, { MaybeDawnTime, MaybeDuskTime } } =
        get_celestial_info( MaybeCelestialInfo, State ),

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Getting programmed presence for "
            "slots: ~ts.", [ text_utils:strings_to_string(
                [ slot_to_string( S ) || S <- Slots ] ) ], State ) ),

    NewPscSim = case get_programmed_presence( Slots, CurrentTime ) of

        % For good, today, hence light always needed:
        always_present ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace( debug, "Enabled, smart lighting requested, "
                    "simulating a constant presence from now, hence lighting "
                    "and not planning any presence transition.", State ) ),

            ensure_constant_light( CurrentTime, MaybeDawnTime, MaybeDuskTime,
                PscSim, IsActivated, MaybePscTaskInfo, State );

        always_absent ->
            % Easy case:
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace( debug, "Enabled, smart lighting requested, "
                    "simulating a constant absence from now, hence not "
                    "lighting and not planning any presence transition.",
                    State ) ),

            UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
            ensure_no_planned_presence_transition( UnlitPscSim,
                                                   MaybePscTaskInfo );

        { present_until, AbsStart } ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Enabled, smart lighting requested, "
                    "simulating a presence until ~ts, hence lighting "
                    "and planning a presence transition.",
                    [ time_utils:time_to_string( AbsStart ) ], State ) ),

            ensure_light_until( AbsStart, CurrentTime, MaybeDawnTime,
                MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State );

        { absent_until, PresStart } ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Enabled, smart lighting requested, "
                    "simulating a presence from ~ts, hence not lighting "
                    "and planning a presence transition.",
                    [ time_utils:time_to_string( PresStart ) ], State ) ),

            ensure_light_from( PresStart, CurrentTime, MaybeDawnTime,
                MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State )

    end,

    { NewPscSim, CelestialInfo }.




-doc """
Returns whether, for the specified time, a presence shall be simulated, and what
is the transition (if any) to plan next this day.
""".
-spec get_programmed_presence( [ presence_slot() ], time() ) ->
        'always_present' | 'always_absent'
    | { 'present_until', time() } | { 'absent_until', time() }.
get_programmed_presence( _Slots=[], _Time ) ->
    % No more presence slot, hence:
    always_absent;

% We must include the equal case in the tests, as by design we are awoken most
% of the time at the exact threshold millisecond:
%
get_programmed_presence( _Slots=[ { _StartPscTime, StopPscTime } | T ], Time )
                                when Time >= StopPscTime ->
    % Time of interest not reached in the list, continuing:
    get_programmed_presence( T, Time );

% We are before a slot:
get_programmed_presence( _Slots=[ { StartPscTime, _StopPscTime } | _T ], Time )
                                when Time < StartPscTime ->
    { absent_until, StartPscTime };

% We are in a slot:
get_programmed_presence( _Slots=[ { _StartPscTime, StopPscTime } | _T ],
                         _Time ) ->
    % Implicit: StartPscTime <= Time < StopPscTime; comparing to next midnight:
    case StopPscTime < ?last_time of

        true ->
            { present_until, StopPscTime };

        false ->
            always_present

    end.



-doc "Initialises the automated actions for home automation.".
-spec init_automated_actions( user_action_specs(), wooper:state() ) ->
                                                wooper:state().
init_automated_actions( UserActSpecs, State ) when is_list( UserActSpecs ) ->

    BuiltinActSpecs =

        [ { _Header="Home Automation Device management", [
            { _ActName=devices,
              _UserDesc="lists all known home automation devices",
              _ReqName=listDevices },

            { sensors, "lists all known home automation sensor devices",
              listSensors },

            { commands, "lists all known home automation command devices",
              listCommandDevices },

            { actuators, "lists all known home automation actuator devices",
              listActuators },

            { describe, "describes the specified device", describeDevice,
              [ { dynamic, dev_designator, "string()" } ] },

            { activate, "activates the specified actuator", activateDevice,
              [ { dynamic, dev_designator, "string()" } ] },

            { deactivate, "deactivates the specified actuator",
              deactivateDevice,
              [ { dynamic, dev_designator, "string()" } ] } ] },

          { "Presence management", [
            { presence, "tells whether somebody is considered at home",
              isPresent },

            { at_home, "declares that somebody is present at home",
              declarePresent },

            { away, "declares that nobody is present at home",
              declareNotPresent },

            { is_presence_simulated,
              "tells whether presence simulation is enabled",
              isPresenceSimulationEnabled },

            { enable_presence_simulation, "enables the presence simulation",
              enablePresenceSimulation },

            { disable_presence_simulation, "disables the presence simulation",
              disablePresenceSimulation } ] } ],

    AllActSpecs = BuiltinActSpecs ++ UserActSpecs,

    ?debug_fmt( "Initialising automated actions, based on their user "
                "specifications:~n ~p", [ AllActSpecs ] ),

    { RegActTable, RegHdInfo } = us_action:register_action_specs( AllActSpecs,
        ?getAttr(action_table), ?getAttr(header_info), ?getAttr(typedef_table),
        wooper:get_classname( State ) ),

    setAttributes( State, [ { action_table, RegActTable },
                            { header_info, RegHdInfo } ] );

init_automated_actions( Other, State ) ->
    ?error_fmt( "Invalid automated actions for home automation "
                "(not a list): ~p.", [ Other ] ),

    throw( { invalid_home_automation_actions, Other, not_list } ).



-doc """
Ensures that the lighting is on for this presence simulation.

Trusts the internal state: switches the lights on only conditionally.
""".
-spec ensure_lighting( presence_simulation(), boolean(), wooper:state() ) ->
            presence_simulation().
ensure_lighting( PscSim, _IsActivated=true, State ) ->

    % Nothing done, as here we trust the known state and suppose no external
    % interference.

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug,
            "Presence simulation #~B already activated, nothing to do.",
            [ PscSim#presence_simulation.id ], State ),
        basic_utils:ignore_unused( State ) ),

    PscSim;

ensure_lighting( PscSim=#presence_simulation{ actuator_event_specs=ActEvSpecs },
                 _IsActivated=false, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Activating presence simulation #~B.",
            [ PscSim#presence_simulation.id ], State ) ),

    oceanic:trigger_actuators( ActEvSpecs, ?getAttr(oc_srv_pid) ),

    % DeviceTable = ?getAttr(device_table),

    % DevState = table:get_value( TargetEurid, DeviceTable ),

    % NewDevState = declare_waited_ack( TargetEurid,
    %   { smart_plug_status_report_event, power_on }, DevState ),

    % NewDeviceTable = table:add_entry( _K=TargetEurid, _V=NewDevState,
    %                                   DeviceTable ),

    % NewState = setAttribute( State, device_table, DeviceTable ),

    PscSim#presence_simulation{ activated=true }.



-doc """
Ensures that the lighting is off for this presence simulation.

Trusts the internal state: switches the lights off only conditionally.
""".
-spec ensure_not_lighting( presence_simulation(), boolean(), wooper:state() ) ->
            presence_simulation().
ensure_not_lighting( PscSim=#presence_simulation{
                                actuator_event_specs=ActEvSpecs },
                     _IsActivated=true, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug,
            "Deactivating presence simulation #~B, as it was registered "
            "as activated.", [ PscSim#presence_simulation.id ], State ) ),

    oceanic:trigger_actuators_reciprocal( ActEvSpecs, ?getAttr(oc_srv_pid) ),

    PscSim#presence_simulation{ activated=false };


ensure_not_lighting( PscSim, _IsActivated=false, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug,
            "Presence simulation #~B already deactivated, nothing to do.",
            [ PscSim#presence_simulation.id ], State ),
        basic_utils:ignore_unused( State ) ),

    PscSim.




-doc """
Ensures that the specified presence task (and only it) is planned for an update
at the specified time.
""".
-spec ensure_planned_presence_transition( presence_simulation(), time(),
            option( task_info() ), wooper:state() ) -> presence_simulation().
% Plan if not already planned:
ensure_planned_presence_transition( PscSim=#presence_simulation{ id=PscId },
        PlannedTime, _MaybePscTaskInfo=undefined, _State ) ->

    TaskCmd = { updatePresenceSimulation, [ PscId ] },

    PlannedTimestamp = { date(), PlannedTime },

    class_USScheduler:get_server_pid() !
        { registerOneshotTask, [ TaskCmd, PlannedTimestamp ], self() },

    receive

        { wooper_result, task_done } ->
            % Better than waiting for ever; this may happen if timed sendings
            % have done just before; the corresponding command has then been
            % directly sent by the scheduler:
            %
            PscSim#presence_simulation{ presence_task_info=undefined };


        { wooper_result, { task_registered, TaskId } } ->
            TaskInfo = { TaskId, PlannedTime },
            PscSim#presence_simulation{ presence_task_info=TaskInfo }

    end;

% Here such planning exists (as task info) and is already at the correct time:
ensure_planned_presence_transition( PscSim, PlannedTime,
        _PscTaskInfo={ _PscTaskId, PlannedTime }, _State ) ->
    PscSim;

% Planning exists, yet with different times; correcting it:
ensure_planned_presence_transition( PscSim, PlannedTime,
        _PrevPscTaskInfo={ PrevPscTaskId, PrevPlannedTime }, State ) ->

    % A priori may be legit:
    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( notice, "Switching, for presence simulation #~B, "
            "planned time from ~ts to ~ts.", [ PscSim#presence_simulation.id,
                time_utils:time_to_string( PrevPlannedTime ),
                time_utils:time_to_string( PlannedTime ) ], State ),
        basic_utils:ignore_unused( [ State, PrevPlannedTime ] ) ),

    % Clearer:
    %ClearedPsim = ensure_no_planned_presence_transition( PscSim,
    %                                                     PrevPscTaskInfo ),

    class_USScheduler:get_server_pid() !
        { unregisterTaskAsync, [ PrevPscTaskId ] },

    % Force rescheduling:
    ensure_planned_presence_transition( PscSim, PlannedTime,
                                        _MaybePscTaskInfo=undefined, State ).



-doc "Ensures that there is no planned presence task.".
-spec ensure_no_planned_presence_transition( presence_simulation(),
    option( task_id() ) ) -> presence_simulation().
ensure_no_planned_presence_transition( PscSim, _MaybePscTaskInfo=undefined ) ->
    PscSim;

ensure_no_planned_presence_transition( PscSim, { PscTaskId, _TaskTime } ) ->
    class_USScheduler:get_server_pid() ! { unregisterTaskAsync, [ PscTaskId ] },
    PscSim#presence_simulation{ presence_task_info=undefined }.



-doc """
Ensures that constant light is available from the specified current time: lights
iff no daylight.

Note that we just plan the next transition, not any next one that could be
determined here.
""".
-spec ensure_constant_light( time(), option( time() ), option( time() ),
    presence_simulation(), boolean(), option( task_info() ), wooper:state() ) ->
            presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence constant lighting
% needed:
ensure_constant_light( _CurrentTime, _MaybeDawnTime=undefined,
        _MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace( debug, "Constant light requested, whereas no dawn, "
            "supposedly no dusk either, constant darkness, hence constant "
            "lighting needed.", State ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_no_planned_presence_transition( LitPscSim, MaybePscTaskInfo );

% A dawn but no dusk: lights (only) until that dawn.
%
% Before dawn here:
ensure_constant_light( CurrentTime, DawnTime, _MaybeDuskTime=undefined, PscSim,
        IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Constant light requested, we are before "
            "dawn, to happen at ~ts (and there is no dusk): lights (only) "
            "until that dawn.",
            [ time_utils:time_to_string( DawnTime ) ], State ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
                                        State );

% After dawn here (implicit: CurrentTime >= DawnTime):
ensure_constant_light( _CurrentTime, DawnTime, _MaybeDuskTime=undefined,
        PscSim, IsActivated, MaybePscTaskInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Constant light requested, we are "
            "after dawn (was at ~ts) and there is no dusk: "
            "not lighting from now.",
            [ time_utils:time_to_string( DawnTime ) ], State ),
        basic_utils:ignore_unused( DawnTime ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_no_planned_presence_transition( UnlitPscSim, MaybePscTaskInfo );

% General case: a dawn and a dusk.
%
% Here before dawn, still in darkness, thus lighting until dawn:
ensure_constant_light( CurrentTime, DawnTime, _DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Constant light requested, we are before "
            "dawn (at ~ts), still in darkness, thus lighting until dawn.",
            [ time_utils:time_to_string( DawnTime ) ], State ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
                                        State );

% Here in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting until
% dusk:
%
ensure_constant_light( CurrentTime, _DawnTime, DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->

    % In daylight, thus no lighting until dusk:
    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Constant light requested, we are after "
            "dawn but before dusk (at ~ts), thus in daylight, thus not "
            "lighting until dusk.",
            [ time_utils:time_to_string( DuskTime ) ], State ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
                                        State );

% Here are already past dusk (CurrentTime >= DuskTime), hence in darkness, thus
% lighting from now on:
%
ensure_constant_light( _CurrentTime, _DawnTime, DuskTime, PscSim,
                       IsActivated, MaybePscTaskInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Constant light requested, we are already "
            "after dusk (was at ~ts), thus in darkness, thus lighting "
            "from now on.",
            [ time_utils:time_to_string( DuskTime ) ], State ),
        basic_utils:ignore_unused( DuskTime ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_no_planned_presence_transition( LitPscSim, MaybePscTaskInfo ).




-doc """
Ensures that light is available until the specified stop time, expected to be in
the future of the specified current time.

Note that we just plan the next transition, not any next one that could be
determined here.
""".
-spec ensure_light_until( time(), time(), option( time() ), option( time() ),
    presence_simulation(), boolean(), option( task_info() ), wooper:state() ) ->
            presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence lighting needed
% until stop time:
ensure_light_until( StopTime, _CurrentTime, _MaybeDawnTime=undefined,
        _MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested until ~ts, no dawn, "
            "supposedly no dusk either, constant darkness, hence "
            "lighting needed until that time.",
            [ time_utils:time_to_string( StopTime ) ], State ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( LitPscSim, StopTime, MaybePscTaskInfo,
                                        State );

% A dawn but no dusk: lights (only) until that dawn.
%
% Before dawn here, stop does not matter yet:
ensure_light_until( StopTime, CurrentTime, DawnTime, _MaybeDuskTime=undefined,
        PscSim, IsActivated, MaybePscTaskInfo, State )
                                when CurrentTime < DawnTime ->

    StopLightingTime = erlang:min( StopTime, DawnTime ),

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested until ~ts, dawn at ~ts, "
            "so lighting until ~ts.",
            [ time_utils:time_to_string( StopTime ),
              time_utils:time_to_string( DawnTime ),
              time_utils:time_to_string( StopLightingTime ) ], State ),
        basic_utils:ignore_unused( [ State, StopLightingTime ] ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( LitPscSim, DawnTime, MaybePscTaskInfo,
                                        State );

% After dawn here (implicit: CurrentTime >= DawnTime), no dusk, hence no
% lighting needed:
%
ensure_light_until( _StopTime, _CurrentTime, _DawnTime,
        _MaybeDuskTime=undefined, PscSim, IsActivated, MaybePscTaskInfo,
        State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace( debug, "Light requested until a stop time, "
            "we are already after dawn, no dusk, hence no lighting needed.",
            State ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_no_planned_presence_transition( UnlitPscSim, MaybePscTaskInfo );

% General case: a dawn and a dusk.
%
% Here we are before dawn, still in darkness, thus lighting until first to come
% between dawn and stop:
%
ensure_light_until( StopTime, CurrentTime, DawnTime, _DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->

    StopLightingTime = erlang:min( StopTime, DawnTime ),

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested until ~ts, "
            "we are before dawn (at ~ts), hence lighting until ~ts.",
            [ time_utils:time_to_string( StopTime ),
              time_utils:time_to_string( DawnTime ),
              time_utils:time_to_string( StopLightingTime ) ], State ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( LitPscSim, StopLightingTime,
                                        MaybePscTaskInfo, State );

% Here we are in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting
% until dusk (and stop time does not matter):
%
ensure_light_until( _StopTime, CurrentTime, _DawnTime, DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested until a stop time, yet "
            "we are between dawn and dusk (~ts), hence no lighting needed.",
            [ time_utils:time_to_string( DuskTime ) ], State ) ),

    % In daylight, thus no lighting until dusk:
    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
                                        State );

% Here we are already past dusk (CurrentTime >= DuskTime), hence in darkness,
% thus lighting from now on to the stop time (by design in the future):
%
ensure_light_until( StopTime, _CurrentTime, _DawnTime, _DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested until ~ts, "
            "we are after dusk, hence lighting before stop time.",
            [ time_utils:time_to_string( StopTime ) ], State ) ),

    LitPscSim = ensure_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( LitPscSim, StopTime, MaybePscTaskInfo,
                                        State ).



-doc """
Ensures that light is available from the specified start time, expected to be in
the future of the specified current time.

Note that we just plan the next transition, not any next one that could be
determined here.
""".
-spec ensure_light_from( time(), time(), option( time() ), option( time() ),
    presence_simulation(), boolean(), option( task_info() ), wooper:state() ) ->
            presence_simulation().
% No dawn, supposedly no dusk either, constant darkness, hence lighting (only)
% needed from that start time:
ensure_light_from( StartTime, _CurrentTime, _MaybeDawnTime=undefined,
        _MaybeDuskTime, PscSim, IsActivated, MaybePscTaskInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested from ~ts, "
            "lighting (only) from then, as there is no dawn.",
            [ time_utils:time_to_string( StartTime ) ], State ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( UnlitPscSim, StartTime,
                                        MaybePscTaskInfo, State );

% A dawn but no dusk: lights (only) until that dawn happens (if not already).
%
% As always, we are still before start time, so no lighting, which is to happen
% if start time happens before dawn:
%
ensure_light_from( StartTime, CurrentTime, DawnTime, _MaybeDuskTime=undefined,
        PscSim, IsActivated, MaybePscTaskInfo, State )
                                when CurrentTime < DawnTime ->
    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    case StartTime < DawnTime of

        true ->
            % A bit of lighting needed from StartTime (till dawn):
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Light requested from ~ts, "
                    "so will light from then, and before dawn at ~ts "
                    "(and there is no dusk).",
                    [ time_utils:time_to_string( StartTime ),
                      time_utils:time_to_string( DawnTime ) ], State ) ),

            ensure_planned_presence_transition( UnlitPscSim, StartTime,
                                                MaybePscTaskInfo, State );

        false ->
            % Already daylight when starting, no dusk today, no lighting to
            % plan:
            %
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Light requested from ~ts, "
                    "which is to happen after dawn (at ~ts), "
                    "hence no lighting to do or plan (and there is no dusk).",
                    [ time_utils:time_to_string( StartTime ),
                      time_utils:time_to_string( DawnTime ) ], State ) ),

            ensure_no_planned_presence_transition( UnlitPscSim,
                                                   MaybePscTaskInfo )

    end;

% We are after dawn here (implicit: CurrentTime >= DawnTime), no dusk, hence no
% lighting ever needed:
%
ensure_light_from( StartTime, _CurrentTime, DawnTime,
        _MaybeDuskTime=undefined, PscSim, IsActivated, MaybePscTaskInfo,
        State ) ->
    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested from ~ts, "
            "we are already after dawn (at ~ts) and there is no dusk, "
            "hence no lighting to do or plan.",
            [ time_utils:time_to_string( StartTime ),
              time_utils:time_to_string( DawnTime ) ], State ),
        basic_utils:ignore_unused( [ StartTime, DawnTime ] ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_no_planned_presence_transition( UnlitPscSim, MaybePscTaskInfo );

% General case: a dawn and a dusk.
%
% Here we are before dawn, still in darkness, thus lighting only to be triggered
% at start time if it is to happen before dawn:
%
ensure_light_from( StartTime, CurrentTime, DawnTime, DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DawnTime ->
    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    case StartTime < DawnTime of

        true ->
            % A bit of lighting needed then between start and dawn:
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Light requested from ~ts, "
                    "so will light from then, and before dawn at ~ts.",
                    [ time_utils:time_to_string( StartTime ),
                      time_utils:time_to_string( DawnTime ) ], State ) ),

            ensure_planned_presence_transition( UnlitPscSim, StartTime,
                                                MaybePscTaskInfo, State );

        false ->
            % Start to be done already in daylight (StartTime>=DawnTime), next
            % event is switching on at the latest between dusk and the start
            % time:
            %
            LitTime = erlang:max( DuskTime, StartTime ),
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Light requested from ~ts, "
                    "which is to happen after dawn (at ~ts), "
                    "hence no lighting to be done currently.",
                    [ time_utils:time_to_string( StartTime ),
                      time_utils:time_to_string( DawnTime ) ], State ) ),

            ensure_planned_presence_transition( UnlitPscSim, LitTime,
                                                MaybePscTaskInfo, State )

    end;

% Here in daylight (DawnTime <= CurrentTime < DuskTime), thus no lighting until
% dusk (and start time does not matter at this point):
%
ensure_light_from( _StartTime, CurrentTime, _DawnTime, DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) when CurrentTime < DuskTime ->

    % In daylight, thus no lighting until at least dusk:
    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested from a start time, yet "
            "we are between dawn and dusk (~ts), hence no lighting needed.",
            [ time_utils:time_to_string( DuskTime ) ], State ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( UnlitPscSim, DuskTime, MaybePscTaskInfo,
                                        State );

% Here already past dusk (CurrentTime >= DuskTime), hence in darkness, thus
% will light up at the start time (by design in the future):
%
ensure_light_from( StartTime, _CurrentTime, _DawnTime, DuskTime, PscSim,
        IsActivated, MaybePscTaskInfo, State ) ->

    cond_utils:if_defined( us_main_debug_presence_simulation,
        send_psc_trace_fmt( debug, "Light requested from start time ~ts, "
            "which will require lighting, as we are already after dusk (~ts).",
            [ time_utils:time_to_string( StartTime),
              time_utils:time_to_string( DuskTime ) ], State ),
        basic_utils:ignore_unused( DuskTime ) ),

    UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),
    ensure_planned_presence_transition( UnlitPscSim, StartTime,
                                        MaybePscTaskInfo, State ).




-doc """
Ensures that all possible lighting is activated, for all presence simulations.

Trusted their internal activation status, now, for easier direct testing, done
unconditionally.
""".
-spec ensure_all_lighting( wooper:state() ) -> wooper:state().
ensure_all_lighting( State ) ->

    PscTable = ?getAttr(presence_table),

    ensure_all_lighting( table:values( PscTable ), _PscTable=table:new(),
                         State ).



% (helper)
ensure_all_lighting( _PscSims=[], PscTable, State ) ->
    setAttribute( State, presence_table, PscTable );

% If not activated, must be switched on:
ensure_all_lighting( _PscSims=[ PscSim=#presence_simulation{
                        id=Id,
                        % Unconditional now: activated=false,
                        actuator_event_specs=ActEvSpecs } | T ],
                     PscTable, State ) ->

    oceanic:trigger_actuators( ActEvSpecs, ?getAttr(oc_srv_pid) ),

    NewPscSim = PscSim#presence_simulation{ activated=true },

    NewPscTable = table:add_new_entry( _K=Id, _V=NewPscSim, PscTable ),

    ensure_all_lighting( T, NewPscTable, State ).

% (clause used when was conditional)
%
% Nothing to do:
% ensure_all_lighting( _PscSims=[ PscSim=#presence_simulation{ id=Id } | T ],
%                    PscTable, State ) ->

%   NewPscTable = table:add_new_entry( _K=Id, _V=PscSim, PscTable ),

%   ensure_all_lighting( T, NewPscTable, State ).




-doc """
Ensures that there is no more lighting at all, for all presence simulations.

Trusted their internal activation status, now, for easier direct testing, done
unconditionally.
""".
-spec ensure_not_any_lighting( wooper:state() ) -> wooper:state().
ensure_not_any_lighting( State ) ->

    PscTable = ?getAttr(presence_table),

    ensure_not_any_lighting( table:values( PscTable ), _PscTable=table:new(),
                             State ).


% (helper)
ensure_not_any_lighting( _PscSims=[], PscTable, State ) ->
    setAttribute( State, presence_table, PscTable );

% If activated, must be switched off:
ensure_not_any_lighting( _PscSims=[ PscSim=#presence_simulation{
        id=PscId
        % Unconditional now: activated=true
                                                               } | T ],
                         PscTable, State ) ->

    NewPscSim = ensure_not_lighting( PscSim, _IsActivated=true, State ),

    NewPscTable = table:add_new_entry( _K=PscId, _V=NewPscSim, PscTable ),

    ensure_not_any_lighting( T, NewPscTable, State ).


% (clause used when was conditional)
%
% Not activated, nothing to do:
% ensure_not_any_lighting(
%       _PscSims=[ PscSim=#presence_simulation{ id=PscId } | T ],
%       PscTable, State ) ->

%   cond_utils:if_defined( us_main_debug_presence_simulation,
%       send_psc_trace_fmt( debug, "ensure_not_any_lighting: "
%           "presence simulation #~B was already not activated.",
%           [ PscId ], State ) ),

%   NewPscTable = table:add_new_entry( _K=PscId, _V=PscSim, PscTable ),

%   ensure_not_any_lighting( T, NewPscTable, State ).



-doc """
Returns the relevent celestial times, once computed if necessary.

If defined, celestial times are expected to be correct (deprecated times shall
have been set to `undefined`).
""".
-spec get_celestial_info( option( celestial_info() ), wooper:state() ) ->
                                celestial_info().
get_celestial_info( _MaybeCelestialTimes=undefined, State ) ->
    resolve_logical_milestones( ?getAttr(server_location), State );

get_celestial_info( CelestialTimes, _State ) ->
    CelestialTimes.



-doc "Computes (if possible) the actual dawn/dusk times.".
-spec resolve_logical_milestones( option( position() ), wooper:state() ) ->
                                    celestial_info().
resolve_logical_milestones( _MaybeSrvLoc=undefined, State ) ->

    DefaultDawnTime = { 8, 15, 0 },
    DefaultDuskTime = { 19, 15, 0 },

    ?warning_fmt( "No server location specified, logical milestones cannot be "
        "determined; using fixed, default deadlines: "
        "~ts for dawn, ~ts for dusk. ",
        [ time_utils:time_to_string( DefaultDawnTime ),
          time_utils:time_to_string( DefaultDuskTime ) ] ),

    { erlang:date(), { DefaultDawnTime, DefaultDuskTime } };

resolve_logical_milestones( SrvLoc={ LatDegrees, LongDegrees }, State ) ->

    Date = erlang:date(),

    LatRadians = math_utils:degrees_to_radians( LatDegrees ),
    % LongRadians = math_utils:degrees_to_radians( LongDegrees ),

    % Refer to https://www.astrolabe-science.fr/duree-du-jour-et-latitude/; we
    % compute based on universal, UTC time before converting to local time:

    AngleRad = math_utils:degrees_to_radians( 23.4 ),

    DayInYear = time_utils:get_day_in_year( Date ),

    Degrees = 360 * ( DayInYear - 81 ) / 365.2422,

    DeclinationDegrees = math_utils:radians_to_degrees(
        math:asin( math:sin( AngleRad )
            * math:sin( math_utils:degrees_to_radians( Degrees ) ) ) ),

    { RiseHour, SetHour } =
        get_sun_rise_and_set_times( LatRadians, DeclinationDegrees ),

    % Correction regarding longitude:
    %
    % (4 minutes per degree, then converted in decimal hours)
    %
    LongCorrection = LongDegrees * 4 / 60,

    LongCorrectedRiseHour = RiseHour - LongCorrection,
    LongCorrectedSetHour  = SetHour  - LongCorrection,

    % Clamping leap years:
    DayIndex = case DayInYear of

        366 ->
            365;

        _ ->
            DayInYear

    end,

    % Applying the time equation:

    % Index start at 1:
    TECorrection =
        table:get_value( DayIndex, ?getAttr(time_equation_table) ) / 60,

    TECorrectedRiseHour = LongCorrectedRiseHour + TECorrection,
    TECorrectedSetHour  = LongCorrectedSetHour  + TECorrection,

    UTCDawnTime = from_decimal_hour( TECorrectedRiseHour ),
    UTCDuskTime = from_decimal_hour( TECorrectedSetHour ),


    UTCDawnTimestamp = { Date, UTCDawnTime },

    % Both for time zone and Daylight Saving Time:
    LocalDawnTimestamp = { LocalDawnDate, LocalDawnTime } =
        calendar:universal_time_to_local_time( UTCDawnTimestamp ),

    % If ever dates where to change (always possible if distant from Greenwich):
    LocalDawnDate =:= Date orelse
        ?error_fmt( "Unexpected date change when transforming UTC "
            "to local dawn time: ~ts converts into ~ts.",
            [ time_utils:timestamp_to_string( UTCDawnTimestamp ),
              time_utils:timestamp_to_string( LocalDawnTimestamp ) ] ),


    UTCDuskTimestamp = { Date, UTCDuskTime },

    LocalDuskTimestamp = { LocalDuskDate, LocalDuskTime } =
        calendar:universal_time_to_local_time( UTCDuskTimestamp ),

    LocalDuskDate =:= Date orelse
        ?error_fmt( "Unexpected date change when transforming UTC "
            "to local dusk time: ~ts converts into ~ts.",
            [ time_utils:timestamp_to_string( UTCDuskTimestamp ),
              time_utils:timestamp_to_string( LocalDuskTimestamp ) ] ),

    % To account for dim daylight (forcing to switch on longer, in order to
    % compensate for insufficient light) - even if atmospheric refraction will
    % help a bit - the following duration margins, expressed in seconds, apply:

    % 15 minutes were too large, 5 are better:
    EnoughLightAfterDawnMargin  = 5*60,
    EnoughLightBeforeDuskMargin = 5*60,

    % To end the lighting a little later than the actual forecast dawn:
    RetainedDawnTime = time_utils:offset_time( LocalDawnTime,
                                               EnoughLightAfterDawnMargin ),

    % To start the lighting a little earlier than the actual forecast dusk:
    RetainedDuskTime = time_utils:offset_time( LocalDuskTime,
                                               -EnoughLightBeforeDuskMargin ),

    send_psc_trace_fmt( info,
        "For the specified server location (~ts) and date, "
        "computed following deadlines this day (with some light "
        "margins included): ~ts for dawn and ~ts for dusk.~n~n"
        "Raw celestial timestamps were indeed:~n"
        " - in UTC time: ~ts for dawn and ~ts for dusk~n"
        " - in local time (with time zone and DST, before applying "
        "margins): ~ts for dawn and ~ts for dusk",
        [ unit_utils:position_to_string( SrvLoc ),
          time_utils:time_to_string( RetainedDawnTime ),
          time_utils:time_to_string( RetainedDuskTime ),
          time_utils:timestamp_to_string( UTCDawnTimestamp ),
          time_utils:timestamp_to_string( UTCDuskTimestamp ),
          time_utils:timestamp_to_string( LocalDawnTimestamp ),
          time_utils:timestamp_to_string( LocalDuskTimestamp ) ],
        State ),

    { Date, { RetainedDawnTime, RetainedDuskTime } }.



% Not unit_utils:latitude(), as would be degrees:
-spec get_sun_rise_and_set_times( radians(), declination() ) ->
                                        { float(), float() }.
get_sun_rise_and_set_times( LatRadians, DeclinationDegrees ) ->

    DeclinationRadians = math_utils:degrees_to_radians( DeclinationDegrees ),

    % Decimal hour:
    ZeroHourDegrees = math_utils:radians_to_degrees( math:acos(
        -math:tan( LatRadians ) * math:tan( DeclinationRadians ) ) ),

    ZeroHourFrac = ZeroHourDegrees / 15,

    { 12 - ZeroHourFrac, 12 + ZeroHourFrac }.



-doc "Converts a decimal hour to hours and minutes.".
-spec from_decimal_hour( float() ) -> time().
from_decimal_hour( DecHour ) ->
    Hours = math_utils:floor( DecHour ),
    DecMinutes = 60 * ( DecHour - Hours ),
    Minutes = math_utils:floor( DecMinutes ),

    % Not round/1, as could lead to (improper) 60 seconds:
    Seconds = math_utils:floor( 60 * ( DecMinutes - Minutes ) ),

    { Hours, Minutes, Seconds }.



-doc """
Setups the monitoring of Oceanic, with test reports and/or periodic restarts.
""".
-spec init_oceanic_monitor( option( oceanic_server_pid() ), wooper:state() ) ->
                                        option( task_id() ).
init_oceanic_monitor( _MaybeOcSrvPid=undefined, _State ) ->
    undefined;

init_oceanic_monitor( _OcSrvPid, State ) ->

    % Every 4 hours:
    DHMSPeriodicity = { _D=0, _H=4, _M=0, _S=0 },

    send_oc_mon_trace_fmt( info, "Starting the monitoring of Oceanic, "
        "based on a periodicity of ~ts.", [
        time_utils:dhms_to_string( DHMSPeriodicity ) ], State ),

    class_USScheduler:get_server_pid() ! { registerTask,
        [ _CmdMsg=monitorOceanic, _StartTime=flexible, DHMSPeriodicity,
          _Count=unlimited ], self() },

    receive

        { wooper_result, { task_registered, MonTaskId } } ->
            send_oc_mon_trace_fmt( debug,
                "Oceanic monitoring task #~B defined.", [ MonTaskId ], State ),
            MonTaskId

    end.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    ?debug_fmt( "Deletion initiated, while state is: ~ts.",
                [ to_string( State ) ] ),

    case ?getAttr(oc_srv_pid) of

        undefined ->
            ok;

        OcSrvPid ->
            oceanic:synchronous_stop( OcSrvPid )

    end,

    ?info( "Deleted." ),
    State.



% Method section, except for remote commands.


-doc """
Notifies this server of state information regarding a new set of devices.

Typically called back by an Oceanic server, after it received a
`getAllDeviceStateInfo` message.
""".
-spec notifyDeviceStateInfos( wooper:state(), [ device_state_info() ] ) ->
                                                    oneway_return().
notifyDeviceStateInfos( State, DevStateInfos ) ->

    ?debug_fmt( "Received ~B device state information:~n ~p",
                [ length( DevStateInfos ), DevStateInfos ] ),

    AugmentedDevTable = lists:foldl( fun register_device_state_from_info/2,
        _Acc0=?getAttr(device_table), _List=DevStateInfos ),

    % Have to rebuild it from scratch:
    { NewDevSpellTree, WithSplitDevTable, NewDevSplitterTable } =
        recompute_splitters( AugmentedDevTable ),

    UpdatedState = setAttributes( State, [
        { device_table, WithSplitDevTable },
        { device_spell_tree, NewDevSpellTree },
        { dev_splitter_table, NewDevSplitterTable } ] ),

    wooper:return_state( UpdatedState ).



-doc """
Converts the specified device state information into a state of a new device,
which is registered.

Typically useful to register configured devices initially.
""".
-spec register_device_state_from_info( device_state_info(), device_table() ) ->
                                                                device_table().
register_device_state_from_info( DevStateInfo={ Eurid, MaybeBinName,
        MaybeBinShortName, MaybeEepId, MaybeDevType, MaybeLastSeenTimestamp,
        MaybeAvailStatus }, DevTable ) ->

    case table:lookup_entry( _K=Eurid, DevTable ) of

        key_not_found ->
            DevState = #device_state{
                eurid=Eurid,
                name=MaybeBinName,
                short_name=MaybeBinShortName,
                % splitter to be set next
                type=MaybeDevType,
                eep_ids=set_utils:singleton_maybe( MaybeEepId ),
                last_seen=MaybeLastSeenTimestamp,
                availability=MaybeAvailStatus },

            table:add_entry( Eurid, DevState, DevTable );


        { value, DevState } ->
            trace_bridge:warning_fmt( "Received information (~p) for device "
                "of EURID ~ts, ignoring them as it was already registered "
                "as: ~ts",
                [ DevStateInfo, oceanic_text:eurid_to_string( Eurid ),
                  device_state_to_string( DevState ) ] ),
            DevTable

    end.



-doc """
Requests the specified presence simulation to be updated, typically after a
milestone, as planned through a scheduler.
""".
-spec updatePresenceSimulation( wooper:state(), presence_sim_id() ) ->
                                            oneway_return().
updatePresenceSimulation( State, PscId ) ->

    PscTable = ?getAttr(presence_table),

    PscSim = table:get_value( _K=PscId, PscTable ),

    % As in the meantime (between the planning and this actual scheduling), the
    % presence simulation may have been disabled, or somebody may have come back
    % home:
    %
    UpdateState = case ?getAttr(presence_simulation_enabled)
                                andalso not ?getAttr(actual_presence) of

        true ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Requested to update ~ts "
                    "(planning next).",
                    [ presence_simulation_to_string( PscSim ) ], State ) ),

            % Newer scheduling possibly planned:
            { UpdatedPscSim, MaybeCelestialInfo } = manage_presence_simulation(
                PscSim, erlang:time(), ?getAttr(celestial_info), State ),

            UpdatedPscTable = table:add_entry( PscId, UpdatedPscSim, PscTable ),

            setAttributes( State, [ { presence_table, UpdatedPscTable },
                                    { celestial_info, MaybeCelestialInfo } ] );


        false ->
            cond_utils:if_defined( us_main_debug_presence_simulation,
                send_psc_trace_fmt( debug, "Requested to update ~ts "
                    "(stopping).",
                    [ presence_simulation_to_string( PscSim ) ], State ) ),

            IsActivated = PscSim#presence_simulation.activated,

            UnlitPscSim = ensure_not_lighting( PscSim, IsActivated, State ),

            % No planning task to withdraw.

            UpdatedPscTable = table:add_entry( PscId, UnlitPscSim, PscTable ),

            setAttribute( State, presence_table, UpdatedPscTable )

    end,

    wooper:return_state( UpdateState ).



-doc """
Called whenever having to update presence programs, typically at midnight for
this new day.

See also the `midnight_task_id` attribute.
""".
-spec updatePresencePrograms( wooper:state() ) -> oneway_return().
updatePresencePrograms( State ) ->

    PscTable = ?getAttr(presence_table),

    UpdatedState = case table:values( PscTable ) of

        [] ->
            case ?getAttr(midnight_task_id) of

                undefined ->
                    send_psc_trace( error, "Triggered whereas no presence "
                        "simulation is to update; moreover no midnight task "
                        "is tracked.", State ),
                    State;

                MidnightTaskId ->
                    send_psc_trace_fmt( warning, "Requested to update presence "
                        "programs whereas no presence simulation is registered;"
                        " unscheduling these updates (task #~B).",
                        [ MidnightTaskId ], State ),

                    class_USScheduler:get_server_pid() !
                        { unregisterTaskAsync, [ MidnightTaskId ] },

                    setAttribute( State, midnight_task_id, undefined )

            end;

        PscSims ->
            % Force a reevaluation thereof, once any past presence task is
            % cleared:

            SchedPid = class_USScheduler:get_server_pid(),

            ClearedPscTable = lists:foldl(
                fun( Psc, PTableAcc ) ->
                    ClearPsc = clear_any_presence_task( Psc, SchedPid, State ),
                    table:add_new_entry( _K=ClearPsc#presence_simulation.id,
                                         _V=ClearPsc,
                                         PTableAcc )
                end,
                _Acc0=table:new(),
                _List=PscSims ),

            ClearedState = setAttribute( State, presence_table,
                                         ClearedPscTable ),

            apply_presence_simulation( ClearedState )

    end,

    % Just for inspection purpose:
    cond_utils:if_defined( us_main_debug_home_automation,
                           class_USScheduler:get_server_pid() ! logState ),

    wooper:return_state( UpdatedState ).



-doc """
Stops any ongoing alarm, from the scheduler.

Typically called to disengage automatically a triggered alarm after a maximum
duration.
""".
-spec stopAlarmScheduled( wooper:state() ) -> oneway_return().
stopAlarmScheduled( State ) ->

    send_alarm_trace( info, "Requested (presumably by the scheduler) "
                      "to stop the alarm now.", State ),

    DoneState = setAttribute( State, alarm_stop_task_id, undefined ),

    StopState = apply_alarm_status( _NewStatus=false, DoneState ),

    wooper:return_state( StopState ).



-doc """
Called whenever having to monitor Oceanic (typically periodically).

See also the `oceanic_monitor_task_id` attribute.
""".
-spec monitorOceanic( wooper:state() ) -> const_oneway_return().
monitorOceanic( State ) ->

    OcSrvPid = ?getAttr(oc_srv_pid),

    MustRestart = case ?getAttr(oc_periodic_restart) of

        true ->
            send_oc_mon_trace( debug, "Unconditional periodic restarts "
                "enabled, so performing it now.", State ),
            true;

        false ->
            send_oc_mon_trace_fmt( debug, "Only conditional periodic restarts "
                "enabled, so testing whether the serial interface of "
                "the Oceanic server ~w is still available.", [ OcSrvPid ],
                State ),

            case oceanic:is_serial_available( OcSrvPid ) of

                true ->
                    send_oc_mon_trace( info, "Oceanic reported as available, "
                        "so no restarting done.", State ),
                    false;

                false ->
                    send_oc_mon_trace( warning, "Oceanic reported as not "
                        "available, so restarting it.", State ),
                    true

            end

    end,

    MustRestart andalso
        begin

            send_oc_mon_trace_fmt( info, "Restarting the serial interface "
                "of the Oceanic server ~w.", [ OcSrvPid ], State ),

            oceanic:restart_serial_interface( OcSrvPid ),

            send_oc_mon_trace( info, "Oceanic serial interface restarted.",
                               State ),

            oceanic:is_serial_available( OcSrvPid ) orelse
                send_oc_mon_trace( error, "Oceanic serial interface found "
                                   "unavailable despite restart.", State )

        end,

    wooper:const_return().



-doc """
Ensures that no past presence task lingers in the specified simulation.
""".
-spec clear_any_presence_task( presence_simulation(), scheduler_pid(),
                               wooper:state() ) -> presence_simulation().
clear_any_presence_task( Psc=#presence_simulation{
                    presence_task_info={ TaskId, _Time } }, SchedPid, State ) ->
    ?warning_fmt( "Clearing a task during the daily presence simulation "
        "update (abnormal) for ~ts.",
        [ presence_simulation_to_string( Psc ) ] ),

    SchedPid ! { unregisterTaskAsync, [ TaskId ] },
    Psc#presence_simulation{ presence_task_info=undefined };

% presence_task_info already set to 'undefined', as expected:
clear_any_presence_task( Psc, _SchedPid, _State ) ->
    Psc.



% Some helpers.


%-spec execute_command( device_id(), wooper:state() ) -> wooper:state().




% Management of messages sent by the Oceanic server:


-doc """
Handles a device-related first detection of a configured device (typically a
sensor report) notified by the specified Oceanic server.
""".
-spec onEnoceanConfiguredDeviceFirstSeen( wooper:state(), device_event(),
        device_description(), oceanic_server_pid() ) -> oneway_return().
onEnoceanConfiguredDeviceFirstSeen( State, DeviceEvent, BinDevDesc, OcSrvPid )
                                        when is_tuple( DeviceEvent ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

    % Longer description when first seen:
    Msg = text_utils:format( "The device '~ts', declared in the configuration, "
        "has been detected for the first time, based on the following "
        "event: ~ts.~n~nFull device information: ~ts.",
        [ BinDevName, oceanic_text:device_event_to_string( DeviceEvent ),
          BinDevDesc ] ),

    class_TraceEmitter:send_named_emitter( notice, State, Msg,
        get_trace_emitter_name_from( BinDevName ) ),

    RecState = record_new_device( DeviceEvent, State ),

    PresState = manage_presence_switching( DeviceEvent, RecState ),

    AlarmState = manage_alarm_switching( DeviceEvent, PresState ),

    wooper:return_state( AlarmState );


onEnoceanConfiguredDeviceFirstSeen( State, OtherEvent, BinDevDesc, OcSrvPid ) ->

    ?error_fmt( "Received an invalid first-seen device event (~p) "
        "from ~w for '~p', ignoring it.",
        [ OtherEvent, OcSrvPid, BinDevDesc ] ),

    wooper:const_return().



-doc """
Handles a device-related discovery of a device (typically a sensor report) that
was not in the configuration, as notified by the specified Oceanic server.
""".
-spec onEnoceanDeviceDiscovery( wooper:state(), device_event(),
        device_description(), oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceDiscovery( State, DeviceEvent, BinDevDesc, OcSrvPid )
                                when is_tuple( DeviceEvent ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

    % Longer description at this discovery time:
    Msg = text_utils:format( "The device '~ts' has been discovered "
        "(detected yet not declared in the configuration), based on "
        "the following event: ~ts.~n~nFull device information: ~ts.",
        [ BinDevName, oceanic_text:device_event_to_string( DeviceEvent ),
          BinDevDesc ] ),

    class_TraceEmitter:send_named_emitter( warning, State, Msg,
        get_trace_emitter_name_from( BinDevName ) ),

    RecState = record_new_device( DeviceEvent, State ),

    % If ever the device was not configured, yet declared as presence-switching:
    PresState = manage_presence_switching( DeviceEvent, RecState ),

    AlarmState = manage_alarm_switching( DeviceEvent, PresState ),

    wooper:return_state( AlarmState );


onEnoceanDeviceDiscovery( State, OtherEvent, BinDevDesc, OcSrvPid ) ->

    ?error_fmt( "Received an invalid discovery device event (~p) "
        "from ~w for '~p', ignoring it.",
        [ OtherEvent, OcSrvPid, BinDevDesc ] ),

    wooper:const_return().



-doc """
Handles a notification sent by the specified Oceanic server regarding the first
receiving of a telegram from an unresolved device (typically not configured and
of an EEP that could not be determined), whose telegrams thus cannot be (at
least fully) decoded.
""".
-spec onEnoceanUnresolvedDeviceFirstSeen( wooper:state(),
    unresolved_device_event(), device_description(), oceanic_server_pid() ) ->
            oneway_return().
onEnoceanUnresolvedDeviceFirstSeen(
        State,
        UnresolvedDevEvent=#unresolved_device_event{ source_eurid=Eurid },
        BinDevDesc, OcSrvPid ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    % Longer description when first seen:
    Msg = text_utils:format( "The device of EURID ~ts, not declared in the "
        "configuration and that cannot be resolved, "
        "has been detected for the first time, based on the following "
        "event: ~ts.~n~nFull device information: ~ts.",
        [ oceanic_text:eurid_to_string( Eurid ),
          oceanic_text:device_event_to_string( UnresolvedDevEvent ),
          BinDevDesc ] ),

    BinDevName = get_trace_emitter_name_for_unresolved( Eurid ),

    class_TraceEmitter:send_named_emitter( notice, State, Msg, BinDevName ),

    RecState = record_new_device( UnresolvedDevEvent, State ),

    % Not relevant here:
    %PresState = manage_presence_switching( DeviceEvent, RecState ),
    %AlarmState = manage_alarm_switching( DeviceEvent, PresState ),

    wooper:return_state( RecState ).



-doc """
Handles a notification sent by the specified Oceanic server regarding the
receiving of new telegrams (after the first one) from an unresolved device
(typically not configured and of an EEP that could not be determined), whose
telegrams thus cannot be (at least fully) decoded.
""".
-spec onEnoceanUnresolvedDevice( wooper:state(), unresolved_device_event(),
        oceanic_server_pid() ) -> const_oneway_return().
onEnoceanUnresolvedDevice( State,
        UnresolvedDevEvent=#unresolved_device_event{ source_eurid=Eurid },
        OcSrvPid ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    % Verbose, yet needed so that this event is reported in the traces at least
    % once:
    %
    %cond_utils:if_defined( us_main_debug_home_automation,
    %   begin
    Msg = oceanic_text:device_event_to_short_string( UnresolvedDevEvent ),

    BinDevName = get_trace_emitter_name_for_unresolved( Eurid ),

    class_TraceEmitter:send_named_emitter( info, State, Msg, BinDevName ),
    %   end),

    wooper:const_return().


-doc "Records the existence of a device not expected to be already detected.".
-spec record_new_device( device_event(), wooper:state() ) -> wooper:state().
record_new_device( DeviceEvent, State ) ->

    DevEurid = oceanic:get_source_eurid( DeviceEvent ),

    MaybeEepId = oceanic:get_maybe_eep( DeviceEvent ),

    DevType = oceanic:guess_type_from_eep( MaybeEepId ),

    InitStatus = case DevType of

        % Only status that does not get fully updated at each read, and which
        % thus must be initialised:
        %
        double_rocker ->
            { _AIState=is_released, _AOState=is_released,
              _BIState=is_released, _BOState=is_released };

        _ ->
            undefined

    end,

    DevTable = ?getAttr(device_table),

    % This device may be already configured or not:

    BaseDevState = #device_state{
        eurid=DevEurid,
        name=oceanic:get_best_device_name_from( DeviceEvent, DevType ),
        short_name=oceanic:get_maybe_device_short_name( DeviceEvent ),

        % Not 'undefined', in order to be picked to be updated next:
        splitter=ok,

        % Is set just after:
        eep_ids=undefined,

        initial_event=DeviceEvent,
        last_event=DeviceEvent,

        % No: last_seen=oceanic:get_last_seen_info( DeviceEvent ),
        last_seen=oceanic:get_timestamp( DeviceEvent ),

        availability=online,
        current_status=get_status_from_event( DeviceEvent,
                                              InitStatus ) },

    NewDevState = case table:lookup_entry( DevEurid, DevTable ) of

        % Not already configured, hence directly set:
        key_not_found ->
            BaseDevState#device_state{
                name=oceanic:get_best_device_name_from( DeviceEvent, DevType ),
                short_name=oceanic:get_maybe_device_short_name( DeviceEvent ),
                type=DevType,
                eep_ids=set_utils:singleton_maybe( MaybeEepId ) };

        % Was configured; thus merged, priority going to any
        % initially-configured field:
        %
        { value, _PrevDevState=#device_state{ name=MaybePrevName,
                                              short_name=MaybePrevShortName,
                                              type=MaybePrevType,
                                              eep_ids=PrevEepIds } } ->
            NewEepIds = case set_utils:is_empty( PrevEepIds ) of

                true ->
                    set_utils:singleton_maybe( MaybeEepId );

                false ->
                    PrevEepIds

            end,

            BaseDevState#device_state{

                name=basic_utils:set_option( MaybePrevName,
                    oceanic:get_best_device_name_from( DeviceEvent, DevType ) ),

                short_name=basic_utils:set_option( MaybePrevShortName,
                    oceanic:get_maybe_device_short_name( DeviceEvent ) ),

                type=basic_utils:set_option( MaybePrevType, DevType ),

                eep_ids=NewEepIds }

    end,

    RicherDevTable = table:add_entry( DevEurid, NewDevState, DevTable ),

    % Previously determining splitters only for devices that may be acted upon;
    % now for all devices, at least as they can at least be requested to be
    % described:

    % TargetEeps = oceanic:get_actuator_eeps() ,

    %TargetEeps = set_utils:union( [ oceanic:get_sensor_eeps(),
    %    oceanic:get_command_eeps(), oceanic:get_actuator_eeps() ] ),

    %case set_utils:member( _Elem=MaybeEepId, TargetEeps ) of

    %    true ->

    % When a name is added, all splitters have to be recomputed:
    { NewDevSpellTree, NewDevTable, NewDevSplitterTable } =
        recompute_splitters( RicherDevTable ),

     setAttributes( State, [
         { device_table, NewDevTable },
         { device_spell_tree, NewDevSpellTree },
         { dev_splitter_table, NewDevSplitterTable } ] ).


    %    false ->
    %        State
    %
    %end.



% (helper)
-spec recompute_splitters( device_table() ) ->
                    { spell_tree(), device_table(), device_splitter_table() }.
recompute_splitters( DevTable ) ->

    % When a name is added, all splitters have to be recomputed:
    ToSplitPairs =
        %[ P || P={ _Eurid, DS } <- table:enumerate( DevTable ),
        %       DS#device_state.splitter =/= undefined ],
        table:enumerate( DevTable ),

    { Eurids, ToSplitDevStates } = lists:unzip( ToSplitPairs ),

    % Any short name otherwise EURID; in order:
    DevDesignators =
        [ get_best_device_designator( DS ) || DS <- ToSplitDevStates ],

    %trace_utils:debug_fmt( "Device designators: ~p", [ DevDesignators ] ),

    DevSpellTree = spell_tree:create( DevDesignators ),

    cond_utils:if_defined( us_main_debug_actions, trace_utils:debug_fmt(
        "Recomputed spell tree for splitters: ~ts",
        [ spell_tree:to_string( DevSpellTree ) ] ) ),

    % Arbitrary order:
    AllSplitters = spell_tree:get_splitters( DevSpellTree ),

    { OrderedSplitDevStates, DevSplitterTable } =
        assign_splitters( ToSplitDevStates, DevDesignators, AllSplitters ),

    SplitEntries = lists:zip( Eurids, OrderedSplitDevStates ),

    % Just overwrites the update entries (and keep others):
    UpdatedDevTable = table:add_entries( SplitEntries, DevTable ),

    { DevSpellTree, UpdatedDevTable, DevSplitterTable }.



-doc "Assigns to each device its splitters, preserving the device order.".
-spec assign_splitters( [ device_state() ], [ user_device_designator() ],
        [ splitter() ] ) -> { [ device_state() ], device_splitter_table() }.
assign_splitters( DevStates, DevDesignators, Splitters ) ->

    % Prepare (recompose) once for easier matching:
    SplitPairs = [ { Pfx++Rest, Sp } || Sp={ Pfx, Rest } <- Splitters ],

    assign_splitters( DevStates, DevDesignators, SplitPairs, _AccDevs=[],
                      _DSTable=table:new() ).



% (helper)
% All three lists exhausted:
assign_splitters( _DevStates=[], _DevDesignators, _SplitPairs, AccDevs,
                  DSTable ) ->
    { lists:reverse( AccDevs ), DSTable };

assign_splitters( _DevStates=[ DevState=#device_state{ eurid=Eurid } | T ],
                  _DevDesignators=[ Des | TDes ], SplitPairs, AccDevs,
                  DSTable ) ->

    { Splitter, ShrunkSplitPairs } =
        list_table:extract_entry( _K=Des, SplitPairs ),

    UpdatedDevState = DevState#device_state{ splitter=Splitter },

    UpdatedDSTable = table:add_new_entry(
        spell_tree:bin_concatenate( Splitter ), _V=Eurid, DSTable ),

    assign_splitters( T, TDes, ShrunkSplitPairs,
                      [ UpdatedDevState | AccDevs ], UpdatedDSTable ).


-doc """
Returns the best designator for the device whose state is specified: any short
name, otherwise its EURID.
""".
-spec get_best_device_designator( device_state() ) -> ustring().
% No short name:
get_best_device_designator( #device_state{ eurid=Eurid,
                                           short_name=undefined } ) ->
    oceanic_text:eurid_to_string( Eurid );

get_best_device_designator( #device_state{ short_name=DevShortName } ) ->
    text_utils:atom_to_string( DevShortName ).





-doc """
Handles a device-related event (typically a sensor report) notified by the
specified Oceanic server.

This device has been already discovered; tells whether this device was
considered lost until now.

This is by far the most frequent device-related message (once a
discovery-related message has been received first).
""".
-spec onEnoceanDeviceEvent( wooper:state(), device_event(), back_online_info(),
                            oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceEvent( State, DeviceEvent, _BackOnlineInfo=undefined, OcSrvPid )
                                when is_tuple( DeviceEvent ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    % Verbose, yet needed so that this event is reported in the traces at least
    % once:
    %
    %cond_utils:if_defined( us_main_debug_home_automation,
    %   begin
    Msg = oceanic_text:device_event_to_short_string( DeviceEvent ),
    BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),
    class_TraceEmitter:send_named_emitter( info, State, Msg,
        get_trace_emitter_name_from( BinDevName ) ),
    %   end),

    ProcessedState = process_device_event( DeviceEvent, State ),

    PresState = manage_presence_switching( DeviceEvent, ProcessedState ),

    AlarmState = manage_alarm_switching( DeviceEvent, PresState ),

    wooper:return_state( AlarmState );


onEnoceanDeviceEvent( State, DeviceEvent, _BackOnlineInfo=BinDevDesc, OcSrvPid )
                                when is_tuple( DeviceEvent ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

    Msg = text_utils:format( "The device '~ts', which was considered lost, "
        "is back online: ~ts.~n~nFull device information: ~ts.",
        [ BinDevName, oceanic_text:device_event_to_string( DeviceEvent ),
          BinDevDesc ] ),

    class_TraceEmitter:send_named_emitter( notice, State, Msg,
        get_trace_emitter_name_from( BinDevName ) ),

    ProcessedState = process_device_event( DeviceEvent, State ),

    PresState = manage_presence_switching( DeviceEvent, ProcessedState ),

    AlarmState = manage_alarm_switching( DeviceEvent, PresState ),

    wooper:return_state( AlarmState );


onEnoceanDeviceEvent( State, OtherEvent, _BackOnlineInfo, OcSrvPid ) ->

    ?error_fmt( "Received an invalid device event (~p) from ~w, "
                "ignoring it.", [ OtherEvent, OcSrvPid ] ),

    wooper:const_return().



-doc """
Updates, based on the specified event, the stored state of the corresponding
device.

Any reaction to this event to be done next by the caller.
""".
-spec process_device_event( device_event(), wooper:state() ) -> wooper:state().
process_device_event( DeviceEvent=#teach_request_event{}, State ) ->
    trace_utils:debug_fmt( "(ignoring ~ts)",
        [ oceanic_text:device_event_to_string( DeviceEvent ) ] ),
    State;

process_device_event( DeviceEvent, State ) ->

    DevEurid = oceanic:get_source_eurid( DeviceEvent ),

    DevTable = ?getAttr(device_table),

    case table:lookup_entry( _K=DevEurid, DevTable ) of

        key_not_found ->
            ?error_fmt( "No device found for ~ts, after event ~ts, "
                        "which is therefore ignored.", [
                    oceanic_text:eurid_to_string( DevEurid ),
                    oceanic_text:device_event_to_string( DeviceEvent ) ] ),
            State;

        { value, DevState=#device_state{ current_status=PrevStatus } } ->

            NewDevState = DevState#device_state{
                last_seen=oceanic:update_last_seen_info( DeviceEvent,
                    DevState#device_state.last_seen ),
                availability=online,
                current_status=get_status_from_event( DeviceEvent,
                                                      PrevStatus ) },

            NewDevTable = table:add_entry( DevEurid, NewDevState, DevTable ),

            setAttribute( State, device_table, NewDevTable )

    end.



-doc """
Returns an updated status corresponding to the specified device event,
considering the specified previous device status.
""".
% A priori, no need to take into account the device type here; however the
% current (soon previous) status is needed in some cases (e.g. double rocker),
% when an event can affect only partially a status (e.g. only a subset of the
% buttons).
%
-spec get_status_from_event( device_event(), device_state() ) ->
                                                device_status().
get_status_from_event( _DeviceEvent=#thermometer_event{
                            temperature=Temperature }, _PrevStatus ) ->
    Temperature;

get_status_from_event( _DeviceEvent=#thermo_hygro_event{
                            temperature=Temperature,
                            relative_humidity=RelHumidity }, _PrevStatus ) ->
    { Temperature, RelHumidity };


get_status_from_event( _DeviceEvent=#motion_detector_event{
                            motion_detected=MotionDetected,
                            supply_voltage=MaybeVoltage }, _PrevStatus ) ->
    { MotionDetected, MaybeVoltage };

get_status_from_event( _DeviceEvent=#motion_detector_event_with_illumination{
                            motion_detected=MotionDetected,
                            illuminance=MaybeIlluminance,
                            supply_voltage=MaybeVoltage }, _PrevStatus ) ->
    { MotionDetected, MaybeVoltage, MaybeIlluminance };



get_status_from_event( _DeviceEvent=#single_input_contact_event{
                            contact=ContactStatus }, _PrevStatus ) ->
    ContactStatus;

get_status_from_event( _DeviceEvent=#push_button_switch_event{
                            transition=Transition }, _PrevStatus ) ->
    oceanic:button_transition_to_state( Transition );

get_status_from_event( _DeviceEvent=#double_rocker_switch_event{
                            %application_style=1,
                            %first_action_button=FirstButLoc,
                            first_designator=FirstDesignator,
                            energy_bow=ButTrans,
                            %second_action_button=SecondButLoc,
                            second_designator=SecondDesignator,
                            second_action_valid=IsSecondValid },
                       _PrevStatus={ AIState, AOState, BIState, BOState } ) ->

    NewButState = oceanic:button_transition_to_state( ButTrans ),

    FirstStatus = { FirstAIState, FirstAOState, FirstBIState, FirstBOState } =
            case FirstDesignator of

        button_ai ->
            { NewButState, AOState, BIState, BOState };

        button_ao ->
            { AIState, NewButState, BIState, BOState };

        button_bi ->
            { AIState, AOState, NewButState, BOState };

        button_bo ->
            { AIState, AOState, BIState, NewButState }

    end,

    case IsSecondValid of

       true ->
            % We think that this means that the same transition applies:
            % (same as for the first designator above)
            %
            case SecondDesignator of

                button_ai ->
                    { NewButState, FirstAOState, FirstBIState, FirstBOState };

                button_ao ->
                    { FirstAIState, NewButState, FirstBIState, FirstBOState };

                button_bi ->
                    { FirstAIState, FirstAOState, NewButState, FirstBOState };

                button_bo ->
                    { FirstAIState, FirstAOState, FirstBIState, NewButState }

            end;

       false ->
            FirstStatus

    end;


get_status_from_event( _DeviceEvent=#double_rocker_multipress_event{
                    % Not managing three_or_four or undefined:
                    button_counting=none, % Apparently to be understood as 'all'
                    energy_bow=ButtonTransition },
                       _PrevStatus ) ->

    NewButState = oceanic:button_transition_to_state( ButtonTransition ),

    { NewButState, NewButState, NewButState, NewButState };


get_status_from_event( _DeviceEvent=#smart_plug_status_report_event{
                         output_power=OutputPower,
                         power_failure_detected=PowerFailureDetected,
                         overcurrent_triggered=SwitchedOffDueToOvercurrent,
                         hardware_status=HardwareStatus,
                         local_control_enabled=LocalControlEnabled },
                       _PrevStatus ) ->
    { OutputPower, PowerFailureDetected, SwitchedOffDueToOvercurrent,
      HardwareStatus, LocalControlEnabled };

% If matching, it is the sign that the above clause for
% double_rocker_switch_event did not match, as the previous status was not a
% quadruplet of booleans, but a quintuplet; this happens because some smart
% plugs (e.g. Eltako ones) send a double_rocker_switch_event whenever their
% on/off ("local control") button is pressed; not an interesting event here:
%
get_status_from_event( _DeviceEvent=#double_rocker_switch_event{},
                       PrevStatus={ _OutputPower, _PowerFailureDetected,
                            _SwitchedOffDueToOvercurrent, _HardwareStatus,
                            _LocalControlEnabled } ) ->
    PrevStatus;

% Remaining 'undefined':
get_status_from_event( _DeviceEvent=#unresolved_device_event{},
                       PrevStatus ) ->
    PrevStatus;

get_status_from_event( OtherDeviceEvent, PrevStatus ) ->

    trace_bridge:error_fmt( "Unexpected (thus ignored) device event: ~ts.",
        [ oceanic_text:device_event_to_string( OtherDeviceEvent ) ] ),

    % Thus unchanged:
    PrevStatus.




-doc """
Handles a teach-in event of a device, as notified by the specified Oceanic
server.
""".
-spec onEnoceanDeviceTeachIn( wooper:state(), device_event(),
        device_description(), oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceTeachIn( State, DeviceEvent, BinDevDesc, OcSrvPid )
                                when is_tuple( DeviceEvent ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    BinDevName = oceanic:get_best_device_name_from( DeviceEvent ),

    % Longer description at this teach-in time:
    Msg = text_utils:format( "The device '~ts' has emitted the following "
        "teach-in event: ~ts.~n~nFull device information: ~ts.",
        [ BinDevName, oceanic_text:device_event_to_string( DeviceEvent ),
          BinDevDesc ] ),

    class_TraceEmitter:send_named_emitter( notice, State, Msg,
        get_trace_emitter_name_from( BinDevName ) ),

    RecState = record_new_device( DeviceEvent, State ),

    wooper:return_state( RecState );


onEnoceanDeviceTeachIn( State, OtherEvent, BinDevDesc, OcSrvPid ) ->
    ?error_fmt( "Received an invalid teach-in device event (~p) "
        "from ~w for '~p', ignoring it.",
        [ OtherEvent, OcSrvPid, BinDevDesc ] ),

    wooper:const_return().



-doc """
Handles a device-related request failure regarding the specified operation
(typically a smart plug failing to report a state change) notified by the
specified Oceanic server.
""".
-spec onEnoceanDeviceRequestFailed( wooper:state(), eurid(),
    option( device_name() ), option( device_short_name() ), device_operation(),
    oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceRequestFailed( State, DeviceEurid, MaybeDevName,
        MaybeDevShortName, DeviceOp, OcSrvPid ) ->

    Msg = text_utils:format( "The ~ts request on ~ts is reported as failed "
        "by the Oceanic server ~w.",
        [ DeviceOp, oceanic_text:get_best_naming( MaybeDevName,
            MaybeDevShortName, DeviceEurid ), OcSrvPid ] ),

    DevName = case MaybeDevName of

        undefined ->
            "unnamed device";

        DName ->
            DName

    end,

    class_TraceEmitter:send_named_emitter( error, State, Msg,
        get_trace_emitter_name_from( DevName ) ),

    DevTable = ?getAttr(device_table),
    DevState = table:get_value( _K=DeviceEurid, DevTable ),

    UpdatedDevState = DevState#device_state{ availability=unreachable },

    UpdatedDevTable = table:add_entry( DeviceEurid, UpdatedDevState, DevTable ),

    FailedState = setAttribute( State, device_table, UpdatedDevTable ),

    wooper:return_state( FailedState ).


-doc """
Handles the detection of the vanishing of the specified device by the specified
Oceanic server.
""".
-spec onEnoceanDeviceLost( wooper:state(), eurid(), device_name(),
        device_description(), boolean(), timestamp(), milliseconds(),
        oceanic_server_pid() ) -> oneway_return().
onEnoceanDeviceLost( State, DeviceEurid, BinDeviceName, BinDevDesc,
        _IsNewLoss=true, LastSeenTimestamp, TimeOutMs, OcSrvPid ) ->

    Msg = text_utils:format( "The device '~ts' (EURID: ~ts) is just considered "
        "lost, last seen on ~ts, after a waiting of ~ts) "
        "by the Oceanic server ~w.~n~nFull device information: ~ts.",
        [ BinDeviceName, oceanic_text:eurid_to_string( DeviceEurid ),
          time_utils:timestamp_to_string( LastSeenTimestamp ),
          time_utils:duration_to_string( TimeOutMs ), OcSrvPid, BinDevDesc ] ),

    class_TraceEmitter:send_named_emitter( error, State, Msg,
        get_trace_emitter_name_from( BinDeviceName ) ),

    DevTable = ?getAttr(device_table),
    DevState = table:get_value( _K=DeviceEurid, DevTable ),

    UpdatedDevState = DevState#device_state{ last_seen=LastSeenTimestamp,
                                             availability=lost },

    UpdatedDevTable = table:add_entry( DeviceEurid, UpdatedDevState, DevTable ),

    LostState = setAttribute( State, device_table, UpdatedDevTable ),

    wooper:return_state( LostState );


onEnoceanDeviceLost( State, DeviceEurid, BinDeviceName, BinDevDesc,
        _IsNewLoss=false, LastSeenTimestamp, TimeOutMs, OcSrvPid ) ->

    Msg = text_utils:format( "Device '~ts' (EURID: ~ts) still considered lost "
        "(last seen on ~ts, after a waiting of ~ts) "
        "by Oceanic server ~w.~n~nFull device information: ~ts.",
        [ BinDeviceName, oceanic_text:eurid_to_string( DeviceEurid ),
          time_utils:timestamp_to_string( LastSeenTimestamp ),
          time_utils:duration_to_string( TimeOutMs ), OcSrvPid, BinDevDesc ] ),

    % Device state a priori not needing any change.

    class_TraceEmitter:send_named_emitter( warning, State, Msg,
        get_trace_emitter_name_from( BinDeviceName ) ),

    wooper:const_return().



-doc """
Handles a possible jamming attempt, as suspected and reported by the specified
Oceanic server.
""".
-spec onEnoceanJamming( wooper:state(), bytes_per_second(),
                        oceanic_server_pid() ) -> const_oneway_return().
onEnoceanJamming( State, TrafficLevel, OcSrvPid ) ->

    % Check:
    OcSrvPid = ?getAttr(oc_srv_pid),

    ?alert_fmt( "Received a notification from Oceanic server ~w of a "
        "possible jamming attempt (traffic level of ~B bytes per second).",
        [ OcSrvPid, TrafficLevel ] ),

    wooper:const_return().




% Section for remote commands, sent by clients like the US-Main controller
% script (see control-us-main.sh).



% Presence-related commands:


-doc """
Requests whether, from the point of view of this server, somebody is at home.
""".
-spec getPresenceStatus( wooper:state() ) -> const_request_return( boolean() ).
getPresenceStatus( State ) ->
    Status = ?getAttr(actual_presence),

    ?debug_fmt( "Presence status requested, returning ~ts.", [ Status ] ),

    % For testing:
    %trace_utils:error( "getPresenceStatus: waiting (for overlap)." ),
    %timer:sleep( 1000 ),

    wooper:const_return_result( Status ).


-doc """
Sets whether, from the point of view of this server, somebody should be
considered at home.
""".
-spec setPresenceStatus( wooper:state(), boolean() ) -> oneway_return().
setPresenceStatus( State, NewStatus ) when is_boolean( NewStatus ) ->

    PrevStatus = ?getAttr(actual_presence),

    ?info_fmt( "Presence status set to ~ts (was ~ts).",
               [ NewStatus, PrevStatus ] ),

    NewState = case NewStatus of

        PrevStatus ->
            State;

        _ ->
            on_presence_change( NewStatus, State )

    end,

    wooper:return_state( NewState ).



-doc """
Called whenever the actual presence status changed.
""".
-spec on_presence_change( boolean(), wooper:state() ) -> wooper:state().
on_presence_change( NewStatus, State ) ->

    ?debug_fmt( "Applying new presence status: ~ts (from ~ts).",
                [ NewStatus, ?getAttr(actual_presence) ] ),

    SetState = setAttribute( State, actual_presence, NewStatus ),

    case NewStatus of

        true ->
            ?debug( "Somebody at home, hence disabling alarm and presence "
                    "simulation." ),

            AlarmState = setAttributes( SetState, [
                { alarm_inhibited, true },
                % Just for extra safety:
                { alarm_triggered, false } ] ),

            apply_presence_simulation( AlarmState );

        false ->
            ?debug( "Nobody at home, hence enabling alarm and presence "
                    "simulation." ),

            AlarmState = setAttributes( SetState, [
                % Directly done, so any absence shall be declared whereas
                % already outside (otherwise the door opening will trigger the
                % alarm):
                %
                { alarm_inhibited, false },
                % Just for extra safety:
                { alarm_triggered, false } ] ),

            apply_presence_simulation( AlarmState )

    end.





% Alarm-related commands:


-doc """
Requests whether the alarm is currently activated.
""".
-spec getAlarmStatus( wooper:state() ) -> const_request_return( boolean() ).
getAlarmStatus( State ) ->
    wooper:const_return_result( ?getAttr(alarm_triggered) ).


-doc """
Sets whether the alarm shall be active.

Direct order, bypasses any restriction (notably ignores the `alarm_inhibited`
attribute).
""".
-spec setAlarmStatus( wooper:state(), boolean() ) -> oneway_return().
setAlarmStatus( State, NewStatus ) when is_boolean( NewStatus ) ->

    PrevStatus = ?getAttr(alarm_triggered),

    ?info_fmt( "Alarm status set to ~ts (was ~ts).",
               [ NewStatus, PrevStatus ] ),

    NewState = case NewStatus of

        % Do nothing if no status change:
        PrevStatus ->
            State;

        _ ->
            case NewStatus of

                true ->
                    activate_alarm( State );

                false ->
                    deactivate_alarm( State )

            end

    end,

    wooper:return_state( NewState ).



-doc """
Returns the types of events that bypass any inhibition of an alarm and that are
unconditionally taken into account.
""".
-spec get_passthrough_event_types() -> [ device_event_type() ].
get_passthrough_event_types() ->
    % For example not a single_input_contact_event:
    [ push_button_event, double_rocker_switch_event ].



% Lighting-related commands:


-doc """
Starts all possible lighting, as controlled by presence simulations.

Note presence programs may afterwards apply other operations.
""".
-spec startLighting( wooper:state() ) -> oneway_return().
startLighting( State ) ->

    ?info( "Requested to start all possible presence-related lighting." ),
    NewState = ensure_all_lighting( State ),
    wooper:return_state( NewState ).



-doc """
Stops all possible lighting, as controlled by presence simulations.

Note presence programs may afterwards apply other operations.
""".
-spec stopLighting( wooper:state() ) -> oneway_return().
stopLighting( State ) ->

    ?info( "Requested to stop all possible presence-related lighting." ),
    NewState = ensure_not_any_lighting( State ),
    wooper:return_state( NewState ).





% Helper section.


-doc "Activates the alarm.".
-spec activate_alarm( wooper:state() ) -> wooper:state().
activate_alarm( State ) ->

    case ?getAttr(alarm_actuator_specs) of

        [] ->
            send_alarm_trace( warning, "Activating alarm immediately "
                "- yet there is no alarm actuator registered.", State );

        CanonEmittedEvSpecs ->
            OcSrvPid = ?getAttr(oc_srv_pid),

            send_alarm_trace_fmt( warning, "Activating alarm immediately, "
                "triggering the corresponding actuators: ~ts.",
                [ oceanic_text:canon_emitted_event_specs_to_string(
                    CanonEmittedEvSpecs, OcSrvPid ) ], State ),

            oceanic:trigger_actuators( CanonEmittedEvSpecs, OcSrvPid )

    end,

    setAttribute( State, alarm_triggered, true ).


-doc "Deactivates the alarm.".
-spec deactivate_alarm( wooper:state() ) -> wooper:state().
deactivate_alarm( State ) ->

    case ?getAttr(alarm_actuator_specs) of

        [] ->
            send_alarm_trace( warning, "Deactivating alarm immediately "
                "- yet there is no alarm actuator registered.", State );

        CanonEmittedEvSpecs ->
            OcSrvPid = ?getAttr(oc_srv_pid),
            send_alarm_trace_fmt( warning, "Deactivating alarm immediately, "
                "reciprocal triggering of the corresponding actuators: ~ts.",
                [ oceanic_text:canon_emitted_event_specs_to_string(
                    CanonEmittedEvSpecs, OcSrvPid ) ], State ),

            % The switching off is derived from the base switching on by
            % Oceanic, as based on the EURID it knows the actual device types:
            %
            oceanic:trigger_actuators_reciprocal( CanonEmittedEvSpecs,
                                                  OcSrvPid )

    end,

    setAttribute( State, alarm_triggered, false ).



-doc """
Returns a suitable name for a trace emitter for the specified device.
""".
-spec get_trace_emitter_name_from( device_name() ) -> bin_string().
get_trace_emitter_name_from( BinDevName ) ->
    text_utils:bin_concatenate( <<"Devices.">>, BinDevName ).


-doc """
Returns a suitable name for a trace emitter for the unresolved device of
specified EURID.
""".
-spec get_trace_emitter_name_for_unresolved( eurid() ) -> bin_string().
get_trace_emitter_name_for_unresolved( Eurid ) ->
    text_utils:bin_format( "Devices.~ts (unresolved)",
                           [ oceanic_text:eurid_to_string( Eurid ) ] ).




-doc """
Manages a presence-related event: checks whether an actual presence switching
(somebody being at home or not) is to be done.
""".
-spec manage_presence_switching( device_event(), wooper:state() ) ->
                                        wooper:state().
manage_presence_switching( DevEvent, State ) ->

    % Too verbose:
    %cond_utils:if_defined( us_main_debug_home_automation,
    %   ?debug_fmt( "Examining whether the following event relates to presence "
    %      "switching: ~ts",
    %   [ oceanic_text:device_event_to_string( DevEvent ) ] ) ),

    % [canon_listened_event_spec()]:
    CLESs = ?getAttr(presence_switching_trigger_specs),

    case oceanic:event_matches_trigger( DevEvent, CLESs ) of

        false ->
            cond_utils:if_defined( us_main_debug_home_automation,
                send_psc_trace_fmt( debug, "The received event does not "
                    "match any of the presence switching ones, which are: ~ts"
                    "This event corresponds to: ~ts",
                    [ oceanic_text:canon_listened_event_specs_to_string(
                        CLESs ),
                      oceanic_text:device_event_to_string( DevEvent ) ],
                                    State ) ),
            State;

        % Presence declared:
        { true, EmitterEurid, _NewDevStatus=on } ->

            OcSrvPid = ?getAttr(oc_srv_pid),

            BaseMsg = text_utils:format( "Presence switched on (declaring "
                "that someone is at home) by ~ts "
                "(due to the following event: ~ts)",
                [ oceanic:get_device_description( EmitterEurid, OcSrvPid ),
                  oceanic_text:device_event_to_string( DevEvent ) ] ),

            case ?getAttr(actual_presence) of

                true ->
                    ?debug_fmt( "~ts, yet already considered at home, "
                                "so nothing done.", [ BaseMsg ] ),
                    State;

                false ->
                    ?debug_fmt( "~ts whereas considered away, "
                                "so switching presence on now.", [ BaseMsg ] ),
                    on_presence_change( _NewPresStatus=true, State )

            end;


        % Absence declared:
        { true, EmitterEurid, _NewDevStatus=off } ->

            OcSrvPid = ?getAttr(oc_srv_pid),

            BaseMsg = text_utils:format( "Presence switched off (declaring "
                "that nobody is at home) by ~ts "
                "(due to the following event: ~ts)",
                [ oceanic:get_device_description( EmitterEurid, OcSrvPid ),
                  oceanic_text:device_event_to_string( DevEvent ) ] ),

            case ?getAttr(actual_presence) of

                true ->
                    ?debug_fmt( "~ts whereas considered at home, "
                                "so switching presence off now.", [ BaseMsg ] ),
                    on_presence_change( _NewPresStatus=false, State );

                false ->
                    ?debug_fmt( "~ts, yet already considered away, "
                                "so nothing done.", [ BaseMsg ] ),
                    State

            end;


        % Presence status inverted:
        { true, EmitterEurid, _NewDevStatus=inverted } ->

            OcSrvPid = ?getAttr(oc_srv_pid),

            BaseMsg = text_utils:format( "Presence status inverted by ~ts "
                "(due to the following event: ~ts): switching to presence ",
                [ oceanic:get_device_description( EmitterEurid, OcSrvPid ),
                  oceanic_text:device_event_to_string( DevEvent ) ] ),


            case ?getAttr(actual_presence) of

                true ->
                    ?debug_fmt( "~tsoff (nobody at home).", [ BaseMsg ] ),
                    on_presence_change( _NewPresStatus=false, State );

                false ->
                    ?debug_fmt( "~tson (somebody at home).", [ BaseMsg ] ),
                    on_presence_change( _NewPresStatus=true, State )

            end

    end.



-doc """
Manages an alarm-related event: checks whether an actual switching of the alarm
status (activating/deactivating) is to be done.

Note that, depending on the type of device that switches (on or off) an alarm, a
trigger will be ignored or not: push buttons/rockers will directly decide of the
alarm status, whereas opening detectors will switch it on (of course never off)
iff the alarm is not inhibited.

Indeed, if using an (emergency) button, we want to be able to switch on/off an
alarm in all cases, whereas, if the alarm is inhibited (typically if being at
home), we do not want the opening of a door to trigger an alarm then.
""".
-spec manage_alarm_switching( device_event(), wooper:state() ) ->
                                        wooper:state().
manage_alarm_switching( DevEvent, State ) ->

    % Too verbose:
    %cond_utils:if_defined( us_main_debug_home_automation,
    %   ?debug_fmt( "Examining whether the following event relates to alarm "
    %       "switching: ~ts",
    %       [ oceanic_text:device_event_to_string( DevEvent ) ] ) ),

    % [canon_listened_event_spec()]:
    CLESs = ?getAttr(alarm_trigger_specs),

    % Match is determined in all cases, even if at home, as one may want to
    % be able to deactivate the alarm even if at home:
    %
    case oceanic:event_matches_trigger( DevEvent, CLESs ) of

        false ->
            cond_utils:if_defined( us_main_debug_alarm,
                send_alarm_trace_fmt( debug, "The following event does not "
                    "match any of the alarm switching ones "
                    "(which are ~ts): ~ts",
                    [ oceanic_text:canon_listened_event_specs_to_string(
                        CLESs ),
                      oceanic_text:device_event_to_string( DevEvent ) ],
                                    State ) ),
            State;

        % Alart start requested:
        { true, EmitterEurid, _NewDevStatus=on } ->

            OcSrvPid = ?getAttr(oc_srv_pid),

            BaseMsg = text_utils:format( "Alarm switched on (intrusion "
                "detected) by ~ts, (due to the following event: ~ts)",
                [ oceanic:get_device_description( EmitterEurid, OcSrvPid ),
                  oceanic_text:device_event_to_string( DevEvent ) ] ),

            % We could rely on the alarm_triggered attribute not to trigger it
            % again, yet for such a critical message we prefer triggering it
            % (switching it on or off) unconditionally (hence possibly again) -
            % provided it is not inhibited with a non-prioritary event, of
            % course.

            EvType = oceanic:get_event_type( DevEvent ),

            case { ?getAttr(alarm_inhibited),
                    lists:member( EvType, get_passthrough_event_types() ) } of

                { _AnyIsInhibited, _IsPassThroughEventType=true } ->

                    send_alarm_trace_fmt( info,
                        "~ts, whose type, ~ts, bypasses any alarm inhibition; "
                        "so triggering the alarm now.",
                        [ BaseMsg, EvType ], State ),

                    apply_alarm_status( _NewAlarmStatus=true, State );


                % Typically single_input_contact_event, for opening detectors:
                { _IsInhibited=true, _NotPassThroughEventType } ->

                    send_alarm_trace_fmt( debug,
                        "~ts, whose type (~ts) respects the current "
                        "alarm inhibition; so not triggering specifically the "
                        "alarm now.", [ BaseMsg, EvType ], State ),

                    % Does not stop it either:
                    State;


                { _IsInhibited=false, _NotPassThroughEventType } ->

                    send_alarm_trace_fmt( debug, "~ts, whereas the alarm "
                        "is not inhibited, so triggering it now.",
                        [ BaseMsg, EvType ], State ),

                    apply_alarm_status( _NewAlarmStatus=true, State )

            end;


        % Alart stop requested:
        { true, EmitterEurid, _NewDevStatus=off } ->

            % No inhibition taken into account when wanting to stop a running
            % alarm:

            EndMsg = text_utils:format( "the following event: ~ts",
                [ oceanic_text:device_event_to_string( DevEvent ) ] ),

            OcSrvPid = ?getAttr(oc_srv_pid),

            EvType = oceanic:get_event_type( DevEvent ),

            case lists:member( EvType, get_passthrough_event_types() ) of

                true ->
                    send_alarm_trace_fmt( info,
                        "Alarm switched off (end of intrusion) by ~ts "
                        "(unconditionally, as of type ~ts), due to ~ts",
                        [ oceanic:get_device_description( EmitterEurid,
                                                          OcSrvPid ),
                          EvType, EndMsg ],
                        State ),

                    apply_alarm_status( _NewAlarmStatus=false, State );

                false ->
                    send_alarm_trace_fmt( debug,
                        "Not switching alarm off after event from ~ts "
                        "of (non-passthrough) type ~ts: ignoring ~ts.",
                        [ EvType, oceanic:get_device_description(
                            EmitterEurid, OcSrvPid ), EndMsg ], State ),

                    State

            end

    end.



-doc """
Called whenever the actual alarm status changed.

Unconditional: any effect of alarm inhibition supposed to be taken into account,
and any prior alarm status ignored (too critical to rely on assumptions).
""".
-spec apply_alarm_status( boolean(), wooper:state() ) -> wooper:state().
apply_alarm_status( NewStatus=true, State ) ->

    send_alarm_trace_fmt( notice,
        "Switching now the alarm on (trigger status was ~ts).",
        [ ?getAttr(alarm_triggered) ], State ),

    % First priority:
    oceanic:trigger_actuators( ?getAttr(alarm_actuator_specs),
                               ?getAttr(oc_srv_pid) ),

    SchedPid = class_USScheduler:get_server_pid(),

    % Setting-up a timer to stop that alarm, but disabling any past one first:
    case ?getAttr(alarm_stop_task_id ) of

        undefined ->
            ok;

        PastTaskId ->
            SchedPid ! { unregisterTask, PastTaskId, self() },

            cond_utils:if_defined( us_main_debug_alarm,
                send_alarm_trace_fmt( debug,
                    "Unregistering past alarm stop task #~B.", [ PastTaskId ],
                    State ) ),

            receive

                { wooper_result, { task_unregistered, PastTaskId } } ->
                    ok;

                { wooper_result, { task_already_done, PastTaskId } } ->
                    send_alarm_trace( error,
                        "Unregistered alarm stop task already done.", State );

                { wooper_result, { task_unregistration_failed, Reason,
                                   PastTaskId } } ->
                    send_alarm_trace_fmt( error, "Unregistration of the alarm "
                        "stop task failed; reason: ~p.", [ Reason ], State )

            end

    end,

    AfterDuration = ?getAttr(alarm_duration),

    SchedPid ! { registerOneshotTaskIn,
                 [ _TaskCmd=stopAlarmScheduled, AfterDuration ], self() },

    cond_utils:if_defined( us_main_debug_alarm, send_alarm_trace_fmt( debug,
        "Registering alarm stop task to happen in ~ts.",
        [ time_utils:duration_to_string( 1000 * AfterDuration ) ], State ) ),

    % Full lighting wanted as well in case of alarm (a bit of interleaving):
    LightState = ensure_all_lighting( State ),

    % For debug, no noise:
    %LightState = State,

    % For registerOneshotTaskIn:
    MaybeTaskId = receive

        { wooper_result, { task_registered, TId } } ->
            TId;

        { wooper_result, task_done } ->
            send_alarm_trace( error,
                "Just-registered alarm stop task reported already done.",
                State ),
            undefined

    end,

    setAttributes( LightState, [ { alarm_triggered, NewStatus },
                                 { alarm_stop_task_id, MaybeTaskId } ] );


apply_alarm_status( NewStatus=false, State ) ->

    send_alarm_trace_fmt( notice,
        "Switching now the alarm off (trigger status was ~ts).",
        [ ?getAttr(alarm_triggered) ], State ),

    oceanic:trigger_actuators_reciprocal( ?getAttr(alarm_actuator_specs),
                                          ?getAttr(oc_srv_pid) ),

    SetState = setAttributes( State, [ { alarm_triggered, NewStatus },
                                       % Anticipating next actions:
                                       { alarm_stop_task_id, undefined } ] ),

    % No specific lighting stop wanted.

    case ?getAttr(alarm_stop_task_id ) of

        undefined ->
            ok;

        PastTaskId ->
            class_USScheduler:get_server_pid() !
                { unregisterTask, PastTaskId, self() },

            cond_utils:if_defined( us_main_debug_alarm, send_alarm_trace_fmt(
                debug, "Unregistering past alarm stop task #~B.",
                [ PastTaskId ], SetState ) ),

            receive

                { wooper_result, { task_unregistered, PastTaskId } } ->
                    ok;

                { wooper_result, { task_already_done, PastTaskId } } ->
                    send_alarm_trace( error,
                        "Unregistered alarm stop task already done.",
                        SetState );

                { wooper_result, { task_unregistration_failed, Reason,
                                   PastTaskId } } ->
                    send_alarm_trace_fmt( error, "Unregistration of the "
                        "alarm stop task failed; reason: ~p.", [ Reason ],
                        SetState )

            end

    end,

    SetState.



-doc """
Callback triggered, if this server enabled the trapping of exits, whenever a
linked process terminates.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
        basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StoppedPid, _ExitType=normal ) ->
    % Possibly useless to trace:
    ?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
    wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

    % Typically: "Received exit message '{{nocatch,
    %  {wooper_oneway_failed,<0.44.0>,class_XXX,
    %   FunName,Arity,Args,AtomCause}}, [...]}"

    % Redundant information, yet useful for console outputs:
    ?warning_fmt( "US home automation server ~w received and ignored "
        "following exit message from ~w:~n  ~p",
        [ self(), CrashPid, ExitType ] ),

    wooper:const_return().




% Static subsection.


% Implemented from class_USServer:

-doc "Returns the naming lookup information of this server.".
-spec get_lookup_info() -> static_return( lookup_info() ).
get_lookup_info() ->
   wooper:return_static( { ?us_main_home_automation_server_registration_name,
       ?us_main_home_automation_server_registration_scope } ).


-doc """
Returns the PID of the current, supposedly already-launched, home automation
server, waiting for it if needed.

It is better to obtain it each time from the naming service rather than to
resolve and store its PID once for all, as, for an increased robustness, servers
may be restarted (hence any former PID may not reference a live process
anymore).
""".
-spec get_server_pid () -> static_return( home_automation_server_pid() ).
get_server_pid() ->

    SrvPid = class_USServer:resolve_server_pid(
        _RegName=?us_main_home_automation_server_registration_name,
        _RegScope=?us_main_home_automation_server_registration_scope ),

    wooper:return_static( SrvPid ).



% Configuration-related section.
%
% Typically called by the US-Main configuration server.




% Section dedicated to the implementation of actions.


% Subsection for Home Automation Device management:

-doc "Lists all known home automation devices yet (built-in action).".
-spec listDevices( wooper:state() ) ->
                        const_request_return( successful( ustring() ) ).
listDevices( State ) ->

    AllDevStates = table:values( ?getAttr(device_table) ),

    AllDevStrs = list_devices_ordered( AllDevStates ),

    Str = case AllDevStrs of

        [] ->
            "No known device yet.";

        [ SingleDevStr ] ->
            text_utils:format( "A single known device: ~ts.",
                               [ SingleDevStr ] );

        DevStrs ->
            text_utils:format( "~B known devices: ~ts", [ length( DevStrs ),
                text_utils:strings_to_string( DevStrs ) ] )

    end,

    wooper:const_return_result( { ok, Str } ).



-doc "Lists all known home automation sensor devices (built-in action).".
-spec listSensors( wooper:state() ) ->
                        const_request_return( successful( ustring() ) ).
listSensors( State ) ->

    AllDevStates = table:values( ?getAttr(device_table) ),

    SensorEepSet = oceanic:get_sensor_eeps(),

    SensorDevStrs = list_devices_ordered( [ DS || DS <- AllDevStates,
        not set_utils:is_empty( set_utils:intersection(
            DS#device_state.eep_ids, SensorEepSet ) ) ] ),

    Str = case SensorDevStrs of

        [] ->
            "No known sensor yet.";

        [ SingleDevStr ] ->
            text_utils:format( "A single known sensor: ~ts.",
                               [ SingleDevStr ] );

        DevStrs ->
            text_utils:format( "~B known sensors: ~ts",
                [ length( DevStrs ), text_utils:strings_to_string( DevStrs ) ] )

    end,

    wooper:const_return_result( { ok, Str } ).



-doc "Lists all known home automation command devices (built-in action).".
-spec listCommandDevices( wooper:state() ) ->
                        const_request_return( successful( ustring() ) ).
listCommandDevices( State ) ->

    AllDevStates = table:values( ?getAttr(device_table) ),

    CommandEepSet = oceanic:get_command_eeps(),

    CommandDevStrs = list_devices_ordered( [ DS || DS <- AllDevStates,
        not set_utils:is_empty( set_utils:intersection(
            DS#device_state.eep_ids, CommandEepSet ) ) ] ),

    Str = case CommandDevStrs of

        [] ->
            "No known command device yet.";

        [ SingleDevStr ] ->
            text_utils:format( "A single known command device: ~ts.",
                               [ SingleDevStr ] );

        DevStrs ->
            text_utils:format( "~B known command devices: ~ts",
                [ length( DevStrs ), text_utils:strings_to_string( DevStrs ) ] )

    end,

    wooper:const_return_result( { ok, Str } ).


-doc "Lists all known home automation actuator devices (built-in action).".
-spec listActuators( wooper:state() ) ->
                        const_request_return( successful( ustring() ) ).
listActuators( State ) ->

    AllDevStates = table:values( ?getAttr(device_table) ),

    ActuatorEepSet = oceanic:get_actuator_eeps(),

    ActuatorDevStrs = list_devices_ordered( [ DS || DS <- AllDevStates,
        not set_utils:is_empty( set_utils:intersection(
            DS#device_state.eep_ids, ActuatorEepSet ) ) ] ),

    Str = case ActuatorDevStrs of

        [] ->
            "No known actuator yet.";

        [ SingleDevStr ] ->
            text_utils:format( "A single known actuator: ~ts.",
                               [ SingleDevStr ] );

        DevStrs ->
            text_utils:format( "~B known actuators: ~ts",
                [ length( DevStrs ), text_utils:strings_to_string( DevStrs ) ] )

    end,

    wooper:const_return_result( { ok, Str } ).



-doc """
Lists the short descriptions of the devices specified by state, latest seen
first.
""".
-spec list_devices_ordered( [ device_state() ] ) -> [ ustring() ].
list_devices_ordered( DevStates ) ->
    % Index of the 'last_seen' field in a device_state record:
    OrderedDevStates = lists:reverse(
        lists:keysort( _LastSeenIndex=10, DevStates ) ),

    [ device_state_to_short_string( DS ) || DS <- OrderedDevStates ].



-doc "Describes the specified device.".
-spec describeDevice( wooper:state(), ustring() ) ->
                                const_request_return( string_fallible() ).
describeDevice( State, DevDesignator ) ->

    DevSpellTree = ?getAttr(device_spell_tree),

    case spell_tree:resolve( DevDesignator, DevSpellTree ) of

        undefined ->
            SplitStrs = lists:sort( [ spell_tree:splitter_to_string( SP )
                || SP <- spell_tree:get_splitters( DevSpellTree ) ] ),

            Str = text_utils:format( "Unknown device designator specified "
                "('~p'); known ones are: ~ts", [ DevDesignator,
                text_utils:strings_to_string( SplitStrs ) ] ),

            wooper:const_return_result( { error, Str } );

        DevDesigStr ->
            Eurid = table:get_value(
                _K=text_utils:string_to_binary( DevDesigStr ),
                ?getAttr(dev_splitter_table) ),

            cond_utils:if_defined( us_main_debug_actions,?debug_fmt(
                "Device designator '~ts' converted to '~ts', i.e. ~ts.",
                [ DevDesignator, DevDesigStr,
                  oceanic_text:eurid_to_string( Eurid ) ] ) ),

            DescStr = device_state_to_string(
                table:get_value( Eurid, ?getAttr(device_table) ) ),

            wooper:const_return_result( { ok,
                text_utils:uppercase_initial_letter( DescStr ) ++ "." } )

    end.



-doc "Activates the specified device.".
-spec activateDevice( wooper:state(), ustring() ) ->
                                const_request_return( string_fallible() ).
activateDevice( State, DevDesignator ) ->

    DevSpellTree = ?getAttr(device_spell_tree),

    case spell_tree:resolve( DevDesignator, DevSpellTree ) of

        undefined ->
            SplitStrs = lists:sort( [ spell_tree:splitter_to_string( SP )
                || SP <- spell_tree:get_splitters( DevSpellTree ) ] ),

            Str = text_utils:format( "Unknown device designator specified "
                "('~ts'); known ones are: ~ts", [ DevDesignator,
                text_utils:strings_to_string( SplitStrs ) ] ),

            wooper:const_return_result( { error, Str } );

        DevDesigStr ->
            Eurid = table:get_value(
                _K=text_utils:string_to_binary( DevDesigStr ),
                ?getAttr(dev_splitter_table) ),

            cond_utils:if_defined( us_main_debug_actions,?debug_fmt(
                "Device designator '~ts' converted to '~ts', i.e. ~ts.",
                [ DevDesignator, DevDesigStr,
                  oceanic_text:eurid_to_string( Eurid ) ] ) ),

            oceanic:trigger_actuator( _CEES={ Eurid, switch_on },
                                      ?getAttr(oc_srv_pid) ),

            Str = text_utils:format(
                "Device ~ts (~ts) triggered for activation.",
                [ DevDesigStr, oceanic_text:eurid_to_string( Eurid ) ] ),

            % Returns immediately (triggered, not activated for sure):
            wooper:const_return_result( { ok, Str } )

    end.


-doc "Deactivates the specified device.".
-spec deactivateDevice( wooper:state(), ustring() ) ->
                                const_request_return( string_fallible() ).
deactivateDevice( State, DevDesignator ) ->

    DevSpellTree = ?getAttr(device_spell_tree),

    case spell_tree:resolve( DevDesignator, DevSpellTree ) of

        undefined ->
            SplitStrs = lists:sort( [ spell_tree:splitter_to_string( SP )
                || SP <- spell_tree:get_splitters( DevSpellTree ) ] ),

            Str = text_utils:format( "Unknown device designator specified "
                "('~p'); known ones are: ~ts", [ DevDesignator,
                text_utils:strings_to_string( SplitStrs ) ] ),

            wooper:const_return_result( { error, Str } );

        DevDesigStr ->
            Eurid = table:get_value(
                _K=text_utils:string_to_binary( DevDesigStr ),
                ?getAttr(dev_splitter_table) ),

            oceanic:trigger_actuator( _CEES={ Eurid, switch_off },
                                      ?getAttr(oc_srv_pid) ),

            Str = text_utils:format(
                "Device ~ts (~ts) triggered for deactivation.",
                [ DevDesigStr, oceanic_text:eurid_to_string( Eurid ) ] ),

            % Returns immediately (triggered, not deactivated for sure):
            wooper:const_return_result( { ok, Str } )

    end.



% Subsection for Presence management:


-doc "Tells whether somebody is considered at home (built-in action).".
-spec isPresent( wooper:state() ) ->
                        const_request_return( successful( ustring() ) ).
isPresent( State ) ->
    Str = "Considering currently that " ++ case ?getAttr(actual_presence) of

        true ->
            "somebody";

        false ->
            "nobody"

    end ++ " is at home.",

    wooper:const_return_result( { ok, Str } ).



-doc "Declares that somebody is present at home (built-in action).".
-spec declarePresent( wooper:state() ) ->
                        request_return( successful( ustring() ) ).
declarePresent( State ) ->

    case ?getAttr(actual_presence) of

        true ->
            Str = "Somebody was already considered at home.",
            wooper:const_return_result( { ok, Str } );

        false ->
            SetState = setAttribute( State, actual_presence, true ),
            ApplyState = apply_presence_simulation( SetState ),
            wooper:return_state_result( ApplyState,
                { ok, "Now somebody is considered at home." } )

    end.



-doc "Declares that nobody is present at home (built-in action).".
-spec declareNotPresent( wooper:state() ) ->
                        request_return( successful( ustring() ) ).
declareNotPresent( State ) ->

    case ?getAttr(actual_presence) of

        true ->
            SetState = setAttribute( State, actual_presence, false ),
            ApplyState = apply_presence_simulation( SetState ),
            wooper:return_state_result( ApplyState,
                { ok, "Now nobody is considered at home." } );

        false ->
            Str = "Nobody was already considered at home.",
            wooper:const_return_result( { ok, Str } )

    end.



-doc "Tells whether somebody is considered at home (built-in action).".
-spec isPresenceSimulationEnabled( wooper:state() ) ->
                        const_request_return( successful( ustring() ) ).
isPresenceSimulationEnabled( State ) ->
    Str = "Presence simulation is currently " ++
            case ?getAttr(presence_simulation_enabled) of

        true ->
            "enabled";

        false ->
            "disabled"

    end ++ ".",

    wooper:const_return_result( { ok, Str } ).



-doc "Enables the presence simulation (built-in action).".
-spec enablePresenceSimulation( wooper:state() ) ->
                        request_return( successful( ustring() ) ).
enablePresenceSimulation( State ) ->

    case ?getAttr(presence_simulation_enabled) of

        true ->
            Str = "Presence simulation was already enabled.",
            wooper:const_return_result( { ok, Str } );

        false ->
            SetState = setAttribute( State, presence_simulation_enabled, true ),
            ApplyState = apply_presence_simulation( SetState ),
            wooper:return_state_result( ApplyState,
                { ok, "Presence simulation is now enabled." } )

    end.



-doc "Disables the presence simulation (built-in action).".
-spec disablePresenceSimulation( wooper:state() ) ->
                        request_return( successful( ustring() ) ).
disablePresenceSimulation( State ) ->

    case ?getAttr(presence_simulation_enabled) of

        true ->
            SetState = setAttribute( State, presence_simulation_enabled,
                                     false ),
            ApplyState = apply_presence_simulation( SetState ),
            wooper:return_state_result( ApplyState,
                { ok, "Presence simulation is now disabled." } );

        false ->
            Str = "Presence simulation was already disabled.",
            wooper:const_return_result( { ok, Str } )

    end.



% actions:
% alarm:
%%  * ${start_alarm_opt}: starts the alarm (siren)
%%  * ${stop_alarm_opt}: stops the alarm
%%  * ${get_alarm_opt}: tells whether the alarm is currently activated (hence wit a roaring siren)

%%  - regarding presence:
%%  * ${declare_presence_opt}: declares that somebody is at home (hence for example deactivate alarm)
%%  * ${declare_absence_opt}: declares that nobody is at home (hence for example activate alarm)
%%  * ${get_presence_opt}: tells whether US-Main considers that somebody is at home

%%  - regarding (presence-related) lighting:
%%  * ${start_lighting_opt}: starts all registered presence lighting
%%  * ${stop_lighting_opt}: stops all registered presence lighting
%% """.



-doc """
Triggers (immediately) the specified operation onto the specified device.

Typically referenced by user-configured action specifications.
""".
-spec actOnDevice( wooper:state(), user_device_designator(),
    extended_device_operation() ) -> const_request_return( string_fallible() ).
actOnDevice( State, UserDevDesig, ExtDevOp ) ->

    cond_utils:if_defined( us_main_debug_actions, ?debug_fmt( "Triggering the "
        "'~ts' operation on device designated by ~w.",
        [ ExtDevOp, UserDevDesig ] ) ),

    ActionResStr = case resolve_device_operation( ExtDevOp, State ) of

        undefined ->
            text_utils:format( "For the device designated by '~ts', "
                "the '~ts' action is not needed.", [ UserDevDesig, ExtDevOp ] );


        ActualDevOp ->
            DevDesig = oceanic:get_internal_device_designator( UserDevDesig ),

            DeviceAction = { ActualDevOp, DevDesig },

            % No result to expect:
            ?getAttr(oc_srv_pid) ! { performAction, DeviceAction },

            text_utils:format(
                "Action '~ts' triggered on device designated by '~ts'.",
                [ ExtDevOp, UserDevDesig ] )

    end,

    wooper:const_return_result( { ok, ActionResStr } ).





-doc "Resolves the specified device operation into a possible actual one".
-spec resolve_device_operation( extended_device_operation(), wooper:state() ) ->
                                                option( device_operation() ).
resolve_device_operation( ExtDevOp, State ) ->

    IsAtHome = ?getAttr(actual_presence),

    case ExtDevOp of

        switch_on_if_at_home ->
            case IsAtHome of
                true -> switch_on;
                false -> undefined
            end;

        switch_off_if_at_home ->
            case IsAtHome of
                true -> switch_off;
                false -> undefined
            end;

        switch_on_if_away ->
            case IsAtHome of
                true -> undefined;
                false -> switch_on
            end;

        switch_off_if_away ->
            case IsAtHome of
                true -> undefined;
                false -> switch_off
            end;

        OcAction ->
            oceanic:check_device_operation( OcAction )

    end.




-doc """
Request that is typically triggered by user-defined actions and that consists in
triggering an extended operation on a device periodically.

Stores the identifier of the corresponding periodical task to allow for any
later unregistering thereof.

This is a request, as actions only trigger such methods, for synchronisation.
""".
-spec schedulePeriodicalActionOnDevice( wooper:state(),
    user_device_designator(), extended_device_operation(), extended_timestamp(),
    user_periodicity(), schedule_count() ) ->
        request_return( string_fallible() ).
schedulePeriodicalActionOnDevice( State, UserDevDesig, ExtDevOp,
        StartExtTimestamp, DHMSPeriodicity, SchedCount ) ->

    cond_utils:if_defined( us_main_debug_actions, ?debug_fmt( "Scheduling a "
        "periodical '~ts' operation on device designated by ~w, "
        "starting from ~w, for a DHMS periodicity of ~w and "
        "a schedule count of ~w.",
        [ ExtDevOp, UserDevDesig, StartExtTimestamp, DHMSPeriodicity,
          SchedCount ] ) ),

    check_extended_device_operation( ExtDevOp ),

    % Convert early to check:
    DevDesig = oceanic:get_internal_device_designator( UserDevDesig ),

    SchedPid = class_USScheduler:get_server_pid(),

    TaskTable = ?getAttr(task_table),

    UnschedStr = case table:lookup_entry( _K=DevDesig, TaskTable ) of

        key_not_found ->
            "";

        { value, PrevTaskId } ->
            ?warning_fmt( "For device user-designated as '~p' "
                "(internally as '~ts'), a periodical task (~B) was "
                "already registered, unscheduling it first.",
                [ UserDevDesig,
                  oceanic_text:device_designator_to_string( DevDesig ),
                  PrevTaskId ] ),

            % No feedback wanted:
            SchedPid ! { unregisterTaskAsync, PrevTaskId },
            text_utils:format( " (a prior task #~B had to be unscheduled "
                               "first)", [ PrevTaskId ] )

    end,

    % The oneway that will be triggered by the scheduler onto the Oceanic
    % server:
    %
    UserTaskCommand = { performExtendedDeviceAction, [ DevDesig, ExtDevOp ] },

    % For example to translate {next_possible_day, {19,54,0}} and avoid
    % requesting a start in the past:
    %
    StartTimestamp = time_utils:resolve_timestamp( StartExtTimestamp ),

    % We cannot request the Oceanic server to be targeted directly, as extended
    % actions like switch_on_if_at_home depend on the state of this current
    % automation server:
    %
    SchedPid ! { registerTask,
        [ UserTaskCommand, StartTimestamp, _UserPeriodicity=DHMSPeriodicity,
          SchedCount, _UserActPid=self() ], self() },

    { ActionResStr, SchedState } = receive

        % Surprising:
        { wooper_result, task_done } ->
            cond_utils:if_defined( us_main_debug_actions,
                ?debug( "Periodical device action already done." ) ),
            { "Task done" ++ UnschedStr, State };

        { wooper_result, { task_registered, TaskId } } ->

            cond_utils:if_defined( us_main_debug_actions, ?debug_fmt(
                "Periodical device action registered as task #~B~ts.",
                [ TaskId, UnschedStr ] ) ),

            NewTaskTable = table:add_entry( DevDesig, TaskId, TaskTable ),

            NewState = setAttribute( State, task_table, NewTaskTable ),

            { text_utils:format( "Periodical task #~B scheduled~ts.",
                                 [ TaskId, UnschedStr ] ),
              NewState }

    end,

    wooper:return_state_result( SchedState, { ok, ActionResStr } ).


-doc """
Unschedules any periodical action registered for the specified device.
""".
-spec unschedulePeriodicalActionOnDevice( wooper:state(),
    user_device_designator() ) -> request_return( string_fallible() ).
unschedulePeriodicalActionOnDevice( State, UserDevDesig ) ->

    cond_utils:if_defined( us_main_debug_actions, ?debug_fmt(
        "Unscheduling any periodical operation on device designated by ~w.",
        [ UserDevDesig ] ) ),

    DevDesig = oceanic:get_internal_device_designator( UserDevDesig ),

    SchedPid = class_USScheduler:get_server_pid(),

    TaskTable = ?getAttr(task_table),

    { ActionRes, SchedState } =
            case table:lookup_entry( _K=DevDesig, TaskTable ) of

        key_not_found ->
            ?warning_fmt( "For device user-designated as '~p' "
                "(internally as '~ts'), no periodical task was "
                "registered, so none to unschedule.",
                [ UserDevDesig,
                  oceanic_text:device_designator_to_string( DevDesig ) ] ),

            { { error, "No corresponding task found" }, State };


        { value, TaskId } ->

            % No feedback wanted:
            SchedPid ! { unregisterTaskAsync, TaskId },

            cond_utils:if_defined( us_main_debug_actions, ?debug_fmt(
                "Periodical device action registered as task #~B unscheduled.",
                [ TaskId ] ) ),

            NewTaskTable = table:remove_entry( DevDesig, TaskTable ),

            NewState = setAttribute( State, task_table, NewTaskTable ),

            { { ok, text_utils:format( "Periodical task #~B unscheduled",
                                       [ TaskId ] ) },
              NewState }

    end,

    wooper:return_state_result( SchedState, ActionRes ).



-doc """
Performs the specified extended device action, if appropriate: then triggers
(immediately) the specified operation onto the specified device.
""".
-spec performExtendedDeviceAction( wooper:state(), device_designator(),
        extended_device_operation() ) -> const_oneway_return().
% Necessarily an oneway for the scheduler, thus not returning:
% successful( 'action_not_needed' | 'action_triggered' ) ).
performExtendedDeviceAction( State, DevDesig, ExtDevOp ) ->

    cond_utils:if_defined( us_main_debug_actions, ?debug_fmt( "Triggering the "
        "'~ts' extended operation on device designated by ~w.",
        [ ExtDevOp, DevDesig ] ) ),

    case resolve_device_operation( ExtDevOp, State ) of

        undefined ->
            %action_not_needed;
            ok;

        ActualDevOp ->
            DeviceAction = { ActualDevOp, DevDesig },

            % No result to expect:
            ?getAttr(oc_srv_pid) ! { performAction, DeviceAction }

            %action_triggered

    end,

    wooper:const_return().



% Section related to the US-Main configuration files.


-doc """
Returns the known home automation-related keys in US-Main configuration files.
""".
-spec get_licit_config_keys() -> [ list_table:key() ].
get_licit_config_keys() ->
    [ ?us_main_server_location_key,
      ?us_main_alarm_triggers_key, ?us_main_alarm_actuators_key,
      ?us_main_presence_triggers_key, ?us_main_presence_settings_key,
      ?us_main_home_automation_actions_key ] ++ ?supported_oceanic_config_keys.



-doc """
Handles the home automation-related entries in the user settings specified in
US-Main configuration files.

Note that the specified state is the one of a US-Main configuration server.
""".
-spec manage_configuration( us_main_config_table(), wooper:state() ) ->
                                        wooper:state().
manage_configuration( ConfigTable, State ) ->

    % First checking:
    MaybePosition = case table:lookup_entry( ?us_main_server_location_key,
                                             ConfigTable ) of

        key_not_found ->
            send_psc_trace( info, "No user settings regarding server location.",
                            State),
            undefined;


        { value, { { latitude, Lat }, { longitude, Long } } } ->
            is_float( Lat ) orelse throw(
                { invalid_latitude, Lat, ?us_main_server_location_key } ),

            is_float( Long ) orelse throw(
                { invalid_longitude, Long, ?us_main_server_location_key } ),

            { Lat, Long }

    end,


    AlarmTriggerListenEvSpecs = case table:lookup_entry(
            ?us_main_alarm_triggers_key, ConfigTable ) of

        VAT when VAT =:= key_not_found; VAT =:= { value, [] } ->
            send_alarm_trace( info, "No alarm trigger configured.", State ),
            [];

        { value, ALESs } when is_list( ALESs ) ->
            CanALESs = oceanic:canonicalise_listened_event_specs( ALESs ),
            send_alarm_trace_fmt( info, "The following ~B configured alarm "
                "trigger listening specifications will be used: ~ts.",
                [ length( CanALESs ), text_utils:strings_to_string(
                    [ oceanic_text:canon_listened_event_spec_to_string(
                        CanALES ) || CanALES <- CanALESs ] ) ], State ),

            CanALESs;

        { value, ANotLESs } ->
            throw( { invalid_alarm_trigger_specs, ANotLESs,
                     ?us_main_alarm_triggers_key } )

    end,


    AlarmActuatorEmitEvSpecs = case table:lookup_entry(
            ?us_main_alarm_actuators_key, ConfigTable ) of

        VAA when VAA =:= key_not_found; VAA =:= { value, [] } ->
            send_alarm_trace( info, "No alarm actuator configured.", State ),
            [];

        { value, AEESs } when is_list( AEESs ) ->
            CanAEESs = oceanic:canonicalise_emitted_event_specs( AEESs ),

            send_alarm_trace_fmt( info, "The following ~B configured alarm "
                "actuator emitting specifications will be used: ~ts.",
                [ length( CanAEESs ), text_utils:strings_to_string(
                    [ oceanic_text:canon_emitted_event_spec_to_string(
                        CanAEES ) || CanAEES <- CanAEESs ] ) ], State ),

            CanAEESs;

        { value, ANotAEESs } ->
            throw( { invalid_alarm_actuator_specs, ANotAEESs,
                     ?us_main_alarm_actuators_key } )

    end,


    PscTriggerListenEvSpecs = case table:lookup_entry(
            ?us_main_presence_triggers_key, ConfigTable ) of

        VPT when VPT =:= key_not_found; VPT =:= { value, [] } ->
            send_psc_trace( info, "No presence-switching trigger configured.",
                            State ),
            [];

        { value, PLESs } when is_list( PLESs ) ->
            CanPLESs = oceanic:canonicalise_listened_event_specs( PLESs ),

            send_psc_trace_fmt( info, "The following ~B configured presence "
                "trigger listening specifications will be used: ~ts.",
                [ length( CanPLESs ), text_utils:strings_to_string(
                    [ oceanic_text:canon_listened_event_spec_to_string(
                        CanPLES ) || CanPLES <- CanPLESs ] ) ], State ),

            CanPLESs;

        { value, PNotLESs } ->
            throw( { invalid_presence_trigger_specs, PNotLESs,
                     ?us_main_presence_triggers_key } )

    end,

    % Finally we kept the actuators per-presence simulation (not defined
    % globally):

    % PscActuatorEmitEvSpecs = case table:lookup_entry(
    %       ?us_main_presence_actuators_key, ConfigTable ) of
    %
    %   VPA when VPA =:= key_not_found; VPA =:= { value, [] } ->
    %       send_psc_trace( info, "No presence actuator configured.", State ),
    %       [];
    %
    %   { value, PEESs } when is_list( PEESs ) ->
    %       CanPEESs = oceanic:canonicalise_emitted_event_specs( PEESs ),
    %
    %       send_psc_trace_fmt( info, "The following ~B configured presence "
    %           actuator emitting specifications will be used: ~ts.",
    %           [ length( CanPEESs ), text_utils:strings_to_string(
    %               [ oceanic_text:canon_emitted_event_spec_to_string(
    %                   CanPEES ) || CanPEES <- CanPEESs ] ) ], State ),
    %
    %       CanPEESs;
    %
    %   { value, PNotPEESs } ->
    %       throw( { invalid_presence_actuator_specs, PNotPEESs,
    %                ?us_main_presence_actuators_key } )

    % end,

    SetPscUSimSettings = case table:lookup_entry(
            ?us_main_presence_settings_key, ConfigTable ) of

        key_not_found ->
            send_psc_trace( info,
                "No presence simulation settings configured.", State ),
            [];

        % Expecting a list of psc_simu_user_setting() (checked later directly by
        % this home automation server):
        %
        { value, PscUSimSettings } when is_list( PscUSimSettings ) ->
            send_psc_trace_fmt( info, "The following ~B user-configured "
                "presence simulation settings will be used:~n ~p",
                [ length( PscUSimSettings ), PscUSimSettings ], State ),
            PscUSimSettings;

        { value, NotPscUSimSettings } ->
            throw( { invalid_presence_simulation_settings, NotPscUSimSettings,
                     ?us_main_presence_settings_key } )

    end,

    UserActSpecs = case table:lookup_entry(
            ?us_main_home_automation_actions_key, ConfigTable ) of

        key_not_found ->
            ?info( "No action settings defined for home automation." ),
            [];


        { value, HAActSettings } ->
            % Checked later directly by this home automation server:
            HAActSettings

    end,

    % Fetching any Oceanic-related settings:
    { OcSettings, _SkrunkCfgTable } = table:extract_entries_if_existing(
        _OcKeys=?supported_oceanic_config_keys, ConfigTable ),

    % Not specifically checked at this level; will be done by the home
    % automation server (not the configuration server):
    %
    HACoreSettings = { AlarmTriggerListenEvSpecs, AlarmActuatorEmitEvSpecs,
        PscTriggerListenEvSpecs, SetPscUSimSettings, UserActSpecs,
        OcSettings },

    setAttributes( State, [
        { server_location, MaybePosition },
        { home_automation_core_settings, HACoreSettings } ] ).



-doc """
Checks the specified extended device operation.

Refer to the `device_operation/0` type.
""".
-spec check_extended_device_operation( term() ) -> extended_device_operation().
check_extended_device_operation( DevOp=switch_on_if_at_home ) ->
    DevOp;

check_extended_device_operation( DevOp=switch_off_if_at_home ) ->
    DevOp;

check_extended_device_operation( DevOp=switch_on_if_away ) ->
    DevOp;

check_extended_device_operation( DevOp=switch_off_if_away ) ->
    DevOp;

check_extended_device_operation( DevOp ) ->
    oceanic:check_device_operation( DevOp ).



% Section for the sending of traces.


-doc """
Sends the specified alarm simulation trace, to have it correctly categorised.
""".
-spec send_alarm_trace( trace_severity(), trace_message(), wooper:state() ) ->
                                void().
send_alarm_trace( TraceSeverity, TraceMsg, State ) ->
    class_TraceEmitter:send_categorised_named_emitter( TraceSeverity, State,
        TraceMsg,
        _EmitterCateg=?trace_emitter_categorization ".Alarm",
        _EmitterName="Configuration"  ).


-doc """
Sends the specified alarm simulation formatted trace, to have it correctly
categorised.
""".
-spec send_alarm_trace_fmt( trace_severity(), trace_format(), trace_values(),
                          wooper:state() ) -> void().
send_alarm_trace_fmt( TraceSeverity, TraceFormat, TraceValues, State ) ->
    TraceMsg = text_utils:format( TraceFormat, TraceValues ),
    send_alarm_trace( TraceSeverity, TraceMsg, State ).



-doc """
Sends the specified presence simulation trace, to have it correctly categorised.
""".
-spec send_psc_trace( trace_severity(), trace_message(), wooper:state() ) ->
                                void().
send_psc_trace( TraceSeverity, TraceMsg, State ) ->
    class_TraceEmitter:send_categorised_named_emitter( TraceSeverity, State,
        TraceMsg,
        _EmitterCateg=?trace_emitter_categorization ".Presence simulation",
        _EmitterName="Configuration"  ).


-doc """
Sends the specified presence simulation formatted trace, to have it correctly
categorised.
""".
-spec send_psc_trace_fmt( trace_severity(), trace_format(), trace_values(),
                          wooper:state() ) -> void().
send_psc_trace_fmt( TraceSeverity, TraceFormat, TraceValues, State ) ->
    TraceMsg = text_utils:format( TraceFormat, TraceValues ),
    send_psc_trace( TraceSeverity, TraceMsg, State ).



-doc """
Sends the specified Oceanic monitoring trace, to have it correctly categorised.
""".
-spec send_oc_mon_trace( trace_severity(), trace_message(), wooper:state() ) ->
                                void().
send_oc_mon_trace( TraceSeverity, TraceMsg, State ) ->
    class_TraceEmitter:send_named_emitter( TraceSeverity, State, TraceMsg,
        <<"Oceanic serial monitoring">> ).


-doc """
Sends the specified Oceanic monitoring formatted trace, to have it correctly
categorised.
""".
-spec send_oc_mon_trace_fmt( trace_severity(), trace_format(), trace_values(),
                             wooper:state() ) -> void().
send_oc_mon_trace_fmt( TraceSeverity, TraceFormat, TraceValues, State ) ->
    TraceMsg = text_utils:format( TraceFormat, TraceValues ),
    send_oc_mon_trace( TraceSeverity, TraceMsg, State ).




-doc "Returns a textual description of this server.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    MaybeOcSrvPid = ?getAttr(oc_srv_pid),

    OcSrvStr = case MaybeOcSrvPid of

        undefined ->
            "not relying on an Oceanic server";

        OcSrvPid ->
            text_utils:format( "relying on its Oceanic server ~w "
                "(source identifier being EURID ~ts; "
                "periodic restarts enabled: ~ts)",
                [ OcSrvPid,
                  oceanic_text:eurid_to_string( ?getAttr(oc_src_eurid) ),
                  ?getAttr(oc_periodic_restart) ] )

    end,


    LocStr = case ?getAttr(server_location) of

        undefined ->
            "with no location defined";

        { Lat, Long } ->
            % Degrees as raw floats rather than with °, minutes and al:
            text_utils:format( "located at latitude ~f degrees and "
                "longitude ~f degrees", [ Lat, Long ] )

    end,


    AtHomeStr = "considering that " ++ case ?getAttr(actual_presence) of

        true ->
            "someone";

        false ->
            "nobody"

    end ++ " is at home",


    AlarmStr = "is " ++ case ?getAttr(alarm_inhibited) of

        true ->
            "";

        false ->
            "not "

                        end ++ "currently inhibited and "

        ++ case ?getAttr(alarm_triggered) of

            true ->
                "";

            false ->
                "not "

           end ++ text_utils:format( "triggered.~nFor the alarm control: ~ts",
               [ case MaybeOcSrvPid of
                    undefined ->
                        oceanic_text:canon_listened_event_specs_to_string(
                            ?getAttr(alarm_trigger_specs) );

                    FirstOcSrvPid ->
                        oceanic_text:canon_listened_event_specs_to_string(
                            ?getAttr(alarm_trigger_specs), FirstOcSrvPid )

                  end ] )
        ++ text_utils:format( "~nIn terms of alarm actuators: ~ts",
               [ case MaybeOcSrvPid of
                    undefined ->
                        oceanic_text:canon_emitted_event_specs_to_string(
                            ?getAttr(alarm_actuator_specs) );

                    SecondOcSrvPid ->
                        oceanic_text:canon_emitted_event_specs_to_string(
                            ?getAttr(alarm_actuator_specs), SecondOcSrvPid )

                  end] ),


    PscStr = case ?getAttr(presence_simulation_enabled) of

        true ->
            PscSims = table:values( ?getAttr(presence_table) ),
            "enabled, with " ++ case PscSims of

                [] ->
                    "no presence defined ";

                [ PscSim ] ->
                    "a single presence defined: "
                        ++ presence_simulation_to_string( PscSim );

                PscSims ->
                    text_utils:format( "~B presences defined: ~ts~n",
                        [ length( PscSims ), text_utils:strings_to_string(
                            [ presence_simulation_to_string( PS )
                                || PS <- PscSims ] ) ] )

            end ++ text_utils:format( "~n~ts",
                [ case ?getAttr(time_equation_table) of

                    undefined ->
                        "No time equation table used";

                    TimeEqTable ->
                        text_utils:format( "Using a time equation table "
                            "comprising ~B entries",
                            [ table:size( TimeEqTable ) ] )

                  end ] );

        false ->
            "disabled"

    end,

    % Defined as much as presence_switching_device:
    PscSwitchStr = case ?getAttr(presence_switching_device_desc) of

        undefined ->
            "no presence-switching device has been defined";

        BinPscSwitchDesc ->
            BinPscSwitchDesc

    end,

    MidTaskStr = case ?getAttr(midnight_task_id) of

        undefined ->
            "No midnight update task defined";

        MidTaskId ->
            text_utils:format( "Midnight update task #~B defined",
                               [ MidTaskId ] )

    end,

    ActStr = us_action:action_table_to_string( ?getAttr(action_table) ),


    text_utils:format( "US home automation server ~ts, ~ts, ~ts, "
        "and that the alarm ~ts~n"
        "The presence simulator is currently ~ts, knowing that ~ts.~n~ts.~n~n"
        "This server has ~ts."
        "This server is currently ~ts~n."
        "Registering the following device splitters: ~ts~n",
        [ OcSrvStr, LocStr, AtHomeStr, AlarmStr, PscStr, PscSwitchStr,
          MidTaskStr, ActStr,
          device_table_to_string( ?getAttr(device_table) ),
          table:to_string( ?getAttr(dev_splitter_table) ) ] ).



-doc """
Returns a textual description of the specified presence simulation internal
record.
""".
-spec presence_simulation_to_string( presence_simulation() ) -> ustring().
presence_simulation_to_string( PscSim ) ->
    presence_simulation_to_string( PscSim, _MaybeOcSrvPid=undefined ).


-doc """
Returns a textual description of the specified presence simulation internal
record.
""".
-spec presence_simulation_to_string( presence_simulation(),
                                option( oceanic_server_pid() ) ) -> ustring().
presence_simulation_to_string( #presence_simulation{
        id=Id,
        enabled=IsEnabled,
        activated=IsActivated,
        actuator_event_specs=ActEvSpecs,
        program=Program,
        smart_lighting=IsSmart,
        random_activity=RandomAct,
        presence_task_info=MaybeTaskInfo },
                             MaybeOcSrvPid ) ->

    SmartStr = case IsSmart of

        true ->
            "";

        false ->
            "not"

    end,

    PscTaskStr = case MaybeTaskInfo of

        undefined ->
            "no presence task defined";

        { PscTaskId, PscTime } ->
            text_utils:format( "presence task #~B scheduled at ~ts",
                [ PscTaskId, time_utils:time_to_string( PscTime ) ] )

    end,

    ActStr = case ActEvSpecs of

        [] ->
            "no actuator";

        [ SingleAct ] ->
            Str = case MaybeOcSrvPid of

                undefined ->
                    oceanic_text:canon_emitted_event_spec_to_string(
                        SingleAct );

                OcSrvPid ->
                    oceanic_text:canon_emitted_event_spec_to_string( SingleAct,
                                                                     OcSrvPid )

            end,
            text_utils:format( "a single ~ts", [ Str ] );

        Acts ->
            Str = case MaybeOcSrvPid of

                undefined ->
                    oceanic_text:canon_emitted_event_specs_to_string( Acts );

                OcSrvPid ->
                    oceanic_text:canon_emitted_event_specs_to_string( Acts,
                                                                      OcSrvPid )

            end,
            text_utils:format( "~B actuators: ~ts", [ length( Acts ), Str ] )

    end,

    text_utils:format( "presence simulation of id #~B, ~ts, "
        "~tsusing smart lighting, based on ~ts, whose presence program ~ts~n"
        "The switching of this presence relies on ~ts(~ts)",
        [ Id, case IsEnabled of
                    true ->  "enabled";
                    false -> "disabled"
              end ++ " and currently "
          ++ case IsActivated of
                    true ->  "activated";
                    false -> "non-activated"
             end, SmartStr, random_activity_to_string( RandomAct ),
          program_to_string( Program ), ActStr, PscTaskStr ] ).



-doc "Returns a textual description of the specified presence program.".
-spec program_to_string( presence_program() ) -> ustring().
program_to_string( _Prog=constant_presence ) ->
    "is constant presence";

program_to_string( _Prog=constant_absence ) ->
    "is constant absence";

program_to_string( _Slots=[] ) ->
    "has no slot defined";

program_to_string( _Slots=[ SingleSlot ] ) ->
    text_utils:format( "has a single presence slot defined: ~ts",
                       [ slot_to_string( SingleSlot ) ] );

program_to_string( Slots ) ->

    SlotStrs = program_to_string( Slots, _FirstMaybePrevEndTime=undefined,
                                  _Acc=[] ),

    text_utils:format( "has ~B presence slots defined: ~ts",
        [ length( Slots ), text_utils:strings_to_string( SlotStrs ) ] ).


% (helper)
program_to_string( _Slots=[], _FirstMaybePrevEndTime, Acc ) ->
    lists:reverse( Acc );

program_to_string( _Slots=[ S={ _Start, Stop } | T  ], MaybePrevEndTime,
                   Acc ) ->
    SlotStr = slot_to_string( S, MaybePrevEndTime ),
    program_to_string( T, Stop, [ SlotStr | Acc ] ).



-doc "Returns a textual description of the specified presence slot.".
-spec slot_to_string( presence_slot() ) -> ustring().
slot_to_string( Slot ) ->
    slot_to_string( Slot, _PrevEndTime=undefined ).



-doc "Returns a textual description of the specified presence slot.".
-spec slot_to_string( presence_slot(), option( time() ) ) -> ustring().
slot_to_string( _Slot={ StartTime, StopTime }, _MaybePrevEndTime=undefined ) ->

    DurSec = time_utils:get_intertime_duration( StartTime, StopTime ),

    text_utils:format( "from ~ts to ~ts (duration: ~ts)", [
        time_utils:time_to_string( StartTime ),
        time_utils:time_to_string( StopTime ),
        time_utils:duration_to_string( 1000 * DurSec ) ] );


slot_to_string( _Slot={ StartTime, StopTime }, PrevEndTime ) ->

    FromPrevDurSec = time_utils:get_intertime_duration( PrevEndTime,
                                                        StartTime ),

    DurSec = time_utils:get_intertime_duration( StartTime, StopTime ),

    text_utils:format( "(after ~ts) from ~ts to ~ts (duration: ~ts)", [
        time_utils:duration_to_string( 1000 * FromPrevDurSec ),
        time_utils:time_to_string( StartTime ),
        time_utils:time_to_string( StopTime ),
        time_utils:duration_to_string( 1000 * DurSec ) ] ).



-doc """
Returns a textual description of the specified canonical random activity
settings.
""".
-spec random_activity_to_string( canon_random_activity_settings() ) ->
          ustring().
random_activity_to_string( _CRAS=false ) ->
    "no random activity";

random_activity_to_string( _CRAS={ MeanLightDuration, MeanNoLightDuration } ) ->
    text_utils:format( "random activity, with a mean lighting duration of ~ts "
        "and a mean non-lighting duration of ~ts",
        [ time_utils:duration_to_string( 1000 * MeanLightDuration ),
          time_utils:duration_to_string( 1000 * MeanNoLightDuration ) ] ).



-doc "Returns a textual description of any specified last-seen timestamp.".
-spec last_seen_info_to_string( option( timestamp() ) ) -> ustring().
last_seen_info_to_string( MaybeTimestamp ) ->
    last_seen_info_to_string( MaybeTimestamp, _Now=time_utils:get_timestamp() ).


-doc """
Returns a textual description of any specified last-seen timestamp, relying on
the second timestamp to define "now".
""".
-spec last_seen_info_to_string( option( timestamp() ),
                                option( timestamp() ) ) -> ustring().
last_seen_info_to_string( _MaybeLastSeenTimestamp=undefined, _MaybeNow ) ->
    " [never seen]";

last_seen_info_to_string( LastSeenTimestamp, _MaybeNow=undefined ) ->
    last_seen_info_to_string( LastSeenTimestamp,
                              _Now=time_utils:get_timestamp() );

last_seen_info_to_string( LastSeenTimestamp, Now ) ->
    DurSec = time_utils:get_duration( LastSeenTimestamp, Now ),
    text_utils:format( " [last seen about ~ts ago]",
        [ time_utils:duration_to_synthetic_string( 1000 * DurSec ) ] ).



-doc """
Returns a textual description of any specified last-seen timestamp of the
specified device type.
""".
-spec last_seen_info_with_type_to_string( option( timestamp() ),
                                          device_type() ) -> ustring().
last_seen_info_with_type_to_string( MaybeTimestamp, DevType ) ->
    last_seen_info_with_type_to_string( MaybeTimestamp,
        _Now=time_utils:get_timestamp(), DevType ).


-doc """
Returns a textual description of any specified last-seen timestamp of the
specified device type, relying on the second timestamp to define "now".
""".
-spec last_seen_info_with_type_to_string( option( timestamp() ),
        option( timestamp() ), device_type() ) -> ustring().
last_seen_info_with_type_to_string( _MaybeLastSeenTimestamp=undefined,
                                    _MaybeNow, DevType ) ->
    text_utils:format( " [~ts, never seen]",
                       [ oceanic_text:device_type_to_string( DevType ) ] );

last_seen_info_with_type_to_string( LastSeenTimestamp, _MaybeNow=undefined,
                                    DevType ) ->
    last_seen_info_with_type_to_string( LastSeenTimestamp,
        _Now=time_utils:get_timestamp(), DevType );

last_seen_info_with_type_to_string( LastSeenTimestamp, Now, DevType ) ->
    DurSec = time_utils:get_duration( LastSeenTimestamp, Now ),
    text_utils:format( " [~ts, last seen about ~ts ago]",
        [ oceanic_text:device_type_to_string( DevType ),
          time_utils:duration_to_synthetic_string( 1000 * DurSec ) ] ).



-doc "Returns a textual description of the specified device state.".
-spec device_state_to_string( device_state() ) -> ustring().
device_state_to_string( #device_state{
        eurid=Eurid,
        name=BinName,
        short_name=MaybeShortName,
        splitter=MaybeSplitter,
        type=MaybeType,
        eep_ids=EepIds,
        % Skipped: initial_event / last_event
        last_seen=MaybeLastSeenTimestamp,
        availability=AvailStatus,
        current_status=MaybeStatus } ) ->

    SplitStr = case MaybeSplitter of

        undefined ->
            case MaybeShortName of

                undefined ->
                    "";

                ShortName ->
                    text_utils:format( ", user-designated as '~ts'",
                                       [ ShortName ] )

            end;

        Splitter ->
            text_utils:format( ", whose splitter is '~ts'",
                               [ spell_tree:splitter_to_string( Splitter ) ] )

    end,

    text_utils:format( "device '~ts' (EURID: ~ts~ts), a ~ts "
        "(detected EEPs: ~ts), which currently is considered ~ts "
        "and which ~ts~ts",
        [ BinName, oceanic_text:eurid_to_string( Eurid ), SplitStr,
          oceanic_text:device_type_to_string( MaybeType ),
          text_utils:atoms_to_listed_string( set_utils:to_list( EepIds ) ),
          AvailStatus, maybe_status_to_string( MaybeStatus, MaybeType ),
          last_seen_info_to_string( MaybeLastSeenTimestamp ) ] ).



-doc """
Returns a short textual description of the specified device state.
<
Typically useful to report compact statuses on limited text interfaces (command
line, SMS, etc.).
""".
-spec device_state_to_short_string( device_state() ) -> ustring().
device_state_to_short_string( #device_state{
        name=BinName,
        splitter=MaybeSplitter,
        last_seen=MaybeLastSeenTimestamp,
        availability=lost } ) ->
    text_utils:format( "'~ts'~ts reported as lost~ts",
        [ BinName, splitter_to_string( MaybeSplitter ),
          last_seen_info_to_string( MaybeLastSeenTimestamp ) ] );

% availability=online from there.
device_state_to_short_string( #device_state{
        name=BinName,
        splitter=MaybeSplitter,
        type=DevType,
        last_seen=MaybeLastSeenTimestamp,
        current_status=MaybeStatus } ) ->
    text_utils:format( "'~ts'~ts ~ts~ts",
        [ BinName, splitter_to_string( MaybeSplitter ),
          maybe_status_to_string( MaybeStatus, DevType ),
          last_seen_info_with_type_to_string( MaybeLastSeenTimestamp,
                                              DevType ) ] ).



-doc "Returns a textual description of the specified device splitter.".
-spec splitter_to_string( option( splitter() ) ) -> ustring().
splitter_to_string( _MaybeSplitter=undefined ) ->
    "";

splitter_to_string( Splitter ) ->
    % Better than " ( '~ts')" or " (designated by '~ts')":
    text_utils:format( " (i.e. '~ts')",
                       [ spell_tree:splitter_to_string( Splitter ) ] ).



-doc """
Returns a textual description of any specified status of a device of any
specified type.

Expected to be appended after sentences like "This device ", thus to be followed
by a conjugated verb (thus like "reports XXX", or "is YYY").
""".
-spec maybe_status_to_string( option( device_status() ),
                              option( device_type() ) ) -> ustring().
maybe_status_to_string( _DevStatus=undefined, _DevType=thermometer ) ->
    "has no temperature to report yet";

maybe_status_to_string( _DevStatus=Temperature, _DevType=thermometer ) ->
    text_utils:format( "reports ~ts",
                       [ oceanic_text:temperature_to_string( Temperature ) ] );


maybe_status_to_string( _DevStatus=undefined, _DevType=thermo_hygro_sensor ) ->
    "has no temperature or hygrometry to report yet";

maybe_status_to_string( _DevStatus={ Temperature, RelHumidity },
                        _DevType=thermo_hygro_sensor ) ->
    text_utils:format( "reports ~ts and ~ts",
        [ oceanic_text:temperature_to_string( Temperature ),
          oceanic_text:relative_humidity_to_string( RelHumidity ) ] );


maybe_status_to_string( _DevStatus=undefined, _DevType=motion_detector ) ->
    "has no motion status to report yet";

maybe_status_to_string( _DevStatus={ MotionDetected, MaybeVoltage },
                        _DevType=motion_detector ) ->

    % Only relevant information wanted:
    "reports " ++ oceanic_text:motion_detection_to_string( MotionDetected )
       ++ case MaybeVoltage of

            undefined ->
                "";

            Voltage ->
                "and " ++ oceanic_text:maybe_voltage_to_string( Voltage )

          end;


maybe_status_to_string(
        _DevStatus={ MotionDetected, MaybeVoltage, MaybeIlluminance },
        _DevType=motion_detector ) ->

    % Only relevant information wanted:
    VoltStr = case MaybeVoltage of

        undefined ->
            "";

        Voltage ->
            ", " ++ oceanic_text:maybe_voltage_to_string( Voltage )

    end,
    text_utils:format( "reports ~ts~ts and ~ts",
        [ oceanic_text:motion_detection_to_string( MotionDetected ),
          VoltStr,
          oceanic_text:maybe_illuminance_to_string( MaybeIlluminance ) ] );


maybe_status_to_string( _DevStatus=undefined, _DevType=opening_detector ) ->
    "has no contact state to report yet";

maybe_status_to_string( _DevStatus=ContactStatus, _DevType=opening_detector ) ->
    text_utils:format( "is in ~ts state",
        [ oceanic_text:get_contact_status_description( ContactStatus ) ] );


maybe_status_to_string( _DevStatus=undefined, _DevType=push_button ) ->
    "has no push-button state to report yet";

maybe_status_to_string( _DevStatus=ButtonState, _DevType=push_button ) ->
    text_utils:format( "reports being ~ts",
        [ oceanic_text:get_button_state_description( ButtonState ) ] );


maybe_status_to_string( _DevStatus=undefined, _DevType=double_rocker ) ->
    "has no double-rocker state to report yet";

maybe_status_to_string( _DevStatus={ AIState, AOState, BIState, BOState },
                        _DevType=double_rocker ) ->

    PressedButs = case AIState of
        is_pressed -> [ "AI" ];
        is_released -> []
    end ++ case AOState of
        is_pressed -> [ "AO" ];
        is_released -> []
    end  ++ case BIState of
        is_pressed -> [ "BI" ];
        is_released -> []
    end ++ case BOState of
        is_pressed -> [ "BO" ];
        is_released -> []
    end,

    "has " ++ case PressedButs of

        [] ->
            "no button pressed";

        [ SingleButName ] ->
            text_utils:format( "the ~ts button pressed", [ SingleButName ] );

        ButNames ->
            text_utils:format( "the ~ts buttons pressed",
                               [ text_utils:strings_to_string( ButNames ) ] )

    end;


maybe_status_to_string( _DevStatus=undefined, _DevType=in_wall_module ) ->
    "has no moduler state to report yet";


% Generally the status cannot be decoded if the EEP of the smart plug is not
% known a priori:
%
maybe_status_to_string( _DevStatus=undefined, _DevType=smart_plug ) ->
    "has no state to report yet";

maybe_status_to_string( _DevStatus={ OutputPower, PowerFailureDetected,
        SwitchedOffDueToOvercurrent, HardwareStatus, _LocalControlEnabled },
        _DevType=smart_plug ) ->

    % Only report (briefly) extraordinary elements:

    MaybePfStr = oceanic_text:interpret_briefly_power_failure(
        PowerFailureDetected ),

    MaybeOcStr = oceanic_text:interpret_briefly_overcurrent_trigger(
        SwitchedOffDueToOvercurrent ),

    MaybeHSStr = oceanic_text:interpret_briefly_hardware_status(
        HardwareStatus ),

    ReportStr = case text_utils:join_maybe( _Sep=", ",
            [ MaybePfStr, MaybeOcStr, MaybeHSStr ] ) of

        "" ->
            "";

        Str ->
            ", with " ++ Str

    end,

    text_utils:format( "~ts~ts",
        [ oceanic_text:interpret_briefly_power_report( OutputPower ),
          ReportStr ] );


%status_to_string( _DevStatus=, _DevType=in_wall_module ) ->
%    text_utils:format( "~", [  ] );

maybe_status_to_string( _DevStatus=undefined, _DevType=undefined ) ->
    "reports no specific status (and its type is not known)";

maybe_status_to_string( DevStatus, _DevType=undefined ) ->
    text_utils:format( "reports an unknown status (~p)", [ DevStatus ] );

maybe_status_to_string( DevStatus, DevType ) ->
    text_utils:format( "reports an unknown ~ts status (~p)",
        [ oceanic_text:device_type_to_string( DevType ), DevStatus ] ).



-doc "Returns a textual description of the specified device table.".
-spec device_table_to_string( device_table() ) -> ustring().
device_table_to_string( DevTable ) ->
    case table:values( DevTable ) of

        [] ->
            "not registering any device";

        [ SingleDev ] ->
            text_utils:format( "registering a single device: ~ts",
                [ device_state_to_string( SingleDev ) ] );

        Devs ->
            text_utils:format( "registering ~B devices: ~ts",
                [ length( Devs ), text_utils:strings_to_string(
                    [ device_state_to_string( D ) || D <- Devs ] ) ] )

    end.
