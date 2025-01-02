#!/bin/sh

# The default US-Main configuration file *for remote access*:
um_cfg_filename="us-main-remote-access.config"



# Presence-related options:

# If at home:
declare_presence_opt="declare_present"

# If away:
declare_absence_opt="declare_not_present"

get_presence_opt="is_present"



# Alarm-related options:

# Activate it (beware!):
start_alarm_opt="start_alarm"
stop_alarm_opt="stop_alarm"

get_alarm_opt="is_alarm_active"



# Lighting-related options:

start_lighting_opt="start_lighting"
stop_lighting_opt="stop_lighting"



usage="Usage: $(basename $0) [-h|--help] [US_MAIN_REMOTE_ACCESS_CONFIG_FILENAME] COMMAND [CMD_ARGS]: controls the target US-Main instance (possibly running on a remote host), based either on a default '${um_cfg_filename}' configuration filename or on a specified one, both looked-up in the US configuration directory found through the default US search paths (if not already absolute), by issuing the specified command (possibly with arguments).

Supported commands:

 - regarding presence:
	* ${declare_presence_opt}: declares that somebody is at home (hence for example deactivate alarm)
	* ${declare_absence_opt}: declares that nobody is at home (hence for example activate alarm)
	* ${get_presence_opt}: tells whether US-Main considers that somebody is at home

 - regarding alarm:
	* ${start_alarm_opt}: starts the alarm (siren)
	* ${stop_alarm_opt}: stops the alarm
	* ${get_alarm_opt}: tells whether the alarm is currently activated (hence wit a roaring siren)

 - regarding lighting:
	* ${start_lighting_opt}: starts all registered lighting
	* ${stop_lighting_opt}: stops all registered lighting

Example of use: './$(basename $0) us-main-remote-access-for-development.config stop_alarm ', this configuration file being located in the standard US configuration search paths, for example in the ~/.config/universal-server/ directory."


# So we are not looking up configuration files such as us.config (that regards
# US applications to be hosted locally), but ones for the remote access to such
# applications.



# Function needed for recursion.
#
# First argument expected to be set in the 'cmd' variable and shifted.
parse_arguments()
{

	#echo "Examining argument '${cmd}' (received args: $*)."

	case ${cmd} in

		${declare_presence_opt}) #echo "Declare presence"
								 args="$*"
								 if [ -n "${args}" ]; then
									 echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
									 exit 50
								 fi
								 ;;

		${declare_absence_opt}) args="$*"
								if [ -n "${args}" ]; then
									echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
									exit 51
								fi
								;;

		${get_presence_opt}) args="$*"
							 if [ -n "${args}" ]; then
								 echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
								 exit 52
							 fi
							 ;;


		${start_alarm_opt}) args="$*"
							if [ -n "${args}" ]; then
								echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
								exit 53
							fi
							;;


		${stop_alarm_opt}) args="$*"
						   if [ -n "${args}" ]; then
							   echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
							   exit 54
						   fi
						   ;;


		${get_alarm_opt}) args="$*"
						  if [ -n "${args}" ]; then
							  echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
							  exit 55
						  fi
						  ;;

		${start_lighting_opt}) args="$*"
							if [ -n "${args}" ]; then
								echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
								exit 56
							fi
							;;


		${stop_lighting_opt}) args="$*"
						   if [ -n "${args}" ]; then
							   echo "  Error, the '${cmd}' command does not expect arguments.
${usage}" 1>&2
							   exit 57
						   fi
						   ;;

		# Unknown command, supposedly a configuration file path:
		*) um_cfg_filename="${cmd}"
		   #echo "US-Main Configuration file path set to '${um_cfg_filename}'."
		   cmd="$1"
		   shift
		   #sleep 1
		   all_args="$*"
		   if [ -n "${all_args}" ]; then
			   parse_arguments ${all_args}
		   fi
		   ;;

	esac

}




if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi

cmd="$1"

if [ -z "${cmd}" ]; then

	echo "  Error, no command specified.
${usage}" 1>&2

	exit 5

fi

shift

parse_arguments $*

echo "Using command '${cmd}', with arguments '${args}' and configuration file '${um_cfg_filename}'."



# In priv/bin:
dir_name="$(dirname $0)"

common_client_helper="${dir_name}/us-main-common-for-clients.sh"

if [ ! -f "${common_client_helper}" ]; then

	echo "  Error, common US-Main helper script not found ('${common_client_helper}')." 1>&2

	exit 10

fi

. "${common_client_helper}"

# Sets a relevant located_um_cfg_file absolute path:
find_us_main_config_file "${um_cfg_filename}"

echo "Using the US-Main remote access configuration file resolved as '${located_um_cfg_file}'."


# Sets: um_cfg_base_content, um_erl_epmd_port, epmd_opt, remote_vm_cookie:
read_us_main_config_file "${located_um_cfg_file}"


script_dir="$(dirname $0)"

app_dir="${script_dir}/../../src/apps/"

cd "${app_dir}"


# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
echo make -s us_main_monitor_exec CMD_LINE_OPT="${cmd} ${args} --config-file ${located_um_cfg_file} --target-cookie ${remote_vm_cookie}" ${epmd_opt}

make -s us_main_controller_exec CMD_LINE_OPT="${cmd} ${args} --config-file ${located_um_cfg_file} --target-cookie ${remote_vm_cookie}" ${epmd_opt}
