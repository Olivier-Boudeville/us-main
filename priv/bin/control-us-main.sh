#!/bin/sh

# A script to control a (possibly remote) US-Main instance.

conf_opt="--config-filename"

verbose_long_opt="--verbose"
verbose_short_opt="-v"


# The default US-Main configuration file *for remote access*:
um_cfg_filename="us-main-remote-access.config"

usage="Usage: $(basename $0) [-h|--help] [${conf_opt} US_MAIN_REMOTE_ACCESS_CONFIG_FILENAME] [${verbose_long_opt}|${verbose_short_opt}] ACTION [ACTION_ARGS]: controls the target US-Main instance (possibly running on a remote host), based either on a default '${um_cfg_filename}' configuration filename or on a specified one, both looked-up in the US configuration directory found through the default US search paths (if not already absolute), by issuing the specified action (possibly with arguments).

  ${verbose_long_opt} or ${verbose_short_opt}: enable verbose mode

Use the 'help' action to list all available ones.

Example of use: './$(basename $0) ${conf_opt} us-main-remote-access-for-development.config switch_on tv_plug', this configuration file being located in the standard US configuration search paths, for example in the ~/.config/universal-server/ directory."


# So we are not looking up configuration files such as us.config (that regards
# US applications to be launched locally), but ones for the remote access to
# such applications.

# A previous version checked the actions from this script; now that they have
# been generically implemented in US-Common, once having sorted out any
# configuration file specified, this script just forwards the action tokens as
# they are to the corresponding US server.


verbose=1

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


if [ "$1" = "${conf_opt}" ]; then

	shift

	um_cfg_filename="$1"

	if [ -z "${um_cfg_filename}" ]; then

		echo "  Error, no configuration filename specified after ${conf_opt}.
${usage}" 1>&2

		exit 15

	fi

	shift

	# Cannot check file existence at this level, as it must be searched in the
	# US standard paths.

fi


if [ "$1" = "${verbose_short_opt}" ] || [ "$1" = "${verbose_long_opt}" ]; then

	shift
	verbose=0
	echo "(verbose mode activated)"

fi



full_action="$*"

if [ -z "${full_action}" ]; then

	echo "  Error, no action specified.
${usage}" 1>&2

	exit 10

fi


[ $verbose -eq 1 ] || echo "Read configuration file '${um_cfg_filename}' and full action tokens '${full_action}'."


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

[ $verbose -eq 1 ] || echo "Using the US-Main remote access configuration file resolved as '${located_um_cfg_file}'."


# Sets: um_cfg_base_content, um_erl_epmd_port, epmd_opt, remote_vm_cookie:
read_us_main_config_file "${located_um_cfg_file}" 1>/dev/null


script_dir="$(dirname $0)"

app_dir="${script_dir}/../../src/apps/"

cd "${app_dir}"


verbose_str=""

if [ $verbose -eq 0 ]; then
	verbose_str="--verbose"
fi


# The (full) action shall be interpreted as a plain, extra one:

#echo make -s us_main_controller_silent_exec CMD_LINE_OPT="\"${full_action}\" --config-file \"${located_um_cfg_file}\" ${verbose_str} --target-cookie \"${remote_vm_cookie}\" ${epmd_opt}"

make -s us_main_controller_silent_exec ${epmd_opt} CMD_LINE_OPT="\"${full_action}\" --config-file \"${located_um_cfg_file}\" ${verbose_str} --target-cookie ${remote_vm_cookie}"
