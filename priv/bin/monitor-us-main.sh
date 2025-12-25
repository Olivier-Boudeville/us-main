#!/bin/sh

# The default US-Main configuration file *for remote access*:
um_cfg_filename="us-main-remote-access.config"

do_download=1

download_short_opt="-d"
download_long_opt="--download-trace-file"

download_trace_filename="us-main.traces"


usage="Usage: $(basename $0) [-h|--help] [${download_short_opt}|${download_long_opt}] [US_MAIN_REMOTE_ACCESS_CONFIG_FILENAME]: monitors the traces emitted by a US-Main instance (possibly running on a remote host), based either on a default '${um_cfg_filename}' configuration filename or on a specified one, both looked-up in the US configuration directory found through the default US search paths (if not already absolute).

Options:
  ${download_short_opt} or ${download_long_opt}: downloads the corresponding trace file, as '$(download_trace_filename)', typically so that it can be fetched/sent afterwards from the local host

Example of use: './$(basename $0) us-main-remote-access-for-development.config', this configuration file being located in the standard US configuration search paths, for example in the ~/.config/universal-server/ directory."


# So we are not looking up configuration files such as us.config (that regards
# US applications to be hosted locally), but ones for the remote access to such
# applications.


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


if [ "$1" = "${download_short_opt}" ] || [ "$1" = "${download_long_opt}" ]; then

	echo "(will download the trace file for later reuse)"
	do_download=0
	download_final_opt="${download_long_opt} ${download_trace_filename}"
	shift

fi


if [ $# -gt 1 ]; then

	echo "  Error, extra argument specified.
${usage}" 1>&2

	exit 5

fi


# Overrides defaults if specified:
if [ -n "$1" ]; then

	um_cfg_filename="$1"
	located_cfg_file="${um_cfg_filename}"

	# Otherwise would remain in the extra arguments transmitted in CMD_LINE_OPT:
	shift

fi


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
#echo make -s us_main_monitor_exec CMD_LINE_OPT="$* ${download_final_opt} --config-file ${located_um_cfg_file} --target-cookie ${remote_vm_cookie}" ${epmd_opt}

make -s us_main_monitor_exec CMD_LINE_OPT="$* ${download_final_opt} --config-file ${located_um_cfg_file} --target-cookie ${remote_vm_cookie}" ${epmd_opt}
