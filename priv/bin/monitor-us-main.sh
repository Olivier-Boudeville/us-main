#!/bin/sh

us_main_remote_access_config_filename="us-main-remote-access.config"

usage="$(basename $0) [US_MAIN_REMOTE_ACCESS_CONFIG_FILE]: monitors the traces emitted by a US-Main instance (possibly running on a remote host), based either on a default '${us_main_remote_access_config_filename}' configuration filename or on a specified one, both looked-up in the US configuration directory found through the default US search paths.

Example of use: './$(basename $0) us-main-remote-access-for-development.config', this configuration file being located in the standard US configuration search paths, for example in the ~/.config/universal-server directory/."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


if [ -n "$1" ]; then

	us_main_remote_access_config_filename="$1"
	shift

fi


us_main_install_root="$(realpath $(dirname $0)/../..)"

# Will source in turn us-common.sh:
us_main_common_script_name="us-main-common.sh"
us_main_common_script="${us_main_install_root}/priv/bin/${us_main_common_script_name}"

if [ ! -f "${us_main_common_script}" ]; then

	echo "  Error, unable to find ${us_main_common_script_name} script (not found in '${us_main_common_script}')." 1>&2
	exit 35

fi

#echo "Sourcing '${us_main_common_script}'."
. "${us_main_common_script}" 1>/dev/null


read_us_config_file $1 1>/dev/null


# No specific update/check needs regarding vm.args, as the runtime cookie is
# updated on the fly.

#echo "us_config_dir = ${us_config_dir}"

# Now that us_config_dir is known:
um_cfg_file="${us_config_dir}/${us_main_remote_access_config_filename}"

if [ ! -f "${um_cfg_file}" ]; then

	echo "  Error, no US-Main configuration file found (no '${um_cfg_file}')." 1>&2

	exit 5

fi

#echo "Using US-Main configuration file '${um_cfg_file}'."

# US-Main configuration content, read once for all, with comments (%) removed:

um_cfg_base_content=$(/bin/cat "${um_cfg_file}" | sed 's|^[[:space:]]*%.*||1')


us_main_hostname=$(echo "${um_cfg_base_content}" | grep us_main_hostname | sed 's|^[[:space:]]*{[[:space:]]*us_main_hostname,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

if [ -z "${us_main_hostname}" ]; then

	echo "  Error, not remote US-Main hostname specified (no us_main_hostname defined)." 1>&2
	exit 10

fi


# Finally disabled, as a host that does not answer to ping would trigger a too
# long time-out:
#
#if ! ping -c 1 ${us_main_hostname} 1>/dev/null 2>&1; then

	# Not a fatal error, as not all servers answer pings:
	#echo "  Error, unable to ping the '${us_main_hostname}' remote US-Main hostname." 1>&2
	#exit 15

	#echo "  Warning: unable to ping the '${us_main_hostname}' remote US-Main hostname." 1>&2

#fi

#echo "Using '${us_main_hostname}' as remote US-Main hostname."

# Could have been done in the Erlang part:
remote_vm_cookie="$(echo "${um_cfg_base_content}" | grep remote_vm_cookie | sed 's|^[[:space:]]*{[[:space:]]remote_vm_cookie,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')"


if [ -z "${remote_vm_cookie}" ]; then

	if [ -z "${vm_cookie}" ]; then

		echo " Error, no cookie defined for the remote US-Main host (remote_vm_cookie) nor for the base cookie (vm_cookie)." 1>&2

	else

		echo "No cookie defined for the remote US-Main host, using the base one (defined in us.config's vm_cookie): ${vm_cookie}."
		remote_vm_cookie="${vm_cookie}"

	fi

else

	#echo "Using cookie defined for the remote US-Main host (remote_vm_cookie): ${remote_vm_cookie}."
	:

fi


# Needing from the start of the upcoming VM:
if [ -z "${erl_epmd_port}" ]; then

	echo "No Erlang EPMD port specified, not interfering with context defaults."
	epmd_opt=""

else

	echo "Using specified EPMD port, '${erl_epmd_port}'."
	epmd_opt="ERL_EPMD_PORT=${erl_epmd_port}"

fi

script_dir="$(dirname $0)"

app_dir="${script_dir}/../../src/apps/"

cd "${app_dir}"


# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
make -s us_main_monitor_exec EPMD_PORT=${erl_epmd_port} CMD_LINE_OPT="$* --config-file ${um_cfg_file} --target-cookie ${remote_vm_cookie}"
