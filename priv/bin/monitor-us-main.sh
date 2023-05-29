#!/bin/sh

# The default US-Main configuration file *for remote access*:
um_cfg_filename="us-main-remote-access.config"

usage="$(basename $0) [US_MAIN_REMOTE_ACCESS_CONFIG_FILENAME]: monitors the traces emitted by a US-Main instance (possibly running on a remote host), based either on a default '${um_cfg_filename}' configuration filename or on a specified one, both looked-up in the US configuration directory found through the default US search paths (if not already absolute).

Example of use: './$(basename $0) us-main-remote-access-for-development.config', this configuration file being located in the standard US configuration search paths, for example in the ~/.config/universal-server/ directory."


# So we are not looking up configuration files such as us.config (that regards
# US applications to be hosted locally), but ones for the remote access to such
# applications.


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

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


if [ ! -f "${um_cfg_filename}" ]; then

	# Not directly found. It is a missing file if its path is absolute,
	# otherwise it designates a file to be found in one of the possible US
	# configuration directories, which therefore must be found.

	# Checks whether absolute:
	if [[ "${um_cfg_filename:0:1}" == / || "${um_cfg_filename:0:2}" == ~[/a-z] ]]; then

		echo "  Error, the specified US-Main remote access configuration file, '${um_cfg_filename}', is an absolute path and does not exist." 1>&2

		exit 22

	fi

	# So searching this configuration file through the US standard paths:
	# (like, in us-common.sh, 'read_us_main_config_file()')

	if [ -n "${XDG_CONFIG_HOME}" ]; then

		base_path="${XDG_CONFIG_HOME}"

	else

		# May resolve to /root, if run through sudo:
		base_path="${HOME}/.config"

	fi

	app_dir="universal-server"

	us_config_dir="${base_path}/${app_dir}"

	# This is still a remote access configuration file:
	located_cfg_file="${us_config_dir}/${um_cfg_filename}"

	#echo "Looking up first '${located_cfg_file}'..."

	if [ ! -f "${located_cfg_file}" ]; then

		if [ -n "${XDG_CONFIG_DIRS}" ]; then

			# Pops the first element of that list:
			#
			# (note: currently the next - if any - directories in that list are
			# not tested; not implemented yet)
			#
			base_path=$(echo "${XDG_CONFIG_DIRS}" | sed 's|:.*$||1')

		else

			base_path="/etc/xdg"

		fi

		us_config_dir="${base_path}/${app_dir}"
		located_cfg_file="${us_config_dir}/${um_cfg_filename}"

		#echo "Looking up second '${located_cfg_file}'..."

		if [ ! -f "${located_cfg_file}" ]; then

			echo "  Error, unable to locate the '${um_cfg_filename}' configuration file through the US standard configuration paths." 1>&2

			exit 25

		fi

	fi

fi


um_cfg_file="$(realpath ${located_cfg_file})"

echo "Using the US-Main remote access configuration file resolved as '${um_cfg_file}'."


# US-Main configuration content, read once for all, with comments (%) removed:
um_cfg_base_content=$(/bin/cat "${um_cfg_file}" | sed 's|^[[:space:]]*%.*||1')


# Locating and just sourcing us-main-common.sh would have allowed to reuse
# defaults.


# Needed for reaching the target VM:
# (beware to any firewall being in the way)

# The EPMD port may be overridden here in US-Main, so that it does not clash
# with the one of any other US-* application (e.g. US-Web).

um_erl_epmd_port=$(echo "${um_cfg_base_content}" | grep epmd_port | sed 's|^[[:space:]]*{[[:space:]]*epmd_port,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')

if [ -n "${um_erl_epmd_port}" ]; then
	echo "Using the specified EPMD port, ${um_erl_epmd_port}."

else
	um_erl_epmd_port=4507
	echo "Using the default US-Main EPMD port, ${um_erl_epmd_port}."
fi

epmd_opt="EPMD_PORT=${um_erl_epmd_port}"


# Finally useless, as ping below now disabled:
#us_main_hostname=$(echo "${um_cfg_base_content}" | grep us_main_hostname | sed 's|^[[:space:]]*{[[:space:]]*us_main_hostname,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

#if [ -z "${us_main_hostname}" ]; then

#	echo "  Error, no remote US-Main hostname specified (no us_main_hostname defined)." 1>&2
#	exit 10

#fi


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

	if [ ! "$(echo "${remote_vm_cookie}" | wc -w)" = "1" ]; then

		echo " Error, invalid remote VM cookie obtained ('${remote_vm_cookie}'). Multiple 'remote_vm_cookie' configuration keys defined?" 1>&2

		exit 57

	fi

	echo "Using cookie defined for the remote US-Main host (remote_vm_cookie): ${remote_vm_cookie}."

fi


script_dir="$(dirname $0)"

app_dir="${script_dir}/../../src/apps/"

cd "${app_dir}"


# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
#echo make -s us_main_monitor_exec CMD_LINE_OPT="$* --config-file ${um_cfg_file} --target-cookie ${remote_vm_cookie}" ${epmd_opt}

make -s us_main_monitor_exec CMD_LINE_OPT="$* --config-file ${um_cfg_file} --target-cookie ${remote_vm_cookie}" ${epmd_opt}
