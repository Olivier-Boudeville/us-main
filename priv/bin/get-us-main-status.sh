#!/bin/sh

# Script typically meant to be placed in /usr/local/bin of a gateway
#
# Complementary to running, as root:
# 'systemctl status us-main-as-native-build.service'.


# See also start-us-main.sh and stop-us-main.sh.

usage="Usage: $(basename $0): returns the status of a supposedly locally-running US-Main release."

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


#echo "Starting US-Main as following user: $(id)"

# We need first to locate the us-main-common.sh script:

us_main_rel_root=$(/bin/ls -d -t /opt/universal-server/us_main-* 2>/dev/null | head -n 1)

if [ ! -d "${us_main_rel_root}" ]; then

	echo "Error, unable to locate the root of the target US-Main release (tried '${us_main_rel_root}' from '$(pwd)')." 1>&2

	exit 30

fi

# Location expected also by us-common.sh afterwards:
cd "${us_main_rel_root}"

# Will source in turn us-common.sh:
us_main_common_script_name="us-main-common.sh"
us_main_common_script="lib/us_main-latest/priv/bin/${us_main_common_script_name}"

if [ ! -f "${us_main_common_script}" ]; then

	echo "Error, unable to find ${us_main_common_script_name} script (not found as '${us_main_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi

#echo "Sourcing '${us_main_common_script}'."
. "${us_main_common_script}" 1>/dev/null


# Comment redirections for more details:

read_us_config_file "$1" #1>/dev/null

read_us_main_config_file #1>/dev/null


# No specific update/check needs regarding vm.args (no VM launched).

echo
echo " -- Getting status of the us_main application possibly running as user '${us_main_username}' (EPMD port: ${erl_epmd_port}), from '${us_main_rel_dir}'..."


# Yes, twice:
ps_base_opts="-w -w"

# Possibly useful as well: lstart, start_time
ps_format_opts="etime,user,pid,args"

echo
echo "Processes: [launched since] [command]"

echo
echo " -- processes about us_main (expecting run_erl and us_main itself):"
# (removed: '--user ${us_main_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "bin/us_main" | grep -v grep

echo
echo " -- processes for EPMD:"
# (removed: '--user ${us_main_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "bin/epmd" | grep -v grep
echo

echo
echo " -- EPMD listed names:"

if [ -n "${erl_epmd_port}" ]; then

	epmd_port_opt="-port ${erl_epmd_port}"

fi

epmd ${epmd_port_opt} -names
echo

journalctl -xe --unit us-main-as-native-build.service --no-pager --lines=20

# If not finding a us-main log file, might be the sign that us-main is actually
# not running:
#
inspect_us_main_log


exit 0
