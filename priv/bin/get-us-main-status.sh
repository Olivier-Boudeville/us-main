#!/bin/sh

# A script to diagnose the status of a local US-Main instance.

# Script typically meant to be placed in /usr/local/bin of a gateway
#
# Complementary to running, as root:
# 'systemctl status us-main-as-native-build.service'.


# See also the {start,stop,kill,control,monitor}-us-main.sh scripts.


usage="Usage: $(basename $0): returns the status of a supposedly locally-running US-Main release."

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


if [ ! $# -eq 0 ]; then

	echo "  Error, no argument expected.
${usage}" 1>&2

	exit 10

fi


# Getting the real path is useful, as this script may be actually run through a
# symlink located elsewhere (e.g. in /usr/local/bin):
#
this_script_dir="$(dirname $(realpath $0))"

local_us_main_install_root="${this_script_dir}/../.."

if [ -d "${local_us_main_install_root}/priv" ]; then

	us_main_install_root="$(realpath ${local_us_main_install_root})"
	#echo "Selecting US-Main development native build in '${us_main_install_root}'."

else

	echo "  Error, no valid US-Main native build found, the '$(realpath ${local_us_main_install_root})' location was expected." 1>&2

	exit 15

fi


# We need first to locate the us-main-common.sh script:

# Location expected also by us-common.sh afterwards:
cd "${us_main_install_root}" || exit 40



# Will source in turn us-common.sh:
us_main_common_script_name="us-main-common.sh"
us_main_common_script="${us_main_install_root}/priv/bin/${us_main_common_script_name}"

if [ ! -f "${us_main_common_script}" ]; then

	echo "  Error, unable to find ${us_main_common_script_name} script (not found as '${us_main_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Hint for the helper scripts:
us_launch_type="native"

#echo "Sourcing '${us_main_common_script}'."
. "${us_main_common_script}" 1>/dev/null

# We expect a pre-installed US configuration file to exist:
read_us_config_file "$1" 1>/dev/null

read_us_main_config_file 1>/dev/null


# No specific update/check needs regarding vm.args (no VM launched).

echo
echo "Getting the status of the US-Main application possibly running from '${us_main_install_root}' as user '${us_main_username}', on US-Main EPMD port ${us_main_epmd_port}..."

# Yes, twice:
ps_base_opts="-w -w"

# Possibly useful as well: lstart, start_time
ps_format_opts="etime,user,pid,args"

echo
echo "(format for processes: [launched since] [as user] [as PID] [with command])"
echo

echo " -- processes about US-Main (expecting run_erl and the us_main VM process itself):"
# (removed: '--user ${us_main_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "exec us_main_app" | grep -v grep
echo

echo " -- all local EPMD processes:"
# (removed: '--user ${us_main_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "bin/epmd" | grep -v grep
echo


echo " -- names listed by this US-Main EPMD instance:"
epmd -port ${us_main_epmd_port} -names
echo


echo " -- corresponding logs recorded by systemd:"
journalctl -e --unit us-main-as-native-build.service --no-pager --lines=50
echo


echo " -- corresponding US-Main applicative logs:"
# If not finding a us-main log file, might be the sign that US-Main is actually
# not running:
#
inspect_us_main_log
