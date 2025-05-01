#!/bin/sh

# Stops a US-Main instance, supposedly already running as an OTP release.

# Script typically meant to be:
# - placed in /usr/local/bin of a gateway
# - run from systemctl, as root, as: 'systemctl stop us-main.service'
#
# (hence to be triggered by /etc/systemd/system/us-main.service)


usage="Usage: $(basename $0) [US_CONF_DIR]: stops a US-Main server based on a US configuration directory specified on the command-line, otherwise found through the default US search paths."


# Note: all outputs of this script (standard and error ones) are automatically
# redirected by systemd to its journal.
#
# To consult them, use:
#   $ journalctl --pager-end --unit=us-main.service

# See also:
#  - start-us-main.sh
#  - stop-universal-server.sh

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


#echo "Stopping US-Main as following user: $(id)"

# We need first to locate the us-main-common.sh script:

us_main_rel_root=$(/bin/ls -d -t /opt/universal-server/us_main-* 2>/dev/null | head -n 1)

if [ ! -d "${us_main_rel_root}" ]; then

	echo "  Error, unable to locate the root of the target US-Main release (tried, from '$(pwd)', '${us_main_rel_root}')." 1>&2

	exit 30

fi

# Location expected also by us-common.sh afterwards:
cd "${us_main_rel_root}"

# Will source in turn us-common.sh:
us_main_common_script_name="us-main-common.sh"
us_main_common_script="lib/us_main-latest/priv/bin/${us_main_common_script_name}"

if [ ! -f "${us_main_common_script}" ]; then

	echo "  Error, unable to find ${us_main_common_script_name} script (not found as '${us_main_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi

#echo "Sourcing '${us_main_common_script}'."
. "${us_main_common_script}" #1>/dev/null

read_us_config_file "$1" #1>/dev/null

read_us_main_config_file #1>/dev/null

secure_authbind



echo " -- Stopping us_main application as user '${us_main_username}' (EPMD port: ${erl_epmd_port}) with '${us_main_exec}'..."


# We must stop the VM with the right (Erlang) cookie, i.e. the actual runtime
# one, not the dummy, original one:
#
# Commented-out, as we can actually specify it directly on the command-line:
#update_us_main_config_cookie


if [ -n "${vm_cookie}" ]; then
	echo "Using cookie '${vm_cookie}'."
	cookie_env="COOKIE=${vm_cookie}"
else
	cookie_env=""
fi

#echo /bin/sudo -u ${us_main_username} US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_MAIN_APP_BASE_DIR="${US_MAIN_APP_BASE_DIR}" ${cookie_env} ${epmd_opt} ${authbind} --deep ${us_main_exec} stop

/bin/sudo -u ${us_main_username} US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_MAIN_APP_BASE_DIR="${US_MAIN_APP_BASE_DIR}" ${cookie_env} ${epmd_opt} ${authbind} --deep ${us_main_exec} stop

res=$?


# Otherwise at next start, the runtime cookie will be seen with any /bin/ps
# (since being in the release-related launch command-line):
#
#restore_us_main_config_cookie


# Not so reliable unfortunately:
if [ $res -eq 0 ]; then

	echo "  (authbind success reported)"
	echo

	# If wanting to check or have more details:
	inspect_us_main_launch_outcome

	exit 0

else

	# Despite following message: 'Node 'us_main_app@127.0.0.1' not responding to
	# pings."

	echo "  Error: authbind failure reported (code '$res')" 1>&2
	echo

	inspect_us_main_launch_outcome

	# Finally wanting pseudo-failure to propagate:
	res=0

fi

# Restore original cookie file:
#/bin/mv -f "${backup_vm_args_file}" "${vm_args_file}"
#/bin/cp -f "${backup_vm_args_file}" "${vm_args_file}"


# Will generally not work (-relaxed_command_check not having been used):
epmd -port ${erl_epmd_port} -stop us_main

# So 'killall epmd' may also be your friend, although it may affect other
# applications such as the Universal server itself.

exit $res
