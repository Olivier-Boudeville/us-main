#!/bin/sh

# Starts a US-Web instance, to be run as an OTP release.

# Script typically meant to be:
# - placed in /usr/local/bin of a gateway
# - run from systemctl, as root, as: 'systemctl start us-web.service'
#
# (hence to be triggered by /etc/systemd/system/us-web.service)


usage="Usage: $(basename $0) [US_CONF_DIR]: starts a US-Web server based on a US configuration directory specified on the command-line, otherwise found through the default US search paths."


# Note: all outputs of this script (standard and error ones) are automatically
# redirected by systemd to its journal.
#
# To consult them, use:
#   $ journalctl --pager-end --unit=us-web.service

# See also:
#  - stop-us-web.sh
#  - start-universal-server.sh

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


#echo "Starting US-Web as following user: $(id)"

# We need first to locate the us-web-common.sh script:

us_web_rel_root=$(/bin/ls -d -t /opt/universal-server/us_web-* 2>/dev/null | head -n 1)

if [ ! -d "${us_web_rel_root}" ]; then

	echo "Error, unable to locate the root of the target US-Web release (tried '${us_web_rel_root}' from '$(pwd)')." 1>&2

	exit 30

fi

# Location expected also by us-common.sh afterwards:
cd "${us_web_rel_root}"

# Will source in turn us-common.sh:
us_web_common_script_name="us-web-common.sh"
us_web_common_script="lib/us_web-latest/priv/bin/${us_web_common_script_name}"

if [ ! -f "${us_web_common_script}" ]; then

	echo "Error, unable to find ${us_web_common_script_name} script (not found as '${us_web_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi

#echo "Sourcing '${us_web_common_script}'."
. "${us_web_common_script}" #1>/dev/null

read_us_config_file $1 #1>/dev/null

read_us_web_config_file #1>/dev/null

secure_authbind

prepare_us_web_launch



echo
echo " -- Starting us_web application as user '${us_web_username}' (EPMD port: ${erl_epmd_port}) with '${us_web_exec}'..."


#echo /bin/sudo -u ${us_web_username} US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_WEB_APP_BASE_DIR="${US_WEB_APP_BASE_DIR}" ${epmd_opt} ${authbind} --deep ${us_web_exec} start


# Since 'start' was replaced by 'daemon', the release exec does not return, adding &:
/bin/sudo -u ${us_web_username} US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_WEB_APP_BASE_DIR="${US_WEB_APP_BASE_DIR}" ${epmd_opt} ${authbind} --deep ${us_web_exec} daemon &

res=$?


if [ $res -eq 0 ]; then

	# Unfortunately may still be a failure (ex: if a VM with the same name was
	# already running, start failed, not to be reported as a success)

	echo "  (authbind success reported)"

	# If wanting to check or have more details:
	inspect_us_web_log

	# Better diagnosis than the previous res code:
	# (only renamed once construction is mostly finished)
	trace_file="${us_log_dir}/us_web.traces"

	# Not wanting to diagnose too soon, otherwise we might return a failure code
	# and trigger the brutal killing by systemd of an otherwise working us_web:
	#
	sleep 4

	if [ -f "${trace_file}" ]; then

		echo "  (success assumed, as '${trace_file}' found)"
		exit 0

	else

		# For some unknown reason, if the start fails (ex: because a web root
		# does not exist), this script will exist quickly, as expected, yet
		# 'systemctl start' will wait for a long time (most probably because of
		# a time-out).
		#
		echo "  (failure assumed, as '${trace_file}' not found)"
		exit 100

	fi

else

	echo "  Error: authbind failure reported (code '$res')" 1>&2
	echo

	inspect_us_web_log

	exit $res

fi
