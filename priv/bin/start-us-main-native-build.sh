#!/bin/sh

# Starts a US-Main instance, to be run as a native build (on the current host).

# Script typically meant to be:
# - placed in /usr/local/bin of a gateway
# - run from systemctl, as root, as:
# 'systemctl start us-main-as-native-build.service'
#
# (hence to be triggered by /etc/systemd/system/us-main-as-native-build.service)
#
# Note: if run through systemd, all outputs of this script (standard and error
# ones) are automatically redirected by systemd to its journal.
#
# To consult them, use:
#   $ journalctl --pager-end --unit=us-main-as-native-build.service

# See also:
#  - deploy-us-main-native-build.sh
#  - stop-us-main-native-build.sh
#  - start-universal-server.sh


# Either this script is called during development, directly from within a
# US-Main installation, in which case this installation shall be used, or
# (typically if called through systemd) the standard US-Main base directory
# shall be targeted:
#
this_script_dir="$(dirname $0)"

# Possibly:
local_us_main_install_root="${this_script_dir}/../.."

# Checks based on the 'priv' subdirectory for an increased reliability:
if [ -d "${local_us_main_install_root}/priv" ]; then

	us_main_install_root="$(realpath ${local_us_main_install_root})"
	echo "Selecting US-Main development native build in '${us_main_install_root}'."

else

	# The location enforced by deploy-us-main-native-build.sh:
	us_main_install_root="/opt/universal-server/us_main-native/us_main"
	echo "Selecting US-Main native build in standard location '${us_main_install_root}'."

	if [ ! -d "${us_main_install_root}/priv" ]; then

		echo "  Error, no valid US-Main native build found, neither locally (as '$(realpath ${local_us_main_install_root})') nor at the '${us_main_install_root}' standard location." 1>&2

		exit 15

	fi

fi


usage="Usage: $(basename $0) [US_CONF_DIR]: starts a US-Main server, to run as a native build, based on a US configuration directory specified on the command-line, otherwise found through the default US search paths. The US-Main installation itself will be looked up in '${us_main_install_root}'."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


#echo "Starting US-Main, to run as a native build with following user: $(id)"

# We need first to locate the us-main-common.sh script:

# Location expected also by us-common.sh afterwards:
cd "${us_main_install_root}" || exit

# As expected by us-main-common.sh for the VM logs:
log_dir="${us_main_install_root}/log"
if [ ! -d "${log_dir}" ]; then

	mkdir "${log_dir}"

fi

# Will source in turn us-common.sh:
us_main_common_script_name="us-main-common.sh"
us_main_common_script="priv/bin/${us_main_common_script_name}"

if [ ! -f "${us_main_common_script}" ]; then

	echo "  Error, unable to find ${us_main_common_script_name} script (not found as '${us_main_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Hint for the helper scripts:
us_launch_type="native"

#echo "Sourcing '${us_main_common_script}'."
. "${us_main_common_script}" #1>/dev/null

# We expect a pre-installed US configuration file to exist:
read_us_config_file "$1" #1>/dev/null

read_us_main_config_file #1>/dev/null

secure_authbind

prepare_us_main_launch

cd src || exit


# Note that a former instance of EPMD may wrongly report that a node with the
# target name is still running (whereas no Erlang VM is even running). Apart
# from killing this EPMD instance (jeopardising any other running Erlang
# application), no solution exists apparently (names cannot be unregistered from
# EPMD, as we do not launch it with -relaxed_command_check).

echo
echo " -- Starting US-Main natively-built application as user '${us_main_username}' (EPMD port: ${erl_epmd_port})..."


# Previously the '--deep' authbind option was used; apparently the minimal depth
# is 6:

#echo /bin/sudo -u ${us_main_username} VM_LOG_DIR="${us_main_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_MAIN_APP_BASE_DIR="${US_MAIN_APP_BASE_DIR}" ${cookie_env} ${epmd_opt} ${authbind} --depth 6 make us_main_exec_service

/bin/sudo -u ${us_main_username} VM_LOG_DIR="${us_main_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_MAIN_APP_BASE_DIR="${US_MAIN_APP_BASE_DIR}" ${cookie_env} ${epmd_opt} ${authbind} --depth 6 make -s us_main_exec_service

res=$?

# If a launch time-out keeps on progressing, this might be the sign that a
# previous US-Main instance is running.

if [ ${res} -eq 0 ]; then

	# Unfortunately may still be a failure (ex: if a VM with the same name was
	# already running, start failed, not to be reported as a success)

	echo "  (authbind success reported)"

	# If wanting to check or have more details:
	inspect_us_main_log

	# Better diagnosis than the previous res code:
	# (only renamed once construction is mostly finished)
	#
	trace_file="${us_main_vm_log_dir}/us_main.traces"

	# Not wanting to diagnose too soon, otherwise we might return a failure code
	# and trigger the brutal killing by systemd of an otherwise working us_main:
	#
	sleep 4

	if [ -f "${trace_file}" ]; then

		echo "  (success assumed, as '${trace_file}' found)"
		exit 0

	else

		# For some unknown reason, if the start fails (ex: because a main root
		# does not exist), this script will exit quickly, as expected, yet
		# 'systemctl start' will wait for a long time (most probably because of
		# a time-out).
		#
		echo "  (failure assumed, as '${trace_file}' not found)"
		exit 100

	fi

else

	echo "  Error: authbind failure reported (code '$res')" 1>&2
	echo

	inspect_us_main_log

	exit ${res}

fi
