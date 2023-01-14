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
#  - kill-us-main.sh: to ensure, for a proper testing, that no previous instance
#    lingers


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


usage="Usage: $(basename $0) [US_CONF_DIR]: starts a US-Main server, to run as a native build, based on a US configuration directory specified on the command-line, otherwise found through the default US search paths. The US-Main installation itself will be looked up in '${us_main_install_root}'. This script must be run as root."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


if [ ! $(id -u) -eq 0 ]; then

	# As operations as chown will have to be performed:
	echo "  Error, this script must be run as root.
${usage}" 1>&2
	exit 5

fi


# XDG_CONFIG_DIRS defined, so that the US server as well can look it up:
xdg_cfg_dirs="${XDG_CONFIG_DIRS}:/etc/xdg"


maybe_us_config_dir="$1"

if [ -n "${maybe_us_config_dir}" ]; then

	if [ ! -d "${maybe_us_config_dir}" ]; then

		echo "  Error, the specified US configuration directory, '${maybe_us_config_dir}', does not exist." 1>&2

		exit 20

	fi

	# As a 'universal-server/us.config' suffix will be added to each candidate
	# configuration directory, so we remove the last directorty:

	candidate_dir="$(dirname $(realpath ${maybe_us_config_dir}))"

	xdg_cfg_dirs="${candidate_dir}:${xdg_cfg_dirs}"

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
read_us_config_file "${maybe_us_config_dir}" #1>/dev/null

read_us_main_config_file #1>/dev/null

secure_authbind

prepare_us_main_launch

cd src || exit


#echo "epmd_make_opt=${epmd_make_opt}"

# Launching explicitly a properly-condigured EPMD for US-Main (preferably from
# the current weakly-privileged user):
#
# (note that a former instance of EPMD may wrongly report that a node with the
# target name is still running, whereas no Erlang VM even exists)
#
make -s launch-epmd ${epmd_make_opt} || exit


if [ -n "${erl_epmd_port}" ]; then
	epmd_start_msg="configured EPMD port ${erl_epmd_port}"
else
	epmd_start_msg="default US-Main port ${default_us_main_epmd_port}"
fi


echo
echo " -- Starting US-Main natively-built application as user '${us_main_username}', on ${epmd_start_msg}, whereas log directory is '${us_main_vm_log_dir}'..."


# Apparently variables may be indifferently set prior to make, directly in the
# environment (like 'XDG_CONFIG_DIRS=xxx make TARGET') or as arguments (like
# 'make TARGET XDG_CONFIG_DIRS=xxx'), even if there are a sudo and an authbind
# in the equation.

# Previously the '--depth' authbind option was used, and apparently a depth of 6
# was sufficient - but no interest in taking risks.

#echo Starting US-Main: /bin/sudo -u ${us_main_username} ${authbind} --deep make -s us_main_exec_service XDG_CONFIG_DIRS="${xdg_cfg_dirs}" VM_LOG_DIR="${us_main_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_MAIN_APP_BASE_DIR="${US_MAIN_APP_BASE_DIR}" ${cookie_env} ${epmd_make_opt}

/bin/sudo -u ${us_main_username} ${authbind} --deep make -s us_main_exec_service XDG_CONFIG_DIRS="${xdg_cfg_dirs}" VM_LOG_DIR="${us_main_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_MAIN_APP_BASE_DIR="${US_MAIN_APP_BASE_DIR}" ${cookie_env} ${epmd_make_opt}

res=$?

# If a launch time-out keeps on progressing, this might be the sign that a
# previous US-Main instance is running.

if [ ${res} -eq 0 ]; then

	# Unfortunately may still be a failure (e.g. if a VM with the same name was
	# already running, start failed, not to be reported as a success)

	echo "  (authbind success reported)"

	# If wanting to check or have more details:
	inspect_us_main_log

	# Not wanting to diagnose too soon, otherwise we might return a failure code
	# and trigger the brutal killing by systemd of an otherwise working us_main:
	#
	sleep 4

	# Better diagnosis than the previous res code:
	#
	# (only renamed once construction is mostly finished; trace_file supposedly
	# inherited from us-main-common.sh)
	#
	if [ -f "${trace_file}" ]; then

		echo "  (success assumed, as '${trace_file}' found)"
		exit 0

	else

		# For some unknown reason, if the start fails, this script will exit
		# quickly, as expected, yet 'systemctl start' will wait for a long time
		# (most probably because of a time-out).
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
