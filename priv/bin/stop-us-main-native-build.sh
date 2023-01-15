#!/bin/sh

# Stops a US-Main instance, supposedly already running as a native build on the
# current host.

# Script typically meant to be:
# - placed in /usr/local/bin of a gateway
# - run from systemctl, as root, as:
# 'systemctl stop us-main-as-native-build.service'
#
# (hence to be triggered by /etc/systemd/system/us-main-as-native-build.service)
#
# Note: if run through systemd, all outputs of this script (standard and error
# ones) are automatically redirected by systemd to its journal.
#
# To consult them, use:
#   $ journalctl --pager-end --unit=us-main-as-native-build.service

# See also:
#  - kill-us-main-native-build.sh
#  - start-us-main-native-build.sh
#  - deploy-us-main-native-build.sh



# Implementation notes:

# To stop US-Main, we only have to know the target host (which is here the local
# one), the node name ('us_main' here) and the right cookie and EPMD port, which
# requires thus to find and read the relevant US (not US-Main) configuration
# file. For that we rely our helper shell scripts, and thus we have to locate
# the right US-Main installation as a first move.


# We use a dedicated stop client rather than pipes, as currently pipes are found
# unresponsive:
#
# $ to_erl /tmp/launch-erl-1258957
#Attaching to /tmp/launch-erl-1258957 (^D to exit)
#
#^L

# And no entered command is managed (only CTRL-C and CTL-D work), altough the
# TERM environment variable was set before run_erl and to_erl.



# Either this script is called during development, directly from within a
# US-Main installation, in which case this installation shall be used, or
# (typically if called through systemd) the standard US-Main base directory
# shall be targeted:
#
this_script_dir="$(dirname $0)"

# Possibly:
local_us_main_install_root="${this_script_dir}/../.."

# Check:
if [ -d "${local_us_main_install_root}/priv" ]; then

	us_main_install_root="$(realpath ${local_us_main_install_root})"
	echo "Selecting US-Main development native build in '${us_main_install_root}'."

else

	# The location enforced by deploy-us-main-native-build.sh:
	us_main_install_root="/opt/universal-server/us_main-native/us_main"
	echo "Selecting US-Main native build in standard location '${us_main_install_root}'."

	if [ ! -d "${us_main_install_root}" ]; then

		echo "  Error, no US-Main native build found, neither locally (as '$(realpath ${local_us_main_install_root})') nor at the '${us_main_install_root}' standard location." 1>&2

		exit 15

	fi

fi

usage="Usage: $(basename $0) [US_CONF_DIR]: stops a US-Main server, running as a native build, based on a US configuration directory specified on the command-line, otherwise found through the default US search paths. The US-Main installation itself will be looked up in '$(realpath ${us_main_install_root})'."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


epmd="$(which epmd 2>/dev/null)"

if [ ! -x "${epmd}" ]; then

	echo "  Error, no EPMD executable found." 1>&2

	exit 8

fi


if [ $# -gt 1 ]; then

	echo "  Error, extra argument expected.
${usage}" 1>&2

	exit 10

fi


#echo "Stopping US-Main native build, as following user: $(id)"

# We need first to locate the us-main-common.sh script:

# Location expected also by us-common.sh afterwards:
cd "${us_main_install_root}"

# Will source in turn us-common.sh:
us_main_common_script_name="us-main-common.sh"
us_main_common_script="priv/bin/${us_main_common_script_name}"

if [ ! -f "${us_main_common_script}" ]; then

	echo "  Error, unable to find ${us_main_common_script_name} script (not found as '${us_main_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Will source in turn us-common.sh:
us_main_common_script_name="us-main-common.sh"
us_main_common_script="priv/bin/${us_main_common_script_name}"

if [ ! -f "${us_main_common_script}" ]; then

	echo "  Error, unable to find ${us_main_common_script_name} script (not found as '${us_main_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Hints for the helper scripts:
export us_launch_type="native"

#echo "Sourcing '${us_main_common_script}'."
. "${us_main_common_script}" #1>/dev/null

#echo "Reading US configuration file:"
read_us_config_file "$1" #1>/dev/null

# Now we also need to read the US-Main configuration file here, to fetch any
# EPMD port there:
#
#echo "Reading US-Main configuration file:"
read_us_main_config_file #1>/dev/null

# Not needed:
#secure_authbind


echo " -- Stopping US-Main natively-built application (EPMD port: ${us_main_erl_epmd_port})..."


# We must stop the VM with the right (Erlang) cookie, i.e. the actual runtime
# one, not the dummy, original one:
#
# Commented-out, as we can actually specify it directly on the command-line:
#update_us_main_config_cookie


if [ -n "${vm_cookie}" ]; then
	#echo "Using cookie '${vm_cookie}'."
	cookie_env="COOKIE=${vm_cookie}"
else
	cookie_env=""
fi

cd src/apps

# No sudo or authbind necessary here, no US_* environment variables either:
echo make -s us_main_stop_exec EPMD_PORT=${us_main_erl_epmd_port} CMD_LINE_OPT="$* --target-cookie ${vm_cookie}"

make -s us_main_stop_exec EPMD_PORT=${us_main_erl_epmd_port} CMD_LINE_OPT="$* --target-cookie ${vm_cookie}"

res=$?

if [ $res -eq 0 ]; then

	echo "  (stop success reported)"
	echo

	# If wanting to check or have more details:
	#inspect_us_main_log

	exit 0

else

	# Despite following message: 'Node 'us_main_app@127.0.0.1' not responding to
	# pings."

	echo "  Error: stop failure reported (code '$res')" 1>&2
	echo

	#inspect_us_main_log

	# Finally wanting pseudo-failure to propagate:
	res=0

fi

# Restore original cookie file:
#/bin/mv -f "${backup_vm_args_file}" "${vm_args_file}"
#/bin/cp -f "${backup_vm_args_file}" "${vm_args_file}"


# Unfortunately, at least in some cases, EPMD will believe that this just
# shutdown US-Main instance is still running; as the next epmd command may not
# work (if -relaxed_command_check was not used at EPMD startup; we nevertheless
# ensure that it is the case if using our start scripts), the only remaining
# solution would be to kill this EPMD; yet this may impact other running Erlang
# applications (see kill-us-main.sh to minimise the harm done), so it is not
# done here.

if [ -n "${us_main_erl_epmd_port}" ]; then

	echo "Using user-defined US-Main EPMD port ${us_main_erl_epmd_port}."
	export ERL_EPMD_PORT="${us_main_erl_epmd_port}"

else

	# Using the default US-Main EPMD port (see the EPMD_PORT make variable),
	# supposing that the instance was properly launched (see the 'launch-epmd'
	# make target):

	echo "Using default US-Main EPMD port ${default_us_main_epmd_port}."

	export ERL_EPMD_PORT="${default_us_main_epmd_port}"

fi

if ! ${epmd} -stop us_main; then

	echo "  Error while unregistering the US-Main server from the EPMD daemon at port ${ERL_EPMD_PORT}." 1>&2

	exit 25

fi

# kill-us-main.sh may also be your last-resort friend.

exit ${res}
