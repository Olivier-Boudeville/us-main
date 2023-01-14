#!/bin/sh

usage="Usage: $(basename $0): kills any running US-Main instance(s), and ensures none is registered in the specifically-associated EPMD."

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"
	exit 0

fi

if [ ! $# -eq 0 ]; then

	echo "  Error, no argument expected.
${usage}" 1>&2

	exit 10

fi


epmd="$(which epmd 2>/dev/null)"

if [ ! -x "${epmd}" ]; then

	echo "  Error, no EPMD executable found." 1>&2

	exit 8

fi


# Of course using stop-us-main-{native-build,release}.sh shall be preferred, as
# we kill (not terminate properly) any US-Main instance(s).
#
# We used to kill EPMD as well, but we do not want to interfere with other
# Erlang applications. However, killing all US-Main instances is not always
# sufficient, we may still have "Protocol 'inet_tcp': the name us_main@xxxx
# seems to be in use by another Erlang node" errors, because of EPMD.
#
# Unregistering a name (e.g. 'us_main') is not very relevant either with the
# default, automatically-launched EPMD (as for example it does not support
# relaxed rules), so the best option is now to rely on different,
# per-application EPMD instances, running on different ports.
#
# As a result, even a script to kill brutally instances has to be aware of the
# US configuration files (for any 'epmd_port' entry defined there), in order to
# determine the relevant EPMD port (then available in the erl_epmd_port
# variable).
#
# For that, we have to determine the path to the various US scripts involved. We
# do the same as, for example, start-us-main-native-build.sh :

# Getting the real path is useful, as this script may be actually run through a
# symlink located elsewhere (e.g. in /usr/local/bin):
#
this_script_dir="$(dirname $(realpath $0))"

local_us_main_install_root="${this_script_dir}/../.."

if [ -d "${local_us_main_install_root}/priv" ]; then

	us_main_install_root="$(realpath ${local_us_main_install_root})"
	echo "Selecting US-Main development native build in '${us_main_install_root}'."

else

	echo "  Error, no valid US-Main native build found, the '$(realpath ${local_us_main_install_root})' location was expected." 1>&2

	exit 15

fi


# We need first to locate the us-main-common.sh script:

# Location expected also by us-common.sh afterwards:
cd "${us_main_install_root}" || exit


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



echo "Killing brutally (not stopping gracefully) any US-Main instance(s) found."

to_kill="$(ps -edf | grep us_main | grep -v run_erl | grep -v $0 | grep -v grep | grep -v emacs | awk '{ print $2 }')"


if [ -n "${to_kill}" ]; then

	echo "Following US-Main processes to kill found, as $(id -un): ${to_kill}."

	# The signal 9 *is* necessary in some cases:
	if ! kill -9 ${to_kill}; then  # 2>/dev/null

		echo "  Error: failed to kill processes of PIDs ${to_kill}." 1>&2

		exit 40

	fi

	# Actually not specifically safer:
	#for p in ${to_kill}; do
	#
	#   echo " - killing process $p"
	#   kill -9 $p
	#
	#done

else

	echo "(no US-Main process to kill found)"

fi



if [ -n "${erl_epmd_port}" ]; then

	echo "Using user-defined US-Main EPMD port ${erl_epmd_port}."
	export ERL_EPMD_PORT="${erl_epmd_port}"

else

	# Using the default US-Main EPMD port (see the EPMD_PORT make variable),
	# supposing that the instance was properly launched (see the 'launch-epmd'
	# make target):

	echo "Using default US-Main EPMD port ${default_us_main_epmd_port}."

	export ERL_EPMD_PORT="${default_us_main_epmd_port}"

fi


# Not always working:
if ! ${epmd} -stop us_main; then

	echo "  Error while unregistering the US-Main server from the EPMD daemon at port ${ERL_EPMD_PORT}." 1>&2

	exit 5

fi


sleep 1


echo "Resulting US-Main processes found: $(ps -edf | grep us_main | grep -v grep)"
echo "Resulting EPMD entries found: $(${epmd} -names)"
