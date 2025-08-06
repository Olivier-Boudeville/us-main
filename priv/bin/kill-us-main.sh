#!/bin/sh

# A script to kill for sure a local US-Main instance (and, hopefully, only such
# an instance).
#
# See also the {start,stop,control,monitor}-us-main.sh and get-us-main-status.sh
# scripts.


usage="Usage: $(basename $0) [US_CONF_DIR]: kills any running US-Main instance(s), and ensures that none is registered in the specifically-associated EPMD.

Determines which US-Main instance is to kill based on any specified US configuration directory, otherwise on the standard locations of US configuration files.

Examples of use:
 $ kill-us-main.sh
 $ sudo kill-us-main.sh $HOME/.config/universal-server
"


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"
	exit 0

fi

if [ $# -gt 1 ]; then

	echo "  Error, no extra argument expected.
${usage}" 1>&2

	exit 10

fi

# If an argument is set, it will be interpreted as US_CONF_DIR by
# read_us_config_file.



epmd="$(which epmd 2>/dev/null)"

if [ ! -x "${epmd}" ]; then

	echo "  Error, no EPMD executable found." 1>&2

	exit 8

fi


# Of course using stop-us-main-{native-build,release}.sh shall be preferred, as
# here we kill (not terminate properly) any US-Main instance(s).
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
# do the same as, for example, start-us-main-native-build.sh.

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
. "${us_main_common_script}" #1>/dev/null

# We expect a pre-installed US configuration file to exist:
read_us_config_file "$1" 1>/dev/null

read_us_main_config_file 1>/dev/null



echo "Killing any US-Main instance(s) found."

epmd_port_regexp='.*'

# As may not be defined:
if [ -n "${us_main_epmd_port}" ]; then

	epmd_port_regexp="${us_main_epmd_port}"

fi

# Targeting as precisely as possible the instance:
# (superfluous now: '| grep -v emacs')
# (not wanting to catch for example us_main_monitor_app)
to_kill="$(ps -edf | grep beam.smp | grep us_main_app | grep "${epmd_port_regexp}" | grep -v run_erl | grep -v $0 | grep -v grep | awk '{ print $2 }')"

#to_kill_processes="$(ps -edf | grep beam.smp | grep us_main_app | grep "${epmd_port_regexp}" | grep -v run_erl | grep -v $0 | grep -v grep)"

#echo "Will be killed: ${to_kill_processes}"


if [ -n "${to_kill}" ]; then

	echo "Following US-Main processes to kill (gracefully) found, as $(id -un): ${to_kill}."

	# The signal 9 *is* necessary in some cases (possibly always); however most
	# probably that a brutal kill (unlike a normal one) prevents the killed VM
	# to notify its EPMD daemon, which will then prevent any next launching
	# thereof - unless being killed in turn). Now attempting a normal kill first
	# (the current user may or may not be able to kill these processes):

	if ! kill ${to_kill}; then  # 2>/dev/null

		echo "  Error: failed to kill gracefully processes of PIDs ${to_kill}." 1>&2

		exit 41

	fi

	# Inspecting again:
	to_kill="$(ps -edf | grep beam.smp | grep us_main_app | grep "${epmd_port_regexp}" | grep -v run_erl | grep -v $0 | grep -v grep | awk '{ print $2 }')"

	if [ -n "${to_kill}" ]; then

		echo "  Error: having to kill brutally processes of PIDs ${to_kill} (their EPMD daemon will thus not be notified of their termination)." 1>&2

		if ! kill -9 ${to_kill}; then  # 2>/dev/null

			echo "  Error: failed to brutally kill processes of PIDs ${to_kill}." 1>&2

			exit 45

		fi

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


# Previously any (possibly the default) US-level EPMD port applied here, now the
# US-Main one applies unconditionally:

#if [ -n "${erl_epmd_port}" ]; then
#
#   echo "Using user-defined US-Main EPMD port ${erl_epmd_port}."
#   export ERL_EPMD_PORT="${erl_epmd_port}"
#
#else

    # Using the default US-Main EPMD port (see the EPMD_PORT make variable),
    # supposing that the instance was properly launched (see the 'launch-epmd'
    # make target):

#   echo "Using default US-Main EPMD port ${default_us_main_epmd_port}."

#   export ERL_EPMD_PORT="${default_us_main_epmd_port}"

#fi

# Already resolved by us-main-common.sh:
echo "Using, for US-Main EPMD port, ${us_main_epmd_port}."
export ERL_EPMD_PORT="${us_main_epmd_port}"

# Not always working:
if ! ${epmd} -stop us_main 1>/dev/null; then

	echo "  Error while unregistering the US-Main server from the EPMD daemon at port ${ERL_EPMD_PORT}." 1>&2

	exit 5

fi

sleep 1

# At least this script:
echo "Resulting US-Main processes found, afterwards: $(ps -edf | grep us_main | grep -v grep)"
echo "Resulting EPMD entries found: $(${epmd} -names)"
