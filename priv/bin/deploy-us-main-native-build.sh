#!/bin/sh

# Copyright (C) 2020-2025 Olivier Boudeville

# A script to automatically deploy a US-Main native build from scratch (provided
# that Erlang is already available).
#
# This is the most common procedure, especially if using the '--no-launch'
# command-line option.


# See also:
# - the get-us-main-from-sources.sh script to install a test version of US-Main
# - the deploy-us-main-release.sh script to deploy OTP releases of US-Main
#  instead
#
# (we prefer and better support the current, native mode of operation enforced
# by this script, though)

# Note: this file has been directly adapted from its US-Web counterpart
# (deploy-us-web-release.sh); they shall remain in sync as much as possible.

# (standalone script)


# Note: unless specified, relying on the master branch for all clones.

# Deactivations useful for testing:

# Tells whether repositories shall be cloned (if not done already):
do_clone=0
#do_clone=1

# Tells whether dependencies shall be built (if not done already):
do_build=0
#do_build=1

do_launch=0
no_launch_opt="--no-launch"

checkout_opt="--checkout"

root_exec_allowed=1
allow_root_exec_opt="--allow-root-exec"

# To avoid typos:
checkout_dir="_checkouts"
priv_dir="priv"

base_us_dir="/opt/universal-server"


# To be able to coexist with OTP releases (named as us_main-${archive_version});
# relative to base_us_dir, and more convenient if timestamped (so that multiple
# versions can easily coexist):
#
native_install_dir="us_main-native-deployment-$(date '+%Y%m%d')"

usage="
Usage: $(basename $0) [-h|--help] [${no_launch_opt}] [${allow_root_exec_opt}] [BASE_US_DIR]: deploys (clones and builds) locally, as a normal user (sudo requested only whenever necessary), a fully functional US-Main environment natively (i.e. from its sources, not as an integrated OTP release) in the specified base directory (otherwise in the default '${base_us_dir}' directory), as '${native_install_dir}', then launches it (unless requested not to, with the '${no_launch_opt}' option).

The '${allow_root_exec_opt}' option allows this script to run as root (mostly useful for continuous integration).

Creates a full installation where most dependencies are sibling directories of US-Main, symlinked in checkout directories, so that code-level upgrades are easier to perform than in an OTP/rebar3 context.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)
 - the 'sensors' package

If any Enocean USB dongle is to be used, a corresponding TTY entry shall have been created (refer to the documentation of Ceylan-Oceanic for further guidance) prior to running US-Main.

If not using the '${no_launch_opt}' option, please ensure that no prior US-Main instance is running (otherwise the start of the new one will fail). Consider running, as root, our 'kill-us-main.sh' script for that.
"

# Commented: - rebar3 (see http://myriad.esperide.org/#getting-rebar3), for dependencies that are not ours


# Will thus install the following US-Main prerequisites:
# - Myriad, WOOPER and Traces
# - [optional] Mobile (then Seaplus as well)
# - [optional] Oceanic (then our erlang-serial as well and a relevant TTY)
# - US-Common

our_github_base="https://github.com/Olivier-Boudeville"


# Note that this mode of obtaining US-Main does not rely on rebar3 for US-Main
# itself, even if it could be used for some dependencies.
#
# This does not lead to duplications (e.g. Myriad being built once in the
# context of LEEC and also once for the other packages), thanks to _checkouts
# directories containing symlinks whenever appropriate.

token_eaten=0

while [ $token_eaten -eq 0 ]; do

	token_eaten=1

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
		echo "${usage}"
		exit 0
	fi

	if [ "$1" = "${no_launch_opt}" ]; then
		echo "(auto-launch disabled)"
		do_launch=1
		token_eaten=0
	fi

	if [ "$1" = "${allow_root_exec_opt}" ]; then
		echo "(root execution enabled)"
		root_exec_allowed=0
		token_eaten=0
	fi

	if [ $token_eaten -eq 0 ]; then
		shift
	fi

done


# Acts as a default option catchall as well:
if [ -n "$1" ]; then

	base_us_dir="$1"

	# If specified, must exist:
	if [ ! -d "${base_us_dir}" ]; then

		echo "  Error, the specified installation directory, '${base_us_dir}', does not exist (from '$(pwd)').
${usage}" 1>&2
		exit 4

	fi

	# Ensure is absolute:
	case "${base_us_dir}" in

		/*) : ;;
		*) base_us_dir="$(realpath ${base_us_dir})" ;;

	esac

fi

#echo "base_us_dir = ${base_us_dir}"


# Selects the (build-time) execution target for all Ceylan layers:
#
# We prefer now 'production', as we want a US-Main server to be robust (e.g. by
# restarting any failed child):
#
#execution_target="development"
execution_target="production"

ceylan_opts="EXECUTION_TARGET=${execution_target}"



# $0 may already be absolute:
case "$0" in

   /*)
	   log_dir="$(dirname $0)";;

   *)
	   log_dir="$(pwd)";;
esac

log_file="${log_dir}/$(basename $0).log"


echo "Writing in log file '${log_file}'."

if [ -f "${log_file}" ]; then
	/bin/rm -f "${log_file}"
fi


display_and_log()
{

	echo "$*" | tee --append "${log_file}"

}



#display_and_log "do_launch = ${do_launch}"
#display_and_log "base_us_dir = ${base_us_dir}"
#display_and_log "ceylan_opts = ${ceylan_opts}"


# Just to avoid error messages if running from a non-existing directory:
cd /


# Checking first:

if [ $root_exec_allowed -eq 1 ]; then

	if [ "$(id -u)" = "0" ]; then

		echo "  Error, this script must not be run as root (sudo will be requested only when necessary)." 1>&2
		exit 5

	fi

fi


erlc="$(which erlc 2>/dev/null)"

# No version checked:
if [ ! -x "${erlc}" ]; then

	echo "  Error, no Erlang compiler (erlc) found. Consider installing Erlang first, possibly thanks to our dedicated script, ${our_github_base}/Ceylan-Myriad/blob/master/conf/install-erlang.sh." 1>&2

	exit 10

fi


rebar3="$(which rebar3 2>/dev/null)"

# No version checked either:
if [ ! -x "${rebar3}" ]; then

	echo "  Error, rebar3 not found. Consider installing it first, one may refer to http://myriad.esperide.org/#getting-rebar3." 1>&2

	exit 11

fi


make="$(which make 2>/dev/null)"

if [ ! -x "${make}" ]; then

	echo "  Error, no 'make' tool found." 1>&2
	exit 18

fi


display_and_log "Securing sudoer rights for the upcoming operations that require it."
if ! sudo echo; then

	echo "  Error, sudo failed." 1>&2
	exit 19

fi


base_us_dir_created=1

if [ ! -d "${base_us_dir}" ]; then

	display_and_log " Creating US base directory '${base_us_dir}'."
	# Most permissive, will be updated later in the installation:
	sudo /bin/mkdir --mode=777 "${base_us_dir}"
	base_us_dir_created=0

fi


# Typically a release-like '/opt/universal-server/us_main{-native,}-deployment'
# tree, containing all dependencies:
#
abs_native_install_dir="${base_us_dir}/${native_install_dir}"

# The US-Main tree itself:
us_main_dir="${abs_native_install_dir}/us_main"



display_and_log "   Installing US-Main in '${abs_native_install_dir}'..."
display_and_log



if [ $do_clone -eq 0 ]; then

	cd "${base_us_dir}"

	if [ -d "${native_install_dir}" ]; then

		echo "  Error, target installation directory, '${base_us_dir}/${native_install_dir}', already exists (from '$(pwd)'). Remove it first (preferably as root, as US-Main may be set to run as a specific user that would then own some log files in this tree)." 1>&2

		exit 20

	fi

	# Parent already exists by design; ensuring any normal user can write the
	# content to install next:
	#
	(sudo mkdir --mode=777 "${native_install_dir}") && cd "${native_install_dir}"

	clone_opts="--quiet"

	git="$(which git 2>/dev/null)"

	if [ ! -x "${git}" ]; then

		echo "  Error, no 'git' tool found." 1>&2
		exit 18

	fi


	display_and_log "Getting the relevant repositories (as $(id -un)):"


    # First US-Main itself, so that any _checkouts directory can be created
    # afterwards:
	#
	display_and_log " - cloning US-Main"

	if ! ${git} clone ${clone_opts} "${our_github_base}/us-main" us_main; then

		echo " Error, unable to obtain US-Main." 1>&2
		exit 40

	fi

	# A specific branch might be selected:
	us_main_branch="master"
	#us_main_branch="web-app-addition"

	# To avoid "Already on 'master'":
	if [ "${us_main_branch}" != "master" ]; then

		cd us_main && ${git} switch "${us_main_branch}" 1>>"${log_file}" && cd ..
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to switch to US-Main branch '${us_main_branch}'." 1>&2
			exit 41

		fi

	fi

	ln -sf us_main/conf/GNUmakefile-for-native-root GNUmakefile


	display_and_log " - cloning jsx"

	if ! ${git} clone ${clone_opts} https://github.com/talentdeficit/jsx.git; then

		echo " Error, unable to obtain jsx parser." 1>&2
		exit 38

	fi


	display_and_log " - cloning our fork of erlang-serial (for TTY control)"

	if ! ${git} clone ${clone_opts} "${our_github_base}/erlang-serial"; then

		echo " Error, unable to obtain erlang-serial." 1>&2
		exit 34

	fi


	display_and_log " - cloning Ceylan-Myriad"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-Myriad" myriad; then

		echo " Error, unable to obtain Ceylan-Myriad." 1>&2
		exit 20

	fi

	# A specific branch might be selected:
	myriad_branch="master"
	#myriad_branch="xxx"

	# To avoid "Already on 'master'":
	if [ "${myriad_branch}" != "master" ]; then

		cd myriad && ${git} switch "${myriad_branch}" 1>>"${log_file}" && cd ..
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to switch to Ceylan-Myriad branch '${myriad_branch}'." 1>&2
			exit 21

		fi

	fi


	display_and_log " - cloning Ceylan-Oceanic (for Enocean support)"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-Oceanic" oceanic; then

		echo " Error, unable to obtain Ceylan-Oceanic." 1>&2
		exit 33

	fi


	display_and_log " - cloning Ceylan-Seaplus (for Erlang/C integration)"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-Seaplus" seaplus; then

		echo " Error, unable to obtain Ceylan-Seaplus." 1>&2
		exit 32

	fi


	display_and_log " - cloning Ceylan-Mobile (for SMS management)"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-Mobile" mobile; then

		echo " Error, unable to obtain Ceylan-Mobile." 1>&2
		exit 31

	fi


	display_and_log " - cloning Ceylan-WOOPER"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-WOOPER" wooper; then

		echo " Error, unable to obtain Ceylan-WOOPER." 1>&2
		exit 25

	fi


	display_and_log " - cloning Ceylan-Traces"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-Traces" traces; then

		echo " Error, unable to obtain Ceylan-Traces." 1>&2
		exit 30

	fi


	display_and_log " - cloning US-Common"

	if ! ${git} clone ${clone_opts} "${our_github_base}/us-common" us_common; then

		echo " Error, unable to obtain US-Common." 1>&2
		exit 35

	fi

fi



if [ ${do_build} -eq 0 ]; then

	cd "${abs_native_install_dir}"

	display_and_log
	display_and_log "Building these packages as $(id -un), with Erlang $(erl -eval '{ok, V} = file:read_file( filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]) ), io:fwrite(V), halt().' -noshell) and following Ceylan options: ${ceylan_opts}, from '$(pwd)':"

	echo " - building erlang-serial"
	cd erlang-serial && ${make} 1>>"${log_file}" && DESTDIR=. ${make} install 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of our fork of erlang-serial failed." 1>&2
		exit 65
	fi
	cd ..


	# Now building our own standalone version of jsx; rebar3 required.
	#
	# The resulting BEAM files are both in 'ebin' and in
	# '_build/default/lib/jsx/ebin':
	#
	display_and_log " - building jsx"
	cd jsx && ${rebar3} compile 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of jsx failed." 1>&2
		exit 90
	fi

	# Otherwise may not be found by US-Main:
	#ln -s _build/default/lib/jsx/ebin

	cd ..


	# As much as possible, notably for our developments, we prefer relying on
	# our any vanilla good old build system (i.e. not on rebar3).

	display_and_log " - building Ceylan-Myriad"
	cd myriad && ${make} all ${ceylan_opts} 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Myriad failed." 1>&2
		exit 50
	fi
	cd ..

	# Our build; uses Myriad's sibling tree:
	display_and_log " - building Ceylan-WOOPER"
	cd wooper && ${make} ${ceylan_opts} all 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-WOOPER failed." 1>&2
		exit 55
	fi
	cd ..

	# Our build; uses Myriad's and WOOPER's sibling trees:
	display_and_log " - building Ceylan-Traces"
	cd traces && ${make} ${ceylan_opts} all 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Traces failed." 1>&2
		exit 60
	fi
	cd ..



	display_and_log " - building Ceylan-Oceanic"
	cd oceanic && ${make} ${ceylan_opts} all 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Oceanic failed." 1>&2
		exit 70
	fi
	cd ..

	display_and_log " - building Ceylan-Seaplus"
	cd seaplus && ${make} ${ceylan_opts} all 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Seaplus failed." 1>&2
		exit 75
	fi
	cd ..

	display_and_log " - building Ceylan-Mobile"
	cd mobile && ${make} ${ceylan_opts} all 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Mobile failed." 1>&2
		exit 80
	fi
	cd ..


	# US-Common does not introduce third-party dependencies, so going again for
	# our native build, which thus uses Myriad's, WOOPER's and Traces' sibling
	# trees:
	#
	display_and_log " - building US-Common"
	cd us_common && ${make} all ${ceylan_opts} 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of US-Common failed." 1>&2
		exit 85
	fi
	cd ..


	display_and_log " - building US-Main"
	cd us_main && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && ln -s ../../wooper && ln -s ../../traces && ln -s ../../seaplus && ln -s ../../mobile && ln -s ../../us_common && ln -s ../../erlang-serial && ln -s ../../oceanic && ln -s ../../jsx && cd ..

	# Our build; uses Ceylan's sibling trees:
	if ! ${make} all ${ceylan_opts} 1>>"${log_file}"; then
		echo " Error, the build of US-Main failed." 1>&2
		exit 95
	fi

	# Post-install: fixing permissions and all.

	# This will also by useful for any next launch:

	# Not wanting the files of this US-Main install to remain owned by the
	# deploying user, so trying to apply a more proper user/group; for that we
	# have to determine them from the configuration files, and thus to locate,
	# read and use them:
	#
	# (will source in turn us-common.sh)
	us_main_common_script="${priv_dir}/bin/us-main-common.sh"

	if [ ! -f "${us_main_common_script}" ]; then

		echo "Error, unable to find us-main-common.sh script (not found in '${us_main_common_script}')." 1>&2
		exit 35

	fi

	# Hint for the helper scripts:
	us_launch_type="native"
	us_main_install_root="${us_main_dir}"

	display_and_log "Sourcing '${us_main_common_script}' from $(pwd)."
	. "${us_main_common_script}" #1>>"${log_file}"

	read_us_config_file #1>>"${log_file}"

	read_us_main_config_file #1>>"${log_file}"

	# First permissions (chmod), then owner/group (chown):

	dir_perms="770"

	# abs_native_install_dir/* rather than only us_main_dir, as the dependencies
	# shall also have their permissions updated:

	display_and_log " Changing the permissions of deployed roots in '${abs_native_install_dir}' to ${dir_perms}."

	# Not wanting to select non-directories:
	dirs="$(/bin/ls -d ${abs_native_install_dir}/*/)"

	if ! sudo chmod ${dir_perms} ${dirs}; then

		echo "Error, changing permissions of deployed roots in '${abs_native_install_dir}' to ${dir_perms} failed." 1>&2

		exit 40

	fi

	display_and_log " Changing the permissions of deployed root '${abs_native_install_dir}' itself to ${dir_perms}."
	if ! sudo chmod ${dir_perms} "${abs_native_install_dir}"; then

		echo "Error, changing permissions of deployed root '${abs_native_install_dir}' itself to ${dir_perms} failed." 1>&2

		exit 41

	fi

	if [ ${base_us_dir_created} -eq 0 ]; then

		display_and_log " Changing the permissions of base US install directory '${base_us_dir}' to ${dir_perms}."

		if ! sudo chmod ${dir_perms} "${base_us_dir}"; then

			echo "Error, changing permissions of '${base_us_dir}' failed." 1>&2

			exit 42

		fi

	fi


	# Now owner/group; not leaving deployed content as initial user:

	if [ -n "${us_main_username}" ]; then

		chown_spec="${us_main_username}"

		if [ -n "${us_groupname}" ]; then
			chown_spec="${chown_spec}:${us_groupname}"
		fi

		display_and_log " Changing recursively owner/group of all deployed elements as ${chown_spec} from '${abs_native_install_dir}'."
		if ! sudo chown --recursive "${chown_spec}" "${abs_native_install_dir}"; then

			echo "Error, changing recursively user/group owner (as ${chown_spec}) from '${abs_native_install_dir}' failed." 1>&2

			exit 45

		fi

		if [ ${base_us_dir_created} -eq 0 ]; then

			display_and_log " Changing also the owner/group of base US install directory '${base_us_dir}' as '${chown_spec}'."

			# This one is not recursive:
			if ! sudo chown "${chown_spec}" "${base_us_dir}"; then

				echo "Error, changing owner/group owner of '${base_us_dir}' (as '${chown_spec}') failed." 1>&2

				exit 46

			fi

		fi

	fi


	# Final touch for the build:

	cd "${base_us_dir}"

	# Designates this install as the latest one then.
	#
	# Rare option needed, otherwise apparently mistook for a directory resulting
	# in an incorrect link:
	#
	sudo /bin/ln -sf --no-target-directory "${native_install_dir}" us_main-native
	sudo /bin/ln -sf --no-target-directory us_main-native us_main-latest

	display_and_log
	display_and_log "Native US-Main built and ready in ${abs_native_install_dir}."

fi


# Not checking specifically the expected US and US-Main configuration files:
# running US-Main will tell us whether they exist and are legit.

display_and_log


if [ $do_launch -eq 0 ]; then

	# Not expecting here a previous native instance to run.

	# Absolute path; typically in
	# '/opt/universal-server/us_main-native/us_main':
	#
	if [ ! -d "${us_main_dir}" ]; then

		echo "  Error, the target US-Main directory, '${us_main_dir}', does not exist." 1>&2
		exit 75

	fi

	display_and_log "   Running US-Main native application (as '$(id -un)' initially, from '${us_main_dir}'))"

	# Simplest: cd src && ${make} us_main_exec

	cd "${us_main_dir}" || exit 80


	# Actual cookie managed there:
	start_script="${priv_dir}/bin/start-us-main-native-build.sh"

	if [ ! -x "${start_script}" ]; then

		echo "  Error, no start script found (no '${start_script}' found)." 1>&2
		exit 30

	fi


	cd "${base_us_dir}"

	# Automatic shutdown (that was deferred as much as possible) of any prior
	# US-Main release running:
	#
	for d in $(/bin/ls -d us_main-*.*.* 2>/dev/null); do

		exec="${d}/bin/us_main"

		display_and_log "Testing for ${exec}..."

		if [ -x "${exec}" ]; then
			display_and_log " Trying to stop gracefully any prior release in ${d}."
			sudo ${exec} stop 1>>"${log_file}" 2>&1
		fi

	done

	display_and_log "### Launching US-Main native build now, specifying '${us_config_dir}' as US configuration directory"

	cd "${abs_native_install_dir}/us_main" || exit 81

	# The next start script will switch to the US-Main configured user; this
	# requires an US configuration file to be found, and we have to specify here
	# the one that was previously selected, as otherwise running the next start
	# script thanks to sudo may not select the proper configuration file
	# (typically if the intended one is located in the
	# ~/.config/universal-server directory of the launching user):

	# Needing to specify the US configuration *directory* (not file):
	sudo ${start_script} "${us_config_dir}"
	res=$?

	if [ $res -eq 0 ]; then

		display_and_log " US-Main launched (start script reported success)."

	else
		echo "  Error, start script ('${start_script}') failed (code: ${res})." 1>&2

		exit ${res}

	fi

	#sleep 1

	# Maybe use get-us-main-native-build-status.sh in the future.

    display_and_log "Consider running our 'us_main/priv/bin/monitor-us-main.sh' script if wanting more detailed information regarding that launched instance."

else

	display_and_log "(no auto-launch enabled; one may execute, as root, 'systemctl daemon-reload && systemctl restart us-main-as-native-build.service; sleep 5; systemctl status us-main-as-native-build.service')"

    display_and_log "Any prior US-Main instance that would still linger could be removed thanks to our 'kill-us-main.sh' script. Use 'journalctl -eu us-main-as-native-build.service' to consult the corresponding systemd-level logs."

fi


new_log_file="${base_us_dir}/$(basename $0).log"

echo "(moving finally log file '${log_file}' to '${new_log_file}')"
sudo /bin/mv -f "${log_file}" "${new_log_file}"
