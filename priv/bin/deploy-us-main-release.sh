#!/bin/sh

usage="Usage: $(basename $0): deploys (installs and runs) locally a US-Main release."

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


# Just to avoid error messages if running from a non-existing directory:
cd /


if [ ! $(id -u ) -eq 0 ]; then

	echo "  Error, this script must be run as root." 1>&2
	exit 5

fi

archive_dir="/tmp"

# As listed in alphabetical order, selecting the last line shall yield the
# highest (hence more recent) version:
#
rel_archive=$(/bin/ls -1 ${archive_dir}/us_main-*.tar.gz | tail -1 2>/dev/null)

if [ -z "${rel_archive}" ]; then

	echo "  Error, no US-Main release archive found in '${archive_dir}'." 1>&2
	exit 10

fi


archive_version=$(echo ${rel_archive} |sed "s|^${archive_dir}/us_main-||1" | sed 's|.tar.gz$||1')

echo " Detected US-Main version: '${archive_version}'."

archive_name="us_main-${archive_version}.tar.gz"

archive_path="${archive_dir}/${archive_name}"


if [ ! -f "${archive_path}" ]; then

	echo "  Error, US-Main release archive '${archive_path}' is not a file." 1>&2
	exit 15

fi


base_rel_dir="/opt/universal-server"

base_rel_dir_created=1

if [ ! -d "${base_rel_dir}" ]; then

	echo " Creating base directory '{base_rel_dir}'."
	/bin/mkdir "${base_rel_dir}"
	base_rel_dir_created=0

fi

cd "${base_rel_dir}"

for d in $(/bin/ls -d us_main-*.*.* 2>/dev/null) ; do

	exec="${d}/bin/us_main"

	#echo "Testing for ${exec}..."

	if [ -x "${exec}" ]; then
		echo " Trying to stop gracefully any prior release in ${d}."
		${exec} stop 1>/dev/null 2>&1
	fi

done


rel_dir="us_main-${archive_version}"

# Not wanting to inherit from remaining elements:
if [ -d "${rel_dir}" ]; then

	echo " Removing already-existing ${rel_dir}."
	/bin/rm -rf ${rel_dir}

fi

mkdir "${rel_dir}"

# Rare option needed, otherwise apparently mistook for a directory resulting in
# an incorrect link:
#
/bin/ln -sf --no-target-directory "${rel_dir}" us_main-latest

cd "${rel_dir}"

# Now we let the archive at its original place:
#/bin/mv -f "${archive_path}" .

if ! tar xzf "${archive_path}"; then

	echo " Error, the archive '${archive_name}' could not be decompressed in '$(pwd)'." 1>&2

	exit 20

fi

rel_root="${base_rel_dir}/${rel_dir}"

cd "${rel_root}/lib"

/bin/ln -sf "${rel_dir}" us_main-latest

# To facilitate finding vm.args (ex: for a proper stop w.r.t. cookie):
cd "${rel_root}/releases"

/bin/ln -sf "${archive_version}" latest-release


# Just a check:
rel_exec="${rel_root}/bin/us_main"

if [ -x "${rel_exec}" ]; then

	echo " US-Main release ready in '${rel_exec}'."

else

	echo "Error, no release executable found (no '${rel_exec}' found)." 1>&2
	exit 25

fi


priv_dir="${rel_root}/lib/${rel_dir}/priv"

start_script="${priv_dir}/bin/start-us-main.sh"

if [ ! -x "${start_script}" ]; then

	echo "Error, no start script found (no '${start_script}' found)." 1>&2
	exit 30

fi

# Not wanting the files of that US-Main install to remain owned by root, so
# trying to apply a more proper user/group; for that we have to determine them
# from the configuration files:
#
# (will source in turn us-common.sh)
us_main_common_script="${priv_dir}/bin/us-main-common.sh"

if [ ! -f "${us_main_common_script}" ]; then

	echo "Error, unable to find us-main-common.sh script (not found in '${us_main_common_script}')." 1>&2
	exit 35

fi


# We are in ${rel_root} now, so that the us-common.sh script (and thus the
# us_common base first) can be found with relative links from us-main-common.sh:
#
# (so we will be typically in /opt/universal-server/us_main-x.y.z from now on)
cd "${rel_root}"

#echo "Sourcing '${us_main_common_script}'."
. "${us_main_common_script}" 1>/dev/null


read_us_config_file 1>/dev/null

read_us_main_config_file 1>/dev/null

echo " Changing, from '${rel_root}', the owner of release files to '${us_main_username}' and their group to '${us_groupname}'."

if ! chown --recursive ${us_main_username}:${us_groupname} ${rel_root}; then

	echo "Error, changing user/group owner from '${rel_root}' failed." 1>&2

	exit 40

fi


if [ ${base_rel_dir_created} -eq 0 ]; then

	if ! chown ${us_main_username}:${us_groupname} ${base_rel_dir}; then

		echo "Error, changing user/group owner of '${base_rel_dir}' failed." 1>&2

		exit 45

	fi

fi

# By default, no auto-launch:
do_launch=1

if [ $do_launch -eq 0 ]; then

	echo "### Launching US-Main release now"

	${start_script}

	sleep 1

	${rel_root}/bin/us_main status

	log_dir="${rel_root}/log"

	# See https://erlang.org/doc/embedded/embedded_solaris.html to understand
	# the naming logic of erlang.log.* files.
	#
	# The goal here is only to select the latest-produced of these rotated log files:
	#
	us_main_vm_log_file=$(/bin/ls -t ${log_dir}/erlang.log.* 2>/dev/null | head -n 1)

	if [ -f "${us_main_vm_log_file}" ]; then

		tail -f "${us_main_vm_log_file}"

	else

		echo "(no VM log file found, tried '${us_main_vm_log_file}')"

	fi

else

	echo "(no auto-launch enabled; one may execute, as root, 'systemctl restart us-main.service')"

fi
