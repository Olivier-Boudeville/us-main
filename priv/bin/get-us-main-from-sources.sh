#!/bin/sh

checkout_opt="--checkout"
checkout_dir="_checkouts"

usage="
Usage: $(basename $0) [-h|--help] [${checkout_opt}]: clones and builds a fully functional US-Main environment in the current directory, then configures a test instance thereof, and runs it.
 By default, creates a basic installation where dependencies are sibling directories of US-Main.
 If the '${checkout_opt}' option is specified, dependencies will be cloned in the us-main/${checkout_dir} directory instead, so that they can be modified and updated by developers more easily.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)"

erlc=$(which erlc 2>/dev/null)
github_base="https://github.com/Olivier-Boudeville"

# Defaults:
checkout_mode=1


if [ $# -eq 1 ]; then

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

		echo "${usage}"
		exit 0

	elif [ "$1" = "${checkout_opt}" ]; then

		echo "Switching to checkout mode."
		checkout_mode=0

	else

		echo "Error, invalid option specified ('$1')." 1>&2
		exit 3

	fi

	shift

fi

if [ ! $# -eq 0 ]; then

	echo "$usage"

	exit 5

fi




# Checking first:

# No version checked:
if [ ! -x "${erlc}" ]; then

	echo "  Error, no Erlang compiler (erlc) found. Consider installing Erlang first, possibly thanks to our dedicated script, ${github_base}/Ceylan-Myriad/blob/master/conf/install-erlang.sh" 1>&2

	exit 10

fi


config_dir="${HOME}/.config/universal-server"

us_config_filename="${config_dir}/us.config"

if [ -f "${us_config_filename}" ]; then

	echo "  Error, a prior US configuration file exists, '${us_config_filename}'. Not overwriting it with the test one, please remove it first." 1>&2

	exit 15

fi


us_main_config_filename="${config_dir}/us-main-for-tests.config"

if [ -f "${us_main_config_filename}" ]; then

	echo "  Error, a prior US-Main configuration file exists, '${us_main_config_filename}'. Not overwriting it, please remove it first." 1>&2

	exit 17

fi


base_install_dir="$(pwd)"

us_main_dir="${base_install_dir}/us_main"


echo
echo "   Installing US-Main in ${base_install_dir}..."
echo


# First US-Main itself, so that any _checkouts directory can be created afterwards:
cd "${base_install_dir}"


git clone ${github_base}/us-main us_main #&& cd us_main && make all

res=$?
if [ ! $res -eq 0 ] ; then

	echo " Error, unable to obtain US-Main." 1>&2
	exit 40

fi

echo


# Not building prerequisites from their source tree (mostly needed for their
# GNU* files to be available to generate the US-Main release), just cloning.

if [ $checkout_mode -eq 0 ]; then

	prereq_install_dir="${us_main_dir}/${checkout_dir}"

	if [ ! -d "{prereq_install_dir}" ]; then

		mkdir -p "${prereq_install_dir}"

	fi

else

	prereq_install_dir="${base_install_dir}"

fi


cd ${prereq_install_dir}


git clone ${github_base}/us-common us_common #&& cd us_common && make all

res=$?
if [ ! $res -eq 0 ] ; then

	echo " Error, unable to obtain US-Common." 1>&2
	exit 35

fi

echo
#cd ..


git clone ${github_base}/Ceylan-Traces traces #&& cd traces && make all

res=$?
if [ ! $res -eq 0 ] ; then

	echo " Error, unable to obtain Traces." 1>&2
	exit 30

fi

echo
#cd ..


git clone ${github_base}/Ceylan-WOOPER wooper #&& cd wooper && make all

res=$?
if [ ! $res -eq 0 ] ; then

	echo " Error, unable to obtain WOOPER." 1>&2
	exit 25

fi

echo
#cd ..


git clone ${github_base}/Ceylan-Myriad myriad #&& cd myriad && make all

res=$?
if [ ! $res -eq 0 ] ; then

	echo " Error, unable to obtain Myriad." 1>&2
	exit 20

fi

echo
#cd ..



echo "   Configuring a US-Main test instance"

if [ ! -d "${config_dir}" ] ; then

	mkdir "${config_dir}"

fi

cd "${config_dir}"


us_common_dir="${prereq_install_dir}/us_common"

us_cfg_for_test="${us_common_dir}/priv/for-testing/us.config"

if [ ! -f "${us_cfg_for_test}" ]; then

	echo " Error, '${us_cfg_for_test}' not found." 1>&2
	exit 70

fi

# Already checked that does not exist:
/bin/ln -s "${us_cfg_for_test}"


us_main_cfg_for_test="${us_main_dir}/priv/for-testing/us-main-for-tests.config"

if [ ! -f "${us_main_cfg_for_test}" ]; then

	echo " Error, '${us_main_cfg_for_test}' not found." 1>&2
	exit 75

fi

# Already checked that does not exist:
/bin/ln -s "${us_main_cfg_for_test}"



cd ${us_main_dir}

echo
echo "   Building and launching a test US-Main release"
make debug

res=$?

if [ $res -eq 0 ]; then

	echo " US-Main launched."

else

	echo " Unable to build and launch the US-Main Server (error code: $res)." 1>&2

	exit $res

fi
