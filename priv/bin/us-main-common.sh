# Common script facilities relating to the US-Main server.
#
# Allows to avoid code duplication. Meant to be sourced, not directly executed.
#
# Used for example by start-us-main.sh and stop-us-main.sh.


# Determining us_common_root:

us_main_script_root=$(dirname $0)
#echo "US-Main script root: ${us_main_script_root}"

us_common_root_in_checkouts="${us_main_script_root}/../../_checkouts/us_common"
us_common_root_in_build="${us_main_script_root}/../../_build/default/lib/us_common"

# To be evaluated from rel_root=/opt/universal-server/us_main-x.y.z (see
# deploy-us-main-release.sh):
#
us_common_in_deployed_release=$(/bin/ls -d -t lib/us_common-* 2>/dev/null | head -n 1)


if [ -d "${us_common_root_in_checkouts}" ]; then

	us_common_root="${us_common_root_in_checkouts}"

else

	if [ -d "${us_common_root_in_build}" ]; then

		us_common_root="${us_common_root_in_build}"

	else

		if [ -d "${us_common_in_deployed_release}" ]; then

			us_common_root="${us_common_in_deployed_release}"

		else

			echo "  Error, no US-Common root found: searched in '${us_common_root_in_checkouts}', '${us_common_root_in_build}' and '${us_common_in_deployed_release}' (from $(pwd))." 1>&2
			exit 95

		fi

	fi

fi

# As depends on it:
us_common_script="${us_common_root}/priv/bin/us-common.sh"

if [ -f "${us_common_script}" ]; then

	. "${us_common_script}" 1>/dev/null

else

	echo "  Error, no US-Common script (${us_common_script}) found." 1>&2
	exit 100

fi



# Sets notably: us_main_config_file, us_main_username, us_main_app_base_dir,
# us_main_log_dir, us_main_rel_dir, us_main_exec.
#
# read_us_config_file must have been run beforehand.
#
read_us_main_config_file()
{

	us_main_config_filename=$(echo "${us_base_content}" | grep us_main_config_filename | sed 's|^[[:space:]]*{[[:space:]]*us_main_config_filename,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	us_main_default_config_filename="us-main.config"

	if [ -z "${us_main_config_filename}" ]; then

		us_main_config_filename="${us_main_default_config_filename}"

		echo "No US-Main configuration filename specified, using default one, '${us_main_config_filename}'."

	else

		echo "Using specified US-Main configuration filename, '${us_main_config_filename}'."

	fi

	us_main_config_file="${base_path}/${app_dir}/${us_main_config_filename}"

	echo "Looking up '${us_main_config_file}'..."

	if [ ! -f "${us_main_config_file}" ]; then

		echo "  Error, US-Main configuration filename '${us_main_config_filename}' not found." 1>&2
		exit 110

	fi

	echo "Using US-Main configuration file '${us_main_config_file}'."


	# US-Main configuration data content, read once for all, with comments (%)
	# removed:
	#
	us_main_base_content=$(/bin/cat "${us_main_config_file}" | sed 's|^[[:space:]]*%.*||1')

	us_main_username=$(echo "${us_main_base_content}" | grep us_main_username | sed 's|^{[[:space:]]*us_main_username,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_main_username}" ]; then

		us_main_username="${USER}"

		if [ -z "${us_main_username}" ]; then

			echo "  Error, no USER environment variable set, whereas not username specified in configuration file." 1>&2
			exit 120

		fi

		echo "No main username specified, using current one, '${us_main_username}'."

	else

		echo "Using specified main username, '${us_main_username}'."

	fi

	#echo "us_main_username = $us_main_username"


	us_main_app_base_dir=$(echo "${us_main_base_content}" | grep us_main_app_base_dir | sed 's|^{[[:space:]]*us_main_app_base_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_main_app_base_dir}" ]; then

		# Environment variable as last-resort:
		if [ -z "${US_MAIN_APP_BASE_DIR}" ]; then

			# Wild guess:
			us_main_app_base_dir=$(/bin/ls -d ${us_app_base_dir}/../../*/us_main 2>/dev/null | xargs realpath)

			echo "No base directory specified for the US-Main application nor US_MAIN_APP_BASE_DIR environment variable set, deriving it from the US application one: trying '${us_main_app_base_dir}'."

		else

			us_main_app_base_dir="${US_MAIN_APP_BASE_DIR}"
			echo "No base directory specified for the US-Main application, using the value of the US_MAIN_APP_BASE_DIR environment variable, trying '${us_main_app_base_dir}'."

		fi


	else

		echo "Using the specified base directory for the US-Main application, '${us_main_app_base_dir}'."

	fi

	if [ ! -d "${us_main_app_base_dir}" ]; then

		echo "  Error, the base directory determined for the US-Main application, '${us_main_app_base_dir}', is not an existing directory." 1>&2
		exit 130

	fi


	# VM-level logs (not based on us_main_log_dir):
	#
	# (note that us_main_vm_log_dir is for US-Main what us_log_dir is for
	# US-Common)
	#
	# (typically here in production mode, as a standard release)
	#
	us_main_vm_log_dir="${us_main_app_base_dir}/log"

	if [ ! -d "${us_main_vm_log_dir}" ]; then

		saved_log_dir="${us_main_vm_log_dir}"

		# Maybe in development mode then (i.e. as a rebar3 build tree):
		us_main_vm_log_dir="${us_main_app_base_dir}/_build/default/rel/us_main/log"

		if [ ! -d "${us_main_vm_log_dir}" ]; then

			# Not an error per se, may happen for example when running a new
			# release:

			#echo "  Error, no US-Main VM log directory found: neither '${saved_log_dir}' (as a standard release) nor '${us_main_vm_log_dir}' (as a rebar3 build tree). Possibly a release not even started?" 1>&2
			#exit 140

			echo "No US-Main VM log directory found, neither '${saved_log_dir}' (as a standard release) nor '${us_main_vm_log_dir}' (as a rebar3 build tree)."

		else

			echo "Rebar3 build tree detected, US-Main VM log directory found as '${us_main_vm_log_dir}'."
		fi

	else

		echo "Standard OTP release detected, US-Main VM log directory found as '${us_main_vm_log_dir}'."

	fi


	# Supposing first the path of a real release having been specified; for
	# example: "/opt/universal-server/us-main-x.y.z":
	#
	us_main_rel_dir="${us_main_app_base_dir}"

	us_main_exec="${us_main_app_base_dir}/bin/us_main"

	if [ ! -x "${us_main_exec}" ]; then

		saved_exec="${us_main_exec}"

		# Maybe then in a rebar3 build tree:
		us_main_rel_dir="${us_main_app_base_dir}/_build/default/rel/us_main"

		us_main_exec="${us_main_rel_dir}/bin/us_main"

		if [ ! -x "${us_main_exec}" ]; then

			echo "  Error, the specified US-Main application base directory ('${us_main_app_base_dir}') does not seem to be a legit one: no '${saved_exec}' (not a standard release) nor '${us_main_exec}' (not a rebar3 build tree)." 1>&2
			exit 150

		else

			echo "Rebar3 build tree detected, US-Main application found as '${us_main_exec}'."
		fi

	else

		echo "Standard OTP release detected, US-Main application found as '${us_main_exec}'."

	fi


	us_main_log_dir=$(echo "${us_main_base_content}" | grep us_main_log_dir | sed 's|^{[[:space:]]*us_main_log_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_main_log_dir}" ]; then

		us_main_log_dir="/var/log"
		echo "No base directory specified for main logs (no 'us_main_log_dir' entry in the US-Main configuration file '${us_main_config_file}'), trying default log directory '${us_main_log_dir}'."

	else

		# Checks whether absolute:
		if [[ "${us_main_log_dir:0:1}" == / || "${us_main_log_dir:0:2}" == ~[/a-z] ]]; then

			echo "Using directly specified directory for main logs, '${us_main_log_dir}'."

		else

			# If it is relative, it is relative to the US-Main application base
			# directory:
			#
			us_main_log_dir="${us_main_app_base_dir}/${us_main_log_dir}"
			echo "Using specified directory for main logs (made absolute), '${us_main_log_dir}'."

		fi

	fi

	if [ ! -d "${us_main_log_dir}" ]; then

		echo "  Error, no US-Main log directory (for main-level logs) found ('${us_main_log_dir}')." 1>&2
		exit 160

	fi

	echo "US-Main (main-level) logs expected in the '${us_main_log_dir}' directory."

}



# Updates the relevant US-Main vm.args release file with any user-defined Erlang
# cookie the VM switched to.
#
# The relevant US configuration file must have been run beforehand (see
# read_us_config_file).
#
# See also: the reciprocal restore_us_main_config_cookie function.
#
update_us_main_config_cookie()
{

	if [ -n "${vm_cookie}" ]; then

		# Let's auto-generate on the fly a vm.args with the right runtime cookie
		# (as it was changed at startup):

		base_rel_cfg_dir="${us_main_app_base_dir}/releases/latest-release"

		if [ ! -d "${base_rel_cfg_dir}" ]; then

			echo "  Error, the base configuration directory for the release, '${base_rel_cfg_dir}' (obtained from '${us_main_app_base_dir}'), could not be found." 1>&2

			exit 170

		fi

		vm_args_file="${base_rel_cfg_dir}/vm.args"

		if [ ! -f "${vm_args_file}" ]; then

			echo " Error, the release vm.args file could not be found (searched for '${vm_args_file}')." 1>&2

			exit 180

		fi

		# The original VM args (typically including a safe, dummy cookie):
		original_vm_args_file="${base_rel_cfg_dir}/.vm.args.original"

		# Do not overwrite original information (ex: if update was run twice
		# with no restore in-between):
		#
		if [ ! -f "${original_vm_args_file}" ]; then

			/bin/mv -f "${vm_args_file}" "${original_vm_args_file}"

		else

			/bin/rm -f "${vm_args_file}"

		fi

		# So in all cases, here original_vm_args_file exists, and vm_args_file
		# not.

		# Avoid reading and writing in the same file:
		/bin/cat "${original_vm_args_file}" | /bin/sed "s|-setcookie.*|-setcookie ${vm_cookie}|1" > "${vm_args_file}"

		#/bin/cp -f "${vm_args_file}" "${base_rel_cfg_dir}/vm.args-for-inspection.txt"

		#echo "Content of newer vm.args:" 1>&2
		#/bin/cat "${vm_args_file}" 1>&2

		# Leaving as is original_vm_args_file, for any future use.

		echo "US-Main vm.args updated with cookie ${vm_cookie}."

		# So both files exist.

	else

		echo "(no cookie defined, no vm.args updated)"

	fi

}



# Restores the original VM args file, after it has been updated, to avoid
# leaking the actual runtime cookie in any future command-line.
#
# (reciprocal function of update_us_main_config_cookie)
#
restore_us_main_config_cookie()
{

	# Depends on whether a specific cookie had been defined:

	if [ -n "${vm_cookie}" ]; then

		if [ -z "${original_vm_args_file}" ]; then

			# No prior update_us_main_config_cookie call?
			echo "  Error, filename of original VM args not set (abnormal)." 1>&2

			exit 190

		else

			if [ -f "${original_vm_args_file}" ]; then

				/bin/mv -f "${original_vm_args_file}" "${vm_args_file}"

			fi

		fi

	fi

}



# Prepares a proper launch of US-Main.
#
# read_us_main_config_file must have been run beforehand.
#
prepare_us_main_launch()
{

	# Not wanting to look at older, potentially misleading logs:

	echo "Removing any logs in '${us_main_vm_log_dir}'."

	# Ensuring that directory exists:
	mkdir -p ${us_main_vm_log_dir}

	/bin/rm -f ${us_main_vm_log_dir}/erlang.log.* ${us_main_vm_log_dir}/run_erl.log 2>/dev/null

	# Needed as a sign that any future start succeeded:
	trace_file="${us_log_dir}/us_main.traces"
	echo "Removing any '${trace_file}' trace file."
	/bin/rm -f "${trace_file}" 2>/dev/null

	echo "Fixing permissions."

	# So that the VM can write its logs despite authbind:
	chown ${us_main_username}:${us_groupname} ${us_main_vm_log_dir}

}


# Inspects the VM logs of US-Main (beware of ancient entries being displayed).
#
# read_us_main_config_file must have been run beforehand.
#
inspect_us_main_log()
{

	# (run_erl.log not that useful)

	# See https://erlang.org/doc/embedded/embedded_solaris.html to understand
	# the naming logic of erlang.log.* files.
	#
	# The goal here is only to select the latest-produced of these rotated log files:
	#
	us_main_vm_log_file=$(/bin/ls -t ${us_main_vm_log_dir}/erlang.log.* 2>/dev/null | head -n 1)

	# A common problem is: "Protocol 'inet_tcp': the name xxx@yyy seems to be in
	# use by another Erlang node". This may not even be true (no VM running),
	# just a lingering EPMD believing this node still exists.


	# Waits a bit if necessary while any writing takes place:
	if [ ! -f "${us_main_vm_log_file}" ]; then
		sleep 1
	fi

	echo
	if [ -f "${us_main_vm_log_file}" ]; then

		echo "EPMD names output:"
		epmd -port ${erl_epmd_port} -names

		echo
		echo "Displaying the end of '${us_main_vm_log_file}':"

		# Still a bit of waiting, otherwise any error may not have been reported
		# yet:
		#
		sleep 1

		# A sufficient height is *necessary*:
		tail --lines=80 "${us_main_vm_log_file}"

	else

		echo "  Error, no US-Main VM log file found (no '${us_main_vm_log_file}', searched in '${us_main_vm_log_dir}')." 1>&2
		exit 200

	fi

}
