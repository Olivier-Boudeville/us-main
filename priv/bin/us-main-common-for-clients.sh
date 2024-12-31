# Common script facilities dedicated to the client scripts
# (e.g. {monitor,control, etc.}-us-main.sh) meant to apply to a potentially
# remote US-Main server.
#
# Allows to avoid code duplication. Meant to be sourced, not directly executed.
#
# Used for example by monitor-us-main.sh.

#echo "Sourcing common US-Main helper script."


# Returns in located_um_cfg_file the absolute path to the US-Main configuration
# file found (if any).
#
# $1: candidate file
find_us_main_config_file()
{

	um_cfg_filename="$1"

	located_um_cfg_file="$(realpath ${um_cfg_filename})"

	if [ ! -f "${located_um_cfg_file}" ]; then

		#echo "(not found: '${located_um_cfg_file}')"

		# Not directly found. It is a missing file if its path is absolute,
		# otherwise it designates a file to be found in one of the possible US
		# configuration directories, which therefore must be found.

		# Checks whether absolute:
		case "${um_cfg_filename}" in

			/*) echo "  Error, the specified US-Main remote access configuration file, '${um_cfg_filename}', is an absolute path and does not exist." 1>&2
				exit 22
				;;

			*) :
				;;

		esac

		# So searching, from the um_cfg_filename relative path, the
		# configuration file through the US standard paths:
		#
		# (like, in us-common.sh, 'read_us_main_config_file()')

		if [ -n "${XDG_CONFIG_HOME}" ]; then

			base_path="${XDG_CONFIG_HOME}"

		else

			# May resolve to /root, if run through sudo:
			base_path="${HOME}/.config"

		fi

		app_dir="universal-server"

		us_config_dir="${base_path}/${app_dir}"

		# This is still a remote access configuration file:
		located_um_cfg_file="${us_config_dir}/${um_cfg_filename}"

		#echo "Looking up first '${located_um_cfg_file}'..."

		if [ ! -f "${located_um_cfg_file}" ]; then

			#echo "('${located_um_cfg_file}' not found)"

			if [ -n "${XDG_CONFIG_DIRS}" ]; then

				# Pops the first element of that list:
				#
				# (note: currently the next - if any - directories in that list
				# are not tested; not implemented yet)
				#
				base_path=$(echo "${XDG_CONFIG_DIRS}" | sed 's|:.*$||1')

			else

				base_path="/etc/xdg"

			fi

			us_config_dir="${base_path}/${app_dir}"
			located_um_cfg_file="${us_config_dir}/${um_cfg_filename}"

			#echo "Looking up second '${located_um_cfg_file}'..."

			if [ ! -f "${located_um_cfg_file}" ]; then

				echo "  Error, unable to locate the '${um_cfg_filename}' configuration file through the US standard configuration paths." 1>&2

				exit 25

			fi

		fi

	fi

}




# Reads the specified US-Main configuration file and returns in:
#  - um_cfg_base_content: the full content of that file
#  - um_erl_epmd_port: the EPMD port to be used
#  - epmd_opt: the corresponding EPMD option
#  - remote_vm_cookie: the corresponding Erlang cookie
#
# $1: a path to the target US-Main configuration file
#
read_us_main_config_file()
{

	um_cfg_file="$1"

	if [ ! -f "${um_cfg_file}" ]; then

		echo "  Error, the specified US-Main configuration file, '${um_cfg_file}', does not exist." 1>&2

		exit 30

	fi


	# US-Main configuration content, read once for all, with comments (%) removed:
	um_cfg_base_content=$(/bin/cat "${located_um_cfg_file}" | sed 's|^[[:space:]]*%.*||1')


	# Locating and just sourcing us-main-common.sh would have allowed to reuse
	# defaults - but we rely on a dedicated file to target a specific US-Main
	# instance.


	# Needed for reaching the target VM:
	# (beware to any firewall being in the way)

	# The EPMD port may be overridden here in US-Main, so that it does not clash
	# with the one of any other US-* application (e.g. US-Web).

	um_erl_epmd_port=$(echo "${um_cfg_base_content}" | grep epmd_port | sed 's|^[[:space:]]*{[[:space:]]*epmd_port,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')

	if [ -n "${um_erl_epmd_port}" ]; then
		echo "Using the specified EPMD port, ${um_erl_epmd_port}."
	else
		um_erl_epmd_port=4507
		echo "Using the default US-Main EPMD port, ${um_erl_epmd_port}."
	fi

	epmd_opt="EPMD_PORT=${um_erl_epmd_port}"


	# Finally useless, as ping below now disabled:
	#us_main_hostname=$(echo "${um_cfg_base_content}" | grep us_main_hostname | sed 's|^[[:space:]]*{[[:space:]]*us_main_hostname,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	#if [ -z "${us_main_hostname}" ]; then

	#	echo "  Error, no remote US-Main hostname specified (no us_main_hostname defined)." 1>&2
	#	exit 10

	#fi


	# Finally disabled as well, as a host that does not answer to ping would
	# trigger a too long time-out:
	#
	#if ! ping -c 1 ${us_main_hostname} 1>/dev/null 2>&1; then

		# Not a fatal error, as not all servers answer pings:
		#echo "  Error, unable to ping the '${us_main_hostname}' remote US-Main hostname." 1>&2
		#exit 15

		#echo "  Warning: unable to ping the '${us_main_hostname}' remote US-Main hostname." 1>&2

	#fi

	#echo "Using '${us_main_hostname}' as remote US-Main hostname."


	# Could have been done in the Erlang part:
	remote_vm_cookie="$(echo "${um_cfg_base_content}" | grep remote_vm_cookie | sed 's|^[[:space:]]*{[[:space:]]remote_vm_cookie,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')"

	if [ -z "${remote_vm_cookie}" ]; then

		if [ -z "${vm_cookie}" ]; then

			echo " Error, no cookie defined for the remote US-Main host (remote_vm_cookie) nor for the base cookie (vm_cookie)." 1>&2

		else

			echo "No cookie defined for the remote US-Main host, using the base one (defined in us.config's vm_cookie): ${vm_cookie}."
			remote_vm_cookie="${vm_cookie}"

		fi

	else

		if [ ! "$(echo "${remote_vm_cookie}" | wc -w)" = "1" ]; then

			echo " Error, invalid remote VM cookie obtained ('${remote_vm_cookie}'). Multiple 'remote_vm_cookie' configuration keys defined?" 1>&2

			exit 57

		fi

		echo "Using cookie defined for the remote US-Main host (remote_vm_cookie): ${remote_vm_cookie}."

	fi

}
