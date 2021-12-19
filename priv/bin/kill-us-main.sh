#!/bin/sh

# Of course using stop-us-main-{native-build,release}.sh shall be preferred:
echo " Killing brutally (not stopping gracefully) any US-Main instance found, and EPMD as well..."


to_kill="$(ps -edf | grep us_main | grep -v run_erl | grep -v grep | awk '{ print $2 }')"

if [ -n "${to_kill}" ]; then

	echo "Following US-Main processes to kill found: ${to_kill}."
	# The signal 9 *is* necessary in some cases:
	kill -9 ${to_kill} # 2>/dev/null

else

	echo "(no US-Main process to kill found)"

fi

killall -9 epmd

sleep 1

echo "Resulting US-Main found: $(ps -edf | grep us_main | grep -v grep)"
