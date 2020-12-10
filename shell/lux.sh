#!/usr/bin/env bash

home() {
	# Find out where the script is located.
	# This is because it is assumed that the Aedifex JAR will be in the same directory.
	local SCRIPT_SOURCE="${BASH_SOURCE[0]}"
	while [ -h "$SCRIPT_SOURCE" ]; do
		# resolve $SCRIPT_SOURCE until the file is no longer a symlink
		local SCRIPT_HOME="$( cd -P "$( dirname "$SCRIPT_SOURCE" )" >/dev/null 2>&1 && pwd )"
		SCRIPT_SOURCE="$(readlink "$SCRIPT_SOURCE")"
		# if $SCRIPT_SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
		[[ $SCRIPT_SOURCE != /* ]] && SCRIPT_SOURCE="$SCRIPT_HOME/$SCRIPT_SOURCE"
	done
	echo "$( cd -P "$( dirname "$SCRIPT_SOURCE" )" >/dev/null 2>&1 && pwd )"
}

aedifex() {
	echo "$(home)/aedifex.jar"
}

java -jar $(aedifex) $@

