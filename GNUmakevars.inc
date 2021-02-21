# Prerequisite: US_MAIN_TOP must be set.


# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(US_MAIN_TOP)
endif


# Project section.


# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = US-Main
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := us_main
endif


# The uniquely-defined version of this layer, for all uses, including rebar:
US_MAIN_VERSION := 0.0.1


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(US_MAIN_VERSION)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(US_MAIN_TOP)
endif


US_MAIN_RELEASE_BASE := us-main-$(PROJECT_VERSION)

US_MAIN_RELEASE_ARCHIVE_ZIP := $(US_MAIN_RELEASE_BASE).zip
US_MAIN_RELEASE_ARCHIVE_BZ2 := $(US_MAIN_RELEASE_BASE).tar.bz2
US_MAIN_RELEASE_ARCHIVE_XZ  := $(US_MAIN_RELEASE_BASE).tar.xz


# Source section.
US_MAIN_SRC := $(US_MAIN_TOP)/src

# BEAM path section.
US_MAIN_BEAM := $(US_MAIN_TOP)/src


MONITOR_SCRIPT = $(US_MAIN_TOP)/priv/bin/monitor-us-main.sh


ifndef VM_NAME
	VM_NAME := us_main_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := us_main_test
endif



ifndef US_CONFIG_FILE
	US_CONFIG_FILE := $(US_TOP)/conf/us.config
endif


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
US_MAIN_BEAM_DIRS = $(US_MAIN_BEAM)


# For later reuse in the next layer of the software stack:
US_MAIN_PLT_FILE := $(US_MAIN_TOP)/us_main.plt


# This is the merged PLT of the level just below in the software stack.
# For us-main, it is the PLT of 'us-common':
ifndef PREDECESSOR_PLT
	PREDECESSOR_PLT = $(US_COMMON_PLT_FILE)
endif


# We define the path to the US-Common layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, dependencies may be found
# either in a local _checkouts child directory (if checkout directories are used
# and if the top-level package of interest is the current one), or in sibling
# directories (i.e. whose build root is at the same level as this one).
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
ifneq ($(wildcard $(US_MAIN_TOP)/../us_common),)

	# By default, expect sibling directories:
	US_COMMON_TOP = $(US_MAIN_TOP)/../us_common

else

	# Otherwise assume a checkout one:
	ifneq ($(wildcard $(US_MAIN_TOP)/_checkouts/us_common),)

		US_COMMON_TOP = $(US_MAIN_TOP)/_checkouts/us_common

   else

		FORCE_EVAL := $(error Unable to find the build root of US-Common)

	endif

endif
