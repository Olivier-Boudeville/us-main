# Prerequisite: US_MAIN_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid US-Main execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:


# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - execution: VM-level information, notably related to the code path
#  - sensors: help the debugging of sensor management
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#US_MAIN_DEBUG_FLAGS += -Dus_main_debug_execution
	#US_MAIN_DEBUG_FLAGS += -Dus_main_debug_sensors
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# Does not exist: US_MAIN_CHECK_FLAGS += -Dus_main_check_xx
endif


ERLANG_COMPILER_OPT_BASE += $(US_MAIN_DEBUG_FLAGS)    \
							$(US_MAIN_CHECK_FLAGS)



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


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := us_main
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses, including rebar:
US_MAIN_VERSION := 0.0.7


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(US_MAIN_VERSION)
endif


# Variable name intentionally not including the current layer, for more generic
# rules:

ifndef VERSION_FOR_REBAR
	VERSION_FOR_REBAR := $(PROJECT_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME := $(PROJECT_NAME)
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
US_MAIN_BEAM_DIR := $(US_MAIN_TOP)/src


MONITOR_SCRIPT = $(US_MAIN_TOP)/priv/bin/monitor-us-main.sh


# We rely on Myriad, WOOPER, Traces and US-Common as well; we add also the
# parent directory of the current layer, as the '-include_lib' directives rely
# on the code path (see Myriad's counterpart file for further information)
#
BEAM_DIRS += $(US_MAIN_BEAM_DIRS) $(US_COMMON_TOP)/..


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
US_MAIN_BEAM_DIRS = $(US_MAIN_BEAM_DIR)


# JSON parser needed for the decoding of sensors data:
USE_JSON = true


ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {includes,["$(LAYER_TOP)/include", "$(US_COMMON_TOP)/.."]}, {overview, "$(LAYER_TOP)/overview.edoc"}]'
endif


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
# First priority is an existing checkout directory:
ifneq ($(wildcard $(US_MAIN_TOP)/_checkouts/us_common/.),)
	US_COMMON_TOP = $(US_MAIN_TOP)/_checkouts/us_common
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(US_MAIN_TOP)/../us_common/.),)
		US_COMMON_TOP = $(US_MAIN_TOP)/../us_common
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(US_MAIN_TOP)/_build/default/lib/us_common/.),)
			US_COMMON_TOP = $(US_MAIN_TOP)/_build/default/lib/us_common
		else
			FORCE_EVAL := $(error Unable to find the build root of US-Common from $(realpath .).)
		endif
	endif
endif



# We do the same for Mobile (which implies Seaplus):
ifneq ($(wildcard $(US_MAIN_TOP)/_checkouts/mobile/.),)
	# By default, expect a checkout directory:
	MOBILE_TOP = $(US_MAIN_TOP)/_checkouts/mobile
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(US_MAIN_TOP)/../mobile/.),)
		MOBILE_TOP = $(US_MAIN_TOP)/../mobile
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(US_MAIN_TOP)/_build/default/lib/mobile/.),)
			MOBILE_TOP = $(US_MAIN_TOP)/_build/default/lib/mobile
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-Mobile from '$(realpath .)'.)
		endif
	endif
endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(US_COMMON_TOP) $(MOBILE_TOP)
endif