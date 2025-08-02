# Prerequisite: US_MAIN_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: 'production', most probably enforced by deployment
	# scripts, as US-Main servers shall generally be robust (e.g. by restarting
	# any failed child):
	#
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
#  - home_automation: all information about the home automation service
#  - presence_simulation: about specifically the presence simulation service
#  - actions: about the actions that may be externally triggered on server
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	US_MAIN_DEBUG_FLAGS += -Dus_main_debug_execution
	US_MAIN_DEBUG_FLAGS += -Dus_main_debug_sensors
	US_MAIN_DEBUG_FLAGS += -Dus_main_debug_home_automation
	US_MAIN_DEBUG_FLAGS += -Dus_main_debug_presence_simulation
	US_MAIN_DEBUG_FLAGS += -Dus_main_debug_alarm
	US_MAIN_DEBUG_FLAGS += -Dus_main_debug_actions
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# Does not exist: US_MAIN_CHECK_FLAGS += -Dus_main_check_xx
endif


ERLANG_COMPILER_OPT_BASE += $(US_MAIN_DEBUG_FLAGS) \
							$(US_MAIN_CHECK_FLAGS)



# Useful to target for example the root of the current layer (e.g. to locate the
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


# PROJECT_NAME_FOR_OTP should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_OTP
	PROJECT_NAME_FOR_OTP := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses (including rebar, hex
# and the code itself, see
# class_USMainCentralServer:get_us_main_version{_string,}/0):
#
US_MAIN_VERSION := 0.0.20

# Used by US-Common:
US_APP_VERSION := $(US_MAIN_VERSION)


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(US_MAIN_VERSION)
endif


# Variable name intentionally not including the current layer, for more generic
# rules:

ifndef VERSION_FOR_OTP
	VERSION_FOR_OTP := $(PROJECT_VERSION)
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


# JSON parser needed for the decoding of sensor data:
USE_JSON = true



# This is the US-Main specific default EPMD port, different from the default
# Myriad one and from the one of the other US applications, so that we can act
# upon the US-Main EPMD without interfering with others (see also the
# 'launch-epmd' make target):
#
EPMD_PORT := 4507


# No $(LAYER_TOP) in includes, it would be misleading as edoc is run from the
# clone root, not from doc:
#
ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {macros,[{$(PROJECT_EDOC_VERSION_KEY),"$(PROJECT_VERSION)"}]}, {includes,["..", "$(US_COMMON_TOP)/..", "$(TRACES_TOP)/..", "$(WOOPER_TOP)/..", "$(OCEANIC_TOP)/..", "$(MOBILE_TOP)/..", "$(SEAPLUS_TOP)/..", "$(MYRIAD_TOP)/.."]}, {overview, "doc/overview.edoc"}]'
endif


# The default path where a US-Main native build is to be deployed on a server:
# (useful to synchronise sources)
#
US_NATIVE_DEPLOY_ROOT := /opt/universal-server/$(PROJECT_SHORT_NAME)-native-deployment

# For Myriad' sync-all-to-server target, if used:
CEYLAN_SYNC_TARGET_ROOT := $(US_NATIVE_DEPLOY_ROOT)


# For later reuse in the next layer of the software stack:
US_MAIN_PLT_FILE := $(US_MAIN_TOP)/US-Main.plt


# This is the merged PLT of the level just below in the software stack.
# For us-main, it is the PLT of 'us-common':
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(US_COMMON_PLT_FILE)
endif


# Prererequisites are supposedly already available and built as siblings,
# otherwise thanks to rebar3:

SIBLING_ROOT = $(US_MAIN_TOP)/..
PREREQ_ROOT = $(US_MAIN_TOP)/_build/default/lib


# Deactivated as expected to already managed by Myriad:
#ifndef JSX_TOP
#	ifneq ($(wildcard $(US_MAIN_TOP)/_checkouts/jsx/.),)
#		JSX_TOP = $(US_MAIN_TOP)/_checkouts/jsx
#	else
#		ifneq ($(wildcard $(SIBLING_ROOT)/jsx/.),)
#			JSX_TOP = $(SIBLING_ROOT)/jsx
#		else
#			JSX_TOP = $(PREREQ_ROOT)/jsx
#		endif
#	endif
#endif

ifndef JIFFY_TOP
	ifneq ($(wildcard $(US_MAIN_TOP)/_checkouts/jiffy/.),)
		JIFFY_TOP = $(US_MAIN_TOP)/_checkouts/jiffy
	else
		ifneq ($(wildcard $(SIBLING_ROOT)/jiffy/.),)
			JIFFY_TOP = $(SIBLING_ROOT)/jiffy
		else
			JIFFY_TOP = $(PREREQ_ROOT)/jiffy
		endif
	endif
endif

# Superseded by built-in 'json':
#US_MAIN_EXTRA_DEPS = $(JSX_TOP) $(JIFFY_TOP)

# $(SIBLING_ROOT) and $(PREREQ_ROOT) are added, as most includes are expected to
# be done with include_lib, hence as, for example, "foobar/include/foobar.hrl":
#
INC += $(foreach dir,$(US_MAIN_EXTRA_DEPS),-I$(dir)/include) -I$(SIBLING_ROOT) -I$(PREREQ_ROOT)


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
# Note also we rely on the names of build roots (e.g. myriad) as opposed to
# project/repository names (e.g. Ceylan-Myriad):
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


# And for Oceanic (which implies erlang-serial):
ifneq ($(wildcard $(US_MAIN_TOP)/_checkouts/oceanic/.),)
	# By default, expect a checkout directory:
	OCEANIC_TOP = $(US_MAIN_TOP)/_checkouts/oceanic
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(US_MAIN_TOP)/../oceanic/.),)
		OCEANIC_TOP = $(US_MAIN_TOP)/../oceanic
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(US_MAIN_TOP)/_build/default/lib/oceanic/.),)
			OCEANIC_TOP = $(US_MAIN_TOP)/_build/default/lib/oceanic
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-Oceanic from '$(realpath .)'.)
		endif
	endif
endif


ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(US_COMMON_TOP) $(MOBILE_TOP) $(OCEANIC_TOP)
endif

ifndef BASE_PLT_FILES
	# Extra PLTs commented-out until the --plts Dialyzer option is fixed:
	BASE_PLT_FILES = $(PREDECESSOR_PLT_FILE) #$(OCEANIC_PLT_FILE) $(MOBILE_PLT_FILE)
endif
