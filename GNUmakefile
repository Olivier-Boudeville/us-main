US_MAIN_TOP = .


.PHONY: help help-intro help-us-main register-version-in-header register-us-main \
		list-beam-dirs add-prerequisite-plts link-plt stats all                  \
		all compile                                                              \
		ensure-dev-release ensure-prod-release                                   \
		release release-dev release-prod                                         \
		export-release just-export-release                                       \
		start debug status stop log cat-log tail-log                             \
		inspect monitor-development monitor-production                           \
		kill shell test test-interactive                                         \
		clean clean-logs real-clean info info-local


MODULES_DIRS = src doc conf priv test


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true

# Base directory for the build:
BUILD_ROOT := _build


## Default build section.

# Base directory for the default build:
DEFAULT_BASE := $(BUILD_ROOT)/default

# Base directory for default releases:
DEFAULT_REL_BASE := $(DEFAULT_BASE)/rel

# Base directory of the default us_main release:
US_MAIN_DEFAULT_REL_DIR := $(DEFAULT_REL_BASE)/us_main

US_DEFAULT_REL_EXEC := $(US_MAIN_DEFAULT_REL_DIR)/bin/us_main



## Production build section.

# Base directory for the production build:
PROD_BASE := $(BUILD_ROOT)/prod

# Base directory for production releases:
PROD_REL_BASE := $(PROD_BASE)/rel

# Base directory of the production us_main release:
US_MAIN_PROD_REL_DIR := $(PROD_REL_BASE)/us_main


# Not ':=', to resolve version:
RELEASE_NAME = us_main-$(US_MAIN_VERSION).tar.gz

RELEASE_PATH = $(US_MAIN_PROD_REL_DIR)/$(RELEASE_NAME)



EXPORT_TARGET := $(WEB_SRV):/tmp


LOG_DIR := $(US_MAIN_DEFAULT_REL_DIR)/log

VM_LOG_FILES := $(LOG_DIR)/erlang.log.*


# Default target:
help: help-intro help-us-main


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-us-main:
	@cd $(US_COMMON_TOP) && $(MAKE) -s help-us-common


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 52 ; else \
	$(MAKE) register-us-main ; fi


register-us-main:
	@echo "-define( us_main_version, \"$(US_MAIN_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(US_MAIN_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'us_main' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(US_MAIN_PLT_FILE)" ]; then ln -s --force $(PLT_FILE) $(US_MAIN_PLT_FILE) ; fi


stats:
	@$(MAKE_CODE_STATS) $(US_MAIN_TOP)


# The 'compile' target just by itself would not recompile a us-main source file
# that would have been changed:
#
#all: compile


compile: rebar3-create-app-file
	@echo "  Compiling us_main from $$(pwd)"
	@$(REBAR3) compile


# Ensures a relevant development release is available.
ensure-dev-release:
	@if [ ! -f "$(US_DEFAULT_REL_EXEC)" ] ; then \
	echo "No $(US_DEFAULT_REL_EXEC) found, building a development release." ; $(MAKE) -s release-dev ; fi


# Ensures a relevant production release is available.
ensure-prod-release:
	@if [ ! -f "$(US_DEFAULT_REL_EXEC)" ] ; then \
	echo "No $(US_DEFAULT_REL_EXEC) found, building a production release." ; $(MAKE) -s release-prod ; fi



# Before generating a release, the 'clean-otp-build-tree' target shall be run,
# as otherwise past elements (ex: in link with .app files) will be re-used.

release: release-prod


# Probably that '@$(REBAR3) tar' exceeds what is needed:
#
# ('compile' is not needed either: same happens because of 'release')
#
release-dev: clean-otp-build-tree rebar3-create-app-file rebar.config #compile #update-release
	@echo "  Generating OTP us_main release in development mode"
	@$(REBAR3) release
	@cd $(US_MAIN_DEFAULT_REL_DIR)/releases && /bin/ln -sf $(US_MAIN_VERSION) latest-release


# Rebuilding all dependencies ('compile' implied):
# (yes, 'tar', not 'release')
#
release-prod: clean-otp-build-tree rebar3-create-app-file rebar.config  #update-release
	@echo "  Generating OTP us_main release in production mode"
	@$(REBAR3) as prod tar


# Rebuilding the "normal" version thereof (not the testing or Hex one):
rebar.config: conf/rebar.config.template
	$(MAKE) -s set-rebar-conf


# Just rebuilding us-main:
release-prod-light: compile
	@echo "  Generating OTP us_main release in production mode (with lightest rebuild)"
	@$(REBAR3) as prod tar


export-release: release-prod-light just-export-release


just-export-release:
	@echo "  Exporting production release $(RELEASE_NAME) to $(EXPORT_TARGET)"
	@scp $(SP) $(RELEASE_PATH) $(EXPORT_TARGET)


# Not used anymore, as the simple_bridge configuration file does not seem to be
# taken into account:
#
update-release:
	@echo "  Updating $(SIMPLE_BRIDGE_CONFIG)"
	@cat $(SIMPLE_BRIDGE_CONFIG) | sed 's|{handler, simple_bridge_handler_sample}|{handler, us_main_handler}|' | sed 's|%% {backend, yaws}|{backend,cowboy}|' > $(SIMPLE_BRIDGE_CONFIG).tmp && /bin/mv $(SIMPLE_BRIDGE_CONFIG).tmp $(SIMPLE_BRIDGE_CONFIG)


# Release shall have been generated beforehand:
#
# (CTRL-C twice in the console will not be sufficient to kill this instance, use
# 'make kill' instead)
#
start: kill clean-logs compile
	@echo "Starting the us_main release (EPMD port: $(EPMD_PORT)):"
	@export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_MAIN_DEFAULT_REL_DIR)/bin/us_main start || echo "Start failed"
	@sleep 1 ; $(MAKE) -s log


debug: ensure-dev-release
	@echo " Running us_main for debug (EPMD port: $(EPMD_PORT))"
	@killall java 2>/dev/null ; export ERL_EPMD_PORT=$(EPMD_PORT) ; $(MAKE) -s start || $(MAKE) log


# A rule such as the following would be bound to fail because of a non-matching
# cookie:
#
#	-@export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_MAIN_DEFAULT_REL_DIR)/bin/us_main status
status:
	@echo "Status of the us_main release (EPMD port: $(EPMD_PORT)):"
	@$(US_MAIN_TOP)/priv/bin/get-us-main-status.sh


# A rule such as the following would be bound to fail because of a non-matching
# cookie:
#
#  @export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_MAIN_DEFAULT_REL_DIR)/bin/us_main stop || ( echo "Stop failed" ; $(MAKE) -s log )
#
stop:
	@echo "Stopping the us_main release (EPMD port: $(EPMD_PORT)):"
	@$(US_MAIN_TOP)/priv/bin/stop-us-main.sh


# Note: this target works; one should just scroll upward sufficiently in one's
# terminal.
#
log: cat-log
#log: tail-log


# Useful to debug when crashing:
cat-log:
	@sleep 1
	@cat $(VM_LOG_FILES)


# When more stable; preferred to more:
tail-log:
	@tail --lines=20 -f $(VM_LOG_FILES)


# run_erl here, not beam.smp:
inspect:
	-@ps -edf | grep run_erl | grep -v grep 2>/dev/null || echo "(no run_erl process)"
	-@ps -edf | grep epmd | grep -v grep 2>/dev/null || echo "(no epmd process)"
	@$(MAKE) -s log


# Monitors a webserver that (already) runs in development mode, from specified
# US config:
#
monitor-development:
	@$(MONITOR_SCRIPT) us-monitor-for-development.config


# Monitors a webserver that (already) runs in production mode, from specified
# US config:
#
monitor-production:
	@$(MONITOR_SCRIPT) us-monitor-for-production.config


kill:
	-@killall epmd run_erl 2>/dev/null || true


test: test-interactive


# Shorthand:
shell: test-interactive


# us_main auto-booted:
#
# One may paste 'io:format( \"~s\", [ us_main:get_runtime_configuration() ] ).'
# in following shell:
#
test-interactive: compile
	@$(REBAR3) shell


# Creates the symbolic links that allow the make system to find its Ceylan
# prerequisites (once they have been cloned once by rebar3; for that, just
# comment-out the include at bottom, and run 'make release'):
#
# (now in _checkouts/)
#
#links:
#	@cd ../ ; for p in myriad wooper traces us_common; do ln -s $$p ; done


clean-local: clean-log
	@echo "  Cleaning from $$(pwd)"
	@$(REBAR3) clean


# Web-related logs already removed by the recursive 'clean' target:
clean-logs:
	-@/bin/rm -f $(VM_LOG_FILES)


real-clean: clean clean-otp-build-tree
	@echo "  Real cleaning from $$(pwd)"
	-@/bin/rm -f rebar.lock


clean-otp-build-tree:
	@echo "  Cleaning OTP build tree"
	-@/bin/rm -rf _build


info: info-local


info-local:
	@echo "REBAR3 = $(REBAR3)"
	@echo "US_MAIN_DEFAULT_REL_DIR = $(US_MAIN_DEFAULT_REL_DIR)"
	@echo "US_DEFAULT_REL_EXEC = $(US_DEFAULT_REL_EXEC)"
	@echo "VM_LOG_FILES = $(VM_LOG_FILES)"
	@echo "RELEASE_PATH = $(RELEASE_PATH)"
	@echo "US_COMMON_TOP = $(US_COMMON_TOP)"
	@echo "TRACES_TOP = $(TRACES_TOP)"
	@echo "WOOPER_TOP = $(WOOPER_TOP)"
	@echo "MYRIAD_TOP = $(MYRIAD_TOP)"


include $(US_MAIN_TOP)/GNUmakesettings.inc
