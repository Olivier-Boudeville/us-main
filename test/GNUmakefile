US_MAIN_TOP = ..


.PHONY: all debug debug-sensor debug-contact

# Provided by Traces, in its priv/bin:
KILL_TRACE_TOOL := kill-trace-supervision-tool.sh

# Default do-nothing target:
all:


# Gammu requires a configuration file, and this location was found the most
# reliable (see https://docs.gammu.org/config/):
#
GAMMU_RC_FILE="${HOME}/.gammurc"

debug: debug-contact


debug-sensor:
	@$(KILL_TRACE_TOOL) 1>/dev/null
	@cd $(MOBILE_TOP)/test && $(MAKE) -s pre-test
	@$(MAKE) -s us_main_otp_sensor_run #$(BATCH_OPT)


debug-contact:
	@$(KILL_TRACE_TOOL) 1>/dev/null
	@$(MAKE) -s us_main_otp_contact_run #$(BATCH_OPT)


include $(US_MAIN_TOP)/GNUmakesettings.inc
