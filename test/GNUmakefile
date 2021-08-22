US_MAIN_TOP = ..


.PHONY: all debug debug-sensor debug-contact


# Default do-nothing target:
all:

debug: debug-contact


debug-sensor:
	@kill-trace-supervision-tool.sh 1>/dev/null
	@$(MAKE) -s us_main_otp_sensor_run #$(BATCH_OPT)


debug-contact:
	@kill-trace-supervision-tool.sh 1>/dev/null
	@$(MAKE) -s us_main_otp_contact_run #$(BATCH_OPT)


include $(US_MAIN_TOP)/GNUmakesettings.inc
