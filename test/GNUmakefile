US_MAIN_TOP = ..


.PHONY: all debug


debug:
	@kill-trace-supervision-tool.sh 1>/dev/null
	@$(MAKE) -s class_USSensor_run #$(BATCH_OPT)


include $(US_MAIN_TOP)/GNUmakesettings.inc
