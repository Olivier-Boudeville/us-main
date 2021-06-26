US_MAIN_TOP = ..

MODULES_DIRS = apps

.PHONY: all debug

# Must be the first target, otherwise BEAM files will not be built:
all:

debug:
	@kill-trace-supervision-tool.sh
	@$(MAKE) -s us_main_exec $(BATCH_OPT)


include $(US_MAIN_TOP)/GNUmakesettings.inc
