US_MAIN_TOP = ../..

.PHONY: all monitor monitor-dev monitor-prod debug


all:


monitor:
	@./monitor-us-main.sh

monitor-dev:
	@./monitor-us-main.sh us-monitor-for-development.config

monitor-prod:
	@./monitor-us-main.sh us-monitor-for-production.config


debug:
	@kill-us-main.sh ; start-us-main-native-build.sh ; ./monitor-us-main.sh


include $(US_MAIN_TOP)/GNUmakesettings.inc
