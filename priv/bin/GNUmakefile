US_MAIN_TOP = ../..

.PHONY: all monitor monitor-dev monitor-prod


all:


monitor:
	@./monitor-us-main.sh

monitor-dev:
	@./monitor-us-main.sh us-monitor-for-development.config

monitor-prod:
	@./monitor-us-main.sh us-monitor-for-production.config


include $(US_MAIN_TOP)/GNUmakesettings.inc
