# Here are gathered explicit (immediate, static) rules for US-Main.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: US_MAIN_TOP must be set.

.PHONY: default-us-main-rule create-us-main-checkout


# The first non-generic, therefore default, rule:
default-us-main-rule: all


# Create a _checkouts directory in which all Ceylan and US dependencies for the
# current layer are referenced through a symlink.
#
create-us-main-checkout:
	@echo "  Creating a checkout directory for US-Main"
	@$(MAKE) -s create-us-common-checkout
	@cd _checkouts && ln -sf ../$(US_COMMON_TOP)
