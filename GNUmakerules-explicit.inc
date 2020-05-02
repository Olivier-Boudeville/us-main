# Here are gathered explicit (immediate, static) rules for us-main.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: US_MAIN_TOP must be set.

.PHONY: default-us-main-rule


# The first non-generic, therefore default, rule:
default-us-main-rule: all
