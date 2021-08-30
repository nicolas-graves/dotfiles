export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):./cfg:$(XDG_CONFIG_HOME)/guix/current/share/guile/site/3.0


.PHONY: yggdrasil-home
yggdrasil-home:
	guix home reconfigure ./cfg/home/yggdrasil/core.scm
