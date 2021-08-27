export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):./cfg

.PHONY: yggdrasil-home
yggdrasil-home:
	guix home reconfigure ./cfg/home/yggdrasil/core.scm
