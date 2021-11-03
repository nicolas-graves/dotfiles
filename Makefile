export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):$(XDG_CONFIG_HOME)/guix


.PHONY: yggdrasil-home
yggdrasil-home:
	 GUILE_LOAD_PATH=./ guix home reconfigure ./home/yggdrasil/core.scm

.PHONY: yggdrasil-system
yggdrasil-system:
	GUILE_LOAD_PATH=./ sudo -E guix system reconfigure ./system/yggdrasil.scm
