export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):./:$(XDG_CONFIG_HOME)/guix/current/share/guile/site/3.0


.PHONY: yggdrasil-home
yggdrasil-home:
	guix home reconfigure ./home/yggdrasil/core.scm

.PHONY: yggdrasil-system
yggdrasil-system:
	sudo -E guix system reconfigure ./system/yggdrasil.scm
