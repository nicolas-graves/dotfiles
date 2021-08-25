asgard-home:
	guix home reconfigure ~/.config/guix/cfg/home/asgard.scm

asgard-system:
	sudo -E guix system -L ~/.config/guix/cfg/ reconfigure ~/.config/guix/cfg/system/asgard.scm

.PHONY: asgard-home asgard-system
