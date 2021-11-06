export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):$(XDG_CONFIG_HOME)/guix

.PHONY: yggdrasil-home
yggdrasil-home:
	GUILE_LOAD_PATH=./ guix home reconfigure ./home/yggdrasil/core.scm
	ln -sf ~/.dotfiles/home/yggdrasil/files/config/ssh/known_hosts ~/.ssh/known_hosts
	rbw get id_ed25519 > ~/.ssh/id_ed25519
	rbw get id_rsa > ~/.ssh/id_rsa
	rbw get id_rsa_git > ~/.ssh/id_rsa_git
	chmod 600  ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_rsa_git

.PHONY: yggdrasil-system
yggdrasil-system:
	GUILE_LOAD_PATH=./ sudo -E guix system reconfigure ./system/yggdrasil.scm
