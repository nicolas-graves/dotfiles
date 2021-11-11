export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):$(XDG_CONFIG_HOME)/guix

.PHONY: yggdrasil-home
yggdrasil-home:
	GUILE_LOAD_PATH=./ guix home reconfigure ./home/yggdrasil/core.scm
	emacs --batch -Q home/yggdrasil/files/config/emacs/Emacs.org -f org-babel-tangle
	emacs --batch -Q home/yggdrasil/files/config/emacs/Workflow.org -f org-babel-tangle
	ln -sf ~/.config/isync/mbsyncrc  ~/.mbsyncrc
	ln -sf ~/.dotfiles/home/yggdrasil/files/config/ssh/known_hosts ~/.ssh/known_hosts
	rbw get id_ed25519 > ~/.ssh/id_ed25519
	rbw get id_rsa > ~/.ssh/id_rsa
	rbw get id_rsa_git > ~/.ssh/id_rsa_git
	chmod 600  ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_rsa_git

.PHONY: yggdrasil-home-init
yggdrasil-home-init:
	mkdir -p ~/.config/guix ~/.config/emacs
	ln -sf ~/.dotfiles/channels.scm ~/.config/guix
	guix pull
	make yggdrasil-home

.PHONY: yggdrasil-system
yggdrasil-system:
	GUILE_LOAD_PATH=./ sudo -E guix system reconfigure ./system/yggdrasil.scm
