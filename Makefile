export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):$(XDG_CONFIG_HOME)/guix

.PHONY: yggdrasil-home
yggdrasil-home:
	GUILE_LOAD_PATH=./ guix home reconfigure ./home/yggdrasil/core.scm
	emacsclient -e "(org-babel-tangle-file \"home/yggdrasil/files/config/emacs/Emacs.org\")"
	emacsclient -e "(org-babel-tangle-file \"home/yggdrasil/files/config/emacs/Workflow.org\")"
	ln -sf ~/.config/isync/mbsyncrc  ~/.mbsyncrc
	ln -sf ~/.dotfiles/home/yggdrasil/files/config/ssh/known_hosts ~/.ssh/known_hosts
	rbw get id_ed25519 > ~/.ssh/id_ed25519
	rbw get id_rsa > ~/.ssh/id_rsa
	rbw get id_rsa_git > ~/.ssh/id_rsa_git
	chmod 600  ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_rsa_git

# FIXME : packages installed in guix system do not seem to be
# here : make vim sed git ...
.PHONY: yggdrasil-home-init
yggdrasil-home-init:
	mkdir -p ~/.config/guix ~/.config/emacs
	mkdir -p ~/.local/src ~/.local/share 
	guix package -i vim git sed 
	#git -C ~/.local/src/ clone ssh://my_git:/srv/git/guix-channel.git guix-channel.git
	cp ./channels.base ./channels.scm
	ln -sf ~/.dotfiles/channels.scm ~/.config/guix
	#guix pull
	GUILE_LOAD_PATH=./ guix home reconfigure ./home/yggdrasil/core.scm 
	emacs --batch --quick home/yggdrasil/files/config/emacs/Emacs.org -f org-babel-tangle
	emacs --batch --quick home/yggdrasil/files/config/emacs/Workflow.org -f org-babel-tangle
	emacs --batch --quick -f all-the-icons-install-fonts
	ln -sf ~/.config/isync/mbsyncrc  ~/.mbsyncrc

.PHONY: yggdrasil-system
yggdrasil-system:
	GUILE_LOAD_PATH=./ sudo -E guix system reconfigure ./system/yggdrasil.scm
