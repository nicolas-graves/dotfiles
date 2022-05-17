export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):$(XDG_CONFIG_HOME)/guix:$(HOME)/.dotfiles

.PHONY: home
home:
	guix home reconfigure ./home/yggdrasil/core.scm --allow-downgrades --fallback
	ln -sf ~/.config/isync/mbsyncrc  ~/.mbsyncrc
	ln -sf ~/.dotfiles/home/yggdrasil/files/config/ssh/known_hosts ~/.ssh/known_hosts
	ln -f ~/.dotfiles/home/yggdrasil/files/config/guix/shell-authorized-directories ~/.config/guix/shell-authorized-directories
	#rbw get id_ed25519 > ~/.ssh/id_ed25519  # TODO gpg
	#rbw get id_rsa > ~/.ssh/id_rsa  # TODO gpg
	#rbw get id_rsa_git > ~/.ssh/id_rsa_git  # TODO gpg
	chmod 600  ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_rsa_git

.PHONY: tangle
tangle:
	mkdir -p ~/.config/guix .emacs.d system
	emacs --batch --quick Home.org -f org-babel-tangle
	emacs --batch --quick home/yggdrasil/Emacs.org -f org-babel-tangle
	emacs --batch --quick home/yggdrasil/Workflow.org -f org-babel-tangle
	emacs --batch --quick System.org -f org-babel-tangle
	emacs --batch --quick Server.org -f org-babel-tangle

# FIXME : packages installed in guix system do not seem to be
# here : make vim sed git ...
.PHONY: home-init
home-init: tangle
	mkdir -p ~/.config/guix ~/.config/emacs
	mkdir -p ~/.local/src ~/.local/share 
	guix package -i vim git sed 
	#git -C ~/.local/src/ clone ssh://my_git:/srv/git/guix-channel.git guix-channel.git
	cp ./channels.base ./channels.scm
	ln -sf ~/.dotfiles/channels.scm ~/.config/guix
	#guix pull
	guix home reconfigure ./home/yggdrasil/core.scm
	emacs --batch --quick -f all-the-icons-install-fonts
	ln -sf ~/.config/isync/mbsyncrc  ~/.mbsyncrc

.PHONY: system
system:
	sudo mkdir -p /etc/NetworkManager/system-connections
	sudo rm -rf /etc/NetworkManager/system-connections.bak
	sudo mv -f /etc/NetworkManager/system-connections /etc/NetworkManager/system-connections.bak
	sudo -E guix system reconfigure ./system/yggdrasil.scm --allow-downgrades --fallback
	for file in $$(ls /etc/NetworkManager/system-connections.ln) ; do \
		cat /etc/NetworkManager/system-connections.ln/$$file > /tmp/$$file ; \
		sudo mv -f /tmp/$$file /etc/NetworkManager/system-connections/$$file ; \
		sudo chmod 600 /etc/NetworkManager/system-connections/$$file ; \
		sudo chown root:root /etc/NetworkManager/system-connections/$$file ; \
	done ;

update-fonts:
	#useful in the case when a font package has been updated 
	guix install fontconfig
	fc-cache -rv

deploy:
	guix deploy ./server/core.scm
	ssh my_server \
		reboot

check:
	guix repl ./system/yggdrasil.scm
	guix repl ./home/yggdrasil/core.scm
	guix repl ./server/core.scm
