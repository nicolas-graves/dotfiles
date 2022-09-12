export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):$(HOME)/spheres/info/guix:$(HOME)/spheres/info/dots:$(HOME)/spheres/info/rde
export GREEN='\033[1;32m'
export BLUE='\033[1;34m'
export RED='\033[1;30m'
export NC='\033[0m'

.PHONY:home
home:
	RDE_TARGET=home guix home reconfigure ./config.scm --fallback
	ln -sf ~/spheres/info/dots/config/ssh/known_hosts ~/.ssh/known_hosts
	ln -f ~/spheres/info/dots/config/guix/shell-authorized-directories ~/.config/guix/shell-authorized-directories

.PHONY:system
system:
	RDE_TARGET=system sudo -E guix system reconfigure ./config.scm --fallback

check:
	guix repl config.scm
	# guix repl ./server/core.scm

# FIXME : packages installed in guix system do not seem to be
# here : make vim sed git ...
# Update...
.PHONY: home-init
home-init:
	mkdir -p ~/.config/guix ~/.config/emacs
	mkdir -p ~/.local/share
	guix package -i vim git sed
	#git -C ~/projects/src/ clone ssh://my_git:/srv/git/guix-channel.git guix-channel.git
	cp ./channels.base ./channels.scm
	ln -sf ~/.dotfiles/channels.scm ~/.config/guix
	#guix pull
	guix home reconfigure ./home/yggdrasil/core.scm
	emacs --batch --quick -f all-the-icons-install-fonts
	ln -sf ~/.config/isync/mbsyncrc  ~/.mbsyncrc
	#rbw get id_ed25519 > ~/.ssh/id_ed25519  # TODO gpg
	#rbw get id_rsa > ~/.ssh/id_rsa  # TODO gpg
	#rbw get id_rsa_git > ~/.ssh/id_rsa_git  # TODO gpg
	chmod 600  ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_rsa_git

# useful in the case when a font package has been updated
update-fonts:
	guix install fontconfig
	fc-cache -rv

deploy:
	guix deploy ./server/core.scm
	ssh my_server \
		reboot

image:
	RDE_TARGET=live-install guix system image ./config.scm --image-size=7G

btrfs:
	mount LABEL=enc /mnt # or mount -t btrfs /dev/mapper/enc /mnt
	btrfs subvolume create /mnt/root
	btrfs subvolume create /mnt/boot
	btrfs subvolume create /mnt/home
	btrfs subvolume create /mnt/snapshots
	btrfs subvolume create /mnt/store
	btrfs subvolume create /mnt/data
	btrfs subvolume create /mnt/log
	umount /mnt
	mount -o subvol=root /dev/mapper/enc /mnt
	mkdir -p /mnt/home
	mkdir -p /mnt/home/.snapshots
	mkdir -p /mnt/gnu/store
	mkdir -p /mnt/data
	mkdir -p /mnt/var/log
	mkdir -p /mnt/boot
	mount -o compress=zstd,discard,space_cache=v2,subvol=home /dev/mapper/enc /mnt/home
	mount -o compress=zstd,discard,space_cache=v2,subvol=snapshots /dev/mapper/enc /mnt/home/.snapshots
	mount -o compress=zstd,discard,space_cache=v2,subvol=store /dev/mapper/enc /mnt/gnu/store
	mount -o compress=zstd,discard,space_cache=v2,subvol=data /dev/mapper/enc /mnt/data
	mount -o compress=zstd,discard,space_cache=v2,subvol=log /dev/mapper/enc /mnt/var/log
	mount -o compress=zstd,discard,space_cache=v2,subvol=boot /dev/mapper/enc /mnt/boot
