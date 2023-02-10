export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):./.guix-profile/guix/share/guile/site/3.0/:.
export GUILE_LOAD_COMPILED_PATH := $(GUILE_LOAD_COMPILED_PATH):./.guix-profile/guix/lib/guile/3.0/site-ccache/
# export GUIX_PACKAGE_PATH := $(GUIX_PACKAGE_PATH):./.guix-profile/guix/share/guile/site/3.0/

all: profile system home

define PROFILE
;; Describe profile in Guile to build profile only one time.
(use-modules
 (git)
 (guix profiles)
 (srfi srfi-1))

(if
 (not
  (reduce (lambda (x y) (and x y)) #f
          (map
           (lambda (x)
             (let* ((elts (cdadar (manifest-entry-properties x)))
                    (repository (repository-open (car (assoc-ref elts 'url)))) ;;'
                    (commit (oid->string
                             (object-id
                              (revparse-single repository
                                               (car (assoc-ref elts 'branch))))))) ;;'
               (string= commit (car (assoc-ref elts 'commit))))) ;;'
           (manifest-entries (profile-manifest "./.guix-profile/guix")))))
 (gmk-expand "	make force-profile"))
endef

profile:
	$(guile $(PROFILE))

home:
	RDE_TARGET=home ./.guix-profile/guix/bin/guix home reconfigure ./config --fallback --allow-downgrades --keep-failed
	ln -sf ~/spheres/info/dots/stale/ssh/known_hosts ~/.ssh/known_hosts
	ln -f ~/spheres/info/dots/stale/guix/shell-authorized-directories ~/.config/guix/shell-authorized-directories

system:
	RDE_TARGET=system sudo -E ./.guix-profile/guix/bin/guix system reconfigure ./config --fallback --allow-downgrades

channels:
	./channels.sh > channels

force-profile:
	mkdir -p .guix-profile
	guix pull --disable-authentication -C ./channels --allow-downgrades --profile=.guix-profile/guix

# git -C /home/graves/spheres/info/guix/ rev-parse master
# FIXME : packages installed in guix system do not seem to be
# here : make vim sed git ...
# Update...
.PHONY: home-init
home-init:
	mkdir -p ~/.config/guix ~/.config/emacs
	mkdir -p ~/.local/share
	guix package -i vim git sed
	#git -C ~/projects/src/ clone ssh://my_git:/srv/git/guix-channel.git guix-channel.git
	cp ./channels.base ./channels
	ln -sf ~/.dotfiles/channels ~/.config/guix
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

check:
	#echo $$GUILE_LOAD_PATH
	guix time-machine --disable-authentication -C ./channels -- repl config

deploy:
	guix deploy ./server/core.scm
	ssh my_server \
		reboot

image:
	RDE_TARGET=live-install guix time-machine --disable-authentication -C ./channels -- system image ./config --image-size=7G

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


repl:
	guix repl ./config
