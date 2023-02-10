export GUILE_LOAD_PATH := $(GUILE_LOAD_PATH):./.guix-profile/guix/share/guile/site/3.0/:.
export GUILE_LOAD_COMPILED_PATH := $(GUILE_LOAD_COMPILED_PATH):./.guix-profile/guix/lib/guile/3.0/site-ccache/
# export GUIX_PACKAGE_PATH := $(GUIX_PACKAGE_PATH):./.guix-profile/guix/share/guile/site/3.0/

.PHONY: all profile channels

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


define CHANNELS
(use-modules
 (guix gexp)
 (ice-9 match)
 (ice-9 pretty-print))

(define* (channel-content
          #:key
          (freeze? #f)
          (urls
           '((nonguix . "https://gitlab.com/nonguix/nonguix.git") ;;'
             (rde     . "https://git.sr.ht/~abcdw/rde")
             (guix    . "https://git.savannah.gnu.org/git/guix.git")))
          (freeze-commits
           '((nonguix . "1aecd24155019cc524bca1c868729102c8b23f24") ;;'
             (rde     . "101313a691f074dcb34e9cbd4f13664df02f4ac7")
             (guix    . "688c3ef28220979e79ffd061c762bda84a663534"))))
  "This function generates then content of the channels file, with
optional commit pinning."
  `(list ;;`
    (channel
     (name 'nonguix) ;;'
     (url ,(cdr (assoc 'nonguix urls))) ;;'
     ,(if freeze? `(commit ,(cdr (assoc 'nonguix freeze-commits))) ;;`
          `(branch "master")) ;;`
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
    (channel
     (name 'rde) ;;'
     (url ,(cdr (assoc 'rde urls))) ;;'
     ,(if freeze? `(commit ,(cdr (assoc 'rde freeze-commits))) ;;`
          `(branch "master")) ;;`
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
    (channel
     (name 'guix) ;;'
     (url ,(cdr (assoc 'guix urls))) ;;'
     ,(if freeze? `(commit ,(cdr (assoc 'guix freeze-commits))) ;;`
          `(branch "master")) ;;`
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(with-output-to-file "channels"
  (lambda ()
    (pretty-print
     (channel-content
      #:freeze? #f
      #:freeze-commits
      '((guix    . "5f8c11d48e4949aa77d7aaa1e7e25568bd8dfa97") ;;'
        (nonguix . "e026dba1dad924aa09da8a28caa343a8ace3f6c7")
        (rde     . "74a3fb8378e86603bb0f70b260cbf46286693392"))
      #:urls
      '((guix    . "/home/graves/spheres/info/guix") ;;'
        (nonguix . "/home/graves/spheres/info/nonguix")
        (rde     . "/home/graves/spheres/info/rde"))))))

endef

channels:
	$(guile $(CHANNELS))

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
