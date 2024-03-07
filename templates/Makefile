MAKEFILE_FLAGS += --always-make

profile:
	mkdir -p .guix-profile
	guix pull --allow-downgrades -C ./channels.scm --profile=.guix-profile/guix

build:
	.guix-profile/guix/bin/guix build -f manifest.scm -K

shell:
	.guix-profile/guix/bin/guix shell

time-machine:
	guix time-machine -C ./channels.scm
