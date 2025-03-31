.DEFAULT: rde

rde:
	guix rde reconfigure $$PWD/configuration.scm

pull:
	guix stack pull -C ./channels.scm --disable-authentication --profile=/home/graves/.config/guix/current --allow-downgrades

