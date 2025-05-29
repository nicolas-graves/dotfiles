.DEFAULT: rde

rde:
	guix rde reconfigure $$PWD/configuration.scm

pull:
	guix stack pull -C $$PWD/channels.scm --disable-authentication --profile=/home/graves/.config/guix/current --allow-downgrades --from-local-channels=$$PWD/channels

pull-sans-local:
	guix stack pull -C $$PWD/channels.scm --disable-authentication --profile=/home/graves/.config/guix/current --allow-downgrades
