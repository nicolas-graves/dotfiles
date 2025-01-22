pull:
	guix stack pull -C ./channels.scm --disable-authentication --profile=/home/graves/.config/guix/current --allow-downgrades

# The issue here is that including part of RDE as a package in guix-rde easily
# creates ABI inconsistencies.  The solutions are probably to be found in
# https://issues.guix.gnu.org/74425 or https://issues.guix.gnu.org/74633
# but that is not available yet in Guix.
home:
	GUILE_LOAD_PATH= GUILE_LOAD_COMPILED_PATH= ./make home -K
