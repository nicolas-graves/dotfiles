(specifications->manifest
 '(
   "direnv"
   "nix"
   "jq"
   "curl"
   "docker-cli"
   "docker-compose"
   "htop"
   "tmux"
   "make"
   "node"
   "openssh"
   "gnupg"
   ;;   "libva-info"
   ))

;; guix package --profile=$GUIX_EXTRA_PROFILES/cli/cli --manifest=$HOME/.config/guix/manifests/cli.scm
