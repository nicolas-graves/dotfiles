(define-module (packages go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go))

(define-public go-github-com-nathasm-git-lfs-rsync-agent
  (package
    (name "go-github-com-nathasm-git-lfs-rsync-agent")
    (version "0.0.0-20180302043657-cce37c1220a3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nathasm/git-lfs-rsync-agent")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "158b3imix5gn7w46bpn6xxcdfjjs6di6b5lvghwv79vix2v3l9ii"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nathasm/git-lfs-rsync-agent"))
    (home-page "https://github.com/nathasm/git-lfs-rsync-agent")
    (synopsis "Rsync Custom Transfer Agent for Git LFS")
    (description
     "The rsync @url{https://git-lfs.github.com/,git-lfs} custom transfer agent allows
transferring the data through rsync, for example using SSH authentication.")
    (license license:expat)))

go-github-com-nathasm-git-lfs-rsync-agent
