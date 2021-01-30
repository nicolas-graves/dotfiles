(use-modules (gnu packages clojure)
             (gnu packages java)
             (gnu packages readline)
             (gnu packages compression)
             (gnu packages bash)
             (gnu packages base)
             (gnu packages)
             (guix download)
             (guix packages)
             (guix profiles)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:))

(define clojure-jdk14
  (package/inherit
   clojure
   (inputs `(("jre" ,openjdk14)))))


(define clojure-tools-deps
  (package
   (name "clojure-tools-deps")
   (version "1.10.2.774")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://download.clojure.org/install/clojure-tools-"
                                version
                                ".tar.gz"))
            (sha256
             (base32 "0z3j8m9k7prmx6n3kpyhj04pjdg7y0plyxv4kp7789shanr6y4qp"))))
   (build-system trivial-build-system)
   (native-inputs `(("tar" ,tar)
                    ("gzip" ,gzip)
                    ("bash" ,bash)))
   (propagated-inputs `(("rlwrap" ,rlwrap)
                        ("jdk" ,openjdk14)))
   (arguments
    `(#:modules ((guix build utils)
                 (srfi srfi-26))
      #:builder
      (begin
        (use-modules (guix build utils)
                     (srfi srfi-26))
        (let* ((tar (string-append (assoc-ref %build-inputs "tar") "/bin"))
               (gzip (string-append (assoc-ref %build-inputs "gzip") "/bin"))
               (bash (string-append (assoc-ref %build-inputs "bash") "/bin"))
               (source (assoc-ref %build-inputs "source"))
               (out (assoc-ref %outputs "out"))
               (libexec (string-append out "/libexec"))
               (bin (string-append out "/bin"))
               (wrap-path (map (compose (cut string-append <> "/bin")
                                        (cut assoc-ref %build-inputs <>))
                               '("jdk" "rlwrap"))))
          (setenv "PATH" (list->search-path-as-string (list tar gzip bash) ":"))
          (invoke "tar" "xzf" source "--strip-components" "1")
          (for-each patch-shebang '("clj" "clojure"))
          (substitute* "clojure" (("PREFIX") out))
          (for-each install-file '("deps.edn" "example-deps.edn"
                                   "exec.jar" "clojure-tools-1.10.2.774.jar"
                                   "clj" "clojure")
                    (list out out libexec libexec bin bin))
          (for-each (cut wrap-program <> `("PATH" ":" suffix ,wrap-path))
                    (find-files bin))))))
   (synopsis "")
   (description "")
   (license license:epl1.0)
   (home-page "")))


(packages->manifest
 `(,clojure-tools-deps
   #;,clojure-jdk14
   ,@(map specification->package
          '("direnv"
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
            "ripgrep"
            "openjdk"))))

;; guix package --profile=$GUIX_EXTRA_PROFILES/cli/cli --manifest=$HOME/.config/guix/manifests/cli.scm
