(define-module (packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system node))

(define-public node-ws
  (package
    (name "node-ws")
    (version "8.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/websockets/ws")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nwig6rxakd72nr1sbcl7qqwab3w5116s7dcblnz45fslf9lql8i"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda _
                      (invoke "npm" "--offline" "--ignore-scripts" "install"
                              "--production"))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis "Websocket client and server for Node.js")
    (description "This package provides a simple websocket client and server
for Node.js.  It doesn't work in a browser.  It can send and receive data,
authenticate clients, broadwast to other websocket clients, use external
APIs...")
    (license license:expat)))

(define-public node-commander
  (package
    (name "node-commander")
    (version "9.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tj/commander.js")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1yvm0qjirxxdrf7dng0h770n8abz83ymzv4r0wa8ww5x2l2xzsvz"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda _
                      (invoke "npm" "--offline" "--ignore-scripts" "install"
                              "--production"))))))
    (home-page "https://github.com/tj/commander.js")
    (synopsis "Complete solution for Node.js command-line interfaces")
    (description "This package can parse arguments into options and
command-arguments, disaply usage errors for problems, and implements a help
system.")
    (license license:expat)))

(define-public node-chrome-remote-interface
  (package
    (name "node-chrome-remote-interface")
    (version "0.31.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyrus-and/chrome-remote-interface")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gbz8cj6s1mxvzaa9wpvdky2mcrdaz3ql552rkzzi6xlziqsrx69"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda _
                      (invoke "npm" "--offline" "--ignore-scripts" "install"
                              "--production"))))))
    (inputs (list node-ws node-commander))
    (home-page "https://github.com/cyrus-and/chrome-remote-interface")
    (synopsis "Chrome Debugging Protocol interface")
    (description "This package is a Chrome Debugging Protocol interface that
helps to instrument Chrome (or any other suitable implementation) by providing
a simple abstraction of commands and notifications using a straightforward
JavaScript API.")
    (license license:expat)))
