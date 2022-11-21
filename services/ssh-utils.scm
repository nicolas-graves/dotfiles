(define-module (services ssh-utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu home-services ssh))

(define-public (ssh-config id)
  (let* ((port
          (open-input-pipe
           (string-append "passage show ssh/ssh_" id " 2>/dev/null")))
         (key (read-line port))
         (ssh-user
          (when (string=? (read-delimited " " port) "Username:")
            (read-line port)))
         (uri
          (when (string=? (read-delimited " " port) "URI:")
            (read-line port)))
         (ssh-port
          (when (string=? (read-delimited " " port) "Port:")
            (read-line port)))
         (hostkey
          (when (string=? (read-delimited " " port) "HostKey:")
            (read-line port)))
         (ssh-options
          `((hostname . ,uri)
            (identity-file . ,(string-append "~/.ssh/" key))
            (port . ,ssh-port)
            (user . ,ssh-user))))
    (close-pipe port)
    (list (ssh-host
           (host id)
           (options ssh-options))
          (string-append uri " " hostkey "\n"))))
