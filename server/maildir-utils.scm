(define-module (server maildir-utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services mcron)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages base)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (home services maildirs)
  #:export (isync-configuration
            isync-service-type))

(define (serialize-isync-config field-name val)
  (define (serialize-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (format #f "~a" e))
      ((? string? e) (format #f "~s" e))
      (e e)))
  (define (serialize-item entry)
    (match entry
      ((? gexp? e) e)
      ((? list lst)
       #~(string-join '#$(map serialize-term lst)))))

  #~(string-append #$@(interpose (map serialize-item val) "\n" 'suffix)))

(define-configuration/no-serialization isync-configuration
  (package
    (package isync)
    "isync package to use.")
  (xdg-flavor?
   (boolean #f)
   "Whether to use the @file{$XDG_CONFIG_HOME/isync/mbsyncrc}
configuration file or not.  If @code{#t} creates a wrapper for mbsync
binary.")
  (config
   (list '())
   "AList of pairs, each pair is a String and String or Gexp."))

(define (add-isync-configuration config)
  `((,(if (isync-configuration-xdg-flavor? config)
          "config/isync/mbsyncrc"
          ".mbsyncrc")
     ,(mixed-text-file
       "mbsyncrc"
       (serialize-isync-config #f (isync-configuration-config config))))))

(define (isync-extensions cfg extensions)
  (isync-configuration
   (inherit cfg)
   (config (append (isync-configuration-config cfg) extensions))))

(define (add-isync-directories config)
  (with-imported-modules
      '((guix build utils)
        (ice-9 match)
        (ice-9 format)
        (home services maildirs))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match)
                     (ice-9 format)
                     (home services maildirs))
        (let ((maildir "/var/mail/"))
          (for-each
           (match-lambda
             ((address dirs ...)
              (for-each
               (lambda (dir)
                 (let ((submaildir (string-append maildir (format #f "~A" address) "/" dir)))
                   (display (string-append submaildir "\n"))
                   (mkdir-p (string-append submaildir "/cur"))
                   (mkdir-p (string-append submaildir "/new"))
                   (mkdir-p (string-append submaildir "/tmp"))))
               (car dirs))))
           %nested-dirs)))))

;; (define (update-isync-job config)
;;   #~(job '(next-minute-from)
;;          (lambda ()
;;            (execl (string-append #$isync "/bin/mbsync")
;;                   "-a"))))

(define-public isync-service-type
  (service-type
   (name 'isync)
   (extensions
    (list
     (service-extension
      special-files-service-type add-isync-configuration)
     (service-extension
      activation-service-type add-isync-directories)
     ;; (service-extension
     ;;  mcron-service-type update-isync-job)
     ))
   (compose concatenate)
   (extend isync-extensions)
   (default-value (isync-configuration))
   (description "Install and configure isync.")))
