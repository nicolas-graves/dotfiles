(define-module (system connections-utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages gnome)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module ((guix import utils) #:select (flatten))
  #:export (system-connections-configuration
            system-connections-extension
            system-connections-service-type
            serialize-system-connections-config))

;; (define (serialize-list val)
;;   (interpose (map serialize-val val) ", "))

;; (define (uglify-field-name field-name)
;;   "Convert symbol FIELD-NAME to a camel case string.
;; @code{symbol-name} => \"@code{symbolName}\"."
;;   (let* ((str (symbol->string field-name))
;; 	 (spl-str (string-split str #\-)))
;;     (apply string-append
;; 	   (car spl-str)
;; 	   (map string-capitalize (cdr spl-str)))))

;; (define (serialize-field field-name val)
;;    (cond
;;     ((boolean? val) (serialize-boolean field-name val))
;;     (else
;;      (list (format #f "~a=" (uglify-field-name field-name))
;; 	   val "\n"))))

;; (define (serialize-alist field-name val)
;;   (generic-serialize-alist append serialize-field val))

;; (define (serialize-boolean field-name val)
;;   (serialize-field field-name (if val "true" "false")))

;; (define serialize-string serialize-field)

(define (serialize-connection-section-header name value)
  (format #f "[~a~a]\n" (uglify-field-name name)
	  (if value (format #f " \"~a\"" value) "")))

(define serialize-connection-section
  (match-lambda
    ((name options)
     (cons
      (serialize-connection-section-header name #f)
      (serialize-alist #f options)))
    ((name value options)
     (cons
      (serialize-connection-section-header name value)
      (serialize-alist #f options)))))

(define (serialize-connection-config field-name val)
  #~(string-append #$@(append-map serialize-connection-section val)))

(define connection-config? list?)

(define-configuration system-connection-extension
  (config
   (connection-config '())
   "List of system connections sections.  The same format as in
@code{home-git-configuration}."))

(define-configuration system-connections-configuration
  (package
   (package network-manager)
   "The NetworkManager package to use.")
  (config
   (connection-config '())
   "List of sections and corresponding options.  Something like this:

@lisp
`((sendmail
   ((annotate . #t))))
@end lisp

will turn into this:

@example
[sendmail]
annotate=true
@end example")
  (config-extra-content
   (string-or-gexp "")
   "String or value of string-valued g-exps will be added to the end
of the configuration file."))

(define (add-connections-configuration config)

  (define (serialize-boolean val)
    (if val "true" "false"))

  (define (serialize-val val)
    (cond
     ((list? val) (serialize-list val))
     ((boolean? val) (serialize-boolean val))
     ((or (number? val) (symbol? val)) (list (maybe-object->string val)))
     (else (list val))))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (symbol->string key)))
      `(,key "=" ,@val "\n")))

  (let ((connection-config
         (map car (map cdr (system-connections-configuration-config config)))))
    (map
     (lambda (con)
       (let ((name (cdr (car (car (cdr (car con)))))))
         `(,(string-append "/etc/NetworkManager/system-connections.ln/"
                           (string-delete #\space name) ".nmconnection")
           ,(apply mixed-text-file
                   (string-delete #\space name)
                   (flatten (generic-serialize-ini-config
                             #:combine-ini interpose
                             #:combine-alist list
                             #:combine-section-alist cons*
                             #:serialize-field serialize-field
                             #:fields con))))))
     connection-config)))

(define (add-system-connection-packages config)
  (list (system-connections-configuration-package config)))

(define (system-connection-extensions original-config extension-configs)
  (system-connections-configuration
   (inherit original-config)
   (config
    (append (system-connections-configuration-config original-config)
	    (append-map
	     system-connection-extension-config extension-configs)))))

(define system-connections-service-type
  (service-type (name 'system-connections)
                (extensions
                 (list (service-extension
                        special-files-service-type
                        add-connections-configuration)
                       ;; (service-extension
                       ;;  home-profile-service-type
                       ;;  add-system-connection-packages)
                  ))
		(compose identity)
		(extend system-connection-extensions)
                (default-value (system-connections-configuration))
                (description "Install and configure system-connections for NetworkManager.")))

(define (generate-system-connections-documentation)
  (generate-documentation
   `((system-connections-configuration
      ,system-connections-configuration-fields))
   'system-connections-configuration))


  ;; (match-let* ((my-config (system-connections-configuration-config config))
  ;;              (my-configs-list (map car (map cdr my-config)))
  ;;              (my-connections (map car my-configs-list))
  ;;              ((((far . fdr) ...) ...) (map car (map cdr my-connections)))
  ;;              (((((far . fdr) ...) ...) ...) (map cdr my-connections))
  ;;              (((section ((far . fdr) ...) ...) ...) my-connections)
  ;;              (((section ((far . fdr) ...) ...) ...) (map car my-configs-list))
  ;;              (((section ((far . fdr) ...) ...) ...) (map car (map cdr my-configs-list)))
  ;;              ((((section ((far . fdr) ...) ...) ...) ...) my-configs-list)
  ;;              (((((section ((far . fdr) ...) ...) ...) ...) ...) (map cdr my-config))
  ;;              (((filename ((_ ((_ . _) ...) ...) ...) ...) ...) my-config)
