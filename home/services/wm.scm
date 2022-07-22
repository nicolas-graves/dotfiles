(define-module (home services wm)
  #:use-module (gnu home services)
  #:use-module (rde home services wm)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)
  #:use-module ((guix import utils) #:select (flatten))
  #:use-module ((gnu home-services-utils) #:select (maybe-object->string))
  #:use-module (ice-9 match)

  #:use-module (home packages swayr)

  #:re-export (home-sway-service-type
	       home-sway-configuration

               sway-config?)

  #:export (home-swayr-service-type
            home-swayr-configuration))

;;; Commentary:
;;;
;;; This module contains services for window managers.
;;;
;;; Code:


;;;
;;; Swayr.
;;;

(define (serialize-swayr-config config)

  (define (uglify-field-name field-name)
    (let ((str (symbol->string field-name)))
      (string-join
       (string-split (string-delete #\? str) #\-)
       "_")))

  (define (quote-val val)
    (string-append "'" val "'"))

  (define (build-list val)
    (string-append
     "["
     (string-join (map quote-val val) ",")
     "]"))

  (define (serialize-val val)
    (cond
     ((list? val) (list (build-list val)))
     ((or (string? val)) (list (quote-val val)))
     (else (list val))))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (uglify-field-name key)))
      `(,key "=" ,@val "\n")))

  (flatten (generic-serialize-ini-config
            #:combine-ini interpose
            #:combine-alist list
            #:combine-section-alist cons*
            #:serialize-field serialize-field
            #:fields config)))

(define swayr-config? list?)

(define-configuration home-swayr-configuration
  (package
    (package swayr)
    "The swayr package to use.")
  (config
   (swayr-config '())
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
;;   (config-extra-content
;;    (string-or-gexp "")
;;    "String or value of string-valued g-exps will be added to the end
;; of the configuration file.")
  )

(define (add-swayr-configuration config)
  (let ((cfg (home-swayr-configuration-config config)))
    `((".config/swayr/config.toml"
       ,(apply mixed-text-file
               "config"
               (serialize-swayr-config cfg))))))

(define (add-swayr-packages config)
  (list (home-swayr-configuration-package config)))

(define home-swayr-service-type
  (service-type
   (name 'home-swayr)
   (extensions
    (list (service-extension
           home-files-service-type
           add-swayr-configuration)
          (service-extension
           home-profile-service-type
           add-swayr-packages)))
   (default-value (home-swayr-configuration))
   (description
    "Install and configure swayr for switching windows with sway.")))

(define (generate-swayr-documentation)
  (generate-documentation
   `((home-swayr-configuration
      ,home-swayr-configuration-fields))
   'home-swayr-configuration))
