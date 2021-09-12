(define-module (home yggdrasil terminals)
  #:use-module (guix packages)
  #:use-module (gnu packages terminals)
  #:use-module (gnu home-services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (ice-9 curried-definitions)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (home-alacritty-configuration
            home-alacritty-service-type))

(define-configuration/no-serialization home-alacritty-configuration
  (package
    (package alacritty)
    "alacritty package to use")
  (config
   (alist '())
   "Association list of key-value pair configuration.  The following configuration:
@lisp
(config
 `((font . ((normal . ((family . Iosevka)
                       (style . Light)))
            (size . 18.0)))
   (key_bindings . #(((key . C)
                      (mods . Alt)
                      (action . Copy))
                     ((key . V)
                      (mods . Alt)
                      (action . Paste))))))
@end lisp

would yield:

@example
font:
  normal:
    family: Iosevka
    style: Light
  size: 18.0
key_bindings:
  -
    key: C
    mods: Alt
    action: Copy
@end example"))

(define (make-yaml-indent depth)
  (make-string (* 2 depth) #\space))

(define ((serialize-yaml-vector-value depth) value)
  (let* ((depth (1+ depth))
         (tab (make-yaml-indent depth)))
    (cond
     ((string? value)
      (list (format #f "~a- '~a'\n" tab value)))
     ((boolean? value)
      (list (format #f "~a- ~a\n" tab (if value "true" "false"))))
     ((alist? value)
      (cons
       (format #f "~a-\n" tab)
       (serialize-yaml-alist value #:depth (1+ depth))))
     ((vector? value)
      (cons
       (format #f "~a-\n" tab)
       (append-map (serialize-yaml-vector-value depth) (vector->list value))))
     (else (format #f "~a- ~a\n" tab value)))))

(define ((serialize-yaml-value depth) key value)
  (let ((tab (make-yaml-indent depth)))
    (cond
     ((string? value)
      (list (format #f "~a~a: '~a'\n" tab key value)))
     ((boolean? value)
      (list (format #f "~a~a: ~a\n" tab key (if value "true" "false"))))
     ((alist? value)
      (cons
       (format #f "~a~a:\n" tab key)
       (serialize-yaml-alist value #:depth (1+ depth))))
     ((vector? value)
      (cons
       (format #f "~a~a:\n" tab key)
       (append-map (serialize-yaml-vector-value depth) (vector->list value))))
     (else (list (format #f "~a~a: ~a\n" tab key value))))))

(define* (serialize-yaml-alist value #:key (depth 0))
  (generic-serialize-alist append (serialize-yaml-value depth) value))

(define (serialize-yaml-config config)
  #~(string-append #$@(serialize-yaml-alist config)))

(define (add-alacritty-configuration config)
  (let ((cfg (home-alacritty-configuration-config config)))
    `(("config/alacritty/alacritty.yml"
       ,(mixed-text-file
         "alacritty.yml"
         (serialize-yaml-config cfg))))))

(define add-alacritty-package
  (compose list home-alacritty-configuration-package))

(define home-alacritty-service-type
  (service-type
   (name 'home-alacritty)
   (extensions
    (list (service-extension
           home-files-service-type
           add-alacritty-configuration)
          (service-extension
	   home-profile-service-type
	   add-alacritty-package)))
   (default-value (home-alacritty-configuration))
   (description "")))

(define-public services
  (list
   (service
    home-alacritty-service-type
    (home-alacritty-configuration
     (config
      `((window . ((dynamic_title . true)))
        (cursor . ((style . ((shape . Beam)))))
        (font . ((normal . ((family . Iosevka)
                            (style . Light)))
                 (bold . ((family . Iosevka)
                          (style . Light)))
                 (italic . ((family . Iosevka)
                            (style . Light)))
                 (size . 18.0)))
        (draw_bold_text_with_bright_colors . true)
        (colors . ((primary . ((background . "#FFFFFF")
                               (foreground . "#000000")))
                   (cursor . ((cursor . "#000000")))
                   (selection . ((background . "#E8DFD1")
                                 (foreground . "#000000")))
                   (normal . ((black . "#000000")
                              (red . "#A60000")
                              (green . "#005E00")
                              (yellow . "#813E00")
                              (blue . "#0031A9")
                              (magenta . "#721045")
                              (cyan . "#00538B")
                              (white . "#BFBFBF")))
                   (bright . ((black . "#595959")
                              (red . "#972500")
                              (green . "#315B00")
                              (yellow . "#70480F")
                              (blue . "#2544BB")
                              (magenta . "#5317AC")
                              (cyan . "#005A5F")
                              (white . "#FFFFFF")))))
        (key_bindings . #(((key . C)
                           (mods . Alt)
                           (action . Copy))
                          ((key . V)
                           (mods . Alt)
                           (action . Paste))))))))))
