(define-module (home yggdrasil terminals)
  #:use-module (guix packages)
  #:use-module (gnu packages terminals)
  #:use-module (gnu home-services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services configuration)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (home-alacritty-configuration
            home-alacritty-service-type
            services))

(define-configuration/no-serialization home-alacritty-configuration
  (package
    (package alacritty)
    "")
  (config
   (alist '())
   ""))

(define (serialize-value value)
  (if (string? value)
      (format #f "'~a'" value)
      value))

(define* (serialize-yaml-config config #:key (depth 0))
  (string-join
   (append-map
    (match-lambda
      ((key . val)
       (let ((tabs (make-string (* 2 depth) #\space)))
         (if (list? val)
             (list
              (format #f "~a~a:" tabs key)
              (serialize-yaml-config val #:depth (1+ depth)))
             (list (format #f "~a~a: ~a" tabs key (serialize-value val)))))))
    config)
   "\n"))

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

(define services
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
                 (size . 14.0)))
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
                              (white . "#FFFFFF")))))))))))
