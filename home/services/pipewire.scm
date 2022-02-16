(define-module (home services pipewire)
  #:use-module (guix gexp)

  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd))

(define (home-pipewire-files-service _)
  `(("config/alsa/asoundrc"
     ,(mixed-text-file
       "asoundrc"
       #~(string-append
          "<"
          #$(file-append
             pipewire-0.3
             "/share/alsa/alsa.conf.d/50-pipewire.conf")
          ">\n<"
          #$(file-append
             pipewire-0.3
             "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
          ">\n"
          "
pcm_type.pipewire {
  lib " #$(file-append
           pipewire-0.3
           "/lib/alsa-lib/libasound_module_pcm_pipewire.so") "
}

ctl_type.pipewire {
  lib " #$(file-append
           pipewire-0.3
           "/lib/alsa-lib/libasound_module_ctl_pipewire.so") "
}
")))))

(define (home-pipewire-shepherd-service _)
  (list
   (shepherd-service
    (requirement '(dbus-home))
    (provision '(pipewire))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire-0.3 "/bin/pipewire")))))
   (shepherd-service
    (requirement '(pipewire))
    (provision '(wireplumber))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append wireplumber "/bin/wireplumber")))))
   (shepherd-service
    (requirement '(pipewire))
    (provision '(pipewire-pulse))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire-0.3 "/bin/pipewire-pulse")))))))

(define-public home-pipewire-service-type
  (service-type
   (name 'home-pipewire)
   (extensions
    (list (service-extension
           home-files-service-type
           home-pipewire-files-service)
          (service-extension
           home-shepherd-service-type
           home-pipewire-shepherd-service)
          (service-extension
           home-profile-service-type
           (const (list pipewire-0.3 pulseaudio)))))
   (default-value #f)
   (description "run pipewire and stuff")))
