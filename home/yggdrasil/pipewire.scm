(define-module (home yggdrasil pipewire)
  #:use-module (gnu home services)
  #:use-module (home services dbus)
  #:use-module (home services pipewire))

(define-public services
  (list
   (service home-dbus-service-type)
   (service home-pipewire-service-type)))
