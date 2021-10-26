(define-module (home yggdrasil pipewire)
  #:use-module (gnu home services)
  #:use-module (kreved home-services dbus)
  #:use-module (home yggdrasil pipewire-service))

(define-public services
  (list
   (service home-dbus-service-type)
   (service home-pipewire-service-type)))
