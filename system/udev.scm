(define-module (udev)
  #:use-module (gnu services base)
  #:export (st-link-rule caterina-rule))

(define st-link-rule
  (udev-rule
   "49-stlinkv2.rules"
   (string-append "SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"0483\","
                  "ATTRS{idProduct}==\"3748\","
                  "MODE:=\"0666\","
                  "SYMLINK+=\"stlinkv2_%n\"")))

(define caterina-rule
  (udev-rule
   "55-caterina.rules"
   (string-append "ATTRS{idVendor}==\"2a03\", ENV{ID_MM_DEVICE_IGNORE}=\"1\""
                  "ATTRS{idVendor}==\"2341\", ENV{ID_MM_DEVICE_IGNORE}=\"1\"")))
