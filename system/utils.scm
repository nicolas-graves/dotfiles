(define-module (utils)
  #:use-module (srfi srfi-10))


(define-reader-ctor 'ml
  (λ strs
    (string-join strs "\n")))
