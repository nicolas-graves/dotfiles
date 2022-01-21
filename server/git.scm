(define-module (server git)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services version-control)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ssh)

  #:use-module ((server base) #:prefix base:))

(define-public packages
  '("git"))

(define-public user
  (user-account (name (getenv "ID_ssh_my_git"))
                (group "users")
                (home-directory "/srv/git")))

(define-public services
  (modify-services base:services
    (openssh-service-type
     config =>
     (openssh-configuration
      (inherit config)
      (authorized-keys
       `((,(getenv "ID_ssh_my_server")
          ,(local-file
            (string-append "../keys/" (getenv "KEY_ssh_my_server") ".pub")))
         (,(getenv "ID_ssh_my_git")
          ,(local-file
            (string-append "../keys/" (getenv "KEY_ssh_my_git") ".pub")))))))))
