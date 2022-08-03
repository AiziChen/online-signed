#lang racket/base

(require db)

(provide *conn*)

(define *conn*
  (virtual-connection
   (connection-pool
    (lambda ()
      (postgresql-connect
       #:database "online_signed"
       #:user "postgres"
       #:password "quanyec")))))
