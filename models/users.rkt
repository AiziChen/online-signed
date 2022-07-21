#lang racket/base

(require deta
         threading
         gregor
         db
         racket/string
         "../sql-connector.rkt")

(provide
 (schema-out user)
 user-signed?
 user-exists?
 user-exists-all?
 user-insert!
 user-update-mac!
 get-all-users
 delete-user-by-id!
 check-unique
 user-active-date)

;;; USER MODEL
(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [[mac ""] string/f]
   [serial-no string/f #:unique #:contract non-empty-string?]
   [[created-at (now)] datetime/f]
   [[updated-at (now)] datetime/f]))

;;; initial user
(unless (table-exists? *conn* "users")
  (create-table! *conn* 'user))

;;; LOGICS
(define (user-signed? serial-no)
  (define rs
    (for/list ([b (in-entities *conn*
                               (~> (from user #:as u)
                                   (where (and (not (= u.mac ""))
                                               (= u.serial-no ,serial-no)))))])
      (user-id b)))
  (>= (length rs) 1))

(define (user-exists? serial-no)
  (define rs
    (for/list ([b (in-entities *conn*
                               (~> (from user #:as u)
                                   (where (= u.serial-no ,serial-no))))])
      (user-id b)))
  (>= (length rs) 1))

(define (user-exists-all? mac serial-no)
  (define rs
    (for/list ([b (in-entities *conn*
                               (~> (from user #:as u)
                                   (where (and (= u.mac ,mac)
                                               (= u.serial-no ,serial-no)))))])
      (user-id b)))
  (>= (length rs) 1))

;;; INSERT
(define (user-insert! mac serial-no [updated-at (now)])
  (insert-one! *conn*
               (make-user #:mac mac
                          #:serial-no serial-no
                          #:updated-at updated-at)))

;;; UPDATE
(define (user-update-mac! mac serial-no)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (and (= mac "")
                            (= u.serial-no ,serial-no))))))
  (if u
      (update! *conn*
               (update-user-mac u (lambda (_) mac))
               (update-user-updated-at u (lambda (_) (now))))
      #f))

(define (get-all-users)
  (for/list ([b (in-entities *conn*
                             (~> (from user #:as u)
                                 (order-by ([u.id #:desc]))))])
    b))

;;; DELETE
(define (delete-user-by-id! id)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (and (= u.id ,id))))))
  (if u
      (delete-one! *conn* u)
      #f))

;;; CHECK

(define (check-unique serial-no)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (= u.serial-no ,serial-no)))))
  (if u #f #t))


(define (user-active-date serial-no)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (= u.serial-no ,serial-no)))))
  (if u
      (let ([updated-at (user-updated-at u)])
        (days-between (->datetime/local updated-at) (now)))
      -1))
