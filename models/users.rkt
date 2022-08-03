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
 user-insert-batch!
 user-update-mac!
 user-unregister-mac!
 user-update-comment!
 get-all-users
 delete-user-by-id!
 check-unique
 user-active-date)

;;; USER MODEL
(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [[mac ""] string/f]
   [serial-no string/f #:unique #:contract non-empty-string?]
   [[comment ""] string/f]
   [[is-active #f] boolean/f]
   [[active-at (now)] datetime/f]
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

(define (user-insert-batch! serial-nos)
  (apply insert!
         (cons *conn* (for/list ([serial-no serial-nos])
                        (make-user #:serial-no serial-no)))))

;;; UPDATE
(define (user-update-mac! mac serial-no)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (and (= mac "")
                            (= u.serial-no ,serial-no))))))
  (if (user-is-active u)
      (and u (update! *conn*
                      (update-user-mac u (lambda (_) mac))
                      (update-user-active-at u (lambda (_) (user-active-at u)))))
      (and u (update! *conn*
                      (update-user-mac u (lambda (_) mac))
                      (update-user-is-active u (lambda (_) #t))
                      (update-user-active-at u (lambda (_) (now)))))))

(define (user-unregister-mac! id)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (= u.id ,id)))))
  (and u (update! *conn* (update-user-mac u (lambda (_) "")))))

(define (user-update-comment! id comment)
  (define u
    (lookup *conn* (~> (from user #:as u)
                       (where (= u.id ,id)))))
  (and u (update! *conn* (update-user-comment u (lambda (_) comment)))))


;;; QUERY
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
      (let ([active-at (user-active-at u)])
        (+ (days-between (->datetime/local active-at) (now)) 1))
      -1))
