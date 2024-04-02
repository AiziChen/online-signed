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
 user-change-time!
 get-all-users
 get-users-like-active-code
 delete-user-by-id!
 check-unique
 user-active-date
 user-available-time)

;;; USER MODEL
(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [[mac ""] string/f]
   [serial-no string/f #:unique #:contract non-empty-string?]
   [[comment ""] string/f]
   [[created-at (now)] datetime/f]
   [[updated-at (now)] datetime/f]
   [active-at datetime/f #:nullable]
   ;; default available time: 1 day
   [[expired-at (+hours (now) 24)] datetime/f]))

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
(define (user-insert! mac serial-no)
  (insert-one! *conn*
               (make-user #:mac mac
                          #:serial-no serial-no)))

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
  (and u (update! *conn*
                  (update-user-mac u (lambda (_) mac))
                  (update-user-active-at u (lambda (_) (now))))))

(define (user-unregister-mac! id)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (= u.id ,id)))))
  (and u (update! *conn*
                  (update-user-mac u (lambda (_) ""))
                  (update-user-updated-at u (lambda (_) (now)))
                  (update-user-active-at u (lambda (_) (user-active-at u))))))

(define (user-update-comment! id comment)
  (define u
    (lookup *conn* (~> (from user #:as u)
                       (where (= u.id ,id)))))
  (and u (update! *conn* (update-user-comment u (lambda (_) comment)))))

(define (user-change-time! id days hours)
  (define u
    (lookup *conn* (~> (from user #:as u)
                       (where (= u.id ,id)))))
  (and u (update! *conn*
                  (update-user-expired-at u
                                          (lambda (_)
                                            (+hours (+days (user-expired-at u) days) hours))))))

;;; QUERY
(define (get-all-users)
  (for/list ([b (in-entities *conn*
                             (~> (from user #:as u)
                                 (order-by ([u.id #:desc]))))])
    b))

(define (get-users-like-active-code active-code)
  (for/list ([b (in-entities *conn*
                             (~> (from user #:as u)
                                 (where (like u.serial-no ,(string-append "%" active-code "%")))
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

(define (user-available-time serial-no)
  (define u
    (lookup *conn*
            (~> (from user #:as u)
                (where (= u.serial-no ,serial-no)))))
  (if u
      (seconds-between (now) (user-expired-at u))
      (- (current-seconds))))
