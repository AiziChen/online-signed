#lang racket/base

(require koyo/random
         koyo/l10n
         gregor
         canvas-list
         racket/gui/base
         racket/class
         racket/string
         "models/users.rkt"
         "tools.rkt")

(load-locales! "resources/locales/")
(define *app-title* (translate 'app-title))
(define *add-active-code* (translate 'add-active-code))
(define *add-active-code?* (translate 'add-active-code?))
(define *add-complete* (translate 'add-complete))
(define *add-failed* (translate 'add-failed))
(define *refresh* (translate 'refresh))
(define *copy-selection* (translate 'copy-selection))
(define *copy-successful* (translate 'copy-successful))
(define *change-comment* (translate 'change-comment))
(define *unregister-selection* (translate 'unregister-selection))
(define *unregister-successful* (translate 'unregister-successful))
(define *unregister-failed* (translate 'unregister-failed))
(define *delete-selection* (translate 'delete-selection))
(define *delete-successful* (translate 'delete-successful))
(define *delete-failed* (translate 'delete-failed))
(define *add* (translate 'add))
(define *mac-address* (translate 'mac-address))
(define *active-code* (translate 'active-code))
(define *active-date* (translate 'active-date))
(define *update-time* (translate 'update-time))

(struct User (user-id mac serial-no updated-at active-date comment) #:transparent)
(define *users (make-hasheqv))

(define frame
  (new frame% [label *app-title*]
       [width 560]
       [height 500]))

(define top-hpanel
  (new horizontal-panel%
       [parent frame]
       [stretchable-height #f]
       [alignment '(center top)]))

(define center-vpanel
  (new vertical-panel%
       [parent frame]))


(define users-list-box
  (new canvas-list%
       [parent center-vpanel]
       [label #f]
       [items '()]))


(define add-btn
  (new button%
       [parent top-hpanel]
       [label *add*]
       [callback
        (lambda (btn evt)
          (define countstr (get-text-from-user "How much active code would you add?" "active code numbers"))
          (define count (and countstr (string->number countstr)))
          (when count
            (define active-codes
              (for/list ([_ count])
                (let loop ([random-str (generate-random-string 6)])
                  (when (not (check-unique random-str))
                    (loop (generate-random-string 6)))
                  random-str)))
            (let* ([content (message-box *add-active-code*
                                         (string-append *add-active-code?* ":\n"
                                                        (apply string-append (map (lambda (v) (string-append v ",")) active-codes)))
                                         frame '(ok-cancel no-icon))])
              (case content
                [(ok)
                 (cond
                   [(user-insert-batch! active-codes)
                    =>
                    (lambda (users)
                      (for ([user users])
                        (update-user user (hash-count *users) #f))
                      ;; update list box after added all of the users
                      (init-users)
                      (message-box *add-active-code* *add-complete* frame '(ok no-icon)))]
                   [else
                    (message-box *add-active-code* *add-failed* frame '(ok no-icon))])]
                [(cancel) (void)]))))]))

(define refresh-btn
  (new button%
       [parent top-hpanel]
       [label *refresh*]
       [callback
        (lambda (btn evt)
          (init-users))]))

(define copy-btn
  (new button%
       [parent top-hpanel]
       [label *copy-selection*]
       [callback
        (lambda (btn evt)
          (let ([index (send users-list-box get-selected-index)]
                [time (send evt get-time-stamp)])
            (when index
              (define user-id (User-user-id (list-ref (hash-values *users) index)))
              (send the-clipboard set-clipboard-string (User-serial-no (hash-ref *users user-id)) time)
              (message-box *copy-selection* *copy-successful* frame '(ok no-icon)))))]))

(define change-comment-btn
  (new button%
       [parent top-hpanel]
       [label *change-comment*]
       [callback
        (lambda (btn evt)
          (let ([index (send users-list-box get-selected-index)]
                [time (send evt get-time-stamp)])
            (when index
              (define user-id (User-user-id (list-ref (hash-values *users) index)))
              (let ([comment (get-text-from-user *change-comment* "Comment Content")])
                (when (and comment (non-empty-string? comment))
                  (let ([users (user-update-comment! user-id comment)])
                    (update-user (car users) user-id)))))))]))

(define unregister-btn
  (new button%
       [parent top-hpanel]
       [label *unregister-selection*]
       [callback
        (lambda (btn evt)
          (let ([index (send users-list-box get-selected-index)]
                [time (send evt get-time-stamp)])
            (when index
              (define user-id (User-user-id (list-ref (hash-values *users) index)))
              (cond
                [(user-unregister-mac! user-id)
                 =>
                 (lambda (users)
                   (message-box *unregister-selection* *unregister-successful* frame '(ok no-icon))
                   (update-user (car users) user-id))]
                [else
                 (message-box *unregister-selection* *unregister-failed* frame '(ok no-icon))]))))]))

(define delete-btn
  (new button%
       [parent top-hpanel]
       [label *delete-selection*]
       [callback
        (lambda (btn evt)
          (let ([index (send users-list-box get-selected-index)]
                [time (send evt get-time-stamp)])
            (when index
              (define user-id (User-user-id (list-ref (hash-values *users) index)))
              (if (delete-user-by-id! user-id)
                  (let ()
                    (message-box *delete-selection* *delete-successful* frame '(ok no-icon))
                    (hash-remove! *users user-id)
                    (update-list-box))
                  (message-box *delete-selection* *delete-failed* frame '(ok no-icon))))))]))

(define (update-user user uid [with-ui #t])
  (when (and user uid)
    (hash-set! *users uid
               (User (user-id user) (user-mac user)
                     (user-serial-no user) (user-updated-at user)
                     (get-active-date (user-updated-at user)) (user-comment user))))
  (when with-ui (update-list-box)))


(define (init-users)
  (hash-clear! *users)
  (let* ([users (get-all-users)])
    (for ([u users])
      (hash-set! *users (user-id u)
                 (User (user-id u) (user-mac u)
                       (user-serial-no u) (user-updated-at u)
                       (get-active-date (user-updated-at u)) (user-comment u)))))
  (update-list-box))

(define (update-list-box)
  (define line
    (for/list ([(id u) *users])
      (string-append
       "id:" (number->string (User-user-id u))
       "|" *active-code* ":" (User-serial-no u)
       "|" *mac-address* ":" (User-mac u)
       "|" *active-date* ":" (number->string (User-active-date u))
       "|" *change-comment* ":" (User-comment u)
       "|" *update-time* ":" (datetime->iso8601 (User-updated-at u)))))
  (send users-list-box set-items line))

(void
 (thread
  (lambda ()
    (init-users))))

(send frame show #t)
