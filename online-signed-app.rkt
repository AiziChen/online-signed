#lang racket/base

(require koyo/random
         koyo/l10n
         racket/gui/base
         racket/class
         gregor
         "models/users.rkt")

(load-locales! "resources/locales/")
(define *app-title* (translate 'app-title))
(define *add-active-code* (translate 'add-active-code))
(define *add-active-code?* (translate 'add-active-code?))
(define *add-complete* (translate 'add-complete))
(define *add-failed* (translate 'add-failed))
(define *refresh* (translate 'refresh))
(define *copy-selection* (translate 'copy-selection))
(define *copy-successful* (translate 'copy-successful))
(define *delete-selection* (translate 'delete-selection))
(define *delete-successful* (translate 'delete-successful))
(define *delete-failed* (translate 'delete-failed))
(define *add* (translate 'add))
(define *mac-address* (translate 'mac-address))
(define *active-code* (translate 'active-code))
(define *user-expired?* (translate 'user-expired?))
(define *update-time* (translate 'update-time))

(struct UserData (user-id mac serial-no datetime) #:transparent)
(define *data* #f)

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
  (new list-box%
       [parent center-vpanel]
       [label #f]
       [choices '()]))


(define add-btn
  (new button%
       [parent top-hpanel]
       [label *add*]
       [callback
        (lambda (btn evt)
          (let loop ([random-str (generate-random-string 6)])
            (when (not (check-unique random-str))
              (loop (generate-random-string 6)))
            (let* ([content (message-box *add-active-code*
                                         (string-append *add-active-code?* ":\n" random-str)
                                         frame '(ok-cancel no-icon))])
              (case content
                [(ok)
                 (if (user-insert! "" random-str)
                     (begin (update-users-list)
                            (message-box *add-active-code* *add-complete* frame '(ok no-icon)))
                     (message-box *add-active-code* *add-failed* frame '(ok no-icon)))]
                [(cancel) (void)]))))]))

(define refresh-btn
  (new button%
       [parent top-hpanel]
       [label *refresh*]
       [callback
        (lambda (btn evt)
          (update-users-list))]))

(define copy-btn
  (new button%
       [parent top-hpanel]
       [label *copy-selection*]
       [callback
        (lambda (btn evt)
          (let ([selections (send users-list-box get-selections)]
                [time (send evt get-time-stamp)])
            (for ([index selections])
              (send the-clipboard set-clipboard-string (UserData-serial-no (list-ref *data* index)) time)
              (message-box *copy-selection* *copy-successful* frame '(ok no-icon)))))]))

(define delete-btn
  (new button%
       [parent top-hpanel]
       [label *delete-selection*]
       [callback
        (lambda (btn evt)
          (let ([selections (send users-list-box get-selections)]
                [time (send evt get-time-stamp)])
            (for ([index selections])
              (if (delete-user-by-id! (UserData-user-id (list-ref *data* index)))
                  (let ()
                    (message-box *delete-selection* *delete-successful* frame '(ok no-icon))
                    (update-users-list))
                  (message-box *delete-selection* *delete-failed* frame '(ok no-icon))))))]))


(define (update-users-list)
  (let ([users (get-all-users)])
    (let ([line
           (for/list ([u users])
             (string-append
              "id:" (number->string (user-id u))
              "|" *active-code* ":" (user-serial-no u)
              "|" *mac-address* ":" (user-mac u)
              "|" *user-expired?* ":" (if (user-expired? (user-serial-no u) 30) "yes" "no")
              "|" *update-time* ":" (datetime->iso8601 (user-updated-at u))))]
          [data
           (for/list ([u users])
             (UserData (user-id u) (user-mac u) (user-serial-no u) (user-updated-at u)))])
      (set! *data* data)
      (send users-list-box set line))))

(void
 (thread
  (lambda ()
    (update-users-list))))

(send frame show #t)
