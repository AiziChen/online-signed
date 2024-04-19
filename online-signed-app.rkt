#lang racket/base

(require koyo/random
         koyo/l10n
         gregor
         canvas-list
         racket/gui/base
         racket/class
         racket/string
         racket/list
         srfi/29
         "models/users.rkt"
         "tools.rkt")

(current-output-port (open-output-file ".error.log" #:mode 'text #:exists 'append))
(load-locales! "resources/locales/")
(current-language 'zh)
(current-country 'cn)
;(current-language 'en)
;(current-country 'us)
(define *app-title* (translate 'app-title))
(define *refresh* (translate 'refresh))
(define *copy-selection* (translate 'copy-selection))
(define *copy-successful* (translate 'copy-successful))
(define *change-comment* (translate 'change-comment))
(define *comment* (translate 'comment))
(define *unregister-selection* (translate 'unregister-selection))
(define *unregister-successful* (translate 'unregister-successful))
(define *unregister-failed* (translate 'unregister-failed))
(define *delete-selection* (translate 'delete-selection))
(define *delete-successful* (translate 'delete-successful))
(define *delete-failed* (translate 'delete-failed))
(define *add* (translate 'add))
(define *id* (translate 'id))
(define *mac-address* (translate 'mac-address))
(define *active-code* (translate 'active-code))
(define *add-active-codes* (translate 'add-active-codes))
(define *add-active-codes?* (translate 'add-active-codes?))
(define *add-active-code* (translate 'add-active-code))
(define *add-active-code?* (translate 'add-active-code?))
(define *add-complete* (translate 'add-complete))
(define *add-failed* (translate 'add-failed))
(define *active-time* (translate 'active-time))
(define *remain-time* (translate 'remain-time))
(define *days* (translate 'days))
(define *hours* (translate 'hours))
(define *expired-time* (translate 'expired-time))
(define *update-time* (translate 'update-time))
(define *change-expired-time* (translate 'change-expired-time))
(define *change-success* (translate 'change-success))
(define *change-failed* (translate 'change-failed))
(define *search* (translate 'search))
(define *search-content* (translate 'search-content))
(define *prompt* (translate 'prompt))
(define *no-search-content* (translate 'no-search-content))
(define *current-time-v1* (translate 'current-time-v1))

(struct User (user-id mac serial-no updated-at expired-at comment) #:transparent)
(define *users '())

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
       [style '(multiple)]
       [choices '()]))


(define add-btn
  (new button%
       [parent top-hpanel]
       [label *add*]
       [callback
        (lambda (btn evt)
          (define countstr (get-text-from-user *add-active-codes?* *add-active-codes*))
          (when countstr
            (define choices (get-choices-from-user "设置有效时间" "" '("1小时" "1天" "1月" "1季" "半年" "1年")))
            (when (= 1 (length choices))
              (define choice (car choices))
              (define hours
                (case choice
                  [(0) 1]
                  [(1) 24]
                  [(2) (* 24 30)]
                  [(3) (* 24 90)]
                  [(4) (* 24 182)]
                  [(5) (* 24 365)]
                  [else 24]))
              (define count (string->number countstr))
              (when count
                (define active-codes
                  (for/list ([_ count])
                    (let loop ([random-str (generate-random-string 6)])
                      (when (not (check-unique random-str))
                        (loop (generate-random-string 6)))
                      (string-append (case choice
                                       [(2) "y"]
                                       [(3) "j"]
                                       [(4) "b"]
                                       [(5) "n"]
                                       [else ""])
                                     random-str))))
                (let* ([content (message-box *add-active-code*
                                             (string-append *add-active-code?* ":\n"
                                                            (apply string-append (map (lambda (v) (string-append v ",")) active-codes)))
                                             frame '(ok-cancel no-icon))])
                  (case content
                    [(ok)
                     (cond
                       [(user-insert-batch! active-codes hours)
                        =>
                        (lambda (_users)
                          ;; update list box after added all of the users
                          (init-users)
                          (message-box *add-active-code* *add-complete* frame '(ok no-icon)))]
                       [else
                        (message-box *add-active-code* *add-failed* frame '(ok no-icon))])]
                    [(cancel) (void)]))))))]))

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
          (let ([indexs (send users-list-box get-selections)]
                [time (send evt get-time-stamp)])
            (define serial-nos
              (string-join
               (for/list ([index indexs])
                 (define user-id (User-user-id (list-ref *users index)))
                 (User-serial-no (list-ref *users index)))
               ","))
            (send the-clipboard set-clipboard-string serial-nos time)
            (message-box *copy-selection* *copy-successful* frame '(ok no-icon))))]))

(define change-comment-btn
  (new button%
       [parent top-hpanel]
       [label *change-comment*]
       [callback
        (lambda (btn evt)
          (let ([indexs (send users-list-box get-selections)]
                [time (send evt get-time-stamp)])
            (unless (null? indexs)
              (define comment (get-text-from-user *change-comment* *comment*))
              (for ([index indexs])
                (define user-id (User-user-id (list-ref *users index)))
                (when comment
                  (let ([users (user-update-comment! user-id comment)])
                    (update-user (car users) index)))))))]))

(define unregister-btn
  (new button%
       [parent top-hpanel]
       [label *unregister-selection*]
       [callback
        (lambda (btn evt)
          (let ([indexs (send users-list-box get-selections)]
                [time (send evt get-time-stamp)])
            (unless (null? indexs)
              (define total
                (count values
                       (for/list ([index indexs])
                         (define user-id (User-user-id (list-ref *users index)))
                         (cond
                           [(user-unregister-mac! user-id)
                            =>
                            (lambda (users)
                              (update-user (car users) index))]
                           [else #f]))))
              (if (>= total (length indexs))
                  (message-box *unregister-selection* *unregister-successful* frame '(ok no-icon))
                  (message-box *unregister-selection* *unregister-failed* frame '(ok no-icon))))))]))

(define delete-btn
  (new button%
       [parent top-hpanel]
       [label *delete-selection*]
       [callback
        (lambda (btn evt)
          (let ([indexs (send users-list-box get-selections)]
                [time (send evt get-time-stamp)])
            (unless (null? indexs)
              (define total
                (count values
                       (for/list ([index indexs])
                         (define user-id (User-user-id (list-ref *users index)))
                         (delete-user-by-id! user-id))))
              (if (>= total (length indexs))
               (begin
                 (message-box *delete-selection* *delete-successful* frame '(ok no-icon))
                 (init-users))
               (message-box *delete-selection* *delete-failed* frame '(ok no-icon))))))]))

(define change-time-btn
  (new button%
       [parent top-hpanel]
       [label *change-expired-time*]
       [callback
        (lambda (btn evt)
          (let ([indexs (send users-list-box get-selections)])
            (unless (null? indexs)
              (define numstr (get-text-from-user *change-expired-time* "增加或减少到期时间，格式：+/-天.小时"))
              (when numstr
                (define total
                  (count values
                         (for/list ([index indexs])
                                   (define num (string->number numstr))
                                   (when num
                                     (define nums (string-split numstr "."))
                                     (define-values (days hours)
                                       (if (= (length nums) 2)
                                           (values (string->number (car nums)) (string->number (cadr nums)))
                                           (values (string->number (car nums)) 0)))
                                     (when (string-contains? numstr "-")
                                       (set! hours (- hours)))
                                     (define user-id (User-user-id (list-ref *users index)))
                                     (user-change-time! user-id days hours)))))
                (if (>= total (length indexs))
                    (begin
                      (message-box *change-expired-time* *change-success* frame '(ok no-icon))
                      (init-users))
                    (message-box *change-expired-time* *change-failed* frame '(ok no-icon)))))))]))

(define search-btn
  (new button%
       [parent top-hpanel]
       [label *search*]
       [callback
        (lambda (btn evt)
          (define activated-code (get-text-from-user *search-content* *active-code*))
          (when activated-code
            (define search-lst (get-users-like-active-code activated-code))
            (if (empty? search-lst)
                (message-box *prompt* *no-search-content* frame '(ok no-icon))
                (update-list-box
                 (for/list ([u search-lst])
                   (User (user-id u) (user-mac u)
                         (user-serial-no u) (user-updated-at u)
                         (user-expired-at u) (user-comment u)))))))]))

(define (update-user user index [with-ui #t])
  (when (and user index)
    (set! *users
          (list-set *users index
                    (User (user-id user) (user-mac user)
                          (user-serial-no user) (user-updated-at user)
                          (user-expired-at user) (user-comment user)))))
  (when with-ui (update-list-box *users)))


(define (init-users)
  (let* ([users (get-all-users)])
    (set! *users
          (for/list ([u users])
            (User (user-id u) (user-mac u)
                  (user-serial-no u) (user-updated-at u)
                  (user-expired-at u) (user-comment u)))))
  (update-list-box *users))

(define (update-list-box users)
  (define line
    (for/list ([u users])
      (define-values (days hours) (get-remain-info (User-expired-at u)))
      (string-append
       *id* ":" (number->string (User-user-id u))
       "|" *active-code* ":" (User-serial-no u)
       "|" *remain-time* ":" (string-append days *days* ", " hours *hours*)
       "|" *comment* ":" (User-comment u)
       "|" *expired-time* ":" (substring (datetime->iso8601 (User-expired-at u)) 0 16)
       ;"|" *update-time* ":" (substring (datetime->iso8601 (User-updated-at u)) 0 16)
       "|" *mac-address* ":" (User-mac u))))
  (send users-list-box set line))

(void
 (thread
  (lambda ()
    (init-users)
    (let loop ()
      (send frame set-label
            (string-append *app-title*
                           " "
                           *current-time-v1*
                           "("
                           (substring (datetime->iso8601 (now)) 0 19)
                           ")"))
      (sleep 1)
      (loop)))))

(send frame show #t)
