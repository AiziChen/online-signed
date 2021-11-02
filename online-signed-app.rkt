#lang racket/base

(require koyo/random
         racket/gui/base
         racket/class
         gregor
         db
         "models/users.rkt")

(define *data* #f)

(define frame
  (new frame% [label "数据库查看工具"]
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
       [label "添加"]
       [callback
        (lambda (btn evt)
          (let loop ([random-str (generate-random-string 6)])
            (when (not (check-unique random-str))
              (loop (generate-random-string 6)))
            (let* ([content (message-box "添加激活码" (string-append "是否添加以下激活码到数据库中:\n" random-str) frame '(ok-cancel no-icon))])
              (case content
                [(ok)
                 (if (user-insert! "" random-str)
                     (begin (update-users-list)
                            (message-box "添加激活码" "添加成功" frame '(ok no-icon)))
                     (message-box "添加激活码" "添加失败" frame '(ok no-icon)))]
                [(cancel) (void)]))))]))

(define refresh-btn
  (new button%
       [parent top-hpanel]
       [label "刷新"]
       [callback
        (lambda (btn evt)
          (update-users-list))]))

(define copy-btn
  (new button%
       [parent top-hpanel]
       [label "复制选中"]
       [callback
        (lambda (btn evt)
          (let ([selections (send users-list-box get-selections)]
                [time (send evt get-time-stamp)])
            (for ([index selections])
              (send the-clipboard set-clipboard-string (list-ref *data* index) time)
              (message-box "复制选中" "复制成功" frame '(ok no-icon)))))]))


(define (update-users-list)
  (let ([users (get-all-users)])
    (let ([data
           (for/list ([u users] [i (length users)])
             (let ([s
                    (string-append
                     "id:" (number->string (user-id u))
                     "|mac地址:" (user-mac u)
                     "|激活码:" (user-serial-no u)
                     "|更新时间:" (datetime->iso8601 (user-updated-at u)))])
               s))])
      (set! *data* data)
      (send users-list-box set data))))

(void
 (thread
  (lambda ()
    (update-users-list))))

(send frame show #t)
