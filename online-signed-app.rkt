#lang racket/base

(require koyo/random
         racket/gui/base
         racket/class
         db
         "models/users.rkt")

(define frame
  (new frame% [label "数据库查看工具"]
       [width 560]
       [height 500]))

(define add-btn
  (new button%
       [parent frame]
       [label "添加"]
       [callback
        (lambda (btn evt)
          (let* ([random-str (generate-random-string)]
                 [content (message-box "添加激活码" (string-append "是否添加以下激活码到数据库中:\n" random-str) frame '(ok-cancel no-icon))])
            (case content
              [(ok)
               (if (user-insert! "" random-str)
                   (begin (update-users-list)
                          (message-box "添加激活码" "添加成功" frame '(ok no-icon)))
                   (message-box "添加激活码" "添加失败" frame '(ok no-icon)))]
              [(cancel) (void)])))]))

#;
(define top-hpanel
  (new horizontal-panel%
       [parent frame]))

(define users-list-box
  (new list-box%
       [parent frame]
       [label #f]
       [choices '()]))


(define (update-users-list)
  (let ([users (get-all-users)])
      (send users-list-box set
            (for/list ([u users])
              (string-append
               "mac地址:" (user-mac u)
               "|激活码:" (user-serial-no u))))))

(void
 (thread
  (lambda ()
    (update-users-list))))

(send frame show #t)
