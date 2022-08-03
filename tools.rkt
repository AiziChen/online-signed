#lang racket/base

(require gregor
         racket/contract
         racket/string
         racket/math)

(provide
 xor-cipher!
 now-pass
 get-passes
 verify-pass
 get-active-date)

(define/contract (xor-cipher! data secret)
  (-> bytes? non-empty-string? bytes?)
  (define data-len (bytes-length data))
  (define secret-len (string-length secret))
  (for ([i data-len])
    (define rem (remainder i secret-len))
    (bytes-set! data i
                (bitwise-xor (bytes-ref data i)
                             (bitwise-ior (char->integer (string-ref secret rem)) rem))))
  data)

(define/contract (now-pass duration)
  (-> positive-integer? positive-integer?)
  (let* ([s (current-seconds)]
         [remain (remainder s duration)]
         [current-s (- s remain)])
    current-s))

(define/contract (get-passes duration)
  (-> positive-integer? (listof positive-integer?))
  (let* ([s (current-seconds)]
         [remain (remainder s duration)]
         [current-s (- s remain)]
         [next-s (+ current-s duration)]
         [previous-s (- current-s duration)])
    (list current-s previous-s next-s)))


(define/contract (verify-pass pass duration)
  (-> positive-integer? positive-integer? (or/c #f (listof positive-integer?)))
  (member pass (get-passes duration)))


(define (get-active-date active-at)
  (+ (days-between (->datetime/local active-at) (now)) 1))
