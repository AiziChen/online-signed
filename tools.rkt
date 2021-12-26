#lang racket/base

(require racket/contract
         racket/string)

(provide xor-cipher)

(define/contract (xor-cipher data secret)
  (-> bytes? non-empty-string? bytes?)
  (define data-len (bytes-length data))
  (define secret-len (string-length secret))
  (let lp ([i 0])
    (cond
      [(= i data-len) data]
      [else
       (define rem (remainder i secret-len))
       (bytes-set! data i
                   (bitwise-xor (bytes-ref data i)
                                (bitwise-ior (char->integer (string-ref secret rem)) rem)))
       (lp (+ i 1))])))
