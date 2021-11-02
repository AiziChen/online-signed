#lang racket/base

(require racket/contract
         racket/string)

(provide xor-cipher)

(define/contract (xor-cipher data secret)
  (-> bytes? non-empty-string? bytes?)
  (let ([secret-len (string-length secret)])
    (list->bytes
     (let loop ([ls (bytes->list data)]
                [i 0])
       (if (null? ls)
           '()
           (let ([rem (remainder i secret-len)])
             (cons (bitwise-xor
                    (bitwise-xor (car ls) (char->integer (string-ref secret rem)))
                    rem)
                   (loop (cdr ls) (+ i 1)))))))))
