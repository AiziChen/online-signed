#lang racket/base

(require web-server/http/json
         net/base64
         json
         "tools.rkt"
         "models/users.rkt")

(provide
 code-verify)

(define *pass* "test")

(define (code-verify req b64-data)
  (with-handlers ([exn? (lambda (_) (response/jsexpr "error"))])
    (let* ([en-data (base64-decode (string->bytes/utf-8 b64-data))]
           [jdata (bytes->jsexpr (xor-cipher en-data *pass*))]
           [mac-str (hash-ref jdata 'mac #f)]
           [serial-no (hash-ref jdata 'serial-no #f)])
      (if (and mac-str serial-no (user-exists? serial-no))
          (let* ([pre-response-data (hasheq 'time (current-milliseconds)
                                            'random serial-no)]
                 [response-data (xor-cipher (jsexpr->bytes pre-response-data) *pass*)]
                 [en-response-data (base64-encode response-data "")])
            (cond
              [(user-exists-all? mac-str serial-no)
               (response/jsexpr (bytes->string/utf-8 en-response-data))]
              [(user-update-mac! mac-str serial-no)
               (response/jsexpr (bytes->string/utf-8 en-response-data))]
              [else
               (response/jsexpr "error")]))
          (response/jsexpr "error")))))
