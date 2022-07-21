#lang racket/base

(require web-server/http/json
         web-server/http/request-structs
         net/base64
         json
         "tools.rkt"
         "models/users.rkt")

(provide
 code-verify)

(define *pass* "TEST")
(define *duration* 180)

(define (code-verify req)
  (with-handlers ([exn? (lambda (_) (response/jsexpr "error"))])
    (let* ([b64-data (request-post-data/raw req)]
           [en-data (base64-decode b64-data)]
           [jdata (bytes->jsexpr (xor-cipher! en-data *pass*))]
           [mac-str (hash-ref jdata 'mac #f)]
           [serial-no (hash-ref jdata 'serial-no #f)]
           [mpass (hash-ref jdata 'mpass)])
      (if (and mac-str
               serial-no
               (user-exists? serial-no)
               (verify-pass mpass *duration*))
          (let* ([pre-response-data (hasheq 'time (current-milliseconds)
                                            'random serial-no
                                            'expired? (user-expired? serial-no 30))]
                 [response-data (xor-cipher! (jsexpr->bytes pre-response-data) *pass*)]
                 [en-response-data (base64-encode response-data "")])
            (cond
              [(user-exists-all? mac-str serial-no)
               (response/jsexpr (bytes->string/utf-8 en-response-data))]
              [(user-update-mac! mac-str serial-no)
               (response/jsexpr (bytes->string/utf-8 en-response-data))]
              [else
               (response/jsexpr "error")]))
          (response/jsexpr "error")))))
