#lang racket/base
(require koyo/dispatch
         koyo/url
         koyo/cors
         koyo/static
         koyo/random
         web-server/dispatch
         web-server/web-server
         web-server/servlet-dispatch
         web-server/http/json
         net/base64
         json
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         racket/list
         "tools.rkt"
         "abstract.rkt")

(define-values (dispatch url roles)
  (dispatch-rules+roles
   [("") (lambda (_) (response/jsexpr "server is on."))]
   [("code-verify" (string-arg)) code-verify]))


(current-cors-origin "*")

(define (stack handler)
  (wrap-cors handler))

(define dispatchers
  (list
   (dispatch/servlet (stack dispatch))
   (make-static-dispatcher "static")))

(define stop
  (serve
   #:dispatch (apply sequencer:make (filter-map values dispatchers))
   #:listen-ip #f
   #:port 80))

(with-handlers
  ([exn:break? (lambda (_) (stop))])
  (sync/enable-break never-evt))
