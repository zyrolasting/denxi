#lang racket/base

(provide (all-defined-out))

(require racket/contract
         racket/function
         racket/exn
         racket/place
         "config.rkt"
         "logging.rkt"
         "message.rkt"
         "message-pump.rkt")

; For use in a creating place
(struct worker (id channel [idle? #:mutable])
  #:property prop:evt (struct-field-index channel)
  #:property prop:procedure
  (λ (self v)
    (when v
      (place-channel-put (worker-channel self) v))
    (set-worker-idle?! self (not v)) ; <-- read this out loud lol
    self))

; For use in a created place
(struct worker-state (id pch value)
  #:transparent
  #:property prop:evt (struct-field-index pch)
  #:property prop:procedure
  (λ (self v)
    (place-channel-put (worker-state-pch self) v)
    self))

(define (worker-main pch handle-message [state (worker-state #f pch #f)])
  (with-handlers ([exn:break? void]
                  [exn? (λ (e)
                          (state ($crash (worker-state-id state)
                                         (exn->string e))))]
                  [(const #t)
                   (λ (e)
                     (state ($crash (worker-state-id state)
                                    (format "Value raised: ~s~n" e))))])
    (define job (sync state))
    (define next-state (handle-message state job))
    (next-state ($done (worker-state-id next-state) job))
    (worker-main pch handle-message next-state)))

(define (start state id value)
  (struct-copy worker-state state
               [id id]
               [value value]))

(define (stop state)
  (exit 0)
  state)

(define-message-pump (handle-worker-message worker-state? default-message-handler)
  start
  stop)
