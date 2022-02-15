#lang racket/base

; Add source/sink semantics to TCP

(provide (struct-out tcp-conduit))

(require racket/function
         racket/generic
         racket/match
         racket/tcp
         "machine.rkt"
         "port.rkt"
         "sink.rkt"
         "source.rkt")

(struct tcp-connection (i o))

(struct tcp-conduit
  (server-hostname
   server-port
   client-hostname
   client-port
   measure
   policy
   sink
   [connection #:mutable])

  #:methods gen:source
  [(define (source-tap source)
     (machine-rule
      (tcp-connection-i (tcp-conduit-connection! source))))
   (define (source-measure source)
     (machine-rule
      ((tcp-conduit-measure source)
       (peeking-input-port (tcp-connection-i (tcp-conduit-connection! source))))))]

  #:methods gen:sink
  [(define/generic sink-source/generic sink-source)
   (define (sink-source sink)
     (machine
      (λ (state)
        (if (tcp-connection-closed? (tcp-conduit-connection sink))
            (state-set-value (sink-source/generic (tcp-conduit-sink sink)))
            (state-halt-with ($open))))))
   (define (sink-open sink)
     (machine-rule (tcp-connection-o (tcp-conduit-connection! sink))))
   (define (sink-keep sink)
     (machine-effect (tcp-connection-close (tcp-conduit-connection sink))))
   (define (sink-drop sink)
     (machine-effect (tcp-connection-close (tcp-conduit-connection sink))))
   (define (sink-policy sink)
     (machine-rule (tcp-conduit-policy sink)))])


(define (tcp-connection-close tcpc)
  (close-input-port (tcp-connection-i tcpc))
  (close-output-port (tcp-connection-o tcpc)))


(define (tcp-connection-closed? tcpc)
  (or (port-closed? (tcp-connection-i tcpc))
      (port-closed? (tcp-connection-o tcpc))))


(define (tcp-connection-open . args)
  (call-with-values (thunk (apply tcp-connect args))
                    tcp-connection))


(define (tcp-conduit-connection! conduit)
  (match-define (tcp-conduit sh sp ch cp _ _ _ connection) conduit)
  (unless connection
    (set-tcp-conduit-connection! conduit (tcp-connection-open sh sp ch cp)))
  (tcp-conduit-connection conduit))


(module+ test
  (require racket/contract
           "test.rkt")

  (define (ephemeral-port listener)
    (call-with-values (thunk (tcp-addresses listener #t))
                      (compose (curry findf (>/c 0))
                               list)))

  (define (server discuss)
    (define parent (current-thread))
    (define child
      (thread
       (λ ()
         (define listener (tcp-listen 0 1 #t "localhost"))
         (thread-send parent (ephemeral-port listener))
         (define-values (from-client to-client) (tcp-accept listener))
         (discuss from-client to-client)
         (close-output-port to-client)
         (close-input-port from-client)
         (tcp-close listener))))
    (values (thread-receive)
            child))

  
  (define (shipping-server response)
    (server
     (λ (from-client to-client)
       (write-byte (bytes-length response) to-client)
       (write-bytes response to-client))))

  
  (define (receiving-server)
    (define parent (current-thread))
    (server
     (λ (from-client to-client)
       (thread-send parent (read from-client)))))

  
  (test tcp-source
        (define-values (server-port server-thread)
          (shipping-server #"abc"))

        (define conduit
          (tcp-conduit "localhost"
                    server-port
                    #f
                    (add1 server-port)
                    read-byte
                    full-trust-transfer-policy
                    (memory-sink #"" full-trust-transfer-policy)
                    #f))

        (define measure (source-measure conduit))
        (define tap (source-tap conduit))
        (assert (equal? (state-get-value (measure)) 3))
        (assert (equal? (port->bytes (state-get-value (tap))) #"\3abc"))
        (thread-wait server-thread))

  (test tcp-sink
        (define-values (server-port server-thread)
          (receiving-server))

        (define conduit
          (tcp-conduit "localhost"
                    server-port
                    #f
                    (add1 server-port)
                    read-byte
                    full-trust-transfer-policy
                    (memory-sink #"" full-trust-transfer-policy)
                    #f))

        (define to-server (state-get-value ((sink-open conduit))))
        (write 100 to-server)
        (flush-output to-server)
        (close-output-port to-server)
        (assert (equal? (thread-receive) 100))
        (thread-wait server-thread)))
