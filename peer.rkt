#lang racket/base

(require racket/format
         racket/function
         racket/match
         (only-in file/sha1 bytes->hex-string)
         "openssl.rkt"
         "machine.rkt"
         "message.rkt"
         "monad.rkt"
         "io.rkt"
         "port.rkt")


(define-message $peer ())
(define-message $peer:unavailable (digest))
(define-message $peer:unnamed (name))
(define-message $peer:receipt (hostname port expected-digest actual-digest))


(struct peer
  (client-hostname
   hash-function
   openssl-identity
   remotes
   requests
   responses
   server-hostname
   server-port
   server-queue-length
   server-timeout
   telemeter
   transfer-policy
   verify-sources))


(define (peer-store p name byte-string)
  (define digest (peer-digest p byte-string))
  (hash-set! (peer-responses p) digest byte-string)
  (hash-set! (peer-requests p) name digest))


(define (peer-request p name)
  (machine
   (λ (state)
     (let/ec return
       (define digest
         (hash-ref (peer-requests p)
                   name
                   (λ ()
                     (return (state-halt-with state ($peer:unnamed name))))))
       (define machines
         (for/list ([remote (peer-remotes p)])
           (define hostname (car remote))
           (define port (cdr remote))
           (peer-try-remote p hostname port digest)))
       (for/fold ([state* state]
                  #:result
                  (if (bytes? (state-get-value state*))
                      state*
                      (state-halt-with state*
                                       ($peer:unavailable digest))))
                 ([m (in-list machines)]
                  #:break (bytes? (state-get-value state*)))
         (m state*))))))


(define (peer-try-remote p hostname port expected-digest)
  (machine
   (λ (state)
     ; Connect
     (define context ((peer-openssl-identity p) 'server))
     (define-values (from-server to-server) (ssl-connect hostname port context))

     ; Send
     (write-bytes expected-digest to-server)
     (close-output-port to-server)

     ; Receive
     (define policy (peer-transfer-policy p))
     (define max-size (transfer-policy-max-size policy))
     (define-values (from-pipe to-pipe) (make-pipe))
     (transfer from-server to-pipe max-size policy)
     (define body (port->bytes from-pipe))
     (close-input-port from-pipe)
     (close-input-port from-server)

     ; Verify
     (define actual-digest (peer-digest p body))
     (define receipt ($peer:receipt hostname port expected-digest actual-digest))

     (state-set-value (state-add-message state receipt)
                      (equal? expected-digest actual-digest)))))



(define (peer-listen p)
  (define expected-hostname (peer-client-hostname p))
  (define queue-len (peer-server-queue-length p))
  (define port (peer-server-port p))
  (define request-length
    (bytes-length (peer-digest p #"")))
  (define listener
    (ssl-listen port
                queue-len
                #f
                expected-hostname
                ((peer-openssl-identity p) 'server?)))
  (define (loop)
    (peer-respond listener request-length p)
    (loop))
  (define cust
    (make-custodian))
  (define (stop)
    (custodian-shutdown-all cust))
  (parameterize ([current-custodian cust])
    (thread loop)
    stop))


(define (peer-respond listener request-length p)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (from-client to-client) (ssl-accept listener))
    (thread
     (λ ()
       (define responses (peer-responses p))
       (define key (read-bytes request-length from-client))
       (define response (hash-ref responses key #f))
       (when response
         (define machine
           (mdo from-tap := (source-tap response)
                (machine-effect (copy-port from-tap to-client))))
         (for-each (peer-telemeter p)
                   (state-get-messages (machine))))
       (close-output-port to-client)
       (close-input-port from-client)))
    (thread
     (λ ()
       (sleep (peer-server-timeout p))
       (custodian-shutdown-all cust)))))


(define (peer-digest p v)
  (string->bytes/utf-8
   (bytes->hex-string
    ((peer-hash-function p)
     (if (bytes? v)
         (open-input-bytes v)
         v)))))
