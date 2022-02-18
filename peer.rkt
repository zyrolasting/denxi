#lang racket/base

(require racket/format
         racket/function
         racket/match
         (only-in file/sha1 bytes->hex-string)
         openssl
         "machine.rkt"
         "message.rkt"
         "monad.rkt"
         "io.rkt"
         "port.rkt"
         "sink.rkt"
         "source.rkt")

(define-message $precosystem ())
(define-message $precosystem:undefined (hostname port name))
(define-message $precosystem:unavailable (digest))


(struct response
  (name sink))


(struct request (peer digest size)
  #:methods gen:source
  [(define (source-measure source)
     (machine-rule (request-size source)))
   (define (source-tap source)
     (request-tap source))])


(define (request-tap req)
  (machine
   (λ (state)
     (define resp
       (request-response req))

     (define tap-response
       (mdo sink := (response-sink resp)
            source := (sink-source resp)
            (source-tap source)))

     (define fail
       (machine-halt-with ($precosystem:undefined (request-digest req))))
     
     (define submachine
       (if resp
           tap-response
           fail))

     (submachine state))))


(define (request-response req)
  (define p (request-peer req))
  (define expected-digest (request-digest req))
  (hash-ref (peer-responses p)
            expected-digest
            (λ ()
              (for/or ([rm (peer-remotes p)])
                (peer-try-remote p rm expected-digest)))))


;--------------------------------------------------------------------------------

(struct remote
  (hostname port))


(struct peer
  (requests
   responses
   remotes
   telemeter
   transfer-policy
   client-hostname
   client-limit
   hash-function
   server-hostname
   server-queue-length
   server-port
   server-timeout
   ciphers
   verify-sources
   certificate-chain
   private-key))


(define (peer-request p name)
  (hash-ref (peer-requests p)
            name
            #f))


(define (peer-try-remote p rm expected-digest)
  ; Connect
  (define hostname (remote-hostname rm))
  (define port (remote-port rm))
  (define context (ssl-context (ssl-make-client-context 'auto) p))
  (define-values (from-server to-server) (ssl-connect hostname port context))

  ; Send
  (write-bytes expected-digest to-server)
  (close-output-port to-server)

  ; Receive
  (define limit (peer-client-limit p))
  (define body (read-bytes limit from-server))

  ; Verify
  (and (equal? expected-digest (peer-digest p body))
       body))


(define (peer-store p name sink)
  (mdo stored := (sink-source sink)
       from-tap := (source-tap stored)
       size := (source-measure stored)
       (machine-effect
        (let ([digest (peer-digest p from-tap)])
          (hash-set! (peer-responses p)
                     digest
                     (response sink))
          (hash-set! (peer-requests p)
                     name
                     (request digest size))))))


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
                (ssl-context (ssl-make-server-context 'auto)
                             p)))
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
           (mdo source := (sink-source response)
                from-tap := (source-tap source)
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


(define (ssl-context ctx p)
  (define ciphers (peer-ciphers p))
  (define vsources (peer-verify-sources p))
  (define certs (peer-certificate-chain p))
  (define key (peer-private-key p))

  (define has-verify-sources?
    (not (null? vsources)))

  (for ([src (in-list vsources)])
    (ssl-load-verify-source! ctx src))

  (ssl-set-verify! ctx has-verify-sources?)
  (ssl-set-verify-hostname! ctx has-verify-sources?)

  (ssl-set-ciphers! ctx ciphers)
  (when (and certs key)
    (ssl-load-certificate-chain! ctx certs)
    (ssl-load-private-key! ctx key))

  (ssl-seal-context! ctx)
  ctx)
