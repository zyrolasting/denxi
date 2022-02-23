#lang racket/base

; P2P protocol for content addressable data.  Meant for small scale
; operation, namely a seed ecosystem. Storage is limited to memory to
; impose this limitation.


(require racket/contract)
(provide (struct-out peer)
         (struct-out $peer)
         (struct-out $peer:listening)
         (struct-out $peer:data)
         (struct-out $peer:data:new)
         (struct-out $peer:data:unavailable)
         (struct-out $peer:request)
         (struct-out $peer:request:sent)
         (struct-out $peer:request:response)
         (contract-out
          [peer-store!
           (-> peer? bytes? void?)]
          [peer-request
           (-> peer? bytes? (or/c #f bytes?))]
          [peer-listen
           (->* (peer?) (#:reuse? any/c) (-> void?))]
          [peer-digest
           (-> peer? (or/c bytes? input-port?) bytes?)]
          [peer-motd-digest
           (-> peer? bytes?)]
          [peer-motd-digest?
           (-> bytes? boolean?)]))

(require racket/format
         racket/function
         racket/match
         (only-in file/sha1 bytes->hex-string)
         "openssl.rkt"
         "message.rkt"
         "monad.rkt"
         "io.rkt"
         "port.rkt")


; Messages for telemetry. Peers can store their own data, but will
; ask other peers for a copy of what they don't have. Data not among
; peers is "unavailable".

(define-message $peer ())
(define-message $peer:listening $peer (hostname port timestamp))

(define-message $peer:data $peer ())
(define-message $peer:data:new $peer:data (digest size))
(define-message $peer:data:unavailable $peer:data (digest))

(define-message $peer:request $peer (hostname port))
(define-message $peer:request:error $peer:request (message))
(define-message $peer:request:response $peer:request (digest))
(define-message $peer:request:sent $peer:request (digest))


(struct peer
  (hash-function
   motd-write
   openssl-identity
   remotes
   responses
   server-hostname
   server-port
   server-queue-length
   server-timeout
   telemeter
   transfer-policy))


(define (peer-log p m)
  ((peer-telemeter p) m))


(define (peer-store! p byte-string)
  (define responses (peer-responses p))
  (define digest (peer-digest p byte-string))
  (define len (bytes-length byte-string))
  (unless (hash-has-key? responses digest)
    (hash-set! responses digest byte-string)
    (peer-log p ($peer:data:new digest len))))


(define (peer-request p digest)
  (let/ec abort
    (for ([remote (in-list (peer-remotes p))])
      (define hostname (car remote))
      (define port (cdr remote))
      (define (disregard-remote e)
        (peer-log ($peer:request:error hostname port (exn-message e)))
        #f)
      (with-handlers ([(and/c exn? (negate exn:break?)) disregard-remote])
        (define data (peer-try-remote p hostname port digest))
        (when data (abort data))))
    (peer-log p ($peer:data:unavailable digest))
    #f))

      

(define (peer-try-remote p hostname port expected-digest)
  ; Connect
  (define context (make-openssl-context (peer-openssl-identity p) 'client))
  (define-values (from-server to-server) (ssl-connect hostname port context))

  ; Send
  (write-bytes expected-digest to-server)
  (close-output-port to-server)
  (peer-log p ($peer:request:sent hostname port expected-digest))

  ; Receive
  (define policy (peer-derived-transfer-policy p))
  (define max-size (transfer-policy-max-size policy))
  (define-values (from-pipe to-pipe) (make-pipe))
  (transfer from-server to-pipe max-size policy)
  (close-output-port to-pipe)
  (define body (port->bytes from-pipe))
  (close-input-port from-server)
  (close-input-port from-pipe)

  ; Verify
  (let/ec return
    (when (peer-motd-digest? expected-digest)
      (peer-log p ($peer:request:response hostname port expected-digest))
      (return body))
    (define actual-digest (peer-digest p body))
    (peer-log p ($peer:request:response hostname port actual-digest))
    (when (equal? expected-digest actual-digest)
      (peer-store! p body)
      (return body))
    #f))


(define (peer-derived-transfer-policy p)
  (define policy (peer-transfer-policy p))
  (struct-copy transfer-policy policy
               [telemeter
                (λ (m)
                  (unless ($transfer:progress? ($transfer:scope-message m))
                    (peer-log p m))
                  ((transfer-policy-telemeter policy) m))]))


(define (peer-listen #:reuse? [reuse? #f] p)
  (define hostname (peer-server-hostname p))
  (define queue-len (peer-server-queue-length p))
  (define port (peer-server-port p))
  (define request-length (bytes-length (peer-digest p #"")))
  (define listener
    (ssl-listen port
                queue-len
                reuse?
                hostname
                (make-openssl-context (peer-openssl-identity p) 'server)))
  (define (loop)
    (peer-respond listener request-length p)
    (loop))
  (define cust
    (make-custodian))
  (define (stop)
    (custodian-shutdown-all cust))
  (parameterize ([current-custodian cust])
    (thread loop)
    (peer-log p ($peer:listening hostname port (current-seconds)))
    stop))


(define (peer-respond listener request-length p)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (from-client to-client) (ssl-accept listener))
    (thread
     (λ ()
       (with-handlers ([(negate exn:break?) (peer-telemeter p)])
         (define responses (peer-responses p))
         (define key (read-bytes request-length from-client))
         (if (peer-motd-digest? key)
             ((peer-motd-write p) to-client)
             (write-bytes (hash-ref responses key #"") to-client))
         (flush-output to-client)
         (close-output-port to-client)
         (close-input-port from-client))))
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


(define (peer-motd-digest p)
  (make-bytes (bytes-length (peer-digest p #"")) 0))


(define (peer-motd-digest? v)
  (for/and ([b (in-bytes v)])
    (zero? b)))


(module+ test
  (require "test.rkt"
           racket/async-channel)

  (define channel
    (make-async-channel))

  (define (next-message)
    (async-channel-try-get channel))
  
  (test peer-comm
        (define test.pem
          (collection-file-path "test.pem" "openssl"))

        (define hostname
          "127.0.0.1")

        (define port
          7777)
        
        (define p
          (peer sha256-bytes
                (λ (to-client)
                  (display "hello world" to-client))
                (openssl-identity #t
                                  #f
                                  "ALL:!COMPLEMENTOFDEFAULT:!eNULL"
                                  (list test.pem)
                                  test.pem
                                  (openssl-private-key test.pem #t #f))
                (list (cons hostname port)) ; talk to self
                (make-hash)
                hostname
                port
                1
                2
                (curry async-channel-put channel)
                full-trust-transfer-policy))

        (peer-store! p #"abc")
        (define digest (car (hash-keys (peer-responses p))))
        (compare equal? (next-message) ($peer:data:new digest 3))

        (define stop (peer-listen p #:reuse? #t))
        (assert ($peer:listening? (next-message)))

        (define (test-request digest expected-response)
          (assert (equal? (peer-request p digest) expected-response))
          (assert (equal? (next-message) ($peer:request:sent hostname port digest)))
          (assert (equal? (next-message) ($peer:request:response hostname port digest))))

        (test-request digest #"abc")
        (test-request (peer-motd-digest p) #"hello world")))
