#lang racket/base


(require racket/contract
         racket/exn
         "machine.rkt"
         "message.rkt")


(define-message $interpreter ())
(define-message $interpreter:done $interpreter ())
(define-message $interpreter:redundant $interpreter ())


(define transaction-mark-key
  (string->uninterned-symbol "transaction"))


; Interprocess locks are hard. Gate process using explicit, prescribed bytes.
(define-subprogram (assert-external-consent path key)
  (if (or (not (file-exists? path))
          (with-input-from-file path
            (λ ()
              (define data (read-bytes (bytes-length key)))
              (and (not (eof-object? data))
                   (bytes=? data key)))))
      (with-output-to-file (interprocess-lock-path ils) #:mode 'truncate/replace
        (λ ()
          (write-bytes (interprocess-lock-key ils))
          ($use (void))))
      ($fail ($locked))))
#lang racket/base

(require racket/contract)


(require racket/generic
         racket/sequence
         "function.rkt"
         "known.rkt"
         "machine.rkt"
         "monad.rkt"
         "port.rkt"
         "source.rkt")


(define-generics storage
  [storage-open storage key fulfil]
  [storage-aliases storage key]
  [storage-resolve-alias storage alias]
  [storage-add-alias storage key alias]
  [storage-remove-alias storage key alias]
  [storage-free storage key]
  [storage-size storage key])


(struct directory-storage (path)
  #:methods gen:storage
  [(define (storage-open storage key fill)
     (let ([path (directory-storage-path storage key)])
       (if (file-exists? path)
           (open-input-file path)
           (begin (call-with-output-file path fill)
                  (storage-open storage key fill)))))

   
   (define (storage-free storage key)
     (when (storage-free? storage)
       (delete-file key)))

   
   (define (storage-aliases storage key)
     (in-generator
      (call-with-input-file (links-path storage)
        (λ (in)
          (for ([line (in-lines in)])
            (when (and (link-exists? (path storage line))
                       (equal? (file-or-directory-identity link-path)
                               (file-or-directory-identity file-path)))
              (yield file-path)))))))
   
   (define (storage-add-alias storage key alias)
     (make-file-or-directory-link key alias))

   (define (storage-remove-alias storage alias)
     (when (link-exists? alias)
       (delete-file alias)))

   (define (storage-free? storage key)
     (andmap (negate link-exists?)
             (storage-aliases storage)))

   


(define (store storage chf source policy)
  (define id (source-id source))
  (define key (chf id))
  (storage-open key
                (λ (to-storage)
                  (transfer-machine source
                                    to-storage
                                    policy))))


(define (index chf key)
  (mdo port := (storage-open key (λ () (machine-halt-with #f)))
       digest := (machine-unit (chf port))
       (bind key digest)
       (machine-unit digest)))



(define (integrity state
                   content-source
                   content-policy
                   digest-source
                   digest-policy)
  (mdo from-content := (obtain state content-source content-policy)
       from-digest  := (obtain state digest-source digest-policy)
       (equal? (port->bytes from-digest)
               ((state-chf state content-source) from-content))))


(define mind-implementation/c
  (mind/c [mind-recall (-> mind? bytes? (-> known?) known?)]
          [mind-knowns (-> mind? bytes? (machine/c sequence?))]
          [mind-forget (-> mind? bytes? (machine/c exact-nonnegative-integer?))]))



(define (mind-clean mind)
  (machine
   (λ (state)
     (define state* ((mind-knowns mind) state))
     (define knowns (state-get-value state*))
     (define (collect key known)
       (mdo names := (known-get-names known)
            (if (null? names)
                (mdo size := (mind-forget mind key)
                     (machine
                      (λ (state)
                        (state-set-value state
                                         (+ size (state-get-value state))))))
                (machine values))))
     (sequence-fold (λ (s m) (m s))
                    (state-set-value state* 0)
                    (sequence-map (curry collect mind) knowns)))))     


(define (intraprocess-mind)
  (memory-mind (make-hash)))


(struct memory-mind (knowns)
  #:methods gen:mind
  [(define (mind-recall mind key learn)
     (machine
      (λ (state)
        (define table (memory-mind-knowns mind))
        (if (hash-has-key? table key)
            (state-set-value state (hash-ref table key))
            (let ([k (know)])
              (hash-set! table key k)
              ((learn k) state))))))


   (define (mind-forget mind key)
     (define known (hash-ref (memory-mind-knowns mind) key #f))
     (if known
         (mdo size := (known-size known)
              (machine
               (λ (state)
                 (hash-remove! (memory-mind-knowns mind) key)
                 state)))
         (machine-unit 0)))


   (define (mind-knowns mind)
     (machine-unit (in-hash (memory-mind-knowns mind))))])


(struct memory-known (aliases data)
  #:mutable
  #:methods gen:known
  [(define (known-get-names k)
     (machine-unit (memory-known-aliases k)))

   (define (known-put-names k names)
     (machine-unit (set-memory-known-aliases! k names)))

   (define (known-put-bytes k external)
     (machine
      (λ (state)
        (define to-bytes (open-output-bytes))
        (copy-port external to-bytes)
        (flush-output to-bytes)
        (set-memory-known-data! k (get-output-bytes to-bytes #t))
        (close-output-port to-bytes)
        (state-set-value state (void)))))

   (define (known-open-bytes k)
     (machine-unit (open-input-bytes (memory-known-data k))))

   (define (known-size k)
     (machine-unit (bytes-length (memory-known-data k))))])


(define (known-get-bytes known)
  (mdo i := (known-open-bytes known)
       (machine-unit (port->bytes i))))



(module+ test
  (require rackunit
           (submod "machine.rkt" test))
  (test-case "Intraprocess mind"
    (define m (intraprocess-mind))
    (define recall (mind-recall m #"a" machine-unit))
    (define k (state-get-value (recall)))
    (check-eq? (state-get-value (recall)) k)
    (void ((known-put-bytes k (open-input-bytes #"xyz"))))
    (check-equal? (sequence->list (sequence-map list (car ((mind-knowns m)))))
                  (list (list #"a" k)))
    (check-machine-value (mind-forget m #"a") 3)
    (check-machine-value (mind-forget m #"a") 0))

  (test-case "In-memory known"
    (define k (know null #"initial"))
    (define alias "A")

    (check-machine-value (known-put-names k (list alias))
                         (? void? _))

    (check-machine-value (known-get-names k)
                         (list alias))

    (check-machine-value (known-get-bytes k) #"initial")

    (check-machine-value (known-put-bytes k (open-input-string alias))
                         (? void? _))

    (check-machine-value (known-get-bytes k) #"A")

    (check-machine-value (known-size k) 1)))
