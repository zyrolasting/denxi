#lang racket/base

; Define a means to exchange hash tables using ports,
; particularly for filesystem and network I/O.
; Define a lookup procedure that encapsulates a read hash table,
; namely by protecting hash table values with contracts.

(define config-closure/c
  (case-> (-> (hash/c symbol? any/c))
          (-> symbol? any/c)
          (-> symbol? flat-contract? any/c)
          (-> symbol? flat-contract? failure-result/c any/c)))

(require racket/place
         racket/path
         "contract.rkt")

(provide
 (contract-out
  [READ_ORDER symbol?]
  [save-config!
   (->* (config-closure/c (or/c path-string? url? output-port?)) void?)]
  [load-config
   (->* ((or/c path? url? string? bytes? input-port?))
        config-closure/c)]
  [make-config-closure
   (-> (hash/c symbol? place-message-allowed?)
       (listof symbol?)
       config-closure/c)]))


(require racket/file
         racket/sequence
         "contract.rkt"
         "string.rkt"
         "url.rkt")


;; High-level config I/O
;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-config! closure variant)
  (cond [(path-string? variant) (save-local-config! closure variant)]
        [(url? variant) (save-remote-config! closure variant)]
        [(output-port? variant) (write-config (closure) (closure READ_ORDER) variant)]))


(define (load-config variant)
  (cond [(path? variant) (load-local-config variant)]
        [(url? variant) (load-remote-config variant)]
        [(string? variant) (load-config (open-input-string variant))]
        [(bytes? variant) (load-config (open-input-bytes variant))]
        [(input-port? variant) (read-config variant)]))

(define (config-equal? closure/a closure/b)
  (equal? (closure/a)
          (closure/b)))

(define (config-visibly-equal? closure/a closure/b)
  (and (config-equal? closure/a closure/b)
       (equal? (closure/a READ_ORDER)
               (closure/b READ_ORDER))))

;; Config output cases
;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-config hash-table read-order o)
  (define (<< k v)
    (writeln (string->keyword (symbol->string k)) o)
    (writeln v o))

  (define unordered
    (sequence-fold hash-remove
                   hash-table
                   (in-list read-order)))

  (define remaining
    (for/fold ([wip hash-table])
              ([k (in-list read-order)])
      (<< k (hash-ref hash-table k))
      (hash-remove wip k)))

  (for ([(k v) (in-hash remaining)])
    (<< k v)))


(define (save-local-config! closure path)
  (define lockfile (make-lock-file-name path))
  (define leading/ (path-only path))
  (when leading/
    (make-directory* leading/))
  (call-with-file-lock/timeout
   path 'exclusive
   (λ ()
     (call-with-output-file #:exists 'truncate/replace
       path (λ (o) (save-config! closure o))))
   (λ () (error 'set-metadatum!
                "Failed to obtain lock for ~a"
                path)))
  (delete-file lockfile))


(define (save-remote-config! closure u)
  (define o (open-output-bytes))
  (save-config! closure o)
  (put-pure-port u (get-output-bytes o)))



;; Config input cases
;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-config in)
  (port-count-lines! in)
  (read-keyword-value-pairs in))


(define (load-remote-config u)
  (define-values (in headers)
    (get-pure-port/headers u #:status? #t))
  (define status (regexp-match #px"(\\d\\d\\d)" headers))
  (if (equal? status '("200"))
      (read-config in)
      (error "Non-200 response from ~a" (url->string u))))


(define (load-local-config path)
  (define lockfile (make-lock-file-name path))
  (dynamic-wind
    void
    (λ ()
      (call-with-file-lock/timeout
       path 'shared
       (λ () (call-with-input-file path read-config))
       (λ () (error 'get-metadatum!
                    "Failed to obtain lock for ~a"
                    path))))
    (λ ()
      (when (file-exists? lockfile)
        (delete-file lockfile)))))


(define (read-keyword-value-pairs in [h (hasheq)] [read-order null])
  (define maybe-keyword (read in))

  (cond [(eof-object? maybe-keyword)
         (make-config-closure h (reverse read-order))]

        [(keyword? maybe-keyword)
         (define maybe-value (read in))
         (when (or (keyword? maybe-value) (eof-object? maybe-value))
           (raise-read-error in "Expected value for ~s. Got ~s" maybe-keyword maybe-value))
         (define sym (string->symbol (keyword->string maybe-keyword)))

         (read-keyword-value-pairs
          in
          (hash-set h sym maybe-value)
          (cons sym read-order))]

        [else
         (raise-read-error in
                           "Expected keyword. Got ~s"
                           maybe-keyword)]))


(define (raise-read-error in fmt-string . args)
  (define-values (line col pos) (port-next-location in))
  (apply error
         'read-rcfile
         (format "line ~~s, col ~~s: ~a" fmt-string)
         line col args))


(define READ_ORDER (string->uninterned-symbol "read-order"))

(define (make-config-closure hash-table read-order)
  (define get-by-key
    (case-lambda
      [() hash-table]
      [(key)
       (if (eq? key READ_ORDER)
           read-order
           (get-by-key key any/c))]
      [(key c)
       (get-by-key key c
                   (λ () (error 'make-config-closure
                                "No value for key ~a"
                                key)))]
      [(key c f)
       (invariant-assertion c
                            (hash-ref hash-table key f))]))
  get-by-key)


(module+ test
  (require racket/file
           racket/function
           racket/port
           rackunit)

  (define expected-read-order
    '(who with in at))

  (define expected-hash-table
    (hasheq 'who "the butler"
            'with "the knife"
            'in 'cellar
            'at '(12 pm)))

  (define closure
    (make-config-closure expected-hash-table
                         expected-read-order))

  (test-eq? "Access hash table with a closure"
            (closure)
            expected-hash-table)

  (test-eq? "Accesses read order with a closure"
            (closure READ_ORDER)
            expected-read-order)

  (test-true "Consider closure over the same hash tables equal"
             (config-equal? (make-config-closure expected-hash-table null)
                            (make-config-closure expected-hash-table null)))

  (test-true "Consider closure over the same hash tables and the same reading order VISIBLY equal"
             (let ([h (hash 'a 1 'b 2)] [r '(a b)])
               (config-visibly-equal?
                (make-config-closure h r)
                (make-config-closure h r))))

  (test-true "If hash tables match but reading order doesn't, then closures are equal but not visibly so"
             (let* ([h (hash 'a 1 'b 2)]
                    [c/a (make-config-closure h '(a b))]
                    [c/b (make-config-closure h null)])
               (and (config-equal? c/a c/b)
                    (not (config-visibly-equal? c/a c/b)))))

  (test-eq? "Access keyed values with a closure"
            (closure 'who)
            (hash-ref expected-hash-table 'who))

  (test-exn "Raise exn:fail on non-existant key"
            exn:fail?
            (λ () (closure 'why)))

  (test-exn "Raise exn:fail:contract if value does not pass contract"
            exn:fail:contract?
            (λ () (closure 'who real?)))

  (test-true "Allow fail thunk only if contract is specified."
             (closure 'why boolean? (const #t)))

  (test-exn "Make fail thunk obey contracts"
            exn:fail:contract?
            (λ () (closure 'why real? (const #t))))

  (test-exn "Raise error if keyword did not appear where expected"
            #rx"Expected keyword"
            (λ () (load-config "value")))

  (test-exn "Raise error if EOF comes before file"
            #rx"Expected value for #:kw"
            (λ () (load-config "#:kw")))

  (test-exn "Raise error if two keywords appear together"
            #rx"Expected value for #:kw"
            (λ () (load-config "#:kw #:ke")))

  (test-exn "Show position information for error"
            #rx"line 4"
            (λ () (load-config "#:a 1\n#:b 8\n#:c\n#:d 3\n")))


  (let ([buffer (open-output-bytes)])
    (save-config! closure buffer)
    (test-true "Save and load visibly equal config using ports"
               (config-visibly-equal? closure
                                      (load-config (get-output-bytes buffer)))))

  (let ([buffer (open-output-bytes)])
    (save-config! (make-config-closure expected-hash-table null) buffer)
    (test-true "Save and load equal config, even if there is no reading order"
               (config-equal? closure
                              (load-config (get-output-bytes buffer)))))

  (define tmp-file (make-temporary-file))
  (dynamic-wind
    void
    (λ ()
      (save-config! closure tmp-file)
      (test-true "Can save and load visibly equal config from disk"
                 (config-visibly-equal? closure
                                        (load-config tmp-file))))
    (λ () (delete-file tmp-file)))


  (test-case "Protect closure from domain violations"
    (define closure/contracted
      (invariant-assertion config-closure/c
                           closure))

    (test-exn "Detect non-symbol key"
              exn:fail:contract?
              (λ () (closure/contracted "non-symbol")))

    (test-exn "Detect non-flat contract"
              exn:fail:contract?
              (λ () (closure/contracted 'too (-> any))))

    (test-exn "Detect non-failure result"
              exn:fail:contract?
              (λ () (closure/contracted 'too any/c (λ (a) a))))))
