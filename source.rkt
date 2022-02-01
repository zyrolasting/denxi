#lang racket/base

; Define methods for fetching bytes

(require (for-syntax racket/base
                     syntax/location
                     syntax/parse)
         net/head
         (only-in net/uri-codec
                  uri-encode)
         racket/contract
         racket/exn
         racket/generic
         racket/match
         racket/path
         "crypto.rkt"
         "format.rkt"
         "integrity/base.rkt"
         "message.rkt"
         "port.rkt"
         "printer.rkt"
         "setting.rkt"
         "string.rkt"
         "subprogram.rkt"
         "url.rkt")


; This procedure acts as an interface between a specific method (HTTP,
; File read, etc.) and the part of Denxi that reads an estimated number
; of bytes from a port.

(define budget/c
  (or/c +inf.0 exact-nonnegative-integer?))

(define tap/c
  (-> input-port? budget/c any))

(define exhaust/c
  (-> any/c any/c))

(define+provide-message $fetch (id errors))
(define+provide-message $http-failure (request-url status-line headers capped-body))
(define+provide-message $bad-source-eval (reason datum context))
(define+provide-message $untrusted-cert (url original-exn))

(define+provide-setting DENXI_HTTP_MAX_REDIRECTS exact-nonnegative-integer? 0)
(define+provide-setting DENXI_FETCH_BUFFER_SIZE_MB (real-in 0.1 20) 0)
(define+provide-setting DENXI_FETCH_TIMEOUT_MS (>/c 0) 0)
(define+provide-setting DENXI_FETCH_TOTAL_SIZE_MB (or/c +inf.0 real?) 0)

(provide define-source
         empty-source
         from-file
         gen:source
         (struct-out exhausted-source)
         (struct-out file-source)
         (struct-out first-available-source)
         (struct-out http-mirrors-source)
         (struct-out http-source)
         (struct-out lines-source)
         (struct-out text-source)
         (struct-out byte-source)
         (contract-out
          [bind-recursive-fetch
           (-> tap/c exhaust/c (->* (source?) (exhaust/c) any))]
          [coerce-source (-> source-variant? source?)]
          [exhaust/c contract?]
          [fetch (-> source? tap/c exhaust/c any/c)]
          [identify (-> source? (or/c #f input-port?))]
          [subprogram-fetch (-> source? tap/c subprogram?)]
          [make-limited-tap (-> exact-nonnegative-integer? tap/c)]
          [make-source-key (-> source? (or/c bytes? #f))]
          [source? predicate/c]
          [source-variant? flat-contract?]
          [sources (->* () #:rest (listof (or/c string? source?)) source?)]
          [lock-source
           (->* (source-variant?)
                (budget/c exhaust/c)
                (or/c source-variant?
                      bytes?))]
          [tap/c contract?]
          [budget/c flat-contract?]
          [current-string->source
           (parameter/c (-> string? source?))]
          [eval-untrusted-source-expression
           (->* (any/c) (namespace?) subprogram?)]))

;-----------------------------------------------------------------------
; Implementation

(define (subprogram-fetch source p)
  (subprogram
   (λ (messages)
     (define id (make-source-key source))
     (fetch source
            (λ a
              (values (apply p a)
                      (cons ($fetch id null)
                            messages)))
            (λ (variant)
              (values FAILURE
                      (cons ($fetch id (log-exhausted-source messages variant))
                            messages)))))))


(define (log-exhausted-source messages variant)
  (cond [(list? variant)
         (foldl (λ (n res) (log-exhausted-source res n)) messages variant)]
        [(exn? variant)
         (cons ($show-string (exn->string variant)) messages)]
        [($message? variant)
         (cons variant messages)]
        [else
         (cons ($show-datum variant) messages)]))


;-----------------------------------------------------------------------
; A source has its own method for producing bytes. Use CPS to handle
; success and failure branches.

(define-generics source
  [fetch source tap fail]
  ; source? -> input-port?
  [identify source])


(define source-variant?
  (or/c bytes? path? string? source?))


(define (lock-source variant [budget 0] [exhaust raise])
  (cond [(bytes? variant)
         variant]
        [(byte-source? variant)
         (byte-source-data variant)]
        [else
         (fetch (coerce-source variant)
                (λ (in est-size)
                  (if (> est-size budget)
                      (begin (close-input-port in)
                             variant)
                      (port->bytes in)))
                exhaust)]))


(define (make-source-key src)
  (define id (identify src))
  (and id (make-digest id)))


(define (coerce-key-port variant)
  (cond [(string? variant) (open-input-string variant)]
        [(bytes? variant) (open-input-bytes variant)]
        [(path? variant) (coerce-key-port (path->string (normalize-path variant)))]
        [(url? variant) (coerce-key-port (url->string variant))]
        [(source? variant) (identify variant)]
        [(list? variant) (apply input-port-append #t (map coerce-key-port variant))]))


(define (bind-recursive-fetch %tap %fail)
  (λ (s [f %fail]) (fetch s %tap f)))


(define-syntax (define-source stx)
  (syntax-case stx ()
    [(_ #:key cache-fn (id [field-id field-contract] ...) . body)
     (syntax-protect
      (with-syntax ([(%fetch %src %tap %fail)
                     (map (λ (v) (datum->syntax stx v)) '(%fetch %src %tap %fail))])
        #'(struct id (field-id ...)
            #:guard
            (invariant-assertion (rename-contract
                                  (-> field-contract ... any/c
                                      (values field-contract ...))
                                  'id)
                                 (λ a (apply values (reverse (cdr (reverse a))))))
            #:methods gen:source
            [(define (fetch %src %tap %fail)
               (match-define (id field-id ...) %src)
               (define %fetch (bind-recursive-fetch %tap %fail))
               . body)
             (define (identify %src)
               (coerce-key-port (cache-fn %src)))])))]))


(define ((make-limited-tap max-size) from-source est-size)
  (make-limited-input-port from-source
                           (inexact->exact (min max-size est-size))
                           #t))


;-----------------------------------------------------------------------
; Source types

(define-source #:key #"" (exhausted-source [value any/c])
  (%fail value))


(define-source #:key byte-source-data (byte-source [data bytes?])
  (%tap (open-input-bytes data)
        (bytes-length data)))


(define-source #:key first-available-source-sources
  (first-available-source [sources (listof source?)] [errors list?])
  (if (null? sources)
      (%fail (reverse errors))
      (%fetch (car sources)
              (λ (e)
                (%fetch (first-available-source (cdr sources) (cons e errors))
                        %fail)))))


(define-source #:key text-source-data
  (text-source [data string?])
  (%fetch (byte-source (string->bytes/utf-8 data))))


(define (lines-source->string s)
  (join-lines #:trailing? #t
              #:suffix (lines-source-suffix s)
              (lines-source-lines s)))

(define-source #:key lines-source->string
  (lines-source [suffix (or/c #f char? string?)] [lines (listof string?)])
  (%fetch (text-source (lines-source->string %src))))


(define-source #:key file-source-path
  (file-source [path path-string?])
  (with-handlers ([exn:fail:filesystem? %fail])
    (%tap (open-input-file path)
          (file-size path))))


(define-source #:key http-source-request-url
  (http-source [request-url (or/c url? url-string?)] [max-redirects exact-nonnegative-integer?])
  (define coerced-url
    (if (url? request-url)
        request-url
        (string->url request-url)))

  (define (handle-network-error network-exn)
    (if (regexp-match? #rx"certificate verify failed"
                       (exn-message network-exn))
        (%fail ($untrusted-cert coerced-url network-exn))
        (%fail network-exn)))

  (define (handle-file-url)
    (let ([path-els (map path/param-path (url-path coerced-url))])
      (%fetch (file-source (apply build-path
                                  (if (and (not (equal? (system-type 'os) 'windows))
                                           (url-path-absolute? coerced-url))
                                      (cons "/" path-els)
                                      path-els))))))

  (define (handle-http-url)
    (define-values (in headers-string)
      (get-pure-port/headers
       #:redirections max-redirects
       #:method #"GET"
       #:status? #t
       coerced-url))

    (define status
      (string->number
       (car (regexp-match #px"\\d\\d\\d"
                          headers-string))))

    (define headers
      (extract-all-fields headers-string))

    (define content-length-pair
      (assf (λ (el) (equal? (string-downcase el) "content-length"))
            headers))

    (if (and (>= status 200) (< status 300))
        (%tap in
              (if content-length-pair
                  (string->number (or (cdr content-length-pair) "+inf.0"))
                  +inf.0))
        (%fail ($http-failure (url->string coerced-url)
                              (let ([m (regexp-match #px"[^\n]+\n" headers-string)])
                                (and m (car m)))
                              headers
                              (read-bytes 512 in)))))

  (with-handlers ([exn:fail:network? handle-network-error] [exn? %fail])
    (if (equal? (url-scheme coerced-url) "file")
        (handle-file-url)
        (handle-http-url))))


(define-source #:key http-mirrors-source-request-urls
  (http-mirrors-source [request-urls (listof (or/c url? url-string?))])
  (%fetch (apply sources (map http-source request-urls))))



;-----------------------------------------------------------------------
; Source expressions

(define empty-source (byte-source #""))


(define (default-string->source s)
  (if (file-exists? s)
      (file-source s)
      (with-handlers ([values exhausted-source])
        (http-source s))))


(define current-string->source
  (make-parameter default-string->source))


(define (coerce-source s)
  (cond [(string? s)
         ((current-string->source) s)]
        [(bytes? s)
         (byte-source s)]
        [(path? s)
         (file-source s)]
        [(source? s) s]))


(define (sources . variants)
  (first-available-source (map coerce-source variants) null))


(define (normalize-source src [budget +inf.0])
  (fetch (coerce-source src)
         (λ (in est-size)
           (if (> est-size budget)
               (begin (close-input-port in)
                      src)
               (port->bytes in)))
         raise))


(define-syntax (from-file stx)
  (syntax-parse stx
    [(_ user-path:string)
     (for ([el (in-list (explode-path (syntax-e #'user-path)))])
       (when (eq? el 'up)
         (raise-syntax-error 'from-file
                             "A relative path source may not reference parent directories."
                             #'user-path)))
     (with-syntax ([wrt (or (syntax-source-directory stx) (current-directory))])
       #'(path->string (path->complete-path user-path wrt)))]))


(define (eval-untrusted-source-expression datum [ns (current-namespace)])
  (subprogram
   (λ (messages)
     (call/cc
      (λ (return)
        (define (block . context)
          (return FAILURE
                  (cons ($bad-source-eval 'security datum context)
                        messages)))
        (define variant
          (parameterize ([current-security-guard
                          (make-security-guard
                           (current-security-guard)
                           (λ (sym path-or-#f ops)
                             (define (block/fs)
                               (block 'fs sym path-or-#f ops))
                             (when path-or-#f
                               (when (or (member 'execute ops)
                                         (member 'write ops)
                                         (member 'delete ops))
                                 (block/fs))))
                           (λ _ (apply block 'network _))
                           (λ _ (apply block 'link _)))])
            (eval-syntax (namespace-syntax-introduce (datum->syntax #f datum) ns) ns)))
        (if (source? variant)
            (values variant messages)
            (values FAILURE
                    (cons ($bad-source-eval 'invariant datum #f)
                          messages))))))))

(module+ test
  (require racket/file
           racket/tcp
           racket/runtime-path
           rackunit
           mzlib/etc
           "file.rkt"
           "setting.rkt"
           (submod "subprogram.rkt" test))

  (test-case "Detect budget amounts using predicate"
    (check-pred budget/c 0)
    (check-pred budget/c -0)
    (check-pred budget/c 1)
    (check-pred budget/c +inf.0)
    (check-false (budget/c -inf.0))
    (check-false (budget/c 0.0))
    (check-false (budget/c -1)))

  (test-case "Lock sources"
    (define str "hello, world")
    (define as-bytes (string->bytes/utf-8 str))
    (define txt (text-source str))
    (check-eq? (lock-source as-bytes) as-bytes)
    (check-eq? (lock-source (byte-source as-bytes)) as-bytes)
    (check-eq? (lock-source txt) txt)
    (check-equal? (lock-source txt (bytes-length as-bytes)) as-bytes)
    (check-eq? (lock-source txt (sub1 (bytes-length as-bytes))) txt)
    (check-eq? (lock-source (exhausted-source 1) 0 values) 1))


  (define (run-tap in est)
    (define out (open-output-bytes))
    (copy-port in out)
    (if (equal? est +inf.0)
        (get-output-bytes out #t)
        (subbytes (get-output-bytes out #t) 0 est)))

  (define (test-tap msg src expected)
    (test-equal? msg
                 (fetch src run-tap void)
                 expected))

  (test-case "Identify source variant types"
    (check-true (source-variant? #""))
    (check-true (source-variant? ""))
    (check-true (source-variant? (build-path ".")))
    (check-true (source-variant? (byte-source #"")))
    (check-false (source-variant? '(#"")))
    (check-false (source-variant? 1)))

  (test-pred "Always fail using exhausted-source"
             void?
             (fetch (exhausted-source (void)) list values))

  (test-tap "Return exact bytes with a byte-source"
            (byte-source #"abc")
            #"abc")

  (test-tap "Coerce exact bytes to a byte-source"
            (coerce-source #"abc")
            #"abc")

  (test-tap "Allow combining sources"
            (sources (exhausted-source (void)) (byte-source #"abc"))
            #"abc")

  (test-tap "Produce text"
            (text-source "abc")
            #"abc")

  (test-tap "Produce lines"
            (lines-source "\r\n" '("a" "b" "cdef"))
            #"a\r\nb\r\ncdef\r\n")

  (test-equal? "Expand paths relative to source file"
               (file-or-directory-identity (from-file "source.rkt"))
               (file-or-directory-identity (build-path (this-expression-source-directory)
                                                       (this-expression-file-name))))


  (test-case "Fetch from file"
    (call-with-temporary-file
     (λ (tmp-path)
       (display-to-file #:exists 'truncate/replace "123" tmp-path)

       (define (tap in est-size)
         (and (equal? (read in) 123)
              (<= (abs (- est-size (file-size tmp-path)))
                  (* 20 1024))))

       (check-true (fetch (file-source tmp-path) tap values))
       (test-true "Fetch file via http-source"
                  (fetch (http-source (~a "file://" tmp-path))
                         tap
                         values)))))


  (test-case "Fetch over HTTP"
    (define listener (tcp-listen 8018 1 #t))
    (define th
      (thread
       (λ ()
         (let loop ()
           (define-values (in out) (tcp-accept listener))
           (display "HTTP/1.1 200 OK\r\n" out)
           (display "https: //www.example.com/\r\n" out)
           (display "Content-Type: text/html; charset=UTF-8\r\n" out)
           (display "Date: Fri, 28 Aug 2020 04:02:21 GMT\r\n" out)
           (display "Server: foo\r\n" out)
           (display "Content-Length: 5\r\n\r\n" out)
           (write-bytes #"12345" out)
           (flush-output out)
           (close-output-port out)
           (close-input-port in)
           (loop)))))

    (define (tap in est-size)
      (check-eq? est-size 5)
      (check-equal? (read-bytes est-size in) #"12345")
      #t)

    (define (check s)
      (check-true (fetch s tap values)))

    (dynamic-wind void
                  (λ ()
                    (check (http-source "http://127.0.0.1:8018"))
                    (check (http-mirrors-source (list "http://127.0.0.1:1" "http://127.0.0.1:8018"))))
                  (λ ()
                    (kill-thread th)
                    (tcp-close listener))))

  (test-case "Fetch using custom coercion rule"
    (define greeting "Hello, world")
    (parameterize ([current-string->source text-source])
      (fetch (coerce-source greeting)
             (λ (in est)
               (define str (port->string in))
               (check-equal? str greeting)
               (check-equal? est (bytes-length (string->bytes/utf-8 greeting))))
             (λ (v) (fail "Coerced source should not be exhausted")))))

  (define-namespace-anchor ns-anchor)
  (define test-ns (namespace-anchor->namespace ns-anchor))

  (define (test-safe-expression intent predicate datum)
    (test-subprogram
     (format "Eval source expression (safe, ~a)" intent)
     (eval-untrusted-source-expression datum test-ns)
     (λ (val messages)
       (check-pred predicate val)
       (check-pred null? messages))))

  (define (test-unsafe-expression intent datum)
    (test-subprogram
     (format "Eval source expression (dangerous, ~a)" intent)
     (eval-untrusted-source-expression datum test-ns)
     (λ (val messages)
       (check-eq? val FAILURE)
       (check-match (car messages)
                    ($bad-source-eval 'security
                                      (? (λ (v) (equal? datum v)) _)
                                      _)))))

  (test-subprogram
   "Eval non-source expression"
   (eval-untrusted-source-expression '1 test-ns)
   (λ (val messages)
     (check-eq? val FAILURE)
     (check-match (car messages)
                  ($bad-source-eval 'invariant
                                    (? (λ (v) (equal? '1 v)) _)
                                    _))))

  (test-safe-expression
   "Literal bytes"
   byte-source?
   '(byte-source #"abc"))

  (test-unsafe-expression
   "file output"
   '(open-output-file "evil"))

  (test-unsafe-expression
   "execute"
   '(let evil ()
      (local-require racket/system compiler/find-exe)
      (system* (find-exe))))

  (test-unsafe-expression
   "network connection"
   '(tcp-connect "127.0.0.1" 80))

  (define-runtime-path here ".")
  (test-case "Identify sources in advance of a fetch"
    (define path (build-path here "file.txt"))
    (check-equal? (port->bytes
                   (identify (sources (http-mirrors-source '("x.com" "y.net"))
                                      (http-source "https://z.org")
                                      (text-source "hello world")
                                      (byte-source #"abc")
                                      (lines-source #\| '("line 1" "line 2"))
                                      (file-source path))))
                  (string->bytes/utf-8
                   (format "x.comy.nethttps://z.orghello worldabcline 1|line 2|~a"
                           (normalize-path path))))))
