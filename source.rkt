#lang racket/base

; Define methods for fetching bytes

(require "contract.rkt"
         "message.rkt")

; This procedure acts as an interface between a specific method (HTTP,
; File read, etc.) and the part of Xiden that reads an estimated number
; of bytes from a port.
(define tap/c
  (-> input-port?
      (or/c +inf.0 exact-positive-integer?)
      any))

(define exhaust/c
  (-> any/c any/c))

(define+provide-message $fetch (id errors))
(define+provide-message $bad-source-eval (reason datum context))

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
          [coerce-source (-> (or/c string? source?) source?)]
          [exhaust/c contract?]
          [from-catalogs (->* (string?) ((listof string?)) (listof url-string?))]
          [fetch (-> source? tap/c exhaust/c any/c)]
          [identify (-> source? (or/c #f input-port?))]
          [logged-fetch (-> any/c source? tap/c logged?)]
          [make-source-key (-> source? (or/c bytes? #f))]
          [source? predicate/c]
          [sources (->* () #:rest (listof (or/c string? source?)) source?)]
          [tap/c contract?]
          [eval-untrusted-source-expression
           (->* (any/c) (namespace?) logged?)]))



;-----------------------------------------------------------------------
; Implementation

(require (for-syntax racket/base
                     syntax/location
                     syntax/parse)
         (only-in net/uri-codec uri-encode)
         racket/exn
         racket/generic
         racket/match
         racket/path
         net/head
         "format.rkt"
         "logged.rkt"
         "message.rkt"
         "openssl.rkt"
         "plugin.rkt"
         "port.rkt"
         "rc.rkt"
         "string.rkt"
         "url.rkt")


(define (logged-fetch id source p)
  (logged
   (λ (messages)
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

(define (make-source-key src)
  (define id (identify src))
  (and id (make-digest id 'sha384)))


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
               (and cache-fn
                    (coerce-key-port (cache-fn %src))))])))]))


;-----------------------------------------------------------------------
; Source types

(define-source #:key #f (exhausted-source [value any/c])
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
          (+ (* 20 1024) ; for Mac OS resource forks
             (file-size path)))))


(define-source #:key http-source-request-url
               (http-source [request-url (or/c url? url-string?)])
  (with-handlers ([exn? %fail])
    (define-values (in headers-string)
      (get-pure-port/headers #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS)
                             #:method #"GET"
                             (if (url? request-url)
                                 request-url
                                 (string->url request-url))))

    (define content-length-pair
      (assf (λ (el) (equal? (string-downcase el) "content-length"))
            (extract-all-fields headers-string)))

    (%tap in
          (if content-length-pair
              (string->number (or (cdr content-length-pair) "+inf.0"))
              +inf.0))))


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

(define (coerce-source s)
  ((if (string? s)
       (plugin-ref 'string->source default-string->source)
       values) s))

(define (sources . variants)
  (first-available-source (map coerce-source variants) null))

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


(define (from-catalogs query-string [url-templates (XIDEN_CATALOGS)])
  (let ([encoded (uri-encode query-string)])
    (map (λ (url-string) (string-replace url-string "$QUERY" encoded))
         url-templates)))


(define (eval-untrusted-source-expression datum [ns (current-namespace)])
  (logged
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
           (submod "logged.rkt" test)
           (submod "plugin.rkt" test))

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

  (test-pred "Always fail using exhausted-source"
             void?
             (fetch (exhausted-source (void)) list values))

  (test-tap "Return exact bytes with a byte-source"
            (byte-source #"abc")
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

  (test-equal? "Expand URLs as a way to search for remote resources"
    (from-catalogs "%cool beans"
                   '("https://example.com?a=$QUERY" "http://example.com/$QUERY"))
    '("https://example.com?a=%25cool%20beans"
      "http://example.com/%25cool%20beans"))


  (test-true "Fetch from file"
    (call-with-temporary-file
     (λ (tmp-path)
       (display-to-file #:exists 'truncate/replace "123" tmp-path)
       (fetch (file-source tmp-path)
              (λ (in est-size)
                (and (equal? (read in) 123)
                     (<= (abs (- est-size (file-size tmp-path)))
                         (* 20 1024))))
              (λ _ #f)))))


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
      (check-true (fetch s tap (λ _ #f))))

    (dynamic-wind void
                  (λ ()
                    (check (http-source "http://127.0.0.1:8018"))
                    (check (http-mirrors-source (list "http://127.0.0.1:1" "http://127.0.0.1:8018"))))
                  (λ ()
                    (kill-thread th)
                    (tcp-close listener))))

  (test-case "Fetch using plugin"
    (define greeting "Hello, world")
    (define plugin-module-datum
      '(module mods racket/base
         (require xiden/source)
         (provide string->source)
         (define (string->source str)
           (text-source str))))

    (call-with-plugin
     plugin-module-datum
     (λ ()
       (fetch (coerce-source greeting)
              (λ (in est)
                (define str (port->string in))
                (check-equal? str greeting)
                (check-equal? est (bytes-length (string->bytes/utf-8 greeting))))
              (λ (v) (fail "Dummy plugin source should not be exhausted"))))))

  (define-namespace-anchor ns-anchor)
  (define test-ns (namespace-anchor->namespace ns-anchor))

  (define (test-safe-expression intent predicate datum)
    (test-logged-procedure
     (format "Eval source expression (safe, ~a)" intent)
     (eval-untrusted-source-expression datum test-ns)
     (λ (val messages)
       (check-pred predicate val)
       (check-pred null? messages))))

  (define (test-unsafe-expression intent datum)
    (test-logged-procedure
     (format "Eval source expression (dangerous, ~a)" intent)
     (eval-untrusted-source-expression datum test-ns)
     (λ (val messages)
       (check-eq? val FAILURE)
       (check-match (car messages)
                    ($bad-source-eval 'security
                                      (? (λ (v) (equal? datum v)) _)
                                      _)))))

  (test-logged-procedure
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
