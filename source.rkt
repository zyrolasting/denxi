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
      any/c))

(define exhaust/c
  (-> any/c any/c))

(define+provide-message $fetch (errors))

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
         (struct-out plugin-source)
         (struct-out text-source)
         (contract-out
          [coerce-source (-> (or/c string? source?) source?)]
          [exhaust/c contract?]
          [from-catalogs (->* (string?) ((listof string?)) (listof url-string?))]
          [fetch (-> source? tap/c exhaust/c any/c)]
          [logged-fetch (-> source? tap/c logged?)]
          [source? predicate/c]
          [sources (->* () #:rest (listof (or/c string? source?)) source?)]
          [tap/c contract?]))



;-----------------------------------------------------------------------
; Implementation

(require (for-syntax racket/base
                     syntax/location
                     syntax/parse)
         (only-in net/uri-codec uri-encode)
         racket/exn
         racket/generic
         racket/match
         net/head
         "logged.rkt"
         "message.rkt"
         "plugin.rkt"
         "port.rkt"
         "rc.rkt"
         "string.rkt"
         "url.rkt")


(define (logged-fetch source p)
  (logged
   (λ (messages)
     (fetch source
            (λ a
              (values (apply p a)
                      (cons ($fetch null)
                            messages)))
            (λ (variant)
              (values FAILURE
                      ($fetch (log-exhausted-source messages variant))))))))


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
  [fetch source tap fail])

(define (bind-recursive-fetch %tap %fail)
  (λ (s [f %fail]) (fetch s %tap f)))

(define-syntax (define-source stx)
  (syntax-case stx ()
    [(_ (id [field-id field-contract] ...) . body)
     (syntax-protect
      (datum->syntax stx
                     (syntax->datum
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
                             . body)]))))]))



;-----------------------------------------------------------------------
; Source types

(define-source (exhausted-source [value any/c])
  (%fail value))


(define-source (byte-source [data bytes?])
  (%tap (open-input-bytes data)
        (bytes-length data)))


(define-source (first-available-source [sources (listof source?)] [errors list?])
  (if (null? sources)
      (%fail (reverse errors))
      (%fetch (car sources)
              (λ (e)
                (%fetch (first-available-source (cdr sources) (cons e errors))
                        %fail)))))


(define-source (variant-source [value any/c] [constructors (-> any/c source?)])
  (%fetch
   (first-available-source
    (map (λ (c) (c value))
         constructors)
    null)))


(define-source (text-source [data string?])
  (%fetch (byte-source (string->bytes/utf-8 data))))


(define-source (lines-source [suffix (or/c #f char? string?)] [lines (listof string?)])
  (define s
    (cond [(not suffix) (if (equal? (system-type 'os) 'windows) "\r\n" "\n")]
          [(char? suffix) (string suffix)]
          [(string? suffix) suffix]))
  (%fetch (text-source (string-append (string-join lines s) s))))


(define-source (file-source [path path-string?])
  (with-handlers ([exn:fail:filesystem? %fail])
    (%tap (open-input-file path)
          (+ (* 20 1024) ; for Mac OS resource forks
             (file-size path)))))


(define-source (http-source [request-url (or/c url? url-string?)])
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


(define-source (http-mirrors-source [request-urls (listof (or/c url? url-string?))])
  (%fetch (apply sources (map http-source request-urls))))



(define-source (plugin-source [hint any/c] [kw-lst list?] [kw-val-lst list?] [lst list?])
  (let ([proc (load-from-plugin 'bind-custom-fetch (λ () (%fail 'undefined)) %fail)])
    (keyword-apply (proc hint %tap %fail)
                   kw-lst
                   kw-val-lst
                   lst)))


;-----------------------------------------------------------------------
; Source expressions

(define empty-source (byte-source #""))

(define (coerce-source s)
  (if (string? s)
      (first-available-source
       (list (file-source s)
             (http-source s)
             (plugin-source 'coerced null null (list s)))
       null)
      s))

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


(module+ test
  (require racket/file
           racket/tcp
           rackunit
           mzlib/etc
           "file.rkt"
           "setting.rkt"
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
    (define plugin-module-datum
      '(module mods racket/base
         (provide bind-custom-fetch)
         (define (bind-custom-fetch hint tap fail)
           (λ (fail?)
             (if fail?
                 (fail 'failed)
                 (tap (open-input-bytes #"") 678))))))

    (define (check fail?)
      (fetch (plugin-source #f null null (list fail?))
             (λ (in est) (check-equal? est 678))
             (λ (v) (check-equal? v 'failed))))

    (call-with-plugin
     plugin-module-datum
     (λ ()
       (check #t)
       (check #f)))))
