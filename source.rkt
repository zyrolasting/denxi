#lang racket/base

; Define means for fetching bytes from some origin.
;
; This addresses a core problem in dependency management: If you
; didn't check your dependencies into version control, then how do you
; get those EXACT dependencies later?
;
; At a high-level, this module defines a "fetch" as an operation that
; tries to fulfill a request for bytes through various methods.

(require "contract.rkt")

; This procedure acts as an interface between a specific method (HTTP,
; File read, etc.) and the part of Xiden that reads an estimated number
; of bytes from a port.
(define request-transfer/c
  (-> input-port?
      (or/c +inf.0 exact-positive-integer?)
      any/c))


(provide (struct-out fetch-state)
         from-file
         sources
         (contract-out
          [request-transfer/c contract?]
          [from-catalogs
           (-> string?
               (listof url-string?))]
          [fetch
           (-> non-empty-string?
               (non-empty-listof any/c)
               request-transfer/c
               any/c)]))


;-----------------------------------------------------------------------
; Define messages to capture relevant states.

(require "message.rkt")

(define+provide-message $fetch ())
(define+provide-message $fetch:scope  $fetch (fetch-name source-name message))
(define+provide-message $fetch:fail   $fetch (method reason))
(define+provide-message $fetch:done   $fetch (ok?))


;-----------------------------------------------------------------------
; Implementation

(require (for-syntax racket/base
                     syntax/location
                     syntax/parse)
         (only-in net/uri-codec uri-encode)
         racket/function
         racket/match
         racket/sequence
         net/head
         "cli-flag.rkt"
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "logged.rkt"
         "monad.rkt"
         "path.rkt"
         "plugin.rkt"
         "port.rkt"
         "printer.rkt"
         "query.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "url.rkt")

(module+ test
  (require racket/file
           racket/tcp
           rackunit
           mzlib/etc
           "setting.rkt"))

(struct fetch-state (source result) #:transparent)

(define (fetch-ok? f)
  (and (fetch-state? f)
       (fetch-state-source f)))


;-----------------------------------------------------------------------
; High-level operations

(define (fetch name sources request-transfer)
  (if (null? sources)
      (logged
       (λ (messages)
         (values (fetch-state #f #f)
                 (cons ($fetch:scope name #f ($fetch:done #f))
                       messages))))
      (logged
       (λ (messages)
         (define source (car sources))
         (define logged-state (fetch-named-source source request-transfer))
         (define-values (state fetch-messages) (run-log logged-state null))
         (define messages*
           (append (map (curry $fetch:scope name source)
                        fetch-messages)
                   messages))
         (if (fetch-state-result state)
             (values state messages*)
             (run-log (fetch name (cdr sources) request-transfer)
                      messages*))))))


; This terminates on the first source to produce a file with the requested bytes.
(define (fetch-named-source source request-transfer)
  (do initial       <- (logged-unit (fetch-state source #f))
      fs-result     <- ((fetch-method 'filesystem fetch-source/filesystem) initial request-transfer)
      http-result   <- ((fetch-method 'http fetch-source/http) fs-result request-transfer)
      plugin-result <- ((fetch-method 'plugin fetch-source/plugin) http-result request-transfer)
      (return plugin-result)))


;------------------------------------------------------------------------------
; Fetch methods are procedures that each attempt to produce bytes from
; a source.  An exception or #f produced by a fetch method only
; rules out that method. A fetch fails if no fetch method returns a true value.


(define (fetch-method method f)
  (λ (fetch-st request-transfer)
    (match-let ([(fetch-state source result) fetch-st])
      (if result
          (logged-unit fetch-st)
          (with-handlers ([values (fetch-exn-handler source method)])
            (update-fetch-state source
                                method
                                (f source request-transfer)))))))


(define (fetch-exn-handler source method)
  (λ (e)
    (logged
     (λ (messages)
       (values (fetch-state source #f)
               (cons ($fetch:fail method (exn-message e))
                     messages))))))


(define (update-fetch-state source method method-result)
  (logged
   (λ (messages)
     (values (fetch-state source method-result)
             (cons (if method-result
                       ($fetch:done #t)
                       ($fetch:fail method #f))
                   messages)))))

(module+ test
  (let* ([method 'zappo]
         [source "source"]
         [request-transfer (λ (v) (check-eq? v 'next) 'done)]
         [finished-state (fetch-state source 'done)]
         [unfinished-state (fetch-state source #f)]
         [bound (fetch-method method
                              (λ (s r) (check-eq? s source)
                                 (check-eq? r request-transfer)
                                 (request-transfer 'next)))]
         [raises (fetch-method method (λ (s r) (error "uh oh")))])
    (test-case "Short circuit use of a fetch method if a result is already available"
      (call-with-values (λ () (run-log (bound finished-state request-transfer) null))
                        (λ (state messages)
                          (check-eq? state finished-state))))

    (test-case "Try to use a fetch method if no result is available"
      (call-with-values (λ () (run-log (bound unfinished-state request-transfer) null))
                        (λ (state messages)
                          (check-equal? state finished-state))))

    (test-case "Treat #f as a failure to produce a result"
      (call-with-values (λ () (run-log (update-fetch-state source method #f) null))
                        (λ (state messages)
                          (check-equal? state (fetch-state source #f))
                          (check-equal? messages (list ($fetch:fail method #f))))))

    (test-case "Treat an exception as a failure to produce a result"
      (call-with-values (λ () (run-log (raises unfinished-state request-transfer) null))
                        (λ (state messages)
                          (check-equal? state (fetch-state source #f))
                          (check-equal? messages (list ($fetch:fail method "uh oh"))))))

    (test-case "Treat anything else as reason to mark a fetch done"
      (call-with-values (λ () (run-log (update-fetch-state source method 'something) null))
                        (λ (state messages)
                          (check-equal? state (fetch-state source 'something))
                          (check-equal? messages (list ($fetch:done #t))))))))




;------------------------------------------------------------------------------
; Fetch method implementations

(define (fetch-source/filesystem source request-transfer)
  (request-transfer (open-input-file source)
                    (+ (* 20 1024) ; for Mac OS resource forks
                       (file-size source))))


(define (fetch-source/http source request-transfer)
  (define in (head-impure-port (string->url source)))
  (define headers (extract-all-fields (port->string in)))
  (define content-length-pair (assf (λ (el) (equal? (string-downcase el) "content-length")) headers))
  (define est-size
    (if content-length-pair
        (string->number (or (cdr content-length-pair) "+inf.0"))
        +inf.0))
  (request-transfer (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS)
                                   (string->url source))
                    est-size))


(define (fetch-source/plugin source request-transfer)
  (define (mod-fallback . _) (const #f))
  ((load-plugin 'fetch-source mod-fallback mod-fallback)
   source request-transfer))


;------------------------------------------------------------------------------
; Source expressions

(define sources
  (procedure-rename list 'sources))


(define-syntax (from-file stx)
  (syntax-parse stx
    [(_ user-path:string)
     (for ([el (in-list (explode-path (syntax-e #'user-path)))])
       (when (eq? el 'up)
         (raise-syntax-error 'from-file
                             "A relative path source may not reference parent directories."
                             #'user-path)))
     (with-syntax ([wrt (or (syntax-source-directory stx) (current-directory))])
       #'(normalize-path user-path wrt))]))


(define (from-catalogs query-string [url-templates (XIDEN_CATALOGS)])
  (let ([encoded (uri-encode query-string)])
    (map (λ (url-string) (string-replace url-string "$QUERY" encoded))
         url-templates)))

(module+ test
  (test-equal? "Alias (list) as (sources)"
               (sources 1 2 3)
               (list 1 2 3))

  (test-equal? "Expand URLs as a way to search for remote resources"
               (from-catalogs "%cool beans"
                              '("https://example.com?a=$QUERY"
                                "http://example.com/$QUERY"))
               '("https://example.com?a=%25cool%20beans"
                 "http://example.com/%25cool%20beans"))

  (test-equal? "Expand paths relative to source file"
               (file-or-directory-identity (from-file "source.rkt"))
               (file-or-directory-identity (build-path (this-expression-source-directory)
                                                       (this-expression-file-name)))))

;------------------------------------------------------------------------------

(module+ test
  (define plugin-module-datum
    '(module mods racket/base
       (provide fetch-source)
       (define (fetch-source source request-transfer)
         (request-transfer (open-input-string source)
                           (string-length source)))))

  (define fetch-name "anon")
  (define mod-source "====[{]::[}]====")
  (define mod-result (build-path "fake-path"))

  (define (try-fetch l proc)
    (call-with-values (λ () (run-log l null)) proc))

  (define (try-mod-fetch proc)
    (try-fetch (fetch "mod"
                      (list mod-source)
                      (λ (in est-size)
                        (check-equal? est-size (string-length mod-source))
                        (check-equal? (read-string est-size in) mod-source)
                        mod-result)) ; <- Must produce a path for a successful fetch
               proc))

  (call-with-temporary-file
   (λ (tmp)
     (write-to-file plugin-module-datum tmp #:exists 'truncate/replace)
     (parameterize ([(setting-derived-parameter XIDEN_PLUGIN_MODULE) tmp])
       (define source (path->string tmp))
       (define (request-transfer in est-size)
         (test-equal? "Read file" (read in) plugin-module-datum)
         (test-true "Estimate file size" (>= est-size (file-size tmp)))
         tmp)

       (test-case "Fetch from file"
         (try-fetch (fetch fetch-name (list source) request-transfer)
                    (λ (result messages)
                      (check-equal? result (fetch-state source tmp))
                      (check-equal? messages
                                    (list ($fetch:scope fetch-name source
                                                        ($fetch:done #t)))))))

       (test-case "Show all errors if transfer crashes"
         (define (raise-always . _) (error "uh oh"))
         (try-fetch (fetch "crashes" (list source) raise-always)
                    (λ (result messages)
                      (check-equal? result (fetch-state #f #f))
                      (check-match messages
                                   (list ($fetch:scope "crashes" #f ($fetch:done #f))
                                         ($fetch:scope "crashes" tmp
                                                       ($fetch:fail (or 'filesystem 'http 'plugin)
                                                                    (? string? _)))
                                         ..3)))))

       (test-case "Fetch from mod"
         (try-mod-fetch
          (λ (result messages)
            (check-equal? result (fetch-state mod-source mod-result))

            ; I wanted to combine these, but an expansion-time issue
            ; keeps me from doing so with the `mod-source` binding
            ; https://github.com/racket/racket/issues/3435
            (check-match (car messages)
                         ($fetch:scope "mod" mod-source
                                       ($fetch:done #t)))
            (check-match (cdr messages)
                         (list ($fetch:scope "mod" mod-source
                                             ($fetch:fail (or 'filesystem 'http) _))
                               ..1))))))))

  ; Notice we just left the parameterize that set the mod path
  (test-case "Fetch fails when mod is not available"
    (try-mod-fetch
     (λ (result messages)
       (check-match messages
                    (list ($fetch:scope "mod" mod-source
                                        (or (? $fetch:fail? _)
                                            ($fetch:done #f)))
                          ..1)))))

  (test-case "Fetch over HTTP"
    (define listener (tcp-listen 8018 1 #t))
    (define http-result "from http")
    (define th
      (thread
       (λ ()
         (let loop ([num 0])
           (define-values (in out) (tcp-accept listener))
           (display "HTTP/1.1 200 OK\r\n" out)
           (display "https: //www.example.com/\r\n" out)
           (display "Content-Type: text/html; charset=UTF-8\r\n" out)
           (display "Date: Fri, 28 Aug 2020 04:02:21 GMT\r\n" out)
           (display "Server: foo\r\n" out)
           (display "Content-Length: 5\r\n\r\n" out)
           (unless (= num 0)
             (display "12345" out))
           (flush-output out)
           (close-output-port out)
           (close-input-port in)
           (loop (add1 num))))))

    (define source "http://127.0.0.1:8018")

    (define (request-transfer in est-size)
      (check-eq? est-size 5)
      (check-equal? (read-bytes est-size in) #"12345")
      (kill-thread th)
      (tcp-close listener)
      http-result)

    (try-fetch (fetch fetch-name (list source) request-transfer)
               (λ (result messages)
                 (check-equal? result (fetch-state source http-result))
                 (check-match messages
                              (list ($fetch:scope fetch-name source
                                                  (or ($fetch:done #t)
                                                      ($fetch:fail (not 'http)
                                                                   (? string? _))))
                                    ..1))))))
