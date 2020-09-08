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
         (contract-out
          [request-transfer/c contract?]
          [fetch
           (-> non-empty-string?
               (non-empty-listof any/c)
               request-transfer/c
               any/c)]))


;-----------------------------------------------------------------------
; Define messages to capture relevant states.

(require "message.rkt")

(define+provide-message $source (fetch-name user-string))
(define+provide-message $source-method-ruled-out $source (reason))
(define+provide-message $source-fetched $source ())
(define+provide-message $fetch-failure (fetch-name))
(define+provide-message $unverified-host (url))


;-----------------------------------------------------------------------
; Implementation

(require racket/function
         racket/sequence
         net/head
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "mod.rkt"
         "monad.rkt"
         "path.rkt"
         "port.rkt"
         "printer.rkt"
         "query.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "url.rkt")


(define (fetch name sources request-transfer)
  (if (null? sources)
      (logged
       (λ (m)
         (values (fetch-state #f name #f request-transfer)
                 (cons ($fetch-failure name) m))))
      (logged
       (λ (m)
         (define logged-state (fetch-named-source name (car sources) request-transfer))
         (define-values (state messages) (run-log logged-state m))
         (if (fetch-state-result state)
             (values state messages)
             (run-log (fetch name (cdr sources) request-transfer) messages))))))

; This action will terminate on the first source to produce a file with
; the requested bytes.
(define (fetch-named-source name source request-transfer)
  (do initial       <- (logged-unit (fetch-unit name source request-transfer))
      fs-result     <- ((fetch-method "Filesystem" fetch-source/filesystem) initial)
      http-result   <- ((fetch-method "HTTP" fetch-source/http) fs-result)
      plugin-result <- ((fetch-method "Plugin" fetch-source/plugin) http-result)
      (return plugin-result)))


; Represents information gathered when trying to fetch bytes
(struct fetch-state
  (source               ; A user-defined string that a method should use to find bytes
   name                 ; A human-friendly name for the fetch used in errors
   result               ; A value representing a fetched resource, or #f. If not #f, the fetch was successful.
   request-transfer)    ; A callback that takes a port and size estimate
  #:transparent)


(define (fetch-exn-handler fetch-st)
  (λ (e)
    (logged
     (λ (messages)
       (values fetch-st
               (cons ($source-method-ruled-out
                      (fetch-state-name fetch-st)
                      (fetch-state-source fetch-st)
                      (if (XIDEN_VERBOSE)
                          (exn->string e)
                          (exn-message e)))
                     messages))))))


(define (fetch-method method-name f)
  (λ (fetch-st)
    (if (fetch-state-result fetch-st)
        (logged-unit fetch-st)
        (with-handlers ([values (fetch-exn-handler fetch-st)])
          (update-fetch-state method-name
                              fetch-st
                              (f (fetch-state-source fetch-st)
                                 (fetch-state-request-transfer fetch-st)))))))


(define (update-fetch-state method-name fetch-st next-result)
  (logged
   (λ (messages)
     (values (struct-copy fetch-state fetch-st [result next-result])
             (cons (log-fetch-update method-name fetch-st next-result)
                   messages)))))


(define (log-fetch-update method-name fetch-st path-or-#f)
  (if path-or-#f
      ($source-fetched (fetch-state-name fetch-st)
                       (fetch-state-source fetch-st))
      ($source-method-ruled-out (fetch-state-name fetch-st)
                                (fetch-state-source fetch-st)
                                (format "~a did not produce a value"
                                        method-name))))


(define (fetch-unit name source request-transfer)
  (fetch-state source
               name
               #f
               request-transfer))


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
        "+inf.0"))
  (request-transfer (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS) (string->url source))
                    est-size))


(define (fetch-source/plugin source request-transfer)
  (define (mod-fallback . _) (const #f))
  ((load-plugin 'fetch-source mod-fallback mod-fallback)
   source request-transfer))


(module+ test
  (require racket/file
           racket/tcp
           rackunit
           "setting.rkt")

  (define plugin-module-datum
    '(module mods racket/base
       (provide fetch-source)
       (define (fetch-source source request-transfer)
         (request-transfer (open-input-string source)
                           (string-length source)))))

  (define mod-source "====[{]::[}]====")

  (define (try-fetch l proc)
    (call-with-values (λ () (run-log l null)) proc))

  (define (try-mod-fetch proc)
    (try-fetch (fetch "mod"
                      (list mod-source)
                      (λ (in est-size)
                        (check-equal? est-size (string-length mod-source))
                        (check-equal? (read-string est-size in) mod-source)
                        (build-path "fake-path"))) ; <- Must produce a path for a successful fetch
               proc))

  (call-with-temporary-file
   (λ (tmp)
     (write-to-file plugin-module-datum tmp #:exists 'truncate/replace)
     (parameterize ([(setting-derived-parameter XIDEN_MODS_MODULE) tmp])
       (define source (path->string tmp))
       (define (request-transfer in est-size)
         (test-equal? "Read file" (read in) plugin-module-datum)
         (test-true "Estimate file size" (>= est-size (file-size tmp)))
         tmp)

       (test-case "Fetch from file"
         (try-fetch (fetch "anon" (list source) request-transfer)
                    (λ (result messages)
                      (check-equal? result
                                    (fetch-state source
                                                 "anon"
                                                 tmp
                                                 request-transfer))
                      (check-equal? messages
                                    (list ($source-fetched "anon" source))))))

       (test-case "Show all errors if transfer crashes"
         (define (raise-always . _) (error "uh oh"))
         (try-fetch (fetch "crashes" (list source) raise-always)
                    (λ (result messages)
                      (check-equal? result
                                    (fetch-state #f "crashes" #f raise-always))
                      (check-equal? (car messages)
                                    ($fetch-failure "crashes"))
                      (check-not-false (findf (λ (m)
                                                (and ($source-method-ruled-out? m)
                                                     (regexp-match? #rx"uh oh"
                                                                    ($source-method-ruled-out-reason m))))
                                              messages)))))

       (test-case "Fetch from mod"
         (try-mod-fetch
          (λ (result messages)
            (check-equal? (findf $source-fetched? messages)
                          ($source-fetched "mod" mod-source))))))))

  ; Notice we just left the parameterize that set the mod path
  (test-case "Fetch fails when mod is not available"
    (try-mod-fetch
     (λ (result messages)
       (check-match messages
                    (list ($fetch-failure "mod")
                          (? $source-method-ruled-out? _ _ _) ..1)))))

  (test-case "Fetch over HTTP"
    (define listener (tcp-listen 8018 1 #t))
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
      "fake")

    (try-fetch (fetch "anon" (list source) request-transfer)
               (λ (result messages)
                 (check-equal? result
                               (fetch-state source
                                            "anon"
                                            "fake"
                                            request-transfer))
                 (check-equal? (filter $source-fetched? messages)
                               (list ($source-fetched "anon" source)))))))


(define+provide-message-formatter format-fetch-message
  [($source-fetched name user-string)
   (format "Fetched ~s" user-string)]

  [($fetch-failure name)
   (format "Failed to fetch ~s" name)]

  [($source-method-ruled-out name user-string reason)
   (format "Could not fetch ~s from source ~s: ~a" name user-string reason)]

  [($unverified-host url)
   (format (~a "~a does not have a valid certificate.~n"
               "Connections to this server are not secure.~n"
               "To trust servers without valid certificates, use ~a.")
           url
           (setting-long-flag XIDEN_TRUST_UNVERIFIED_HOST))])
