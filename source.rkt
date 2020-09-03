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
      (or/c #f path?)))


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
         "config.rkt"
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "mod.rkt"
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
      (fetch-state #f
                   name
                   #f
                   request-transfer
                   (list ($fetch-failure name)))
      (let ([state (fetch-named-source name (car sources) request-transfer)])
        (if (fetch-state-path state)
            state
            (fetch name (cdr sources) request-transfer)))))


(define (fetch-named-source name source request-transfer)
  (define from-plugin (fetch-method "Plugin" fetch-source/plugin))
  (define from-web    (fetch-method "HTTP" fetch-source/http))
  (define from-fs     (fetch-method "Filesystem" fetch-source/filesystem))
  ((compose from-plugin from-web from-fs) (fetch-unit name source request-transfer)))


; Represents information gathered when trying to fetch bytes
(struct fetch-state
  (source               ; A user-defined string that a method should use to find bytes
   name                 ; A human-friendly name for the fetch used in errors
   path                 ; A path pointing to a fetch resource, or #f. If set, the fetch was successful.
   request-transfer     ; A continuation procedure that takes a port and size estimate
   messages)            ; A list of $message instances that logs what happened to produce a given instance.
  #:transparent)


(define (attach-fetch-message fetch-st v)
  (struct-copy fetch-state fetch-st
               [messages (cons v (fetch-state-messages fetch-st))]))


(define (fetch-exn-handler fetch-st)
  (λ (e)
    (attach-fetch-message fetch-st
                          ($source-method-ruled-out
                           (fetch-state-name fetch-st)
                           (fetch-state-source fetch-st)
                           (exn->string e)))))


(define (fetch-method method-name f)
  (λ (fetch-st)
    (if (fetch-state-path fetch-st)
        fetch-st
        (with-handlers ([values (fetch-exn-handler fetch-st)])
          (update-fetch-state method-name
                              fetch-st
                              (f (fetch-state-source fetch-st)
                                 (fetch-state-request-transfer fetch-st)))))))


(define (update-fetch-state method-name fetch-st path-or-#f)
  (attach-fetch-message (struct-copy fetch-state fetch-st [path path-or-#f])
                        (if path-or-#f
                            ($source-fetched (fetch-state-name fetch-st)
                                             (fetch-state-source fetch-st))
                            ($source-method-ruled-out (fetch-state-name fetch-st)
                                                      (fetch-state-source fetch-st)
                                                      (format "~a did not produce a path"
                                                              method-name)))))


(define (fetch-unit name source request-transfer)
  (fetch-state source
               name
               #f
               request-transfer
               null))


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
       (define (fetch-source source make-file)
         (make-file (open-input-string source)
                    (string-length source)))))

  (define mod-source "====[{]::[}]====")

  (define find-message void)

  (define (try-mod-fetch)
    (fetch "mod"
           (list mod-source)
           (λ (in est-size)
             (check-equal? est-size (string-length mod-source))
             (check-equal? (read-string est-size in) mod-source))))

  (call-with-temporary-file
   (λ (tmp)
     (write-to-file plugin-module-datum tmp #:exists 'truncate/replace)
     (parameterize ([(setting-derived-parameter XIDEN_MODS_MODULE) tmp])
       (define source (path->string tmp))
       (define (request-transfer in est-size)
         (test-equal? "Read file" (read in) plugin-module-datum)
         (test-true "Estimate file size" (>= est-size (file-size tmp)))
         tmp)

       (test-equal? "Fetch from file"
                    (fetch "anon" (list source) request-transfer)
                    (fetch-state source
                                 "anon"
                                 tmp
                                 request-transfer
                                 (list ($source-fetched "anon" source))))

       (test-equal? "Fetch from mod"
                    (car (fetch-state-messages (try-mod-fetch)))
                    ($source-fetched "mod" mod-source)))))

  ; Notice we just left the parameterize that set the mod path
  (test-case "Investigate fetch failures"
    (define messages (fetch-state-messages (try-mod-fetch)))
    (test-equal? "Fetch fails when mod is not available"
                 messages
                 (list ($fetch-failure "mod"))))

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

    (define fetch-output (fetch "anon" (list source) request-transfer))

    (check-equal? (struct-copy fetch-state fetch-output
                               [messages (list (car (fetch-state-messages fetch-output)))])
                  (fetch-state source
                               "anon"
                               "fake"
                               request-transfer
                               (list ($source-fetched "anon" source))))))


(define-message-formatter fetch-message-formatter
  [($source-fetched name user-string)
   (format "Fetched ~s" user-string)]

  [($unverified-host url)
   (format (~a "~a does not have a valid certificate.~n"
               "Connections to this server are not secure.~n"
               "To trust servers without valid certificates, use ~a.")
           url
           (setting-long-flag XIDEN_TRUST_UNVERIFIED_HOST))])
