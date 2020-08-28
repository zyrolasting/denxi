#lang racket/base

; Define procedures to write program output to ports

(require "contract.rkt")
(provide
 (contract-out
  [write-output
   (->* ($message?) (output-port?) void?)]))

(require racket/date
         racket/fasl
         racket/match
         racket/pretty
         racket/serialize
         "actor.rkt"
         "derivation.rkt"
         "exn.rkt"
         "format.rkt"
         "input-info.rkt"
         "localstate.rkt"
         "message.rkt"
         "mod.rkt"
         "package.rkt"
         "package-info.rkt"
         "port.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "sentry.rkt"
         "setting.rkt"
         "worker.rkt"
         "workspace.rkt")

(define-message $test-print (v))

(define (filter-output m)
  (if ($verbose? m)
      (and (XIDEN_VERBOSE)
           ($verbose-message m))
      m))

(define (write-output v [out (current-output-port)])
  (define maybe-message (filter-output v))
  (when maybe-message
    (parameterize ([current-output-port out])
      (define to-send
        (if (XIDEN_READER_FRIENDLY_OUTPUT)
            maybe-message
            (format-xiden-message maybe-message)))

      (if (XIDEN_FASL_OUTPUT)
          (s-exp->fasl (serialize to-send) (current-output-port))
          (if (XIDEN_READER_FRIENDLY_OUTPUT)
              (pretty-write #:newline? #t to-send)
              (displayln to-send)))
      (flush-output))))

(define (format-package-info info)
  (package-info-package-name info))

(define (format-setting-flag-example s)
  (format "~a/~a"
          (setting-short-flag s)
          (setting-long-flag s)))

(define (format-xiden-message m)
  (match m
    [($test-print v)
     (format "Testing: ~a" v)]

    [($output v)
     (format-xiden-message v)]

    [($fail v)
     (cond [(exn? v) (exn->string v)]
           [(string? v) v]
           [else (~s v)])]

    [($show-string v) v]

    [($show-datum v)
     (pretty-format #:mode 'write v)]

    [($module-compiled module-path)
     (format "Compiled: ~a" module-path)]

    [($compilation-error module-path message)
     (format "Bytecode compilation error in: ~a~n~a"
             module-path
             message m)]

    [($input-integrity-mismatch input source)
     (format (~a "~a failed its integrity check.~n"
                 "While unsafe, you can force installation using ~a.")
             (format-package-info source)
             (setting-long-flag XIDEN_TRUST_BAD_DIGEST))]

    [($mod-load-failure path error-string)
     (format (~a "Could not load plugin module ~a. Using default implementations.~n"
                 "Load error: ~a")
             path
             error-string)]

    [($input-signature-mismatch input source)
     (format (~a "~s's signature does not match any trusted public key.~n"
                 "While unsafe, you can trust bad signatures using ~a.")
             source
             (setting-long-flag XIDEN_TRUST_BAD_SIGNATURE))]

    [($input-signature-missing (input-info name _ _ _) source)
     (format (~a "~a does not have a signature. If you are testing a package, this is expected.~n"
                 "If you got the package from the Internet, then exercise caution!~n"
                 "To trust unsigned packages, use ~a.")
             name
             (setting-long-flag XIDEN_TRUST_UNSIGNED))]

    [($unverified-host url)
     (format (~a "~a does not have a valid certificate.~n"
                 "Connections to this server are not secure.~n"
                 "To trust servers without valid certificates, use ~a.")
             url
             (setting-long-flag XIDEN_TRUST_UNVERIFIED_HOST))]

    [($package-installed info)
     (format "Installed package ~a"
             (format-package-info info))]

    [($declare-input digest path)
     (format "New input ~a located at ~a" digest path)]

    [($unrecognized-command m)
     (format "Unrecognized command: ~s. Run with -h for usage information.~n"
             ($unrecognized-command-command m))]

    [($transfer-progress name scalar timestamp)
     (format "~a: ~a%" name (~r (* 100 scalar) #:precision 0))]

    [($consent-note)
     (format "To consent to these changes, run again with ~a"
             (setting-short-flag XIDEN_CONSENT))]

    [($source-unfetched user-string)
     (format "Cannot find content for ~s" user-string)]

    [($source-fetched user-string)
     (format "Fetched ~s" user-string)]

    [($setting-not-found name)
     (format "There is no setting called ~s.~n" name)]

    [($init-localstate path)
     (format "Initalizing local state at ~a" path)]

    [($setting-value-rejected name value expected)
     (format "Invalid value for ~a: ~a~n  expected: ~a~n  (Note: (void) only applies for `xiden config repl` use)"
             name
             value
             expected)]

    [($invalid-workspace-envvar)
     (format "Ignoring envvar value for XIDEN_WORKSPACE: ~a~n  falling back to ~a"
             (getenv "XIDEN_WORKSPACE")
             (workspace-directory))]

    [($undeclared-racket-version info)
     (join-lines
      (list (format "~a does not declare a supported Racket version."
                    (format-package-info info))
            (format "To install this package anyway, run again with ~a"
                    (setting-short-flag XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))))]

    [($unsupported-racket-version info)
     (join-lines
      (list (format "~a claims that it does not support this version of Racket (~a)."
                    (format-package-info info)
                    (version))
            (format "Supported versions (ranges are inclusive):~n~a~n"
                    (join-lines
                     (map (λ (variant)
                            (format "  ~a"
                                    (if (pair? variant)
                                        (format "~a - ~a"
                                                (or (car variant)
                                                    PRESUMED_MINIMUM_RACKET_VERSION)
                                                (or (cdr variant)
                                                    PRESUMED_MAXIMUM_RACKET_VERSION))
                                        variant))
                            (package-info-racket-versions info)))))
            (format "To install this package anyway, run again with ~a"
                    (setting-long-flag XIDEN_ALLOW_UNSUPPORTED_RACKET))))]

    [_ (error 'format-xiden-message "Unknown message type: ~s" m)]))


(module+ test
  (require rackunit
           "setting.rkt"
           (submod "package-info.rkt" test))

  (define (capture-bytes p)
    (define buffer (open-output-bytes))
    (p buffer)
    (get-output-bytes buffer #t))

  (define (test-output msg v expected)
    (define buffer (open-output-bytes))

    ; parameterize adds coverage for default value in write-output
    (parameterize ([current-output-port buffer])
      (write-output v))

    (test-true msg
               (if (or (regexp? expected)
                       (pregexp? expected))
                   (regexp-match? expected (get-output-bytes buffer #t))
                   (equal? (get-output-bytes buffer #t) expected))))

  (test-output "By default, program output is human-friendly"
               ($test-print 1)
               #px"Testing: 1")

  (XIDEN_READER_FRIENDLY_OUTPUT #t
    (λ ()
      (test-output "Allow reader-friendly output"
                   ($test-print 1)
                   (capture-bytes
                    (λ (o)
                      (pretty-write #:newline? #t ($test-print 1) o))))

      (XIDEN_FASL_OUTPUT #t
        (λ ()
          (test-case "Allow FASL output"
            (define in
              (open-input-bytes
               (capture-bytes
                (λ (o) (write-output ($test-print 1) o)))))

            (check-equal? (deserialize (fasl->s-exp in))
                          ($test-print 1)))))))

  (test-case "Control verbose output"
    (XIDEN_VERBOSE #f
      (λ ()
        (test-output "Opt out of verbose output"
                     ($verbose ($stop))
                     #"")))

    (parameterize ([(setting-derived-parameter XIDEN_VERBOSE) #t]
                   [(setting-derived-parameter XIDEN_READER_FRIENDLY_OUTPUT) #t])
      (test-output "Opt into verbose output"
                   ($verbose ($stop))
                   #px"\\$stop"))))
