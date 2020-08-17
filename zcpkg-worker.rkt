#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/exn
         racket/path
         racket/place
         racket/sequence
         compiler/cm
         "actor.rkt"
         "archiving.rkt"
         "config.rkt"
         "contract.rkt"
         "download.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "message.rkt"
         "racket-version.rkt"
         "resolve.rkt"
         "sentry.rkt"
         "setup.rkt"
         "signature.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-messages.rkt"
         "zcpkg-query.rkt"
         "zcpkg-settings.rkt")

(define zcpkg-worker%
  (class actor%
    (super-new)
    (inherit pump)
    (init-field pch)

    (define/public (send-up v)
      (place-channel-put pch v))

    (define/public (send-output v)
      (send-up ($output v)))

    (define/public (loop)
      (with-handlers ([(const #t) (λ (e) (fail e))])
        (let again ()
          (pump (sync pch))
          (again))))

    (define/public (fail e)
      (send-up ($fail (if (exn? e) (exn->string e) (~s e))))
      (exit 1))

    (define/public (handle-$start workspace-dir dump)
      (workspace-directory workspace-dir)
      (configure-zcpkg! dump))

    (define/public (handle-$stop)
      (exit 0))

    (define/public (handle-$sentinel)
      (send-up ($sentinel)))

    (define/public (compile-racket-modules install-path)
      (for ([module-path (in-racket-modules install-path)])
        (with-handlers
          ([exn? (λ (e) (send-output ($on-compilation-error module-path (exn->string e))))])
          (managed-compile-zo module-path)
          (send-output ($on-module-compiled module-path)))))

    (define/public (setup-package info dependency-infos exprs)
      (compile-racket-modules (zcpkg-info->install-path info))
      (sequence-for-each (λ (o) (send-output o))
                         (sequence-append
                          (make-zcpkg-links info dependency-infos)
                          (create-launchers info)
                          (load-in-setup-module info exprs))))

    (define/public (install-local-package info dependency-infos package-path)
      (define install-path (zcpkg-info->install-path info))

      (unless (equal? package-path install-path)
        (make-directory* (path-only install-path))
        (if (ZCPKG_LINK)
            (make-file-or-directory-link package-path install-path)
            (copy-directory/files package-path install-path)))

      (send-output ($on-package-installed info)))


    (define/public (install-remote-package info dependency-infos)
      (define install-path   (zcpkg-info->install-path info))
      (define query          (coerce-zcpkg-query info))
      (define artifact-path  (download-artifact query))
      (define public-key     (download-public-key (zcpkg-query-provider-name query)))

      (define integrity-info (zcpkg-integrity-info #f #f))
      (define signature-info (zcpkg-signature-info #f #f #f))

      (define integrous?     (zcpkg-integrity-check integrity-info artifact-path))
      (define authenticated? (zcpkg-signature-check integrity-info signature-info))

      (unless integrous?
        (send-output ($on-bad-digest info)))

      (unless authenticated?
        (send-output ((if (zcpkg-info-signature info)
                              $on-bad-signature
                              $on-missing-signature)
                          info)))

      (when (and integrous? authenticated?)
        (make-directory* (path-only install-path))
        (unpack artifact-path install-path)
        (install-local-package info dependency-infos install-path)))

    (define/public (handle-$setup-package info dependency-infos exprs)
      (setup-package info dependency-infos exprs))

    (define/public (handle-$install-package info dependency-infos url-or-path)
      (define (do-install)
        (if (directory-exists? url-or-path)
            (install-local-package info dependency-infos url-or-path)
            (install-remote-package info dependency-infos)))

      (if (zcpkg-installed? info)
          (send-output ($already-installed info))
          (case (check-racket-version-ranges (version) (zcpkg-info-racket-versions info))
            [(supported) (do-install)]
            [(unsupported)
             (if (ZCPKG_ALLOW_UNSUPPORTED_RACKET)
                 (do-install)
                 (send-output ($unsupported-racket-version info)))]
            [(undeclared)
             (if (or (ZCPKG_ALLOW_UNSUPPORTED_RACKET)
                     (ZCPKG_ALLOW_UNDECLARED_RACKET_VERSIONS))
                 (do-install)
                 (send-output ($undeclared-racket-version info)))])))))


(define (main pch)
  (send (new zcpkg-worker% [pch pch]) loop))


(module+ test
  (require racket/list
           rackunit
           (submod "file.rkt" test))

  (define-values (for-tests for-worker) (place-channel))

  (define (expect-output . expected-output)
    (for ([expected (in-list expected-output)])
      (define actual (sync/timeout 0 for-tests))
      (if (procedure? expected)
          (check-pred expected actual)
          (check-equal? actual expected)))
    (when (sync/timeout 0 for-tests)
      (fail "Extra output left in place channel")))

  (define (accum-output [l null])
    (define m (sync/timeout 0 for-tests))
    (if m (accum-output (cons m l)) (reverse l)))

  (define worker (new zcpkg-worker% [pch for-worker]))

  (test-true "Created instance without incident"
             (is-a? worker zcpkg-worker%))

  (test-case "Send data on place channel"
    (send worker send-up 1)
    (expect-output 1))

  (test-case "Send $output on place channel"
    (send worker send-output 1)
    (expect-output ($output 1)))

  (test-case "Exit with data on failure"
    (parameterize ([exit-handler (λ (v) (check-eq? v 1))])
      (define e (exn:fail "blah blah" (current-continuation-marks)))
      (send worker fail '(value 1 "cool"))
      (send worker fail e)
      (expect-output ($fail "(value 1 \"cool\")")
                     ($fail (exn->string e)))))

  (test-case "Echo $sentinels, and $stop"
    (define th #f)
    (parameterize ([exit-handler
                    (λ (v)
                      (expect-output ($sentinel)
                                     ($sentinel)
                                     ($sentinel))
                      (check-eq? v 0)
                      (kill-thread th))])
      (place-channel-put for-tests ($sentinel))
      (place-channel-put for-tests ($sentinel))
      (place-channel-put for-tests ($sentinel))
      (place-channel-put for-tests ($stop))
      (set! th (thread (λ () (send worker loop))))

      (define alarm
        (alarm-evt (+ (current-inexact-milliseconds)
                      100)))

      (when (eq? alarm (sync alarm (thread-dead-evt th)))
        (kill-thread th)
        (fail "Infinite loop in worker"))))

  (test-workspace "Initialize worker with workspace and configuration"
    (send worker
          handle-$start
          (current-directory)
          (hash 'ZCPKG_SANDBOX_EVAL_TIME_LIMIT_SECONDS 10))
    (check-equal? (workspace-directory) (current-directory))
    (check-eq? (ZCPKG_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
               10))

  (test-workspace "Compile Racket modules"
    (display-to-file "#lang racket/base" "a.rkt")
    (display-to-file "#lang racket/base" "b.rkt")
    (display-to-file "!#sd=f-*" "junk.rkt")
    (display-to-file "(module content racket/base (void))" "c.ss")
    (display-to-file "#lang scribble/base" "d.scrbl")
    (send worker compile-racket-modules (current-directory))

    (for ([name (in-list (list "a_rkt" "b_rkt" "c_ss" "d_scrbl"))])
      (define path (build-path "compiled" name))
      (check-pred file-exists? (path-replace-extension path #".dep"))
      (check-pred file-exists? (path-replace-extension path #".zo")))

    (define output-messages (accum-output))

    (test-true "Report $output" (andmap $output? output-messages))

    (define output-values (map $output-v output-messages))

    (define-values (compile-errors compile-successes)
      (partition $on-compilation-error? output-values))

    (test-eq? "Report the only error"
              (length compile-errors)
              1)

    (test-eq? "Report all successes"
              (length compile-successes)
              4)

    (define compile-error (car compile-errors))

    (test-pred "Report compilation error"
               $on-compilation-error?
               compile-error)

    (test-pred "Report at-fault module using a complete path"
               complete-path?
               ($on-compilation-error-module-path compile-error))

    (test-true "Report expected error as string"
               (regexp-match? #rx"expected a `module` declaration"
                              ($on-compilation-error-message compile-error))))

  (test-case "Detect packages that do not declare a supported Racket version"
    (define info (make-zcpkg-info #:provider-name "provider"
                                  #:package-name "pkg"
                                  #:racket-versions null))
    (send worker handle-$install-package info null "")
    (expect-output ($output ($undeclared-racket-version info))))

  (test-case "Detect packages that declare an unsupported Racket version"
    (define info (make-zcpkg-info #:provider-name "provider"
                                  #:package-name "pkg"
                                  #:racket-versions (list "0.0")))
    (send worker handle-$install-package info null "")
    (expect-output ($output ($unsupported-racket-version info)))))
