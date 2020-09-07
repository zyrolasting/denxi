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
         "contract.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "message.rkt"
         "printer.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "sentry.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt")

(define+provide-message $compilation-error (module-path message))
(define+provide-message $module-compiled (module-path))


(define-message-formatter format-worker-message
  [($module-compiled module-path)
   (format "Compiled: ~a" module-path)]

  [($compilation-error module-path message)
   (format "Bytecode compilation error in: ~a~n~a"
           module-path
           message)])


(define xiden-worker%
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
      (parameterize ([workspace-directory workspace-dir])
        (call-with-applied-settings (hash->list dump)
                                    (λ () (loop)))))

    (define/public (handle-$stop)
      (exit 0))

    (define/public (handle-$sentinel)
      (send-up ($sentinel)))

    (define/public (handle-$compile module-path)
      (with-handlers
        ([exn? (λ (e) (send-output ($compilation-error module-path (exn->string e))))])
        (managed-compile-zo module-path)
        (send-output ($module-compiled module-path))))))


(define (main pch)
  (send (new xiden-worker% [pch pch]) pump))


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

  (define (call-for-active-worker on-exit proc)
    (define th #f)
    (parameterize ([exit-handler
                    (λ (v)
                      (on-exit v)
                      (kill-thread th))])
      (set! th (thread proc))
      (define alarm
        (alarm-evt (+ (current-inexact-milliseconds)
                      100)))
      (when (eq? alarm (sync alarm (thread-dead-evt th)))
        (kill-thread th)
        (fail "Infinite loop in worker"))))


  (define worker (new xiden-worker% [pch for-worker]))

  (test-true "Created instance without incident"
             (is-a? worker xiden-worker%))

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
    (place-channel-put for-tests ($sentinel))
    (place-channel-put for-tests ($sentinel))
    (place-channel-put for-tests ($sentinel))
    (place-channel-put for-tests ($stop))
    (call-for-active-worker
     (λ (v)
       (expect-output ($sentinel)
                      ($sentinel)
                      ($sentinel))
       (check-eq? v 0))
     (λ () (send worker loop))))

  (test-workspace "Initialize worker with workspace and configuration"
    (call-for-active-worker (λ (v)
                              (check-equal? (workspace-directory) (current-directory))
                              (check-eq? (XIDEN_FETCH_TIMEOUT_MS) 10))
                            (λ ()
                              (place-channel-put for-tests ($stop))
                              (send worker
                                    handle-$start
                                    (current-directory)
                                    (hash XIDEN_FETCH_TIMEOUT_MS 10)))))

  (test-workspace "Compile Racket modules"
    (display-to-file "#lang racket/base" "a.rkt")
    (display-to-file "#lang racket/base" "b.rkt")
    (display-to-file "!#sd=f-*" "junk.rkt")
    (display-to-file "(module content racket/base (void))" "c.ss")
    (display-to-file "#lang scribble/base" "d.scrbl")

    (for ([path (in-list (directory-list))])
      (send worker handle-$compile path))

    (for ([name (in-list (list "a_rkt" "b_rkt" "c_ss" "d_scrbl"))])
      (define path (build-path "compiled" name))
      (check-pred file-exists? (path-replace-extension path #".dep"))
      (check-pred file-exists? (path-replace-extension path #".zo")))

    (define output-messages (accum-output))

    (test-true "Report $output" (andmap $output? output-messages))

    (define output-values (map $output-v output-messages))

    (define-values (compile-errors compile-successes)
      (partition $compilation-error? output-values))

    (test-eq? "Report the only error"
              (length compile-errors)
              1)

    (test-eq? "Report all successes"
              (length compile-successes)
              4)

    (define compile-error (car compile-errors))

    (test-pred "Report compilation error"
               $compilation-error?
               compile-error)

    (test-equal? "Report at-fault module using given path"
                 ($compilation-error-module-path compile-error)
                 (build-path "junk.rkt"))

    (test-true "Report expected error as string"
               (regexp-match? #rx"expected a `module` declaration"
                              ($compilation-error-message compile-error)))))
