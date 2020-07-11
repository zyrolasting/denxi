#lang racket/base

; Define a configurable sandbox evaluator that allows a user to pass
; control to a package's installer.

(provide make-installer
         flush-installer-output
         (struct-out installer))

(require racket/exn
         racket/format
         racket/path
         racket/function
         racket/sandbox
         "workspace.rkt"
         "config.rkt"
         "logging.rkt"
         "dependency.rkt"
         "url.rkt"
         "zcpkg-info.rkt")


(struct installer (sandboxed-eval name info)
  #:property
  prop:procedure
  (λ (self datum)
    ((installer-sandboxed-eval self) datum))

  #:methods
  gen:custom-write
  [(define write-proc
     (λ (self out mode)
       (write-string (format "#<~a-installer: ~a>"
                             (if (eq? void (installer-sandboxed-eval self))
                                 "inactive"
                                 "active")
                             (installer-name self))
                     out)))])


(define (make-installer zcpkg-path info)
  (define name (dependency->string (zcpkg-info->dependency info)))
  (define installer-path (get-installer-path zcpkg-path name info))
  (installer
   (if installer-path
       (parameterize ([sandbox-output 'pipe]
                      [sandbox-error-output 'pipe]
                      [sandbox-init-hook (make-sandbox-init zcpkg-path)])
         (make-module-evaluator #:language 'racket/base installer-path))
       void)
   name
   info))


(define (get-installer-path zcpkg-path name info)
  (define installer-path (zcpkg-info-installer info))
  (define path-to-verify (simplify-path (build-path zcpkg-path installer-path)))
  (and installer-path
       (if (path-prefix? path-to-verify zcpkg-path)
           path-to-verify
           (error 'setup
                  "~a's installer path reaches outside of install directory."
                  name))))


(define (make-sandbox-init zcpkg-path)
  (λ () ; Run this in context of sandbox.
    (local-require "config.rkt"
                   "workspace.rkt")
    (current-directory zcpkg-path)
    (ZCPKG_WORKSPACE (find-workspace-directory))
    (reload-configuration!)))


(define (flush-installer-output ins)
  (pump-lines-from-port (installer-name ins)
                        (get-output (installer-sandboxed-eval ins))
                        (current-output-port))
  (pump-lines-from-port (installer-name ins)
                        (get-error-output (installer-sandboxed-eval ins))
                        (current-error-port)))


(define (pump-lines-from-port name in out)
  (define source
    (or (sync/timeout 0.05 in)
        (open-input-bytes #"")))
  (define line (read-line source))
  (or (eof-object? line)
      (begin (<< "~a: ~a~n" name line)
             (pump-lines-from-port name source out))))

(define (path-prefix? to-check prefix-pathy)
  (define maybe-prefixed (explode-path (simplify-path (path->complete-path to-check))))
  (define pref (explode-path (simplify-path (path->complete-path prefix-pathy))))

  (and (<= (length pref)
           (length maybe-prefixed))
       (for/and ([(el index) (in-indexed pref)])
         (equal? (list-ref maybe-prefixed index)
                 el))))
