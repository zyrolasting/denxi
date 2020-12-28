#lang racket/base

; Extend racket/sandbox

(require "contract.rkt")
(provide (all-from-out racket/sandbox)
         (contract-out
          [derive-path-permissions
           (-> list?)]
          [bind-envvar-subset
           (-> (listof (or/c bytes? string?))
               (-> environment-variables?))]))

(require (only-in racket/pretty pretty-write)
         racket/function
         racket/sandbox
         "message.rkt"
         "path.rkt"
         "signature.rkt"
         "string.rkt"
         "url.rkt"
         "workspace.rkt")



;-------------------------------------------------------------------------------
; Extract a selection of envvars for exposure to sandboxed evaluator.

(define (bind-envvar-subset allowed)
  (λ ()
    (apply make-environment-variables
           (for/fold ([mappings null])
                     ([unnormalized-name (in-list allowed)])
             (define name
               (if (string? unnormalized-name)
                   (string->bytes/utf-8 unnormalized-name)
                   unnormalized-name))
             (cons name
                   (cons (environment-variables-ref (current-environment-variables) name)
                         mappings))))))



;-------------------------------------------------------------------------------
; Security guard definition
;
; Packages in the Racket runtime should only concern themselves with
; writing files in designated workspace directories, running
; subprocesses that the user trusts, and downloading data under safety
; limits.
;
; The only way to block an unwanted operation is to raise a value.
; Note that I raise a message type to avoid breaking localization.
; Raising exceptions means imposing a language on the reader.

(define+provide-message $security-incident (reporting-guard summary args))

(define (make-package-security-guard #:trust-any-executable? trust-any-executable?
                                     #:trust-executables trust-executables
                                     #:writeable-directories write-dirs
                                     #:workspace [ws (workspace-directory)])
  (make-security-guard
   (current-security-guard)
   (make-file-guard trust-executables (get-writable-workspace-directories ws))
   (make-network-guard)
   (make-link-guard ws)))


(define (make-file-guard #:trust-any-executable? trust-any-executable?
                         #:trust-executables trust-executables
                         #:writeable-directories write-dirs)
  (let ([trust-executable? (bind-trust-list trust-executables)])

    (λ (sym path-or-#f ops)
      (define (check-destructive-op op path)
        (define test (curry path-prefix? (normalize-path path)))
        (unless (ormap test write-dirs)
          (raise ($security-incident 'file op (list sym path-or-#f ops)))))
      
      (when path-or-#f
        (cond [(member 'execute ops)
               (unless (or (equal? "openssl" (path->string (file-name-from-path path-or-#f)))
                           trust-any-executable?
                           (trust-executable? path-or-#f))
                 (raise ($security-incident 'file
                                            'blocked-execute
                                            (list sym path-or-#f ops))))]

              [(member 'write ops)
               (check-destructive-op 'blocked-write path-or-#f)]

              [(member 'delete ops)
               (check-destructive-op 'blocked-delete path-or-#f)])))))


(define (make-network-guard)
  (λ (sym hostname-or-#f port-or-#f client-or-server)
    (unless hostname-or-#f
      (raise ($security-incident 'network 'blocked-listen
                                 (list sym hostname-or-#f port-or-#f client-or-server))))))


(define (make-link-guard workspace)
  (define (path-ok? p)
    (path-prefix? (simplify-path (if (complete-path? p) p (build-path workspace p)))
                  workspace))

  (λ (op link-path target-path)
    (unless (path-ok? (normalize-path target-path))
      (raise ($security-incident 'link
                                 'blocked-link
                                 (list op link-path target-path))))))



; Use module-level cache because filesystem-root-list may take a while
; on Windows.
(define derive-path-permissions
  (let ([cache #f])
    (λ ()
      (unless cache
        (set! cache
              (append (map (λ (p) `(exists ,p)) (filesystem-root-list))
                      (sandbox-path-permissions))))
      cache)))

(define (get-writable-workspace-directories [wd (workspace-directory)])
  (list (build-path wd "var/xiden")
        (build-path wd "tmp")))

(module+ test
  (require racket/function
           racket/port
           rackunit)
  )
