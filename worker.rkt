#lang racket/base

(provide (all-defined-out))

(require racket/contract
         racket/exn
         racket/format
         racket/function
         racket/list
         racket/match
         racket/path
         racket/place
         "archiving.rkt"
         "config.rkt"
         "dependency.rkt"
         "download.rkt"
         "file.rkt"
         "installer.rkt"
         "logging.rkt"
         "message.rkt"
         "prompt.rkt"
         "url.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt")

(struct workstate (id pch dependencies)
  #:property prop:evt (struct-field-index pch)
  #:property prop:procedure
  (λ (self v)
    (place-channel-put (workstate-pch self) v)
    self))

(define (worker-main pch [state (workstate #f pch (hash))])
  (with-handlers ([exn:break? void]
                  [exn? (λ (e) (state ($on-error (exn->string e))))]
                  [place-message-allowed? state]
                  [(const #t) (λ (v) (state (~s v)))])
    (worker-main pch
                 (handle-message state (sync state)))))

(define (assign-id state id)
  (define next (struct-copy workstate state [id id]))
  (handle-message (next ($on-idle id))
                  (sync next)))

(define (install-package state package-source)
  (void))

(define (uninstall-package state dependency-string)
  (void))

(define (on-new-dependencies state dependent-eds dependencies-ds-list)
  (void)
  #;(prompt/confirmation #:dangerous? #f
                       #:param ZCPKG_INSTALL_DEPENDENCIES
                       (~a* (~a dependent-name " needs the following to work: ")
                            (string-join (map (λ (s) (~a "  " s)) dependencies) "\n")
                            "Install these too?"))
  #;(process-jobs team
                  (add-jobs jobs
                            ($install-package dependent)
                            (map $install-package dependencies))))


(define (before-making-orphans state affected)
  (void)
  #;(prompt/confirmation #:dangerous? #f
                       #:param ZCPKG_UNINSTALL_ORPHANS
                       (~a* (~a "Uninstalling " name " will orphan the following dependent packages.")
                            (string-join (map (λ (s) (~a "  " s)) affected) "\n")
                               "Do you want to uninstall them too?")))


(define (on-bad-digest state name)
  (void)
  #;(prompt/confirmation
   #:param ZCPKG_TRUST_BAD_DIGEST
   (~a* (~a name " may have been corrupted or altered.")
        "Do you want to install this package anyway?")))


(define (on-bad-signature message state)
  (void)
  #;(prompt/confirmation
   #:param ZCPKG_TRUST_BAD_SIGNATURE
   (~a* (~a name "'s signature does not match the provider's key.")
        "If you are testing your own package, you can safely proceed."
        "Otherwise, proceeding means running code from an unverified source."
        "Do you want to install this package anyway?")))

(define (on-missing-signature message state)
  (void)
  #;(prompt/confirmation
   #:param ZCPKG_TRUST_UNSIGNED
   (~a* (~a name " is unsigned.")
        "If you are testing your own package, you can safely proceed."
        "Otherwise, proceeding means running code from an unverified source."
        "Do you want to install this package anyway?")))

(define (on-unverified-host message state) (void))
(define (on-package-installed message state) (void))
(define (on-user-refusal message state) (void))

(define (echo state value)
  (state ($echo value))
  (state ($on-idle (workstate-id state))))

(define (stop state)
  (exit 0))

(define-message-pump (handle-message workstate?)
  assign-id
  echo
  stop)
