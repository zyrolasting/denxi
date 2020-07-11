#lang racket/base

; Define an entry point for parallel work, so that the CLI can
; distribute tasks around the user's hardware.  Use a metaphor to help
; the reader understand how this module manages Racket places.

(provide process-jobs)

(require racket/format
         racket/future
         racket/list
         racket/match
         racket/runtime-path
         racket/place
         racket/set
         racket/match
         (only-in "worker.rkt" worker-main)
         "config.rkt"
         "logging.rkt"
         "message.rkt"
         "prompt.rkt"
         "string.rkt")

; Company metaphor: A company has workers (places) and jobs to do (messages).
; The message pump in this module sends jobs to workers.
(struct company (workers jobs)
  #:property prop:evt
  (λ (self) (apply choice-evt (company-workers self))))

; A worker is a place + the place's resources.
; Applying a worker is the same as sending a message to the corresponding place.
; Syncronizing on a worker is the same as waiting for a report from that worker.
(struct worker (id channel idle?)
  #:property prop:evt (struct-field-index channel)
  #:property prop:procedure
  (λ (self v)
    (place-channel-put (worker-channel self) v)
    self))

; Define an entry point for parallel work, such that messages are
; built into the company metaphor.
(define (process-jobs messages)
  (let loop ([team (make-company messages)])
    (and (company? team)
         (loop (update-company team)))))

(define (make-company messages)
  (company (start-workers (length messages)) messages))

(define (start-workers job-count)
  (for/list ([id (in-range (max 1 (min (sub1 (processor-count)) job-count)))])
    (define pch (place inner-pch (worker-main inner-pch)))
    ; The worker needs to identify itself for some messages.
    (place-channel-put pch ($assign-id id))
    (worker id pch #f)))

(define (stop-workers team)
  (for ([w (in-list (company-workers team))])
    (define ch (worker-channel w))
    (place-channel-put ch ($stop))
    (or (sync/timeout 0.5 (place-dead-evt ch))
        (place-kill ch))))

(define (update-company team)
  (if (and (andmap worker-idle? (company-workers team))
           (null? (company-jobs team)))
      (stop-workers team)
      (let ([variant (sync team)])
        (cond [($message? variant)
               (handle-team-event team variant)]
              [(input-port? variant)
               (displayln (read-line variant))
               team]
              [else (write variant)
                    team]))))

;; Message handlers

; If a worker says it has nothing to do, then give it work.
(define (on-idle team id)
  (match-define (company workers jobs) team)
  (define no-jobs? (null? jobs))
  (company (list-update workers id
                        (λ (w)
                          (unless no-jobs?
                            (w (car jobs)))
                          (struct-copy worker w
                                       [idle? no-jobs?])))
           (if no-jobs? null (cdr jobs))))


(define (echo team value)
  (display value)
  team)

(define (on-unverified-host state host)
  (void))

(define (on-new-dependencies team dependent-name dependencies)
  (define answer
    (prompt/confirmation #:dangerous? #f
                         #:param ZCPKG_INSTALL_DEPENDENCIES
                         (~a* (~a dependent-name " needs the following to work: ")
                              (string-join (map (λ (s) (~a "  " s)) dependencies) "\n")
                              "Install these too?")))

  (define new-jobs
    (map $install-package dependencies))

  ; Guard against duplicate jobs
  (define deduped
    (set-union (apply set new-jobs)
               (company-jobs team)))

  (company (company-workers team)
           (append (set->list deduped)
                   (company-jobs team))))

(define (before-making-orphans state name affected)
  (prompt/confirmation #:dangerous? #f
                       #:param ZCPKG_UNINSTALL_ORPHANS
                       (~a* (~a "Uninstalling " name " will orphan the following dependent packages.")
                            (string-join (map (λ (s) (~a "  " s)) affected) "\n")
                            "Do you want to uninstall them too?")))

(define (on-bad-digest state name)
  (prompt/confirmation
   #:param ZCPKG_TRUST_BAD_DIGEST
   (~a* (~a name " may have been corrupted or altered.")
        "Do you want to install this package anyway?")))


(define (on-bad-signature state name)
  (prompt/confirmation
   #:param ZCPKG_TRUST_BAD_SIGNATURE
   (~a* (~a name "'s signature does not match the provider's key.")
        "If you are testing your own package, you can safely proceed."
        "Otherwise, proceeding means running code from an unverified source."
        "Do you want to install this package anyway?")))

(define (on-missing-signature state name)
  (prompt/confirmation
   #:param ZCPKG_TRUST_UNSIGNED
   (~a* (~a name " is unsigned.")
        "If you are testing your own package, you can safely proceed."
        "Otherwise, proceeding means running code from an unverified source."
        "Do you want to install this package anyway?")))


(define-message-pump (handle-team-event company?)
  on-idle
  echo)
