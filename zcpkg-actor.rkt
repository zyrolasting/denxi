#lang racket/base

(provide (all-defined-out))

(require (only-in racket/generator in-generator yield)
         racket/contract
         racket/function
         racket/exn
         racket/place
         racket/set
         "actor.rkt"
         "config.rkt"
         "message.rkt"
         "string.rkt"
         "workspace.rkt"
         "zcpkg-settings.rkt")


; Flow control messages
(define-message $start (config))
(define-message $schedule (dependent dependencies))
(define-message $frontlog (schedule))
(define-message $backlog  (schedule))
(define-message $crash (message))
(define-message $done (message))
(define-message $stop ())

; Package management messages
(define-message $before-making-orphans (dependents dependency))
(define-message $resolve-source (source requesting-directory order))
(define-message $on-bad-digest (info))
(define-message $on-bad-signature (info))
(define-message $on-missing-signature (info))
(define-message $on-unverified-host (host))
(define-message $on-package-installed (info))
(define-message $on-package-uninstalled (info))
(define-message $install-package (info path source))
(define-message $download-package (info catalog-url source))
(define-message $uninstall-package (dependency-string))

; Coordinate async work dedicated to package management.
(define zcpkg-team%
  (class team%
    (super-new)
    (inherit stop! loop)

    (field [frontlog null]
           [backlog null])

    (define/override (idle?)
      (and (super idle?)
           (null? frontlog)))

    ; A lot goes on here. This processes the given tasks,
    ; prioritizing work discovered along the way.  ("Hey, I found
    ; this dependency. We need to take care of it first", etc). The
    ; return value is a task backlog that the user can review before
    ; deciding how to proceed.
    (define/public (process-frontlog! tasks)
      (set! frontlog (append tasks frontlog))
      (loop)
      (define-values (for-review seen)
        (for*/fold ([for-review null]
                    [seen (set)])
                   ([schedule (in-list backlog)]
                    [next (in-schedule schedule)])
          (if (set-member? seen next)
              (values for-review
                      seen)
              (values (cons next for-review)
                      (set-add seen next)))))

      (reverse for-review))

    (define/public (handle-$done id finished-job)
      (define reporter (list-ref (get-field workers this) id))
      (if (null? frontlog)
          (send reporter value #f)
          (let*-values ([(next-job remaining) (get-next-message (car frontlog))])
            (reporter ($schedule-dependent next-job))
            (set! frontlog
                  (if remaining
                      (cons remaining (cdr frontlog))
                      (cdr frontlog))))))

    (define/public (handle-$crash id exn-string)
      (stop!)
      (raise (exn:fail:user
              (format "A worker crashed. Please report this to the developer:~n~a~n"
                      exn-string)
              (current-continuation-marks))))

    (define/public (handle-$frontlog id v)
      (set! frontlog (cons v frontlog)))

    (define/public (handle-$backlog id v)
      (set! backlog (cons v backlog)))))



(define zcpkg-worker%
  (class actor%
    (super-new)

    (define/public (handle-$start workspace-dir)
      (workspace-directory workspace-dir)
      (load-zcpkg-settings!))

    (define/public (handle-$resolve-source)
      (void))

    (define/public (handle-$stop state)
      (exit 0))))


(define (zcpkg-worker-main pch)
  (with-handlers ([exn:break? void]
                  [(const #t)
                   (λ (e)
                     (place-channel-put pch
                                        ($crash (if (exn? e) (exn->string e) (~s e)))))])
    (send (new zcpkg-worker%) loop)))

(define (zcpkg-start-team!)
  (define team (new zcpkg-team%))
  (define started? #f)

  (define (send-work tasks)
    (unless started?
      (send team start!
            (λ () (new outside-place%
                       [make-place (λ () (place pch (zcpkg-worker-main pch)))]
                       [make-start-message (λ () ($start (workspace-directory)))]
                       [make-stop-message (λ () ($stop))])))
      (set! started? #t))
    (send team process-frontlog! tasks))

  (define (stop)
    (send team stop!))

  (values send-work stop))


(define (get-next-message node)
  (define deps ($schedule-dependencies node))
  (define first-dep (if (null? deps) #f (car deps)))
  (if first-dep
      (let-values ([(next-job variant) (get-next-message first-dep)])
        (values next-job
                ($schedule ($schedule-dependent node)
                           (if variant
                               (cons variant (cdr deps))
                               (cdr deps)))))
      (values node #f)))


(define (in-schedule s)
  (in-generator
   (let loop ([remaining s])
     (and remaining
          (let-values ([(next remaining*) (get-next-message remaining)])
            (yield ($schedule-dependent next))
            (loop remaining*))))))


(module+ test
  (require rackunit
           racket/sequence)

  (test-case "Resolve dependencies first"
    (define $ $schedule)

    (define jobs
      ($ 'root
         (list
          ($ 'a
             (list ($ 'x null)
                   ($ 'y null)
                   ($ 'z
                      (list ($ 'q null)))))
          ($ 'b
             (list ($ 'r null)
                   ($ 's (list ($ 'q null))))))))


    (check-equal?
     (sequence->list (in-schedule jobs))
     '(x y q z a r q s b root))))
