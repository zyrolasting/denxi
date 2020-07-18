#lang racket/base

(provide (all-defined-out))

(require (only-in racket/generator in-generator yield)
         racket/contract
         racket/function
         racket/list
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
(define-message $start    (workspace-directory))
(define-message $frontlog (message))
(define-message $backlog  (message))
(define-message $fail     (to-display))
(define-message $done     ())
(define-message $stop     ())


; Output messages
(define-message $vertex (label))
(define-message $edge (dependent dependency))
(define-message $before-making-orphans (dependents dependency))
(define-message $resolve-source (source requesting-directory))
(define-message $on-bad-digest (info))
(define-message $on-bad-signature (info))
(define-message $on-missing-signature (info))
(define-message $on-unverified-host (host))
(define-message $on-package-installed (info))
(define-message $on-package-uninstalled (info))
(define-message $install-package (source info url-or-path remote?))
(define-message $uninstall-package (dependency-string))



(define (zcpkg-start-team!)
  (define team (new zcpkg-team%))
  (define started? #f)

  (define (work tasks)
    (unless started?
      (send team start!
            (λ () (new outside-place%
                       [make-place (λ () (place pch (zcpkg-worker-main pch)))]
                       [make-start-message (λ () ($start (workspace-directory)))]
                       [make-stop-message (λ () ($stop))])))
      (set! started? #t)))

  (define (stop)
    (send team stop!))

  (values work stop))


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

    (define/public (process-frontlog! tasks)
      (set! frontlog (append tasks frontlog))
      (loop)
      backlog)

    (define/public (handle-$done id finished-job)
      (define reporter (list-ref (get-field workers this) id))
      (if (null? frontlog)
          (send reporter value #f)
          (begin (send reporter value (car frontlog))
                 (set! frontlog (cdr frontlog)))))

    (define/public (handle-$fail id exn-string)
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
    (init-field pch)
    (inherit recv)

    (define/override (idle?)
      #f)

    (define/public (send-up v)
      (place-channel-put pch v))

    (define/public (send-to-backlog v)
      (send-up ($backlog v)))

    (define/public (send-to-frontlog v)
      (send-up ($frontlog v)))

    (define/override (loop)
      (let-values ([(message-id args) (destructure-message (sync (recv)))])
        (apply dynamic-send this (message-id->method-id message-id) args)
        (send-up ($done))
        (loop)))

    (define/public (handle-$start workspace-dir)
      (workspace-directory workspace-dir)
      (load-zcpkg-settings!)
      (send-up ($done)))

    (define/public (handle-$resolve-source)
      (void))

    (define/public (handle-$stop state)
      (exit 0))))


(define (zcpkg-worker-main pch)
  (with-handlers ([exn:break? void]
                  [(const #t)
                   (λ (e)
                     (place-channel-put pch
                                        ($fail (if (exn? e) (exn->string e) (~s e)))))])
    (send (new zcpkg-worker% [pch pch]) loop)))


(define (sort-dependent-work output)
  (define-values ($edges $vertices+rest) (partition $edge? output))
  (define-values ($vertices remaining) (partition $vertex? $vertices+rest))

  (define-values (vertices edges)
    (values (for/mutable-set ([v (in-list $vertices)])
                             ($vertex-label v))
            (for/mutable-set ([e (in-list $edges)])
                             (cons ($edge-dependent e) ($edge-dependency e)))))

  ; Khan's topological sort follows.
  (define leaves (set-copy vertices))
  (for ([e (in-mutable-set edges)])
    (set-remove! leaves (cdr e)))

  (define sorted
    (let loop ([wip null])
      (unless (set-empty? leaves)
        (define n (set-first leaves))
        (set-remove! leaves n)
        (for ([m (in-set vertices)] #:when (set-member? edges (cons n m)))
          (set-remove! edges (cons n m))
          (unless (for/or ([e (in-set edges)]) (equal? (cdr e) m))
            (set-add! leaves m)))
        (loop (cons n wip)))))

  ; The package manager allows dependency cycles, but will need to
  ; know if they exist to warn the user.
  (define acyclic? (set-empty? edges))
  (values acyclic?
          (if acyclic? sorted (map $vertex-label $vertices))
          remaining))
