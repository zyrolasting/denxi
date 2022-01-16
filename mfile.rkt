#lang racket/base

; Define a simplified, in-memory filesystem that allows for I/O
; testing without external effects.

(require racket/contract
         (for-syntax racket/base))

(provide)         

(require racket/function
         racket/generator
         "path.rkt"
         "byte.rkt"
         "string.rkt"
         "sequence.rkt"
         "stream.rkt")

(struct ifs
  (root cwd bios))

(struct ifs-entity
  (name) #:transparent)

(struct ifs-file ifs-entity
  ([content #:mutable]))

(struct ifs-directory ifs-entity
  (entries) #:transparent)

(struct ifs-link ifs-entity
  (path))

(struct ifs-io
  (port [position #:mutable]))

(define (ifs-root-directory? d)
  (and (ifs-directory? d)
       (equal? ifs-root-path (ifs-entity-name d))))

;--------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (define ifs (make-ifs))
  (check-pred ifs-directory? (ifs-root ifs))
  (check-equal? ((ifs-cwd ifs)) (build-path "/")))

(define-syntax-rule (ifs-skeleton . patterns)
  (ifs-skeleton* (in-path-pattern . patterns)))

(define (ifs-skeleton* in-root-relative)
  (define root (make-ifs-directory "/"))
  (for ([path in-root-relative])
    (ifs-sync-path-structure root path))
  root)

(define (ifs-sync-path-structure initial-directory path)
  (define terminal-case
    (let ([file-name (file-name-from-path path)])
      (λ (d)
        (when file-name
          (ifs-directory-set d (ifs-file file-name #"")))
        initial-directory)))

  (define (nest directory name)
    (define new-directory (make-ifs-directory name))
    (ifs-directory-set directory new-directory)
    new-directory)
  
  (let loop ([path-elements (in-list (explode-path path))]
             [directory initial-directory])
    (stream-consume path-elements
                    (lambda (head tail)
                      (if (stream-empty? tail)
                          (terminal-case directory)
                          (loop tail (nest directory head))))
                    (lambda () initial-directory))))


(define ifs-root-path
  (build-path "/"))


(define (make-ifs)
  (ifs (make-ifs-directory ifs-root-path)
       (make-parameter ifs-root-path)
       (make-hash)))

  
(define (make-ifs-directory name)
  (ifs-directory (build-path name) (make-hash)))


(define (ifs-project ifs path consequent alternate)
  (define-values (initial-elements initial-entities)
    (if (complete-path? path)
        (values (reverse (cdr (explode-path path)))
                (list (ifs-root ifs)))
        (values (reverse (explode-path path))
                (list ((ifs-cwd ifs))))))
  (let/ec abort
    (define fail (compose abort alternate))
    (let loop ([elements initial-elements] [entities initial-entities])
      (if (null? elements)
          (if (null? entities)
              (alternate elements entities)
              (consequent entities))
          (let ([next (car elements)])
            (loop (cdr elements)
                  (case next
                    [(up)
                     (cdr entities)]
                    [(same)
                     entities]
                    [else
                     (cons (ifs-directory-ref (car entities)
                                              next
                                              (curry fail elements entities))
                           entities)])))))))



;--------------------------------------------------------------------------------

(define (call-with-ifs-directory ifs path proc fail)
  (ifs-project ifs
               path
               (lambda (entities)
                 (parameterize ([(ifs-cwd ifs) path])
                   (proc)))
               fail))
 

(define (in-ifs-directory dir)
  (in-generator
   (let loop ([current dir])
     (yield (ifs-entity-name current))
     (when (ifs-directory? current)
       (for ([(path entry) (in-hash (ifs-directory-entries dir))])
         (loop entry))))))


(define (ifs-directory-list #:compare [compare #f] ifsd)
  (let ([seq (in-hash-keys (ifs-directory-entries ifsd))])
    (if compare
        (sort (sequence->list seq) compare)
        seq)))


(define (ifs-directory-remove ifsd pathname)
  (hash-remove! (ifs-directory-entries ifsd)
                pathname))


(define (ifs-directory-set ifsd entity)
  (hash-set! (ifs-directory-entries ifsd)
             (ifs-entity-name entity)
             entity))


(define (ifs-directory-ref ifsd pathname [fail-result #f])
  (hash-ref (ifs-directory-entries ifsd)
            pathname
            fail-result))



(define-syntax-rule (for/effect for-clauses . body)
  (for ([t (in-list (for/list for-clauses (lambda () . body)))]) (t)))


(define (ifs-flush ifs)
  (for ([(path io) (in-hash (ifs-bios ifs))])
    (let ([port (ifs-io-port io)])
      (when (and (output-port? port) (not (port-closed? port)))
        (flush-output port)))))


(define (ifs-clean ifs)
  (define bios (ifs-bios ifs))
  (for/effect ([(path io) (in-hash bios)])
    (when (port-closed? (ifs-io-port io))
      (hash-remove! bios path))))


(define (open-output-ifs-file #:position [position 0]
                              #:offset [offset 0]
                              ifs
                              path-like
                              [buffer-size (expt 2 13)])
  (define path
    (build-path path-like))
  
  (define existing
    (ifs-project ifs path values (lambda () #f)))

  (define file-granny
    (make-granny (make-bytes buffer-size)))

  ; contract captures bad paths
  (define file-name
    (file-name-from-path path))
  
  (define target-file
    (if existing
        (car existing)
        (ifs-file (file-name-from-path path)
                  #"")))

  (define initial-position
    (+ offset
       (if (eof-object? position)
           (bytes-length (ifs-file-content target-file))
           position)))

  (define (write-out to-write start end . ignored)
    (give-patch file-granny
                (subbytes to-write start end)
                (ifs-io-position target-file)))

  (define (close-ifs-output-port)
    (flush-output output-port)
    (set-ifs-file-content! target-file (take-quilt file-granny))
    (ifs-directory-set (cadr existing) target-file))

  
  (define output-port
    (make-output-port (ifs-entity-name target-file)
                      always-evt ; grannies add thread safety
                      write-out
                      close-ifs-output-port))

  (hash-set! (ifs-bios ifs)
             (ifs-complete-path ifs path)
             (ifs-io output-port initial-position))

  output-port)

(define (ifs-complete-path ifs path)
  (path->complete-path path ((ifs-cwd ifs))))



(define (open-input-intraprocess-file path)
  (ifs-project ifs
               path
               (λ (projection)
                 (define target (car projection))
                 (and (ifs-file? target)
                      (open-input-bytes (ifs-file-content target))))
               (lambda () #f)))

  

(define ifs-file-position
  (case-lambda
    [(io)
     (ifs-io-position io)]
    [(io pos)
     (set-ifs-io-position! io pos)]))



(module+ test
  (test-case "In-memory directories"
    (define d (ifs-directory (build-path "a") (make-hash)))
    (define x (ifs-file "x" #"xxx"))
    (define y (ifs-file "y" #"yyy"))
    (define z (ifs-file "z" #"zzz"))

    (ifs-directory-set d x)
    (ifs-directory-set d y)
    (ifs-directory-set d z)

    (check-true
     (for/and ([path (ifs-directory-list d)])
       ((or/c "x" "y" "z") path)))

    (check-equal? (ifs-directory-list #:compare string<? d)
                  '("x" "y" "z"))

    (check-eq? (ifs-directory-ref d "x") x)
    (check-eq? (ifs-directory-ref d "y") y)
    (check-eq? (ifs-directory-ref d "z") z)

    (ifs-directory-remove d "z")

    (check-false (ifs-directory-ref d "z"))
    (check-true (ifs-directory-ref d "z" #t))))
