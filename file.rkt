#lang racket/base

(provide (all-defined-out)
         (all-from-out idiocket/file))

(require idiocket/path
         idiocket/file
         idiocket/set
         idiocket/sequence
         "config.rkt"
         "dependency.rkt"
         "revision.rkt"
         "string.rkt"
         "zcpkg-info.rkt"
         "workspace.rkt")

(define (in-install-directory)
  (in-directory (build-workspace-path (ZCPKG_INSTALL_RELATIVE_PATH))))

(define (in-installed-info-paths)
  (sequence-filter
   (λ (p) (equal? (file-name-from-path p)
                  (build-path "info.rkt")))
   (in-install-directory)))

(define (in-installed-info)
  (sequence-map read-zcpkg-info
                (in-installed-info-paths)))

(define (in-installed-dependencies)
  (sequence-map zcpkg-info->dependency
                (in-installed-info)))

(define (in-abstract-dependency-declarations)
  (sequence-fold (λ (wip info)
                   (set-union wip (apply set (zcpkg-info-dependencies info))))
                 (set)
                 (in-installed-info)))

; Resolve the revision names in an abstract dependency
(define (abstract-dependency->concrete-dependency ad/variant infos)
  (define (maybe-resolve-name rev info names)
    (cond [(revision-number? rev) rev]
          [(revision-number-string? rev) rev]
          [(revision-name-string? rev)
           (or (member rev names)
               rev)]))

  (define ad (coerce-dependency ad/variant))

  (define cd
    (cond [(concrete-dependency? ad) ad]
          [(exact-dependency? ad) ad]
          [else
           (for/fold ([wip ad])
                     ([info infos]
                      #:break (concrete-dependency? wip))
             (define names (zcpkg-info-revision-names info))
             (if (dependency-identity=? wip info)
                 (struct-copy dependency wip
                              [revision-min
                               (maybe-resolve-name (dependency-revision-min wip) names)]
                              [revision-max
                               (maybe-resolve-name (dependency-revision-max wip) names)])
                 wip))]))

  (unless (concrete-dependency? cd)
    (error 'abstract-dependency->concrete-dependency
           "Could not resolve revision names in ~s"
           (dependency->string ad)))

  cd)

; Shift the numbers in a concrete dependency to match latest version
; available in package information.
(define (concrete-dependency->exact-dependency cd infos)
  (define latest-info
    (for/fold ([wip #f])
              ([info infos])
      (if wip
          (get-maybe-greater-version wip info)
          info)))

  ; Two info structures might not have comparable
  ; versions. Only search for the maximum version
  ; of an info among comparable alternatives.
  (define (get-maybe-greater-version wip info)
    (define cmp (zcpkg-compare-versions wip info))
    (if cmp
        (if (> cmp 0) info wip)
        wip))

  (struct-copy dependency cd
               [revision-min-exclusive? #f]
               [revision-min (zcpkg-info-revision-number latest-info)]
               [revision-max-exclusive? #f]
               [revision-max (zcpkg-info-revision-number latest-info)]))

(define (find-latest ad info-seq)
  (sequence-ref
   (find-installed-infos
    (concrete-dependency->exact-dependency
     (abstract-dependency->concrete-dependency ad info-seq)
     info-seq)
    info-seq)
   0))

(define (find-info/expect-one dependency-variant)
  (define infos (find-installed-infos dependency-variant))
  (case (sequence-length infos)
    [(0) (error 'uninstall "~a is not installed" dependency-variant)]
    [(1) (void)]
    [else
     (error 'find-exact-info
            "~s is ambiguous. Which of these did you mean?~n~s"
            (string-join (map (compose dependency->string coerce-dependency)
                              infos)
                         "\n"))])
  (sequence-ref infos 0))

(define (find-installed-infos variant all-info)
  (define dep (coerce-dependency variant))
  (sequence-filter (λ (info) (dependency-match? dep info))
                   all-info))

(define (zcpkg-installed? info)
  (directory-exists? (zcpkg-info->install-path info)))
