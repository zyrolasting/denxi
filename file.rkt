#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/file))

(require racket/path
         racket/file
         racket/set
         racket/sequence
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
(define (abstract-dependency->concrete-dependency ad/variant [infos (in-installed-info)])
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

(define (abstract-dependency->exact-dependency ad [infos (in-installed-info)])
  (concrete-dependency->exact-dependency (abstract-dependency->concrete-dependency ad infos) infos))

(define (filter-missing-dependencies dependencies)
  (for/fold ([wip null])
            ([dep (in-list dependencies)])
    (if (= (sequence-length (find-installed-infos dep)) 0)
        (cons dep wip)
        wip)))

(define (find-exactly-one-info dependency-variant)
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

(define (make-zcpkg-install-dir #:link? link? source-path install-path)
  (make-directory* (path-only install-path))
  (if link?
      (make-file-or-directory-link source-path install-path)
      (copy-directory/files source-path install-path)))


(define (make-zcpkg-workspace-link [where (current-directory)])
  (define ws-link (build-path where CONVENTIONAL_WORKSPACE_NAME))
  (unless (link-exists? ws-link)
    (make-file-or-directory-link (ZCPKG_WORKSPACE) ws-link))
  ws-link)


(define (make-zcpkg-dependency-links dependencies [where (current-directory)])
  (define links-dir (build-path where CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))
  (make-directory* links-dir)
  (for/list ([dep-variant (in-list dependencies)])
    (define info (find-latest dep-variant))
    (define install-path (zcpkg-info->install-path info))
    (define link-path (build-path links-dir
                                  (zcpkg-info-provider-name info)
                                  (zcpkg-info-package-name info)))
    (make-file-or-directory-link install-path link-path)
    link-path))


(define (find-installed-infos variant [all-info (in-installed-info)])
  (define dep (coerce-dependency variant))
  (sequence-filter (λ (info) (dependency-match? dep info))
                   all-info))

(define (zcpkg-installed? info)
  (directory-exists? (zcpkg-info->install-path info)))


(define (delete-directory/files/empty-parents path)
  (delete-directory/files path)
  (define cpath (path->complete-path path))
  (let loop ([current (simplify-path cpath)] [next (../ cpath)])
    (if (equal? current next)
        (void)
        (and (directory-empty? next)
             (begin (delete-directory next)
                    (loop))))))

(define (directory-empty? path)
  (null? (directory-list path)))

(define (../ path)
  (simplify-path (build-path path 'up)))

(define (call-with-temporary-directory f #:cd? [cd? #t])
  (define tmp-dir (make-temporary-file "rktdir~a" 'directory))
  (dynamic-wind void
                (λ () (parameterize ([current-directory (if cd? tmp-dir (current-directory))])
                        (f tmp-dir)))
                (λ () (delete-directory/files tmp-dir))))

(define-syntax-rule (with-temporary-directory body ...)
  (call-with-temporary-directory
   (λ (tmp-dir) body ...)))

(module+ test
  (provide temp-fs dir >>)
  (require rackunit
           racket/set
           (for-syntax racket/base))

  (define-syntax-rule (temp-fs expr ...)
    (let ([tmpdir (make-temporary-file "~a" 'directory)])
      (parameterize ([current-directory tmpdir])
        expr ...
        (delete-directory/files tmpdir))))

  (define-syntax (dir stx)
    (syntax-case stx ()
      [(_ kw body ...)
       (with-syntax ([dirname (keyword->string (syntax-e #'kw))])
         #'(begin (make-directory dirname)
                  (parameterize ([current-directory dirname])
                    body ...)))]))

  (define-syntax (>> stx)
    (syntax-case stx ()
      [(_ kw val)
       (let ([e (syntax-e #'val)])
         (with-syntax ([fname (keyword->string (syntax-e #'kw))]
                       [writer (cond [(string? e)
                                      #'(λ (o) (displayln val o))]
                                     [(bytes? e)
                                      #'(λ (o) (write-bytes val o))]
                                     [else #'val])])
           #'(call-with-output-file fname writer)))]
      [(_ kw)
       #'(>> kw "")]))


  (temp-fs [dir #:inst [>> #:a] [dir #:nested [>> #:b]]]
           [dir #:other [>> #:should-not-appear]]
           (parameterize ([ZCPKG_WORKSPACE (current-directory)]
                          [ZCPKG_INSTALL_RELATIVE_PATH "inst"])
             (test-equal? "Provide all paths in install directory"
                          (apply set (sequence->list (in-install-directory)))
                          (apply set (map (λ (p)
                                            (build-path (current-directory) p))
                                          '("inst/a"
                                            "inst/nested"
                                            "inst/nested/b"))))))

  (temp-fs [dir #:inst [dir #:pkgA [>> #:info.rkt "#lang info"]] [dir #:pkgB [>> #:info.rkt "#lang info"]]]
           (parameterize ([ZCPKG_WORKSPACE (current-directory)]
                          [ZCPKG_INSTALL_RELATIVE_PATH "inst"])
             (test-equal? "Provide all paths to installed info.rkt files"
                          (apply set (sequence->list (in-installed-info-paths)))
                          (apply set (map (λ (p)
                                            (build-path (current-directory) p))
                                          '("inst/pkgA/info.rkt"
                                            "inst/pkgB/info.rkt")))))))
