#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/file))

(require racket/file
         racket/generator
         racket/list
         racket/path
         racket/set
         racket/sequence
         "config.rkt"
         "dependency.rkt"
         "string.rkt"
         "zcpkg-info.rkt"
         "workspace.rkt")

(define (path-cycles? path [previous #f] [encountered (set)])
  ; Do not let simplify-path consult filesystem, because that would
  ; follow any link present. We would not get its identity then.
  (define simple (simplify-path path #f))
  (define id (file-or-directory-identity simple #t))
  (cond [(equal? id previous) #f] ; Checks for root directory, given call below.
        [(set-member? encountered id)]
        [else
         (path-cycles? (build-path simple 'up)
                       id
                       (set-add encountered id))]))

(define (build-install-path . args)
  (apply build-workspace-path (ZCPKG_INSTALL_RELATIVE_PATH) args))

(define (in-workspace)
  (in-directory (build-workspace-path)
                (λ (p) (if (link-exists? p)
                           (not (path-cycles? p))
                           (not (member (path->string (file-name-from-path p)) '(".git")))))))

(define (ls path)
  (map path->string (directory-list path)))

(define (get-installed-providers)
  (ls (build-install-path)))

(define (get-installed-provider-packages provider)
  (ls (build-install-path provider)))

(define (get-installed-package-editions provider package)
  (ls (build-install-path provider package)))

(define (get-installed-edition-revisions provider package edition)
  (ls (build-install-path provider package edition)))

(define (in-installed-info-paths)
  (in-generator
   (for* ([provider (in-list (get-installed-providers))]
          [package (in-list (get-installed-provider-packages provider))]
          [edition (in-list (get-installed-package-editions provider package))]
          [revision (in-list (get-installed-edition-revisions provider package edition))])
     (yield (build-install-path provider package edition revision CONVENTIONAL_PACKAGE_INFO_DIRECTORY_NAME)))))

(define (in-installed-info)
  (sequence-map read-zcpkg-info
                (in-installed-info-paths)))

(define (in-abstract-dependency-declarations)
  (sequence-fold (λ (wip info)
                   (set-union wip (apply set (zcpkg-info-dependencies info))))
                 (set)
                 (in-installed-info)))

(define (filter-missing-dependencies dependencies)
  (for/fold ([wip null])
            ([dep (in-list dependencies)])
    (if (= (sequence-length (search-zcpkg-infos dep (in-installed-info)) 0))
        (cons dep wip)
        wip)))

(define (find-exactly-one-info dependency-variant)
  (define infos (search-zcpkg-infos dependency-variant (in-installed-info)))
  (case (sequence-length infos)
    [(0) (error 'find-exactly-one-info "~a is not installed" dependency-variant)]
    [(1) (void)]
    [else
     (error 'find-exactly-one-info
            "~s is ambiguous. Which of these did you mean?~n~s"
            (string-join (map (compose dependency->string coerce-dependency)
                              infos)
                         "\n"))])
  (sequence-ref infos 0))


; Note that these links form cycles. Users won't be able to recurse
; all directories without first removing or breaking the links.
(define (make-zcpkg-links dependencies [where (current-directory)])
  (make-zcpkg-workspace-link where)
  (make-zcpkg-dependency-links dependencies where))


(define (make-zcpkg-workspace-link [where (current-directory)])
  (define ws-link (build-path where CONVENTIONAL_WORKSPACE_NAME))
  (unless (link-exists? ws-link)
    (make-file-or-directory-link (ZCPKG_WORKSPACE) ws-link))
  ws-link)

(define (make-zcpkg-dependency-links dependencies [where (current-directory)])
  (unless (null? dependencies)
    (define links-dir (build-path where CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))
    (make-directory* links-dir)
    (for/list ([dep-variant (in-list dependencies)])
      (define available (search-zcpkg-infos dep-variant (in-installed-info)))
      (define info (sequence-ref available 0))
      (define install-path (zcpkg-info->install-path info))
      (define link-path (build-path links-dir
                                    (zcpkg-info-provider-name info)
                                    (zcpkg-info-package-name info)))
      (make-file-or-directory-link install-path link-path)
      link-path)))


(define (zcpkg-installed? info)
  (directory-exists? (zcpkg-info->install-path info)))


(define (delete-directory/files/empty-parents path)
  (delete-directory/files path)
  (define cpath (path->complete-path path))
  (let loop ([current (simplify-path cpath)] [next (../ cpath)])
    (if (or (equal? current next)
            (not (directory-empty? next)))
        (void)
        (begin (delete-directory next)
               (loop next (../ next))))))

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

  (define (display-to-temp-file content)
    (define path (make-temporary-file "~a"))
    (display-to-file #:exists 'truncate/replace content path)
    path)

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
                          (apply set (sequence->list (in-workspace)))
                          (apply set (map (λ (p)
                                            (build-path (current-directory) p))
                                          '("inst/a"
                                            "inst/nested"
                                            "inst/nested/b"))))))

  (temp-fs [dir #:inst [dir #:pkgA [>> #:info.rkt "#lang info"]] [dir #:pkgB [>> #:info.rkt "#lang info"]]]
           (parameterize ([ZCPKG_WORKSPACE (current-directory)]
                          [ZCPKG_INSTALL_RELATIVE_PATH "inst"])
             (test-equal? "Provide all paths to installed info.rkt files"
                          (apply set (sequence->list (in-workspace)))
                          (apply set (map (λ (p)
                                            (build-path (current-directory) p))
                                          '("inst/pkgA/info.rkt"
                                            "inst/pkgB/info.rkt")))))))
