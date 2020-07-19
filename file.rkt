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
         "setting.rkt"
         "string.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt")

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

(define (in-installed-package-paths)
  (in-generator
   (for* ([provider (in-list (get-installed-providers))]
          [package (in-list (get-installed-provider-packages provider))]
          [edition (in-list (get-installed-package-editions provider package))]
          [revision (in-list (get-installed-edition-revisions provider package edition))])
     (yield (build-install-path provider package edition revision)))))

(define (in-installed-info)
  (sequence-map read-zcpkg-info
                (in-installed-package-paths)))

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
(define (make-zcpkg-links #:search? search? variant-list [where (current-directory)])
  (make-zcpkg-workspace-link where)
  (make-zcpkg-dependency-links #:search? search? variant-list where))

(define (make-link/clobber to link-path)
  (make-directory* (path-only link-path))
  (when (link-exists? link-path)
    (delete-file link-path))
  (make-file-or-directory-link to link-path)
  link-path)


(define (make-zcpkg-workspace-link [where (current-directory)])
  (make-link/clobber (workspace-directory) (build-path where CONVENTIONAL_WORKSPACE_NAME)))

(define (make-zcpkg-dependency-links #:search? search? dependencies [where (current-directory)])
  (unless (null? dependencies)
    (define links-dir (build-path where CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))
    (for/list ([variant (in-list dependencies)])
      (define info (if search? (find-exactly-one-info variant) variant))
      (make-link/clobber (zcpkg-info->install-path info)
                         (build-path links-dir
                                     (zcpkg-info->relative-path info #:abbrev 2))))))


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
  (provide temp-fs dir >> test-workspace)
  (require rackunit
           racket/set
           (for-syntax racket/base))


  (define-syntax-rule (test-workspace message body ...)
    (test-case message
      (call-with-temporary-directory
       #:cd? #t
       (λ (tmp-dir)
         (parameterize ([workspace-directory tmp-dir])
           body ...)))))

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
           [dir #:other [>> #:file]]
           (assume-settings ([ZCPKG_INSTALL_RELATIVE_PATH "inst"])
             (parameterize ([workspace-directory (current-directory)])
               (test-equal? "Provide all paths in workspace"
                            (apply set (sequence->list (in-workspace)))
                            (apply set (map (λ (p)
                                              (build-path (current-directory) p))
                                            '("inst"
                                              "inst/a"
                                              "inst/nested"
                                              "inst/nested/b"
                                              "other"
                                              "other/file"))))))))
