#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/file))

(require racket/file
         racket/function
         racket/generator
         racket/list
         racket/path
         racket/set
         racket/sequence
         "config.rkt"
         "zcpkg-query.rkt"
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

(define (delete-file* path)
  (if (or (file-exists? path)
          (link-exists? path))
      (begin (delete-file path)
             path)
      #f))

(define (build-install-path . args)
  (apply build-workspace-path (ZCPKG_INSTALL_RELATIVE_PATH) args))

(define (in-acyclic-directory start-dir [use-dir? (λ _ #t)])
  (in-directory start-dir
                (λ (p) (if (link-exists? p)
                           (not (path-cycles? p))
                           (use-dir? p)))))

(define (in-racket-modules start-path)
  (sequence-filter (λ (p)
                     (and (not (link-exists? p))
                          (not (member (path->string (file-name-from-path p))
                                       (list CONVENTIONAL_WORKSPACE_NAME
                                             CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME)))
                          (file-exists? p)
                          (member (path-get-extension p)
                                  '(#".rkt" #".ss" #".scrbl"))))
                   (in-acyclic-directory start-path)))


(define (in-matching-files patterns start-dir)
  (in-generator
   (for ([path (in-directory start-dir (negate link-exists?))])
     (define rel-path (find-relative-path start-dir path))
     (when (and (file-exists? rel-path)
                (ormap (λ (p) (regexp-match? p rel-path)) patterns))
       (yield rel-path)))))

(define (in-workspace)
  (in-acyclic-directory (build-workspace-path)
                        (λ (p) (not (member (path->string (file-name-from-path p)) '(".git"))))))

(define (loose-directory-list path)
  (if (directory-exists? path)
      (map path->string (directory-list path))
      null))

(define (get-installed-providers)
  (loose-directory-list (build-install-path)))

(define (get-installed-provider-packages provider)
  (loose-directory-list (build-install-path provider)))

(define (get-installed-package-editions provider package)
  (loose-directory-list (build-install-path provider package)))

(define (get-installed-edition-revisions provider package edition)
  (loose-directory-list (build-install-path provider package edition)))

(define (in-installed-package-paths [use? (λ _ #t)])
  (in-generator
   (for* ([provider (in-list (get-installed-providers))]
          [package (in-list (get-installed-provider-packages provider))]
          [edition (in-list (get-installed-package-editions provider package))]
          [revision (in-list (get-installed-edition-revisions provider package edition))])
     (when (use? provider package edition revision)
       (yield (build-install-path provider package edition revision))))))

(define (in-installed-info)
  (sequence-map
   read-zcpkg-info-from-directory
   (in-installed-package-paths
    (λ (pr pk ed rv)
      (regexp-match? #px"^\\d+$" rv)))))

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

(define (find-latest-info zcpkg-query-variant)
  (define infos (search-zcpkg-infos zcpkg-query-variant (in-installed-info)))
  (when (= (sequence-length infos) 0)
    (error 'find-latest-info "~a is not installed" zcpkg-query-variant))
  (sequence-ref infos 0))

(define (find-exactly-one-info zcpkg-query-variant)
  (define infos (search-zcpkg-infos zcpkg-query-variant (in-installed-info)))
  (case (sequence-length infos)
    [(0) (raise-user-error (format "~a is not installed" zcpkg-query-variant))]
    [(1) (void)]
    [else
     (raise-user-error
      (format "~s is ambiguous. Which of these did you mean?~n~s"
              (if (string? zcpkg-query-variant)
                  zcpkg-query-variant
                  (zcpkg-query->string zcpkg-query-variant))
              (string-join
               (sequence->list
                (sequence-map
                 (compose zcpkg-query->string coerce-zcpkg-query)
                 infos))
               "\n")))])
  (sequence-ref infos 0))


(define (make-link/clobber to link-path)
  (make-directory* (or (path-only link-path) (current-directory)))
  (when (link-exists? link-path)
    (delete-file link-path))
  (make-file-or-directory-link to link-path)
  link-path)


(define (build-dependency-path base-path info)
  (build-path base-path
              CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME
              (zcpkg-info->relative-path info #:abbrev 2)))

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
