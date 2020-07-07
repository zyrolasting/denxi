#lang racket/base

(provide install
         uninstall
         update)

(require idiocket/exn
         idiocket/file
         idiocket/format
         idiocket/path
         idiocket/function
         idiocket/match
         idiocket/sandbox
         "archiving.rkt"
         "download.rkt"
         "workspace.rkt"
         "config.rkt"
         "logging.rkt"
         "dependency.rkt"
         "url.rkt"
         "zcpkg-info.rkt")


(define (install source)
  (define source-directory (get-source-directory source))
  (<< "source ~s resolved to directory ~a~n" source source-directory)
  (define info (getinfo/zcpkg-info source-directory))
  (unless info
    (error 'install
           "No info.rkt file in ~a~n"
           source-directory))
  (if (zcpkg-installed? info)
      (report-already-installed info)
      (setup source-directory info)))


(define (report-already-installed info)
  (<< "Package already installed: ~a~n"
      (zcpkg-info->install-path info)))


(define (uninstall path #:include-dependents? [include-dependents? #t])
  (maybe-run-installer path '(tear-down!))
  (define info (getinfo/zcpkg-info path))
  (delete-directory/files path)
  (when include-dependents?
    (for ([path-to-maybe-broken (in-installed)])
      (define maybe-dependent-info (getinfo/zcpkg-info path-to-maybe-broken))
      (when (depends-on? maybe-dependent-info info)
        (uninstall path-to-maybe-broken)))))


; TODO: Define a pre-condition where the zcp-source resolves to the
; same package path, version non-withstanding.
(define (update package-path zcp-source)
  (uninstall package-path)
  (install zcp-source))


; A package source is a string with many possible meanings. Map the
; string to a URL for context. Pick the next step depending on what
; the URL means.
(define (get-source-directory v)
  (cond #;[(dependency-string? v)
           (download-from-catalog (url->dependency v))]
        [(string? v) (get-source-directory (string->url v))]
        [(find-directory-path v)]
        [else (raise-argument-error 'resolve-source
                                    "A valid package source"
                                    v)]))


(define (get-installer-path zcpkg-path info)
  (define installer-path (zcpkg-info-installer info))
  (if installer-path
      (let* ([maybe-unsafe-installer-path (simplify-path (build-path zcpkg-path installer-path))]
             [rel-path (find-relative-path zcpkg-path maybe-unsafe-installer-path)]
             [reaching-outside? (member 'up (explode-path rel-path))])
        (if reaching-outside?
            (error 'setup
                   "Refusing to run installer module. ~a's installer is trying to reach outside of its directory."
                   (build-path zcpkg-path (zcpkg-info-package-name info)))
            maybe-unsafe-installer-path))
      #f))


(define (in-installed)
  (map path-only
       (find-files (λ (p) (equal? (file-name-from-path p)
                                  (build-path "info.rkt")))
                   (build-workspace-path (ZCPKG_INSTALL_RELATIVE_PATH)))))


(define (maybe-run-installer zcpkg-path info datum)
  (define installer-path (get-installer-path zcpkg-path info))
  (define name
    (format "~a/~a"
            (zcpkg-info-provider-name info)
            (zcpkg-info-package-name info)))

  (when installer-path
    (parameterize ([sandbox-output 'pipe]
                   [sandbox-error-output 'pipe]
                   [sandbox-init-hook
                    (λ () (current-directory zcpkg-path))])
      (with-handlers ([(const #t)
                       (λ (e)
                         (<< "~a raised a value~n~a~n"
                             name
                             (if (exn? e)
                                 (exn->string e)
                                 (~v e))))])
        (define sandbox-eval (make-module-evaluator #:language 'racket/base installer-path))
        (sandbox-eval datum)
        (pump-lines-from-port name
                              (get-output sandbox-eval)
                              (current-output-port))
        (pump-lines-from-port name
                              (get-error-output sandbox-eval)
                              (current-error-port))))))


(define (pump-lines-from-port name in out)
  (define source
    (or (sync/timeout 0.05 in)
        (open-input-bytes #"")))
  (define line (read-line source))
  (or (eof-object? line)
      (begin (<< "~a: ~a~n" name line)
             (pump-lines-from-port name source out))))


(define (setup source-path info)
  (define zcpkg-path (zcpkg-info->install-path info))
  (make-directory* (path-only zcpkg-path))
  (with-handlers ([exn:fail? (λ (e)
                               (delete-directory/files zcpkg-path)
                               (raise e))])
    (copy-directory/files source-path zcpkg-path)
    (maybe-run-installer zcpkg-path info '(set-up!))))


(define (zcpkg-installed? info)
  (directory-exists? (zcpkg-info->install-path info)))


(define (depends-on? dependent maybe-dependency)
  #;(match-define (zcpkg-info provider name edition rev-number _ ...)
    maybe-dependency)
  (ormap (λ (dqs) '(TODO check revision range))
         (zcpkg-info-dependencies dependent)))


(define (download-from-catalog dep)
  (<< "Download ~a" dep)
  (define artifact-info (download-artifact-info dep))
  (define artifact-path (download-artifact dep))
  (define install-path (zcpkg-info->install-path artifact-info))
  (unpack artifact-path #:to install-path)
  install-path)
