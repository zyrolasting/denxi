#lang racket/base

(provide capture-workspace
         CONVENTIONAL_CAPTURE_FILE_NAME)

(require racket/format
         racket/match
         racket/path
         racket/port
         racket/pretty
         "config.rkt"
         "zcpkg-query.rkt"
         "file.rkt"
         "message.rkt"
         "setting.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt")

(define CONVENTIONAL_CAPTURE_FILE_NAME "capture.rkt")

(define (capture-workspace)
  (call-with-output-file CONVENTIONAL_CAPTURE_FILE_NAME
    write-workspace-capture))

(define (zcpkg-workspace-data->module-datum config install-targets)
  `(module capture racket/base
     (require racket/system)
     (define zcpkg (find-executable-path "zcpkg"))
     (define (run . args)
       (define code (apply system*/exit-code zcpkg args))
       (unless (= code 0)
         (eprintf "~s failed with exit code ~a~n"
                  (string-join (cons (~a zcpkg) args) " ")
                  code)))
     (define (set-config! k v) (run "config" "set" k v))
     (define (install! . pkgs) (apply run "install" pkgs))
     ,@(for/list ([(k v) (in-hash config)])
         `(set-config! ',k ,(~s v)))
     (install! . ,install-targets)))

(define (generate-workspace-reproduction-module)
  (zcpkg-workspace-data->module-datum
   ((load-zcpkg-settings!) 'dump)
   (capture-install-targets)))

(define (capture-install-targets)
  (for/list ([info (in-installed-info)])
    (zcpkg-query->string (zcpkg-info->zcpkg-query info))))

(define (write-workspace-capture [o (current-output-port)])
  (pretty-write #:newline? #t (generate-workspace-reproduction-module) o))

(module+ test
  (require racket/list
           rackunit
           (submod "file.rkt" test)
           (submod "zcpkg-info.rkt" test))

  (test-workspace
   "Read and write a module to reproduce a workspace"
   (make-directory* (build-workspace-path (ZCPKG_INSTALL_RELATIVE_PATH)))

   (define foo-info
     (copy-zcpkg-info dummy-zcpkg-info
                      [provider-name "fooby"]
                      [package-name "foo"]
                      [dependencies '()]))

   (write-zcpkg-info-to-directory foo-info (zcpkg-info->install-path foo-info))

   (define buffer (open-output-bytes))
   (write-workspace-capture buffer)

   (define reread (read (open-input-bytes (get-output-bytes buffer #t))))

   (check-equal? reread
                 (generate-workspace-reproduction-module))

   (define depstring (zcpkg-query->string (zcpkg-info->zcpkg-query foo-info)))
   (define command `(install! ,depstring))

   (test-equal? "Installed package is captured as a command"
                (member command reread)
                (list command))

   (define configs
     (filter-map (λ (v)
                   (and (list? v)
                        (eq? (car v) 'set-config!)
                        (cdr v)))
                 reread))

   (test-true "Represent an entire zcpkg configuration in a capture"
              (andmap (λ (set-config!-args)
                        (define config-key (cadr (car set-config!-args))) ; Symbol is in (quote) form
                        (define config-val (cadr set-config!-args))
                        (and (hash-has-key? ZCPKG_SETTINGS config-key)
                             (let ([setting-instance (hash-ref ZCPKG_SETTINGS config-key)])
                               ((setting-valid? setting-instance)
                                (read (open-input-string config-val))))))
                      configs))))
