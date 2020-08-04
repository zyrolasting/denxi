#lang racket/base

(provide compare-path
         write-capture
         capture-workspace)

(require racket/format
         racket/match
         racket/path
         racket/port
         racket/pretty
         racket/set
         "config.rkt"
         "contract.rkt"
         "zcpkg-query.rkt"
         "file.rkt"
         "message.rkt"
         "setting.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt"
         "zcpkg-messages.rkt")


(define (capture-config)
  (for/hash ([(k v) (in-hash ((load-zcpkg-settings!) 'dump))])
    (values k (~s v))))


(define (capture-packages)
  (for/list ([info (in-installed-info)])
    (zcpkg-query->string (zcpkg-info->zcpkg-query info))))


(define (capture-files patterns)
  (parameterize ([current-directory (workspace-directory)])
    (for/fold ([wip (hash)])
              ([path (in-workspace)])
      (define key (path->string (find-relative-path (current-directory) path)))
      (if (and (file-exists? key)
               (not (hash-has-key? wip key))
               (ormap (λ (patt) (regexp-match? patt key)) patterns))
          (hash-set wip key (make-digest* key))
          wip))))

(define (write-capture cap [o (current-output-port)])
  (write-config cap '(config packages digests) o))

(define (capture-workspace patterns)
  (hash 'config (capture-config)
        'packages `,(capture-packages)
        'digests (capture-files patterns)))

(define (compare-path path ours theirs)
  (define we-have-it   (hash-has-key? ours path))
  (define they-have-it (hash-has-key? theirs path))
  (cond [(and we-have-it they-have-it)
         (if (equal? (hash-ref ours path)
                     (hash-ref theirs path))
             ($diff-same-file path)
             ($diff-different-file path))]
        [(and we-have-it (not they-have-it))
         ($diff-extra-file path)]
        [(and (not we-have-it) they-have-it)
         ($diff-missing-file path)]))


(define (make-digest* path)
  (define-values (exit-code digest) (make-digest path))
  (unless (= exit-code 0)
    (raise exit-code))
  digest)


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
   (write-capture (capture-workspace null) buffer)

   (define lookup (load-config (open-input-bytes (get-output-bytes buffer))))

   (define depstring (zcpkg-query->string (zcpkg-info->zcpkg-query foo-info)))

   (test-equal? "Capture files (that users select)"
                (lookup 'digests)
                (hash))

   (test-equal? "Capture installed package"
                (lookup 'packages)
                (list depstring))

   (test-true "Represent an entire zcpkg configuration in a capture"
              (for/and ([(config-key config-val) (in-hash (lookup 'config))])
                (and (hash-has-key? ZCPKG_SETTINGS config-key)
                     (let ([setting-instance (hash-ref ZCPKG_SETTINGS config-key)])
                       ((setting-valid? setting-instance)
                        (read (open-input-string config-val)))))))))
