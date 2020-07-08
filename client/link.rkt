#lang racket/base

(provide link-command)

(require racket/cmdline
         racket/format
         racket/list
         racket/path
         racket/sequence
         "../workspace.rkt"
         "../file.rkt"
         "../zcpkg-info.rkt")

(define (find-package-link depth info)
  (define link-path
    (apply build-path
           (take (list (zcpkg-info-provider-name info)
                       (zcpkg-info-package-name info)
                       (zcpkg-info-edition-name info)
                       (~a (zcpkg-info-revision-number info)))
                 depth)))

  (define install-path (zcpkg-info->install-path info))

  (values link-path
          install-path))


(define (link-command)
  (define depth 1)
  (define exists-ok? #f)

  (define (make-link-visibly link-path install-path)
    (when (> depth 1)
      (make-directory* (path-only link-path)))

    (with-handlers
      ([exn:fail:unsupported?
        (λ (e)
          (display-to-file install-path link-path))])
      (make-file-or-directory-link install-path link-path))

    (printf "~a => ~a~n" link-path install-path))

  (define (make-link-thunks found)
    (for/list ([info (in-list found)])
      (define-values (link-path install-path)
        (find-package-link depth info))
      (if (link-exists? link-path)
          (if exists-ok?
              (λ ()
                (delete-file link-path)
                (make-link-visibly))
              (error "Link ~a already exists. Use -f to overwrite."
                     link-path))
          (λ () (make-link-visibly link-path install-path)))))

  (command-line
   #:program "link"
   #:once-any
   [("-p" "--from-provider")
    "Include provider name in links"
    (set! depth 2)]
   [("-e" "--to-edition")
    "Include edition name in links"
    (set! depth 3)]
   [("-a" "--all")
    "Include all package identity information in links"
    (set! depth 4)]
   [("-f" "--force")
    "Overwrite any existing links"
    (set! exists-ok? #t)]
   #:args (ad)
   (define found (sequence->list (find-installed-infos ad)))

   (unless (member 'write (file-or-directory-permissions (current-directory)))
     (displayln "You need write access in this directory to make links.")
     (exit 1))

   (if (null? found)
       (printf "~s not found in ~a~n"
               found
               (ZCPKG_WORKSPACE))
       (for ([th (in-list (make-link-thunks found))])
         (th)))))
