#lang racket/base

; Define procedures that detect all logical inconsistencies between a
; workspace and a package definition. This is not the same as checking
; integrity information from a capture, because a capture file is not
; involved. A capture is for answering the question "Do I have the
; right content?". This module is for answering the question
; "Does this content seem right?"

(provide analyze-installed-package)

(require racket/format
         "file.rkt"
         "path.rkt"
         "racket-version.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-messages.rkt"
         "zcpkg-settings.rkt")


(define errors (box null))
(define warnings (box null))
(define cont (make-parameter #f))


(define-syntax-rule (record body ...)
  (begin
    (set-box! errors null)
    (set-box! warnings null)
    body ...
    (values (reverse (unbox errors))
            (reverse (unbox warnings)))))

(define-syntax-rule (assert-stops body ...)
  (call/cc (λ (k) (parameterize ([cont k]) body ...))))


(define (make-accumulator box-id)
  (λ (fmt . args)
    (set-box! box-id (cons (apply format fmt args)
                           (unbox box-id)))))


(define add-error!   (make-accumulator errors))
(define add-warning! (make-accumulator warnings))


(define (assert! v fmt . args)
  (unless v
    (apply add-error! fmt args)
    ((cont))))


(define (analyze-dependency-state! install-path dep-info)
  (assert-stops
   (define dep-path (build-dependency-path install-path dep-info))
   (assert! (link-exists? dep-path) "No link at: ~a" dep-path)
   (assert! (directory-exists? dep-path) "Broken link: ~a" dep-path)
   (define other-info (read-zcpkg-info-from-directory dep-path))
   (define other-path (build-dependency-path install-path other-info))

   ; Both paths should match when abbreviated. If they don't, then
   ; either the provider name or the package name disagrees with
   ; other-info.
   (assert! (equal? dep-path other-path)
            (~a "~a points to the wrong directory.~n"
                "  expected: ~a~n"
                "  actual: ~a~n")
            (zcpkg-info->install-path dep-info)
            (zcpkg-info->install-path other-info))))


(define (analyze-setup-module! info)
  (define install-path (zcpkg-info->install-path info))
  (define setup-module-path (zcpkg-info-setup-module info))
  (when setup-module-path
    (define path-to-verify (simplify-path (build-path install-path setup-module-path)))
    (unless (path-prefix? path-to-verify install-path)
      (add-error! "The setup-module path reaches outside of the install directory."))
    (unless (file-exists? path-to-verify)
      (add-error! "Setup module not found at ~a" path-to-verify))))


(define (analyze-racket-version-support! info)
  (assert-stops
   (define racket-versions (zcpkg-info-racket-versions info))
   (with-handlers ([exn:fail:zcpkg:invalid-racket-version-interval?
                    (λ (e)
                      (add-error! "Declares support for invalid Racket version interval ~a - ~a"
                                  (exn:fail:zcpkg:invalid-racket-version-interval-lo e)
                                  (exn:fail:zcpkg:invalid-racket-version-interval-hi e)))])
     (case (check-racket-version-ranges (version) racket-versions)
       [(unsupported)
        (add-warning! "Declares that it does not support this version of Racket (~a)"
                      (version))]
       [(undeclared)
        (add-warning! "Does not declare supported Racket versions.")]

       [else (void)]))))


(define (analyze-dependencies! info)
  (unless (null? (zcpkg-info-dependencies info))
    (assert-stops
     (define install-path (zcpkg-info->install-path info))
     (assert! (directory-exists? (build-path install-path CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))
              "~a does not exist. The package will not be able to access any dependencies."
              CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME)

     (for ([dep (in-list (zcpkg-info-dependencies info))])
       (define dep-info
         (with-handlers ([exn:fail? (λ _ #f)])
           (find-latest-info dep)))
       (if dep-info
           (analyze-dependency-state! add-error! dep-info)
           (add-error! "Dependency ~s is not installed" dep))))))

(define (analyze-launcher-state! info)
  (for/list ([spec (in-list (zcpkg-info-launchers info))])
    (define launcher-path (build-workspace-path (ZCPKG_LAUNCHER_RELATIVE_PATH) (hash-ref spec 'name)))
    (unless (file-exists? launcher-path)
      (add-error! (format "Missing launcher: ~a"
                         launcher-path)))))


(define (analyze-installed-package info)
  (define-values (errors warnings)
    (record (analyze-launcher-state! info)
            (analyze-dependencies! info)))
  ($package-report info errors warnings))


(module+ test
  (require rackunit)

  (test-case "Can record errors and warnings"
    (define-values (errors warnings)
      (record (add-error! "1")
              (add-error! "2")
              (add-warning! "x")
              (add-warning! "y")))
    (check-equal? errors '("1" "2"))
    (check-equal? warnings '("x" "y")))

  (test-case "Can assert to stop certain code sections"
    (define-values (errors warnings)
      (record (assert-stops
               (add-warning! "x")
               (assert! #t "q")
               (add-error! "y")
               (assert! #f "z"))))
    (check-equal? errors '("y" "z"))
    (check-equal? warnings '("x"))))
