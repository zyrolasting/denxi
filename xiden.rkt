#lang info

(define provider "sagegerard.com")
(define package "xiden")
(define edition "draft")
(define revision-number 0)
(define revision-names '("functional"))
(define home-page "https://zcpkg.com")

(define authors-signature
  '(signature (rsa (base32 "")
                   (fingerprint ""))))

(define inputs
  `((input "make-docs.rkt"
           (source "https:// ..."
                   (integrity 'sha384 (base32 "")
                              ,authors-signature)))

    (input "make-lib.rkt"
           (source "https:// ..."
                   (integrity 'sha384 (base32 "")
                              ,authors-signature)))

    (input "source.tgz"
           (source "https://github.com/zyrolasting/zcpkg/archive/alpha.tar.gz"
                   (integrity 'sha384
                              (base32
                               "s3v64e38qcjk7gmp9zw2fjenf0m9tbptk4ptdknwmwcp8zvv3nvgdghvrwwd77kknt916tce6m3hy")
                              ,authors-signature)))))


(define outputs
  '((output "doc" "make-docs.rkt"
            #:module-args ("--pdf")
            #:racket-args ())

    (output "lib"
            "make-libs.rkt"
            #:module-args ("--exclude-tests")
            #:racket-args ())))
