#lang racket/base

(require racket/contract)
(provide
 (contract-out
  [lock-package-definition
   (-> source-variant?
       notary?
       (subprogram/c bare-pkgdef?))]))

(require racket/format
         racket/match
         "artifact.rkt"
         "codec.rkt"
         "input.rkt"
         "integrity.rkt"
         "message.rkt"
         "monad.rkt"
         "notary.rkt"
         "openssl.rkt"
         "package.rkt"
         "pkgdef/static.rkt"
         "printer.rkt"
         "racket-module.rkt"
         "racket-version.rkt"
         "signature.rkt"
         "source.rkt"
         "subprogram.rkt")


(define (lock-package-definition source [notary lazy-notary])
  (mdo pkgdef := (load-user-package-definition source)
       pkg := (build-user-package pkgdef)
       (subprogram-acyclic
        (make-digest (coerce-bytes (~s pkgdef)) DEFAULT_CHF)
        (λ (messages)
          (for/fold ([stripped (strip pkgdef)]
                     [messages* messages])
                    ([input (in-list (package-inputs pkg))])
            (run-subprogram (rewrite-input stripped input notary)
                            messages*))))))


(define (rewrite-input stripped-pkgdef input notary)
  (mdo ; Grab artifact data like a user would
       ; when installing (custom shovel, etc)
       user-artifact :=
       (find-artifact-for-input input)

       ; This populates a cache
       file-record :=
       (fetch-artifact (package-input-name input) user-artifact)

       ; Security critical. Do not proceed until we can verify the
       ; info.
       (verify-artifact user-artifact file-record)

       ; Locking the artifact means all of the content
       ; bytes are in memory. We'll need them.
       locked-user-artifact :=
       (subprogram-unit (lock-artifact user-artifact))

       ; Accumulate as much data in the artifact as possible.
       replacement-artifact :=
       (expand-artifact stripped-pkgdef
                        locked-user-artifact
                        notary)
       
       ; Replace the integrity and/or signature info.
       notarized-artifact :=
       (notarize notary replacement-artifact)

       ; The notary can return an unlocked artifact.
       ; No steps back allowed now.
       locked-notarized-artifact :=
       (subprogram-unit (lock-artifact notarized-artifact))
       
       ; Now we can make the `(input ..) form code that will replace
       ; the old one.
       locked-input-expr :=
       (subprogram-unit
        (make-locked-input-expression
         (package-input-name input)
         locked-notarized-artifact))
       
       ; Actually replace the input expression
       ; with the notarized, correct artifact.
       stripped-pkgdef* :=
       (subprogram-unit
        (replace-input-expression stripped-pkgdef
                                  (package-input-name input)
                                  locked-input-expr))

       (subprogram-unit stripped-pkgdef*)))


(define (expand-artifact stripped-pkgdef locked-user-artifact notary)
  (subprogram-branch #:discard? (not (XIDEN_VERBOSE))
                     ; We'll try recursively locking the artifact as if it
                     ; were a new package definition source. If the data
                     ; is not a package definition, then this will fail
                     ; over to the spot marked [*]
                     (mdo stripped-pkgdef :=
                          (lock-package-definition
                           (artifact-info-source locked-user-artifact)
                           notary)

                          ; If we get here, we got another locked
                          ; package definition. We'll want to embed
                          ; the source code directly.
                          (subprogram-unit
                           (lock-artifact
                            (artifact
                             (text-source
                              (~s (dress stripped-pkgdef)))))))

                     ; [*] If we get here, then the artifact was not a
                     ; package definition. We can embed it as-is.
                     ; Basically, the failure to recurse is the
                     ; terminal case.
                     (subprogram-unit locked-user-artifact)))



(define (make-locked-input-expression name locked)
  (match-define (artifact-info content iinfo sinfo) locked)

  (define signature-expr
    (match sinfo
      [(signature-info pk sg)
       `(signature ,pk ,sg)]
      [_ #f]))

  (define integrity-expr
    (match iinfo
      [(integrity-info a d)
       `(integrity ',a ,d)]
      [_ #f]))

  `(input ,name
          (artifact ,content
                    ,integrity-expr
                    ,signature-expr)))


(module+ test
  (require rackunit
           (submod "subprogram.rkt" test))

  ; TODO: Re-enable verification and use actual
  ; integrity and sig. info below.
  #;(test-case "Lock package definitions"
    (XIDEN_TRUST_BAD_DIGEST #t
     (λ ()
       (check-subprogram
        (lock-package-definition
         (text-source
          (~s '(module anon xiden/pkgdef
                 (input "foo"
                        (artifact (lines-source
                                   "\n"
                                   '("(module next xiden/pkgdef"
                                     "  (input \"nested\""
                                     "         (artifact #\"n\")))"))
                                  (integrity 'md5 (text-source "xyz"))))
                 (input "bar"
                        (artifact (text-source "123")
                                  (integrity 'md5 (text-source "456"))
                                  (signature #"p" #"b"))))))
         fraudulent-notary)
        (λ (result messages)
          (define-values (pkg pkg-messages)
            (run-subprogram
             (build-user-package
              (dress result))))

          (writeln pkg-messages)

          (check-pred package? pkg)

          (define artifacts
            (map package-input-plinth
                 (package-inputs pkg)))

          (for ([arti (in-list artifacts)])
            (match-define
              (artifact-info content
                             (integrity-info _ digest)
                             (signature-info pubkey sig))
              arti)
            (check-true (andmap bytes?
                                (list content
                                      digest
                                      pubkey
                                      sig)))))))))

  (test-case "Create locked input expressions"
    (check-equal?
     (make-locked-input-expression
      "foo"
      (artifact-info #"a"
                     (integrity-info 'md5 #"i")
                     (signature-info #"p" "s")))
     '(input "foo"
             (artifact #"a"
                       (integrity 'md5 #"i")
                       (signature #"p" "s"))))

    (check-equal?
     (make-locked-input-expression "foo"
                                   (artifact-info #"a" #f #f))
     '(input "foo"
             (artifact #"a" #f #f)))))
