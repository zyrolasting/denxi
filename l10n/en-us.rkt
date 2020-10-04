#lang racket/base

(provide get-string)
(require "shared.rkt")

(define (L . lines)
  (string-join lines "\n"))

(define (get-string key)
  (case key
    [(top-level-cli-help)
     (L "<action> is one of"
        "  do   Run transaction"
        "  gc   Collect garbage"
        "  show Print report")]

    [(show-command-help)
     (L "where <what> is one of"
        "  config     Show a (read)able hash table of current settings"
        "  installed  Show a list of installed outputs"
        "  links      Show a list of issued links"
        "  workspace  Show the path to the target workspace directory")]))

(define+provide-message-formatter format-message
  [($output v)
   (format-message v)]

  [($fail v)
   (cond [(exn? v) (exn->string v)]
         [(string? v) v]
         [else (~s v)])]

  [($finished-collecting-garbage bytes-recovered)
   (format "Recovered ~a"
           (if (> bytes-recovered (/ (* 1024 2024) 10))
               (~a (~r (/ bytes-recovered (* 1024 1024)) #:precision 2)
                   " mebibytes")
               (~a bytes-recovered
                   " bytes")))]

  [($show-command-help body suffix-key)
   (format "~a~a" body (if suffix-key (~a "\n" (get-string suffix-key)) ""))]

  [($output-not-found query output-name)
   (format "Cannot find output ~s output for ~s"
           output-name query)]

  [($module-compiled module-path)
   (format "Compiled: ~a" module-path)]

  [($compilation-error module-path message)
   (format "Bytecode compilation error in: ~a~n~a" module-path message)]

  [($unrecognized-command m)
   (format "Unrecognized command: ~s. Run with -h for usage information.~n" m)]

  [($invalid-workspace-envvar)
   (format "Ignoring envvar value for XIDEN_WORKSPACE: ~a~n  falling back to ~a"
           (getenv "XIDEN_WORKSPACE")
           (workspace-directory))]

  [($transfer-progress name bytes-read max-size timestamp)
   (format "~a: ~a%" name (~r (* 100 (/ bytes-read max-size)) #:precision 0))]

  [($transfer-small-budget name)
   (format "Cannot transfer ~s. The configured budget is too small." name)]

  [($transfer-over-budget name size)
   (format "Halting transfer ~s. The transfer produced more than the estimated ~a bytes." name size)]

  [($transfer-timeout name bytes-read)
   (format "Halting transfer ~s after ~a bytes. Read timed out." name bytes-read)]

  [($built-package-output name output-name)
   (format "~a: built ~a" name output-name)]

  [($reused-package-output name output-name)
   (format "~a: reused ~a" name output-name)]

  [($undeclared-racket-version info)
   (join-lines
    (list (format "~a does not declare a supported Racket version."
                  info)
          (format "To install this package anyway, run again with ~a"
                  (shortest-cli-flag --allow-undeclared-racket))))]

  [($package-malformed name errors)
   (format "~a has an invalid definition. Here are the errors for each field:~n~a"
           name
           (join-lines (indent-lines errors)))]

  [($unsupported-racket-version name versions)
   (join-lines
    (list (format "~a does not support this version of Racket (~a)."
                  name
                  (version))
          (format "Supported versions (ranges are inclusive):~n~a~n"
                  (join-lines
                   (map (λ (variant)
                          (format "  ~a"
                                  (if (pair? variant)
                                      (format "~a - ~a"
                                              (or (car variant)
                                                  PRESUMED_MINIMUM_RACKET_VERSION)
                                              (or (cdr variant)
                                                  PRESUMED_MAXIMUM_RACKET_VERSION))
                                      variant)))
                        versions)))
          (format "To install this package anyway, run again with ~a"
                  (format-cli-flags --assume-support))))]

  [($source-fetched source-name fetch-name)
   (format "Fetched ~a" (or fetch-name source-name))]

  [($fetch-failure name)
   (format "Failed to fetch ~a" name)]

  [($source-method-ruled-out source-name fetch-name method-name reason)
   (format "Ruling out ~a ~a~a"
           method-name
           (if (equal? source-name fetch-name)
               (format "for source ~v" source-name)
               (format "for ~a from source ~v" fetch-name source-name))
           (if reason
               (~a ": " reason)
               ""))]

  [($unverified-host url)
   (format (~a "~a does not have a valid certificate.~n"
               "Connections to this server are not secure.~n"
               "To trust servers without valid certificates, use ~a.")
           url
           (format-cli-flags --trust-any-host))]

  [($input-resolve-start name)
   (format "Resolving input ~s" name)]

  [($input-integrity-violation name source)
   (format (~a "Integrity violation for ~s from source ~s.~n"
               "While unsafe, you can force installation using ~a.")
           name
           source
           (format-cli-flags --trust-any-digest))]

  [($signature-mismatch name source)
   (format (~a "Signature mismatch for ~s from source ~s.~n"
               "While unsafe, you can trust bad signatures using ~a.")
           name
           source
           (format-cli-flags --trust-bad-signature))]

  [($signature-missing name source)
   (format (~a "~a does not have a signature. If you are prototyping your own package, this is expected.~n"
               "If you got the package from the Internet, then exercise caution!~n"
               "To trust unsigned packages, use ~a.")
           name
           (format-cli-flags --trust-unsigned))]

  [($input-integrity-verified name source)
   (format "Integrity verified for input ~s from source ~s" name source)]

  [($input-integrity-assumed name source)
   (format "Dangerously trusting input ~s from source ~s" name source)]

  [($signature-unchecked name source)
   (format "Not checking signature for input ~s from source ~s"
           name source)]

  [($signature-distrust-public-key name source)
   (format "Refusing input ~s from source ~s because public key is not trusted."
           name source)]

  [($input-integrity-missing name source)
   (format (~a "~a does not declare integrity information.~n"
               "If you are prototyping your own package, this is expected.~n"
               "Otherwise, please declare integrity information for safety.")
           name)]

  [($signature-trust-unsigned name source)
   (format "Trusting unsigned input ~s from source ~s" name source)]

  [($signature-verified name source)
   (format "Signature verified for input ~s from source ~s" name source)]

  [($signature-mismatch name source)
   (format "Signature mismatch for input ~s from source ~s" name source)])
