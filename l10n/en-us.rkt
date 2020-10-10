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

(define+provide-message-formatter format-message/locale
  [($output v)
   (format-message v)]

  [($fail v)
   (cond [(exn? v) (exn->string v)]
         [(string? v) v]
         [else (~s v)])]

  [($regarding name v)
   (format "~a: ~a"
           (format-message name)
           (format-message v))]

  [($finished-collecting-garbage bytes-recovered)
   (format "Recovered ~a"
           (if (> bytes-recovered (/ (* 1024 2024) 10))
               (~a (~r (/ bytes-recovered (* 1024 1024)) #:precision 2)
                   " mebibytes")
               (~a bytes-recovered
                   " bytes")))]

  [($cli:show-help body suffix-key)
   (format "~a~a" body (if suffix-key (~a "\n" (get-string suffix-key)) ""))]

  [($cli:undefined-command m)
   (format "Unrecognized command: ~s. Run with -h for usage information.~n" m)]

  [($output-not-found query output-name)
   (format "Cannot find output ~s output for ~s"
           output-name query)]

  [($module-compiled module-path)
   (format "Compiled: ~a" module-path)]

  [($compilation-error module-path message)
   (format "Bytecode compilation error in: ~a~n~a" module-path message)]

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

  [($signature ok? stage public-key-path)
   (format (~a "Signature check ~a: ~a")
           (if ok? "PASS" "FAIL")
           (cond [(eq? stage (object-name consider-integrity-trust))
                  "Trusting implicitly"]
                 [(eq? stage (object-name consider-unsigned))
                  (if ok?
                      "Trusting unsigned"
                      (format "Signature required (bypass: ~a)"
                              (format-cli-flags --trust-unsigned)))]

                 [(eq? stage (object-name consider-public-key-trust))
                  (if ok?
                      ""
                      (format (~a "Public key not trusted. To trust this key, add this to ~a:~n"
                                  "(integrity 'sha384 (hex ~s))")
                              (setting-id XIDEN_TRUSTED_PUBLIC_KEYS)
                              (~a (encode 'hex (make-digest public-key-path 'sha384)))))]

                 [(eq? stage (object-name consider-signature-info))
                  (if ok?
                      "Signature verified"
                      "Signature not verified")]))]


  [($integrity algorithm status)
   (case status
     [(missing)  "cannot check integrity without well-formed integrity information"]
     [(trusted)  "integrity assumed"]
     [(verified) (format "integrity verified using ~a" algorithm)]
     [(mismatch) (format (~a "integrity violation due to ~a digest mismatch (unsafe: ~a to bypass)")
                         algorithm
                         (format-cli-flags --trust-any-digest))]
     [else (format "Unknown integrity status ~s. Please inform the maintainers!" status)])])
