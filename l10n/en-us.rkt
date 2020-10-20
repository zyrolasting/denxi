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

(define (localized-comma-list l conjunction)
  (case (length l)
    [(0) ""]
    [(1) (car l)]
    [(2) (format "~a ~a ~a" (car l) conjunction (cadr l))]
    [else
     (let ([r (reverse l)])
       (format "~a ~a"
               (string-join
                (reverse (cons conjunction (cdr r)))
                ", ")
               (car r)))]))


(define+provide-message-formatter format-message/locale
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

  [($invalid-workspace-envvar)
   (format "Ignoring envvar value for XIDEN_WORKSPACE: ~a~n  falling back to ~a"
           (getenv "XIDEN_WORKSPACE")
           (workspace-directory))]

  [($transfer:scope name msg)
   (format "transfer ~a: ~a" name (format-message msg))]

  [($transfer:progress bytes-read max-size timestamp)
   (format "~a%"
           (~r (* 100 (/ bytes-read max-size)) #:precision 0))]

  [($transfer:budget:rejected allowed-max-size proposed-max-size)
   (format "can only copy ~a bytes, but estimate is ~a bytes"
           allowed-max-size
           proposed-max-size)]

  [($transfer:budget:exceeded allowed-max-size overrun-size)
   (format "produced ~a more than the allowed ~a bytes"
           overrun-size
           allowed-max-size)]

  [($transfer:timeout bytes-read wait-time)
   (format "timed out after reading and ~a bytes waiting ~a ms for more"
           bytes-read
           wait-time)]

  [($package:log query output-name messages)
   (format "~a, output ~a~n~a"
           query
           output-name
           (join-lines (map format-message messages)))]

  [($package:output:built)
   "built output"]

  [($package:output:reused)
   "reused output"]

  [($package:output:undefined)
   "requested output is not defined"]

  [($package:definition:value:missing id)
   (format "~a is not defined" (format-symbol-for-display id))]

  [($package:definition:value:invalid id value)
   (format "expected ~a for ~a, but got ~e"
           (case id
             [(provider package edition)
              "a non-empty string"]
             [(racket-versions)
              "a list of Racket version range pairs, e.g. '((\"7.0\" . \"7.8\") ...) (Use #f to remove bound)."]
             [(revision-names)
              "a list of non-empty strings, where each string has at least one non-digit."]
             [(outputs)
              "a list of non-empty strings"]
             [(inputs)
              "a list of well-formed input-info instances"]
             [(revision-number)
              "an exact nonnegative integer"])
           (format-symbol-for-display id)
           value)]

  [($package:definition:undeclared-racket-version)
   (join-lines
    (list (format "does not declare supported Racket versions (bypass: ~a)"
                  (shortest-cli-flag --allow-undeclared-racket))))]

  [($package:definition:unsupported-os supported)
   (format "OS unsupported. Expected ~a"
           (localized-comma-list supported "or"))]

  [($package:definition:unsupported-racket-version versions)
   (join-lines
    (list (format "does not support Racket ~a (bypass: ~a)"
                  (version)
                  (format-cli-flags --assume-support))
          (format "supported versions (ranges are inclusive):~n~a~n"
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
                        versions)))))]

  [($fetch:scope name source message)
   (format "~a: ~a"
           (if (or (equal? name source)
                   (not source))
               (format "fetch ~s" (~a name))
               (format "fetch ~a from ~s" name (~a source)))
           (format-message message))]

  [($fetch:done ok?)
   (if ok? "done" "failed")]

  [($fetch:fail method reason)
   (format "ruling out ~a~a"
           method
           (if (and (XIDEN_VERBOSE) reason)
               (~a ": " reason)
               ""))]

  [($signature ok? stage public-key-path)
   (format (~a "signature ~a: ~a")
           (if ok? "ok" "violation")
           (case stage
             [(consider-integrity-trust)
              (if ok?
                  "trusting implicitly"
                  "requires verification")]

             [(consider-signature-info)
              (if ok?
                  "trusting unsigned"
                  (format "signature required (bypass: ~a)"
                          (format-cli-flags --trust-unsigned)))]

             [(consider-public-key-trust)
              (if ok?
                  "basing trust in input on trust in public key"
                  (format (~a "public key not trusted. To trust this key, add this to ~a:~n"
                              "(integrity 'sha384 (hex ~s))")
                          (setting-id XIDEN_TRUSTED_PUBLIC_KEYS)
                          (~a (encode 'hex (make-digest public-key-path 'sha384)))))]

             [(consider-signature)
              (if ok?
                  "verified"
                  "unverified")]))]


  [($integrity ok? stage intinfo)
   (format (~a "integrity ~a: ~a")
           (if ok? "ok" "violation")
           (case stage
             [(consider-digest-trust)
              (if ok?
                  "trusting implicitly"
                  "not trusting")]

             [(consider-integrity-info)
              (if ok?
                  "trusting implicitly"
                  "no well-formed integrity information")]

             [(consider-digest-match)
              (format "~a integrity ~a" (integrity-info-algorithm intinfo)
                      (if ok? "verified" (format "violation (bypass: ~a)"
                                                 (format-cli-flags --trust-any-digest))))]

             [else (format "Unknown integrity status: ~s Please inform the maintainers!"
                           stage)]))])
