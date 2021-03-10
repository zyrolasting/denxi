#lang racket/base

(provide get-string)
(require "shared.rkt")

(define (L . lines)
  (string-join lines "\n"))

(define (get-string key)
  (case key
    [(top-level-cli-help)
     (L "<action> is one of"
        "  do      Run transaction"
        "  gc      Collect garbage"
        "  show    Print report"
        "  fetch   Transfer bytes from source"
        "  mkint   Make integrity info for bytes"
        "  mkinput Make input expression")]

    [(show-command-help)
     (L "where <what> is one of"
        "  config     Show a (read)able hash table of current settings"
        "  installed  Show a list of installed outputs"
        "  links      Show a list of issued links"
        "  workspace  Show the path to the target workspace directory")]

    [(backwards-racket-version-interval)
     "minimum Racket version cannot exceed maximum Racket version"]

    [(no-user-facing-sources)
     "no useable sources are defined for the end user"]

    [(XIDEN_MEMORY_LIMIT_MB)
     "Memory limit for the process"]

    [(XIDEN_TIME_LIMIT_S)
     "Time limit for the process"]

    [(XIDEN_INSTALL_SOURCES)
     "Add installation to transaction"]

    [(XIDEN_INSTALL_ABBREVIATED_SOURCES)
     "Add installation to transaction, assuming \"default\" output and package name link"]

    [(XIDEN_INSTALL_DEFAULT_SOURCES)
     "Add installation to transaction, assuming \"default\" output"]

    [(XIDEN_PLUGIN_MODULE)
     "A path to a module that extends Xiden."]

    [(XIDEN_TRUST_UNSIGNED)
     "Trust unsigned packages"]

    [(XIDEN_TRUST_BAD_SIGNATURE)
     "Trust signatures that don't match public key"]

    [(XIDEN_TRUST_UNVERIFIED_HOST)
     "Download from any server without authenticating"]

    [(XIDEN_TRUST_BAD_DIGEST)
     "(DANGEROUS) Trust any input."]

    [(XIDEN_TRUST_ANY_PUBLIC_KEY)
     "(DANGEROUS) Trust any public key"]

    [(XIDEN_TRUST_ANY_EXECUTABLE)
     "(DANGEROUS) Trust any executable"]

    [(XIDEN_TRUST_HOST_EXECUTABLES)
     "Trust executable file with given name if it can be found using find-executable-path"]

    [(XIDEN_TRUST_PUBLIC_KEYS)
     "Trust a given public key using integrity information"]

    [(XIDEN_TRUST_EXECUTABLES)
     "Trust an executable using integrity information"]

    [(XIDEN_FASL_OUTPUT)
     "Use FASL program output"]

    [(XIDEN_FETCH_TOTAL_SIZE_MB)
     "Maximum size, in mebibytes, to read from a source. +inf.0 = no limit"]

    [(XIDEN_FETCH_BUFFER_SIZE_MB)
     "Buffer size, in mebibytes, used when reading bytes"]

    [(XIDEN_FETCH_PKGDEF_SIZE_MB)
     "The maximum expected size, in mebibytes, of a package definition when scoping out work"]

    [(XIDEN_FETCH_TIMEOUT_MS)
     "The maximum time, in milliseconds, to wait for a distinct read of bytes from a source"]

    [(XIDEN_READER_FRIENDLY_OUTPUT)
     "Use (read)able program output"]

    [(XIDEN_VERBOSE)
     "Show more information in program output"]

    [(XIDEN_BYTE_ENCODING)
     "Byte encoding to use"]

    [(XIDEN_GENERATED_INPUT_NAME)
     "Name to use for generated input expressions"]

    [(XIDEN_USER_FACING_SOURCES)
     "Add a source for users to fetch data"]

    [(XIDEN_MESSAGE_DIGEST_ALGORITHM)
     "Message digest algorithm to use"]

    [(XIDEN_SIGNER)
     "Information used to sign package inputs"]

    [(XIDEN_CATALOGS)
     "Sets default URL templates in from-catalogs"]

    [(XIDEN_DOWNLOAD_MAX_REDIRECTS)
     "Maximum redirects to follow before downloading data"]

    [(XIDEN_ALLOW_UNSUPPORTED_RACKET)
     "Install packages even if they declare that they do not support the running version of Racket."]

    [(XIDEN_ALLOW_ENV)
     "Names of environment variables to expose to subprocesses"]

    [(XIDEN_SUBPROCESS_TIMEOUT_S)
     "Maximum number of seconds a subprocess may run"]

    [(XIDEN_INPUT_OVERRIDES)
     "Package input overrides"]))


(define (restrict-preamble name)
  (format "~a halted because"
          (let ([str (~a name)])
            (if (equal? str "")
                "Process"
                str))))


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

  [($invalid-workspace-envvar)
   (format "Ignoring envvar value for XIDEN_WORKSPACE: ~a~n  falling back to ~a"
           (getenv "XIDEN_WORKSPACE")
           (workspace-directory))]

  [($racket-module-read-error variant reason content)
   (case reason
     [(exception) content]
     [(unexpected-module-lang)
      (format "Unexpected module lang: ~v"
              content)]
     [(blocked-reader)
      (format "Unexpected reader extension: ~v"
              content)]
     [(bad-module-form)
      (format "Malformed module: ~v"
              variant)]
     [else
      (~s ($racket-module-read-error variant reason content))])]

  [($transfer:scope name msg)
   (format "transfer ~a: ~a" name (format-message msg))]

  [($transfer:progress bytes-read max-size timestamp)
   (if (eq? max-size bytes-read)
       "done"
       (if (eq? max-size +inf.0)
           (format "read ~a bytes" bytes-read)
           (format "~a%" (~r (* 100 (/ bytes-read max-size)) #:precision 0))))]

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

  [($package:unsupported-os supported)
   (format "OS unsupported. Expected ~a"
           (localized-comma-list supported "or"))]

  [($package:unsupported-racket-version versions)
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

  [($package:unavailable-output available)
   (format "Requested output is not available. Available outputs: ~s" available)]

  [($fetch name errors)
   (if (null? errors)
       (format "~a: fetched" name)
       (format "~a: fetch failed~n~a"
               name
               (string-join (map format-message errors)
                            "\n")))]

  [($signature ok? stage public-key)
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
                              "(integrity 'sha384 (base64 ~s))")
                          (setting-id XIDEN_TRUST_PUBLIC_KEYS)
                          (~a (encode 'base64 (make-digest public-key 'sha384)))))]

             [(consider-signature)
              (if ok?
                  "verified"
                  "unverified")]))]


  [($integrity ok? stage intinfo)
   (format (~a "integrity ~a: ~a")
           (if ok? "ok" "violation")
           (case stage
             [(consider-chf-trust)
              (if ok?
                  (format "trusting CHF ~a" (integrity-info-algorithm intinfo))
                  (format "not trusting CHF ~a. To bypass, add it to ~a"
                          (integrity-info-algorithm intinfo)
                          (setting-id XIDEN_TRUST_MESSAGE_DIGEST_ALGORITHMS)))]

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
                           stage)]))]

  [($subprocess:report cmd args wd max-runtime actual-runtime expected-exit-codes actual-exit-code stderr?)
   (L (format "subprocess `~a`" (string-join (cons (~a cmd) args) "` `"))
      (format "  working dir: ~a" wd)
      (format "  seconds left: ~a" (- max-runtime actual-runtime))
      (format "  stderr: ~a"
              (if stderr?
                  "populated"
                  "empty"))
      (format "  exit code: ~a~a"
              actual-exit-code
              (if (member actual-exit-code expected-exit-codes)
                  ""
                  (case (length expected-exit-codes)
                    [(0) ""]
                    [(1) (format " [expected ~a]" (car expected-exit-codes))]
                    [else (format " [expected one of ~s]"
                                  expected-exit-codes)]))))]

  [($subprocess:command-not-found cmd)
   (format "Cannot start subprocess `~a`: command not found" cmd)]

  [($extract-report status target)
   (case status
     [(done)
      (format "Extracted ~a" target)]
     [(unsupported)
      (format "Cannot infer archive format for ~a" target)]
     [else
      (format "Malformed extraction report: ~s"
              ($extract-report status target))])]

  [($restrict:budget name 'time amount)
   (format "~a it took longer than ~a seconds"
           (restrict-preamble name)
           amount)]

  [($restrict:budget name 'space amount)
   (format "~a its custodian held more than ~a mebibytes"
           (restrict-preamble name)
           amount)]

  [($restrict:operation name 'network 'blocked-listen _)
   (format "~a it tried to listen for connections"
           (restrict-preamble name))]

  [($restrict:operation name 'file 'blocked-delete (list _ path _))
   (format "~a it tried to delete ~a"
           (restrict-preamble name))]

  [($restrict:operation name 'file 'blocked-write (list _ path _))
   (format "~a it tried to write to ~a"
           (restrict-preamble name)
           path)]

  [($restrict:operation name 'link 'blocked-link (list _ link-path target-path))
   (format (~a "~a it tried to create a link at ~a (target: ~a)~n"
               "Packages can only create links in their workspace.")
           (restrict-preamble name)
           link-path
           target-path)]

  [($restrict:operation name 'file 'blocked-execute (list _ path _))
   (if (file-exists? path)
       (format (~a "~a it tried to execute ~a.~n"
                   "To trust this executable, add this to ~a:~n"
                   "(integrity 'sha384 (base64 ~s))")
               (restrict-preamble name)
               path
               (setting-id XIDEN_TRUST_EXECUTABLES)
               (~a (encode 'base64 (make-digest path 'sha384))))
       (~a "Unauthorized attempt to execute non-existant " path))]

  [($path-not-found pattern wrt)
   (format "Could not find path matching ~s~n  w.r.t. ~a"
           pattern
           wrt)]

  [($input:not-found name)
   (format "Input not found: ~s"
           name)]

  [($bad-source-eval reason datum context)
   (format "Cannot evaluate alleged source expression: ~e~n  ~a"
           datum
           (case reason
             [(security)
              (format "security violation: ~s" context)]
             [(invariant) "expression did not produce a source"]
             [else "unknown reason"]))]

  [($cycle key)
   (format "Found cycle at ~s. You may have a circular dependency." key)])
