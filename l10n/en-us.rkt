#lang racket/base

(provide get-string)
(require "shared.rkt")

(define (L . lines)
  (string-join lines "\n"))

(define (get-string key)
  (case key
    [(top-level-cli-help)
     (L "<action> is one of"
        "  do     Run transaction"
        "  gc     Collect garbage"
        "  show   Print report"
        "  mkint  Make integrity info for bytes")]

    [(show-command-help)
     (L "where <what> is one of"
        "  config     Show a (read)able hash table of current settings"
        "  installed  Show a list of installed outputs"
        "  links      Show a list of issued links"
        "  workspace  Show the path to the target workspace directory")]

    [(backwards-racket-version-interval)
     "minimum Racket version cannot exceed maximum Racket version"]

    [(XIDEN_SANDBOX_MEMORY_LIMIT_MB)
     "Total memory quota for a sandbox"]

    [(XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB)
     "Memory quota for each sandboxed expression"]

    [(XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
     "Time limit for each sandboxed expression"]

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

    [(XIDEN_TRUSTED_PUBLIC_KEYS)
     "Trust a given public key, by 160-bit SHA-384 fingerprint"]

    [(XIDEN_TRUSTED_EXECUTABLES)
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

    [(XIDEN_PRIVATE_KEY_PATH)
     "The location of a private key"]

    [(XIDEN_CATALOGS)
     "Services to contact when searching for package definitions"]

    [(XIDEN_DOWNLOAD_MAX_REDIRECTS)
     "Maximum redirects to follow when downloading an artifact"]

    [(XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS)
     "Install packages even if they do not declare supported Racket versions"]

    [(XIDEN_ALLOW_UNSUPPORTED_RACKET)
     "Install packages even if they declare that they do not support the running version of Racket."]

    [(XIDEN_ALLOW_ENV)
     "Names of environment variables to expose to packages and subprocesses"]

    [(XIDEN_SUBPROCESS_TIMEOUT_S)
     "Maximum number of seconds a subprocess may run"]))




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

  [($package:unfetched source)
   (format "could not fetch package definition from source: ~s"
           source)]

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
                              "(integrity 'sha384 (base64 ~s))")
                          (setting-id XIDEN_TRUSTED_PUBLIC_KEYS)
                          (~a (encode 'base64 (make-digest public-key-path 'sha384)))))]

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

  [($package:security 'network 'blocked-listen _)
   "Unauthorized attempt to listen for connections"]

  [($package:security 'file 'blocked-delete (list _ path _))
   (format "Blocked unauthorized delete on ~a" path)]

  [($package:security 'file 'blocked-write (list _ path _))
   (format "Blocked unauthorized write to ~a" path)]

  [($package:security 'link 'blocked-link (list _ link-path target-path))
   (format (~a "Blocked link creation at ~a (target: ~a)~n"
               "Packages can only create links in their workspace.")
           link-path target-path)]

  [($package:security 'file 'blocked-execute (list _ path _))
   (if (file-exists? path)
       (format (~a "Unauthorized attempt to execute ~a.~n"
                   "To trust this executable, add this to ~a:~n"
                   "(integrity 'sha384 (base64 ~s))")
               path
               (setting-id XIDEN_TRUSTED_EXECUTABLES)
               (~a (encode 'base64 (make-digest path 'sha384))))
       (~a "Unauthorized attempt to execute non-existant " path))]

  [($path-not-found pattern wrt)
   (format "Could not find path matching ~s~n  w.r.t. ~a"
           pattern
           wrt)])
