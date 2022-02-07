#lang racket/base

(provide get-string)
(require "shared.rkt")

(define (L . lines)
  (string-join lines "\n"))


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


(define format-message/locale
  (match-lambda
    [($finished-collecting-garbage bytes-recovered)
     (format "Recovered ~a"
             (if (> bytes-recovered (/ (* 1024 2024) 10))
                 (~a (~r (/ bytes-recovered (* 1024 1024)) #:precision 2)
                     " mebibytes")
                 (~a bytes-recovered
                     " bytes")))]

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

    [($package:abstract-input name)
     (~a "cannot use abstract input " (~s name)
         ". Either override it using "
         (format-cli-flags --DENXI_INPUT_OVERRIDES)
         ", or configure your launcher to address this.")]

    [($package:output:built)
     "built output"]

    [($package:output:reused)
     "reused output"]

    [($package:output:undefined)
     "requested output is not defined"]

    [($package:unavailable-output available)
     (format "Requested output is not available. Available outputs: ~s" available)]

    [($fetch name errors)
     (if (null? errors)
         (format "~a: fetched" name)
         (format "~a: fetch failed~n~a"
                 name
                 (string-join (map format-message errors)
                              "\n")))]

    [($artifact:signature status public-key)
     (~a "signature check: "
         (case status
           [(signature-verified)
            "passed verification"]

           [(signature-unverified)
            "failed verification"]

           [(skip)
            "trusting implicitly"]

           [(skip-unsigned)
            "trusting unsigned"]

           [(unsigned)
            (format "signature required (bypass: ~a)"
                    (format-cli-flags --trust-unsigned))]

           [(blocked-public-key)
            (define preamble "untrusted public key")
            (define chf (get-default-chf))
            (copy-port (open-input-bytes public-key)
                       (current-output-port))
            (if chf
                ((λ (relevant-setting-id encoded)
                   (~a preamble "\n"
                       "To trust this key, add this to " relevant-setting-id ":\n"
                       "(integrity '" chf " (base64 " (~s encoded) "))"))
                 (setting-id DENXI_TRUST_PUBLIC_KEYS)
                 (encode 'base64 (make-digest public-key)))
                preamble)]))]

    [($artifact:integrity status chf)
     (~a "integrity check: "
         (case status
           [(digests-match)
            "digests match"]

           [(digests-differ)
            (format "digests differ. To (dangerously) bypass, use ~a"
                    (format-cli-flags --trust-any-digest))]

           [(malformed-input)
            "malformed input"]

           [(blocked-chf)
            (format "not trusting CHF ~a. To bypass, add '~a to ~a"
                    chf
                    (setting-id DENXI_TRUST_CHFS))]

           [(skip)
            "trusting implicitly"]

           [else (format "unknown status: ~s" status)]))]

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

    [($http-failure request-url status-line headers capped-body)
     (~a "HTTP fetch failure: " request-url "\n"
         "-----------\n"
         (string-replace status-line "\r" "")
         (join-lines
          (map (λ (pair) (~a (car pair) ": " (cdr pair)))
               (sort headers #:key car string<?)))
         "\n\n"
         (~s capped-body))]

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
     (format (~a "~a it tried to create a link outside of an allowed directory.~n"
                 "  link path: ~a~n"
                 "  target: ~a~n")
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
                 (setting-id DENXI_TRUST_EXECUTABLES)
                 (~a (encode 'base64 (make-digest path 'sha384))))
         (~a "Unauthorized attempt to execute non-existant " path))]

    [($racket-version:invalid-interval min-v max-v)
     (format "Cannot match Racket version in reversed interval: [~a, ~a]"
             min-v
             max-v)]

    [($path-not-found pattern wrt)
     (format "Could not find path matching ~s~n  w.r.t. ~a"
             pattern
             wrt)]

    [($input:not-found name)
     (format "Input not found: ~s"
             name)]

    [($input:log name messages)
     (format "Resolving input ~s~n~a"
             name
             (join-lines (map format-message messages)))]

    [($dig:no-artifact shovel-name hint)
     (format "Artifact not found with ~a and ~v"
             shovel-name
             hint)]

    [($bad-source-eval reason datum context)
     (format "Cannot evaluate alleged source expression: ~e~n  ~a"
             datum
             (case reason
               [(security)
                (format "security violation: ~s" context)]
               [(invariant) "expression did not produce a source"]
               [else "unknown reason"]))]

    [($untrusted-cert uri original-exn)
     (format (~a "Could not connect to a server due to an untrusted certificate.~n"
                 "~n  ~a~n~n"
                 "If you trust those running the servers at ~a, download the~n"
                 "certificate from a location that your operating system trusts ~n"
                 "(to mitigate the risk of man-in-the-middle attacks), then add ~n"
                 "that certificate to ~a.~n~n"
                 "Original error follows:~n"
                 "~a")
             (url->string uri)
             (url-host uri)
             (setting-id DENXI_TRUST_CERTIFICATES)
             (exn->string original-exn))]

    [($chf-unavailable chf)
     (if chf
         (~a "No implementation available for CHF " chf)
         "No cryptographic hash functions installed")]

    [($cycle key)
     (format "Found cycle at ~s. You may have a circular dependency." key)]

    [($package-query-canon:backwards user-query autocompleted-query lo hi)
     (~a (make-$package-query-canon-preamble user-query autocompleted-query)
         "Resolved backwards interval {" lo " .. " hi "}")]

    [($package-query-canon:no-minimum uq aq hint)
     (~a (make-$package-query-canon-preamble uq aq)
         "Unresolved minimum revision:" hint)]

    [($package-query-canon:no-maximum uq aq hint)
     (~a (make-$package-query-canon-preamble uq aq)
         "Unresolved maximum revision:" hint)]

    [($package-query-canon:no-selection uq aq minimum maximum hint)
     (~a (make-$package-query-canon-preamble uq aq)
         "Failed to select best fit in {" minimum "..." maximum "}\n"
         "Value: " hint)]

    [($package-query-canon:oob uq aq minimum maximum selection)
     (~a (make-$package-query-canon-preamble uq aq)
         "Query is limited to {" minimum "..." maximum "},\n"
         "but canon selected: " selection "\n"
         "This is likely a bug with the service.\n"
         "Please report it to the maintainer.")]

    [($crypto:error queue)
     (string-join (map crypto-translate-error! queue) "\n")]))
