#lang racket/base

; This module produces flag specifications suitable for
; `parse-command-line`. The flag handlers return procedures that apply
; thunks in new parameterizations. This prevents oddities that arise
; when maintaining mutable storage for CLI-provided values in addition
; to a set of parameters.
;
; (parse-command-line "prog" (current-command-line-arguments)
;                     (make-cli-flag-table a b c ...)
;                     (λ (flags . args)
;                        (call-with-bound-cli-flags flags (λ () (do-something args)))))
;
; Additionally, each CLI flag specified here always include a canonical flag
; named after its corresponding setting.
;

(require (for-syntax racket/base
                     racket/syntax
                     syntax/stx)
         racket/cmdline
         racket/format
         racket/sandbox
         "artifact.rkt"
         "archive.rkt"
         "codec.rkt"
         "crypto.rkt"
         "dig.rkt"
         "file.rkt"
         "format.rkt"
         "input.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "state.rkt"
         "subprogram.rkt"
         "message.rkt"
         "package.rkt"
         "pkgdef/static.rkt"
         "printer.rkt"
         "port.rkt"
         "query.rkt"
         "racket-module.rkt"
         "racket-version.rkt"
         "security.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "system.rkt"
         "url.rkt")

(provide (struct-out cli-flag)
         (struct-out cli-flag-state)
         (contract-out
          [find-cli-flag
           (-> setting? (or/c #f cli-flag?))]
          [all-flags
           (listof cli-flag?)]
          [make-cli-flag-table
           (->* () #:rest (listof cli-flag?) list?)]
          [cli-flag-strings
           (-> cli-flag? (non-empty-listof string?))]
          [shortest-cli-flag
           (-> cli-flag? string?)]
          [format-cli-flags
           (-> cli-flag? string?)]
          [make-cli-flag-string
           (-> (or/c string? cli-flag?) string?)]
          [call-with-bound-cli-flags
           (-> (listof cli-flag-state?)
               (-> any)
               any)]))

(struct cli-flag
  (setting kind additional-flag-strings arity convert help-strings)
  #:transparent)

(struct cli-flag-state
  (flag-string flag-definition bind)
  #:transparent)


;---------------------------------------------------------------------------
; Define procedures for reviewing flag strings. They are necessary for
; generating documentation and contextual help.

(define (cli-flag-strings c)
  (cons (format "--~a" (setting-id (cli-flag-setting c)))
        (cli-flag-additional-flag-strings c)))

(define (shortest-cli-flag c)
  (get-shortest-string (cli-flag-strings c)))

(define (make-cli-flag-string c)
  (if (cli-flag? c)
      (shortest-cli-flag c)
      c))

(define (format-cli-flags c)
  (string-join (sort (cli-flag-strings c) < #:key string-length)
               "/"))



;---------------------------------------------------------------------------
; These procedures creates a flag specs suitable for
; parse-command-line. The flag handler returns a procedure that binds
; the given setting in a new parameterization

(define (make-cli-flag-table . cli-flags)
  (for/list ([(kind flags) (in-hash (group-cli-flags-by-kind cli-flags))])
    (cons kind (map cli-flag->flag-spec (reverse flags)))))


(define (group-cli-flags-by-kind cli-flags)
  (for/fold ([grouped (hasheq)])
            ([flag (in-list cli-flags)])
    (hash-set grouped
              (cli-flag-kind flag)
              (cons flag (hash-ref grouped
                                   (cli-flag-kind flag)
                                   null)))))

(define (cli-flag->flag-spec c)
  (list (sort (cli-flag-strings c) #:key string-length <)
        (procedure-reduce-arity
         (λ formals
           ; This leverages CPS and the prop:procedure property on the setting struct
           ; The idea is that you can compose procedures like these to construct
           ; a parameterization. Composition means that one handler can build on
           ; top of the current value set by another.
           (cli-flag-state (car formals)
                           c
                           (λ (expects-applied-setting)
                             (λ ()
                               ((cli-flag-setting c)
                                (apply (cli-flag-convert c) formals)
                                expects-applied-setting)))))
         (add1 (cli-flag-arity c)))
        (cons (format "~n    ~a~n" (get-localized-string (setting-id (cli-flag-setting c))))
              (cli-flag-help-strings c))))


(define (call-with-bound-cli-flags flags proc)
  (((apply compose (map cli-flag-state-bind flags)) proc)))


;-------------------------------------------------------------------------------
; CLI argument parsers

(define (keep flag a) a)
(define (arg->value flag a) (string->value a))
(define (keep-for s) (λ (flag a) (cons a (s))))

(define make-parsing-evaluator
  (let ([e #f])
    (λ ()
      (unless e
        (set! e
              (call-with-trusted-sandbox-configuration
               (λ () (make-evaluator PACKAGE_DEFINITION_MODULE_LANG)))))
      e)))


(define (arg->value/evaluated flag a)
  ((make-parsing-evaluator) (arg->value flag a)))


;-------------------------------------------------------------------------------
; CLI flag definitions

(define (cli-flag/unary setting convert help-str)
  (cli-flag setting 'once-each null 1 convert (list help-str)))

(define (cli-flag/boolean setting)
  (cli-flag/unary setting arg->value "bool"))

(define (cli-flag/list setting help-string)
  (cli-flag setting 'multi null 1 (keep-for setting) (list help-string)))


; I use short flags as identifiers below to make Racket stop me if I
; duplicate them across definitions.

;-------------------------------------------------------------------------------
; Unary flags

(define all-flags null)

(define-syntax (flag-out stx)
  (syntax-case stx ()
    [(_ [id alts ...] expr)
     (with-syntax ([(strs ...)
                    (stx-map (λ (x) (datum->syntax x (symbol->string (syntax-e x))))
                             #'(id alts ...))]
                   [canonical-id
                    (syntax-case #'expr () [(_ setting-id . _)
                                            (format-id #'stx
                                                       "--~a"
                                                       #'setting-id)])])
       #'(begin (provide canonical-id
                         (rename-out [canonical-id id]
                                     [canonical-id alts] ...))
                (define canonical-id
                  (struct-copy cli-flag expr
                               [additional-flag-strings
                                (list strs ...)]))
                (set! all-flags (cons canonical-id all-flags))))]))

(define (find-cli-flag s)
  (findf (λ (c) (eq? s (cli-flag-setting c)))
         all-flags))


; Unary flags
(flag-out [-M --memory-limit] (cli-flag/unary DENXI_MEMORY_LIMIT_MB arg->value "mebibytes"))
(flag-out [-S --time-limit] (cli-flag/unary DENXI_TIME_LIMIT_S arg->value "seconds"))
(flag-out [-m --fetch-total-size] (cli-flag/unary DENXI_FETCH_TOTAL_SIZE_MB arg->value "mebibytes-or-+inf.0"))
(flag-out [-n --fetch-buffer-size] (cli-flag/unary DENXI_FETCH_BUFFER_SIZE_MB arg->value "mebibytes"))
(flag-out [-p --fetch-pkgdef-size] (cli-flag/unary DENXI_FETCH_PKGDEF_SIZE_MB arg->value "mebibytes"))
(flag-out [-d --fetch-timeout] (cli-flag/unary DENXI_FETCH_TIMEOUT_MS arg->value "milliseconds"))
(flag-out [-o --max-redirects] (cli-flag/unary DENXI_DOWNLOAD_MAX_REDIRECTS arg->value "exact-nonnegative-integer"))
(flag-out [-r --subprocess-timeout] (cli-flag/unary DENXI_SUBPROCESS_TIMEOUT_S arg->value "positive"))
(flag-out [-w --workspace] (cli-flag/unary DENXI_WORKSPACE arg->value "path"))


; Unary boolean flags
(flag-out [-U --trust-unsigned] (cli-flag/boolean DENXI_TRUST_UNSIGNED))
(flag-out [-T --trust-bad-signature] (cli-flag/boolean DENXI_TRUST_BAD_SIGNATURE))
(flag-out [-H --trust-any-host] (cli-flag/boolean DENXI_TRUST_UNVERIFIED_HOST))
(flag-out [-Y --trust-any-digest] (cli-flag/boolean DENXI_TRUST_BAD_DIGEST))
(flag-out [-F --fasl-output] (cli-flag/boolean DENXI_FASL_OUTPUT))
(flag-out [-R --reader-friendly-output] (cli-flag/boolean DENXI_READER_FRIENDLY_OUTPUT))
(flag-out [-v --verbose] (cli-flag/boolean DENXI_VERBOSE))
(flag-out [-G --assume-support] (cli-flag/boolean DENXI_ALLOW_UNSUPPORTED_RACKET))
(flag-out [--trust-any-pubkey] (cli-flag/boolean DENXI_TRUST_ANY_PUBLIC_KEY))
(flag-out [--trust-any-exe] (cli-flag/boolean DENXI_TRUST_ANY_EXECUTABLE))


; Multi flags
(flag-out [+c ++trust-chf ++trust-message-digest-algorithm]
          (cli-flag DENXI_TRUST_CHFS
                    'multi null 1 (λ (flag algo)
                                    (cons (string->symbol algo)
                                          (DENXI_TRUST_CHFS)))
                    '("cryptographic-hash-function")))

(flag-out [++trust-cert]
          (cli-flag DENXI_TRUST_CERTIFICATES
                    'multi null 1
                    (λ (flag cert-path)
                      (cons (expand-user-path cert-path)
                            (DENXI_TRUST_CERTIFICATES)))
                    '("path")))

(flag-out [+p ++trust-public-key]
          (cli-flag DENXI_TRUST_PUBLIC_KEYS
                    'multi null 1 (λ (flag integrity-expr)
                                    (cons (arg->value/evaluated flag integrity-expr)
                                          (DENXI_TRUST_PUBLIC_KEYS)))
                    '("integrity-expr")))

(flag-out [+x ++trust-executable ++trust-exe]
          (cli-flag DENXI_TRUST_EXECUTABLES
                    'multi null 1 (λ (flag integrity-expr)
                                    (cons (arg->value/evaluated flag integrity-expr)
                                          (DENXI_TRUST_EXECUTABLES)))
                    '("integrity-expr")))

(flag-out [+t ++trust-host-executable]
          (cli-flag DENXI_TRUST_HOST_EXECUTABLES
                    'multi null 1 (λ (flag name)
                                    (cons name (DENXI_TRUST_HOST_EXECUTABLES)))
                    '("file-name")))

(flag-out [+s ++install-source]
          (cli-flag DENXI_INSTALL_SOURCES
                    'multi null 3 (λ (flag link-name output-name source)
                                    (cons (list link-name output-name source)
                                          (DENXI_INSTALL_SOURCES)))
                    '("link-name" "output-name" "source")))

(flag-out [+d ++install-default]
          (cli-flag DENXI_INSTALL_DEFAULT_SOURCES
                    'multi null 2 (λ (flag link-name source)
                                    (cons (list link-name source)
                                          (DENXI_INSTALL_DEFAULT_SOURCES)))
                    '("link-name" "source")))

(flag-out [+a ++install-abbreviated]
          (cli-flag DENXI_INSTALL_ABBREVIATED_SOURCES
                    'multi null 1 (λ (flag source)
                                    (cons source
                                          (DENXI_INSTALL_ABBREVIATED_SOURCES)))
                    '("source")))

(flag-out [+l ++install-link]
          (cli-flag DENXI_INSTALL_ARTIFACTS
                    'multi null 2 (λ (flag link-name hint)
                                    (cons (list link-name hint)
                                          (DENXI_INSTALL_ARTIFACTS)))
                    '("link-name" "hint")))

(flag-out [+e ++env ++envvar]
          (cli-flag DENXI_ALLOW_ENV
                    'multi null 1 (λ (flag name)
                                    (cons name
                                          (DENXI_ALLOW_ENV)))
                    '("envvar")))

(flag-out [+o ++input-override]
          (cli-flag DENXI_INPUT_OVERRIDES
                    'multi null 2 (λ (flag pattern input-expr)
                                    (cons (list (arg->value flag pattern)
                                                (arg->value flag input-expr))
                                          (DENXI_INPUT_OVERRIDES)))
                    '("pregexp-pattern" "input-expr")))


; For use in REPL and tests. Provides a quick way to preview the effect of command
; line flags, and generated help strings shown.
(define (try-flags . args)
  (call/cc (λ (halt)
             (parse-command-line "test" (list->vector args)
                                 (apply make-cli-flag-table all-flags)
                                 (λ (flags)
                                   (call-with-bound-cli-flags
                                    flags
                                    (λ () (for/fold ([dump (hash)])
                                                    ([st (in-list flags)])
                                            (define setting (cli-flag-setting (cli-flag-state-flag-definition st)))
                                            (hash-set dump (setting-id setting) (setting))))))
                                 null
                                 (λ (h) (displayln h) (halt (void)))))))



(module+ test
  (require racket/contract
           racket/function
           racket/match
           rackunit)


  (check-eq? (keep "-a" 1) 1)
  (check-equal? (arg->value "--whatever" "+inf.0") +inf.0)
  (check-equal? ((keep-for (λ () '(2 3))) "--whatever" 1)
                '(1 2 3))


  (check-true (andmap cli-flag? all-flags))

  (define-setting TEST_SETTING real? 10)

  (define TEST_SETTING/flag (cli-flag TEST_SETTING
                                      'once-each
                                      '("-t" "--test")
                                      1
                                      (λ (flag a) (string->value a))
                                      '("value")))

  (test-equal? "Always include setting name as a CLI flag"
               (cli-flag-strings TEST_SETTING/flag)
               '("--TEST_SETTING" "-t" "--test"))

  (test-equal? "Find the shortest CLI flag to help abbreviate messages to the user"
               (shortest-cli-flag TEST_SETTING/flag)
               "-t")

  (test-equal? "Coerce CLI flag string"
               (make-cli-flag-string TEST_SETTING/flag)
               (shortest-cli-flag TEST_SETTING/flag))

  (test-equal? "Return provided CLI flag string"
               (make-cli-flag-string "--whatever")
               "--whatever")

  (test-equal? "Show all flags in order of increasing length when total message length is a non-issue"
               (format-cli-flags TEST_SETTING/flag)
               "-t/--test/--TEST_SETTING")

  (test-case "Convert CLI flag to deferred setting binding"
    (match-define (list flag-strings handler help-strings) (cli-flag->flag-spec TEST_SETTING/flag))
    (check-equal? (sort #:key string-length (cli-flag-strings TEST_SETTING/flag) <) flag-strings)
    (check-equal? (procedure-arity handler) (add1 (cli-flag-arity TEST_SETTING/flag)))
    (check-equal? help-strings '("\n    #<void>\n" "value"))

    (match-define (cli-flag-state flag-string cli-flag-inst args-bound) (handler "-t" "+inf.0"))
    (check-equal? flag-string "-t")
    (check-eq? cli-flag-inst TEST_SETTING/flag)
    (define callback-bound (args-bound (λ () (check-equal? (TEST_SETTING) +inf.0))))

    (callback-bound))

  (test-case "Group CLI flags"
    (define (of-kind k) (cli-flag #f k #f #f #f #f))

    (define-values (oe1 oe2 oa1 oa2 oa3 m1 m2 m3)
      (values (of-kind 'once-each)
              (of-kind 'once-each)
              (of-kind 'once-any)
              (of-kind 'once-any)
              (of-kind 'once-any)
              (of-kind 'multi)
              (of-kind 'multi)
              (of-kind 'multi)))

    (check-equal? (group-cli-flags-by-kind (list oe1 oe2 oa1 oa2 oa3 m1 m2 m3))
                  (hasheq 'once-each (list oe2 oe1)
                          'once-any (list oa3 oa2 oa1)
                          'multi (list m3 m2 m1))))

  (test-case "Ease use of parse-command-line"
    (define-setting TEST_MULTI     list? null)
    (define-setting TEST_ONCE_EACH (or/c #f string?) #f)
    (define-setting TEST_ONCE_ANY  string? "")

    (define (get-current-values)
      (list (TEST_MULTI)
            (TEST_ONCE_EACH)
            (TEST_ONCE_ANY)))

    (define multi-flag
      (cli-flag TEST_MULTI 'multi
                '("-m" "--multi")
                2
                (λ (flag a b)
                  (cons (string->value a) (cons (string->value b) (TEST_MULTI))))
                '("num" "num")))

    (define once-each-flag
      (cli-flag TEST_ONCE_EACH
                'once-each
                '("--quad")
                4
                (λ (flag a b c d)
                  (string-join (list a b c d) " "))
                '("str" "str" "str" "str")))


    (define once-any-flag
      (cli-flag TEST_ONCE_ANY
                'once-any
                '("-a")
                1
                (λ (flag a) a)
                '("str")))


    (test-case "Can compose multiple flag handlers"
      (match-define (list _ handler _) (cli-flag->flag-spec multi-flag))
      (define bind-a (handler "--multi" "1" "2"))
      (define bind-b (handler "--multi" "3" "4"))
      (check-equal? (call-with-bound-cli-flags (list bind-a bind-b) (λ () (TEST_MULTI)))
                    '(3 4 1 2)))


    (define-values (outside-parameterization inside-parameterization positional)
      (parse-command-line "test" #("-m" "1" "2"
                                        "--multi" "3" "4"
                                        "--TEST_MULTI" "5" "6"
                                        "-a" "once"
                                        "--quad" "what" "else" "you" "got?"
                                        "positional" "arguments")
                          (make-cli-flag-table multi-flag once-each-flag once-any-flag)
                          (λ (flags formal1 formal2)
                            (check-equal? (map cli-flag-state-flag-string flags)
                                          '("-m"
                                            "--multi"
                                            "--TEST_MULTI"
                                            "-a"
                                            "--quad"))

                            (values (get-current-values)
                                    (call-with-bound-cli-flags flags get-current-values)
                                    (list formal1 formal2)))
                          '("arg" "arg")))

    (check-equal? outside-parameterization
                  '(() #f ""))

    (check-equal? inside-parameterization
                  '((5 6 3 4 1 2)
                    "what else you got?"
                    "once"))

    (check-equal? positional
                  '("positional" "arguments"))))
