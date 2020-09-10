#lang racket/base

; This module produces values suitable for `parse-command-line`. The
; flag handlers produce procedures that together bind settings in a
; new parameterization. This prevents oddities that arise when
; maintaining mutable storage for CLI-provided values in addition to a
; set of parameters.
;
; (parse-command-line "prog" (current-command-line-arguments)
;                     (make-cli-flag-table a b c ...)
;                     (λ (flags . args)
;                        (call-with-bound-cli-flags (λ () (do-something args)))))
;
; Additionally, each CLI flag specified here always include a canonical flag
; named after its corresponding setting.
;

(require "contract.rkt")

(provide (struct-out cli-flag)
         (contract-out
          [make-cli-flag-table
           (->* () #:rest (listof cli-flag?) list?)]
          [cli-flag-strings
           (-> cli-flag? (non-empty-listof string?))]
          [shortest-cli-flag
           (-> cli-flag? string?)]
          [format-cli-flags
           (-> cli-flag? string?)]
          [call-with-bound-cli-flags
           (-> (listof procedure?)
               (-> any)
               any)]))

(require "setting.rkt"
         "string.rkt")

(struct cli-flag
  (setting kind additional-flag-strings arity convert help-strings)
  #:transparent)


;---------------------------------------------------------------------------
; Define procedures for reviewing flag strings. They are necessary for
; generating documentation and contextual help.

(define (cli-flag-strings c)
  (cons (format "--~a" (setting-id (cli-flag-setting c)))
        (cli-flag-additional-flag-strings c)))

(define (shortest-cli-flag c)
  (get-shortest-string (cli-flag-strings c)))

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
  (list (cli-flag-strings c)
        (procedure-reduce-arity
         (λ formals
           ; This leverages CPS and the prop:procedure property on the setting struct
           ; The idea is that you can compose procedures like these to construct
           ; a parameterization. Composition means that one handler can build on
           ; top of the current value set by another.
           (λ (expects-applied-setting)
             (λ ()
               ((cli-flag-setting c)
                (apply (cli-flag-convert c) formals)
                expects-applied-setting))))
         (add1 (cli-flag-arity c)))
        (cons (setting-description (cli-flag-setting c))
              (cli-flag-help-strings c))))


(define (call-with-bound-cli-flags flags proc)
  (((apply compose flags) proc)))


(module+ test
  (require racket/contract
           racket/cmdline
           racket/function
           racket/match
           rackunit)

  (define-setting TEST_SETTING real? 10 "Test")

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

  (test-equal? "Show all flags in order of increasing length when total message length is a non-issue"
               (format-cli-flags TEST_SETTING/flag)
               "-t/--test/--TEST_SETTING")

  (test-case "Convert CLI flag to deferred setting binding"
    (match-define (list flag-strings handler help-strings) (cli-flag->flag-spec TEST_SETTING/flag))
    (check-equal? (cli-flag-strings TEST_SETTING/flag) flag-strings)
    (check-equal? (procedure-arity handler) (add1 (cli-flag-arity TEST_SETTING/flag)))
    (check-equal? help-strings '("Test" "value"))

    (define args-bound (handler "-t" "+inf.0"))
    (define callback-bound (args-bound (λ () (check-eq? (TEST_SETTING) +inf.0))))

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
    (define-setting TEST_MULTI     list? null "Accepts the numbers 1, 2, or 3.")
    (define-setting TEST_ONCE_EACH (or/c #f string?) #f "Accepts two arguments")
    (define-setting TEST_ONCE_ANY  string? "" "Accepts a string")

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
