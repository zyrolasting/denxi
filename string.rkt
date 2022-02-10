#lang racket/base

; Extend racket/string

(require racket/contract)
(provide (all-from-out racket/string)
         non-empty-string
         (contract-out
          [DEFAULT_STRING "default"]
          [coerce-character (-> any/c (or/c #f char?))]
          [file-name-string? predicate/c]
          [get-shortest-string (-> (non-empty-listof string?) string?)]
          [group/pattstr (-> string? string?)]
          [in-character-range (-> coerce-character coerce-character (sequence/c char?))]
          [in-cartesian-strings (-> (sequence/c (sequence/c (or/c char? string?)))
                                    (sequence/c string?))]
          [make-extension-pattern-string (->* () #:rest (listof string?) string?)]
          [make-rx-matcher (->* (string?) (#:whole? any/c) (-> any/c any/c))]
          [make-rx-predicate (->* (string?) (#:whole? any/c) predicate/c)]
          [maybe-spaces-pattern-string string?]
          [non-empty-bytes? predicate/c]
          [or/pattstr (-> string? string?)]
          [string->value (-> string? any/c)]
          [string-hash-code (-> string? exact-nonnegative-integer?)]
          [string-secondary-hash-code (-> string? exact-nonnegative-integer?)]
          [unix-reserved-character-pattern-string string?]
          [whole/pattstr (-> string? string?)]
          [windows-reserved-character-pattern-string string?]
          [windows-reserved-name-pattern-string string?]))


(require racket/function
         racket/string
         syntax/parse
         "sequence.rkt"
         (for-syntax racket/base))

(define DEFAULT_STRING "default")

(define-syntax-class non-empty-string
  (pattern (~var str string)
           #:when (non-empty-string? (syntax-e #'str))))

(define (whole/pattstr s) (string-append "^" s "$"))
(define (group/pattstr s)
  (string-append "(" s ")"))
(define (or/pattstr . opts)
  (string-append "(?:"
                 (string-join opts "|")
                 ")"))

(define (string-hash-code str)
  (for/sum ([char (in-string str)])
    (char->integer char)))

(define (string-secondary-hash-code str)
  (define code (string-hash-code str))
  (* code code))

(define (make-extension-pattern-string . exts)
  (format "\\.(~a)$"
          (string-join exts "|")))

(define unix-reserved-character-pattern-string
  "[//\x0]")

(define windows-reserved-character-pattern-string
  "[<>\\:\"/\\\\\\|\\?\\*]")

(define windows-reserved-name-pattern-string
  (format "(?i:(~a))(\\..*)?"
          (string-join '("\\.+" "com\\d" "lpt\\d" "nul" "aux" "prn" "con"
                                "CLOCK\\$" "\\$Mft" "\\$MftMirr" "\\$LogFile" "\\$Volume"
                                "\\$AttrDef" "\\$Bitmap" "\\$Boot" "\\$BadClus" "\\$Secure"
                                "\\$Upcase" "\\$Extend" "\\$Quota" "\\$ObjId" "\\$Reparse")
                       "|")))

(define maybe-spaces-pattern-string "\\s*")

(define (non-empty-bytes? b)
  (and (bytes? b)
       (> (bytes-length b) 0)))

(define (make-rx-predicate p #:whole? [whole? #t])
  (define rx (pregexp (if whole? (whole/pattstr p) p)))
  (λ (v)
    (and (string? v)
         (regexp-match? rx v))))

(define (make-rx-matcher p #:whole? [whole? #t])
  (define rx (pregexp (if whole? (whole/pattstr p) p)))
  (λ (v)
    (and (string? v)
         (regexp-match rx v))))

(define file-name-string?
  (procedure-rename
   (conjoin string?
            (negate
             (disjoin (curry equal? "")
                      (make-rx-predicate #:whole? #f unix-reserved-character-pattern-string)
                      (make-rx-predicate #:whole? #f windows-reserved-character-pattern-string)
                      (make-rx-predicate #:whole? #t windows-reserved-name-pattern-string))))
   'file-name-string?))

(define (get-shortest-string strings)
  (for/fold ([shortest (car strings)])
            ([next (in-list (cdr strings))])
    (if (< (string-length next) (string-length shortest))
        next
        shortest)))


(define (string->value s)
  (call-with-default-reading-parameterization
   (λ ()
     (read (open-input-string s)))))


(define (in-cartesian-strings data)
  (in-cartesian-map string-append
    (sequence-map (λ (gear)
                    (sequence-map (λ (v) (if (char? v) (string v) v))
                                  gear))
                                  data)))


(define (in-character-range start end)
  (define s (char->integer start))
  (define e (char->integer end))
  (sequence-map integer->char
                (cond [(< s e)
                       (in-range s (add1 e))]
                      [(> s e)
                       (in-range s (sub1 e) -1)]
                      [else
                       (in-value s)])))


(define (coerce-character variant)
  (cond [(char? variant)
         variant]
        [(and (string? variant)
              (= 1 (string-length variant)))
         (string-ref variant 0)]
        [(symbol? variant)
         (coerce-character (symbol->string variant))]
        [(integer? variant)
         (with-handlers ([(negate exn:break?) (const #f)])
           (integer->char variant))]
        [else #f]))


(module+ test
  (require "test.rkt")

  (test coerce-characters
        (assert (eq? (coerce-character "a") #\a))
        (assert (eq? (coerce-character  'a) #\a))
        (assert (eq? (coerce-character #\a) #\a))
        (assert (not (coerce-character "aa")))
        (assert (not (coerce-character #t))))

  (test character-range-pattern
        (define forward (sequence->list "abc"))
        (define backward (reverse forward))
        (assert (equal? (sequence->list (in-character-range #\a #\c)) forward))
        (assert (equal? (sequence->list (in-character-range #\c #\a)) backward)))

  (test single-character-strings
        (assert (not (coerce-character "")))
        (assert (char=? #\a (coerce-character "a")))
        (assert (not (coerce-character "aa"))))

  (test coerce-infinity
        (assert (equal? +inf.0 (string->value "+inf.0"))))

  (test shortest-string
        (assert (equal? "v" (get-shortest-string '("alvin" "bob" "v" "superduper")))))

  (test file-name-strings
        (assert (not (file-name-string? "/")))
        (assert (not (file-name-string? "\\")))
        (assert (not (file-name-string? ":")))
        (assert (not (file-name-string? "alvin:bet")))
        (assert (file-name-string? "alvin")))

  (define windows-reserved-character?
    (make-rx-predicate windows-reserved-character-pattern-string))
  (define windows-reserved-name?
    (make-rx-predicate windows-reserved-name-pattern-string))

  (test windows-reserved-characters
        (assert (windows-reserved-character? "<"))
        (assert (windows-reserved-character? ">"))
        (assert (windows-reserved-character? ":"))
        (assert (windows-reserved-character? "\\"))
        (assert (windows-reserved-character? "/"))
        (assert (windows-reserved-character? "|"))
        (assert (windows-reserved-character? "?"))
        (assert (windows-reserved-character? "*"))
        (assert (not (windows-reserved-character? "^")))
        (assert (not (windows-reserved-character? "a"))))

  (define (expect-windows-reserved-name name)
    (and (windows-reserved-name? (string-downcase name))
         (windows-reserved-name? (string-upcase name))))

  (define-syntax-rule (expect-windows-reserved-names x ...)
    (begin (assert (expect-windows-reserved-name x)) ...))

  (test windows-names
        (expect-windows-reserved-names
         "CON" "PRN" "AUX" "NUL" "CLOCK$" "." ".." "..." "...."
         "COM0" "COM1" "COM2" "COM3" "COM4" "COM5" "COM6" "COM7" "COM8" "COM9"
         "LPT0" "LPT1" "LPT2" "LPT3" "LPT4" "LPT5" "LPT6" "LPT7" "LPT8" "LPT9"
         "$Mft" "$MftMirr" "$LogFile" "$Volume" "$AttrDef" "$Bitmap" "$Boot"
         "$BadClus" "$Secure" "$Upcase" "$Extend" "$Quota" "$ObjId" "$Reparse"
         "con.txt" "AUx.bin")
        (assert (not (windows-reserved-name? "AuL")))
        (assert (not (windows-reserved-name? "AuL.txt"))))

  (test match-by-predicate
        (define three-digits^$ (make-rx-predicate "\\d\\d\\d"))
        (define three-digits (make-rx-predicate "\\d\\d\\d" #:whole? #f))
        (assert (three-digits^$ "002"))
        (assert (not (three-digits^$ " 002")))
        (assert (three-digits " 002")))

  (test test-or/pattstring
        (define choice (make-rx-predicate (or/pattstr "alpha" "beta") #:whole? #f))
        (assert (choice "xxx alpha yyy"))
        (assert (choice "xxx beta yyy")))

  (test pattern-grouping
        (define capt
          (make-rx-matcher #:whole? #f
                           (string-append
                            (group/pattstr (or/pattstr "\\d\\d" "[a-f][a-f]"))
                            "x"
                            (group/pattstr (or/pattstr "\\d\\d" "[a-f][a-f]")))))
        (assert (equal? (capt "dax29")
                        (list "dax29" "da" "29")))))
