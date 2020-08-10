#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/string))

(require racket/function
         racket/string)

(define (whole/pattstr s) (string-append "^" s "$"))
(define (group/pattstr s)
  (string-append "(" s ")"))
(define (or/pattstr . opts)
  (string-append "(?:"
                 (string-join opts "|")
                 ")"))

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

(define name-string?
  (procedure-rename
   (conjoin string?
            (negate
             (disjoin (curry equal? "")
                      (make-rx-predicate #:whole? #f unix-reserved-character-pattern-string)
                      (make-rx-predicate #:whole? #f windows-reserved-character-pattern-string)
                      (make-rx-predicate #:whole? #t windows-reserved-name-pattern-string))))
   'name-string?))

(module+ test
  (require rackunit)

  (check-false (name-string? "/"))
  (check-false (name-string? "\\"))
  (check-false (name-string? ":"))
  (check-false (name-string? "alvin:bet"))
  (check-pred name-string? "alvin")

  (define windows-reserved-character?
    (make-rx-predicate windows-reserved-character-pattern-string))
  (define windows-reserved-name?
    (make-rx-predicate windows-reserved-name-pattern-string))

  (check-true (windows-reserved-character? "<"))
  (check-true (windows-reserved-character? ">"))
  (check-true (windows-reserved-character? ":"))
  (check-true (windows-reserved-character? "\\"))
  (check-true (windows-reserved-character? "/"))
  (check-true (windows-reserved-character? "|"))
  (check-true (windows-reserved-character? "?"))
  (check-true (windows-reserved-character? "*"))
  (check-false (windows-reserved-character? "^"))
  (check-false (windows-reserved-character? "a"))

  (define (expect-windows-reserved-names . names)
    (for ([name (in-list names)])
      (test-case (format "Recognize ~a as a reserved name on Windows" name)
        (check-true (windows-reserved-name? (string-downcase name)))
        (check-true (windows-reserved-name? (string-upcase name))))))

  (expect-windows-reserved-names "CON" "PRN" "AUX" "NUL" "CLOCK$" "." ".." "..." "...."
                                 "COM0" "COM1" "COM2" "COM3" "COM4" "COM5" "COM6" "COM7" "COM8" "COM9"
                                 "LPT0" "LPT1" "LPT2" "LPT3" "LPT4" "LPT5" "LPT6" "LPT7" "LPT8" "LPT9"
                                 "$Mft" "$MftMirr" "$LogFile" "$Volume" "$AttrDef" "$Bitmap" "$Boot"
                                 "$BadClus" "$Secure" "$Upcase" "$Extend" "$Quota" "$ObjId" "$Reparse"
                                 "con.txt" "AUx.bin")

  (check-false (windows-reserved-name? "AuL"))
  (check-false (windows-reserved-name? "AuL.txt"))

  (let ([three-digits^$ (make-rx-predicate "\\d\\d\\d")]
        [three-digits (make-rx-predicate "\\d\\d\\d" #:whole? #f)])
    (test-case "Match whole pattern by predicate"
      (check-true (three-digits^$ "002"))
      (check-false (three-digits^$ " 002"))
      (check-true (three-digits " 002"))))

  (let ([choice (make-rx-predicate (or/pattstr "alpha" "beta") #:whole? #f)])
    (check-true (choice "xxx alpha yyy"))
    (check-true (choice "xxx beta yyy")))

  (let ([capt (make-rx-matcher #:whole? #f
                               (string-append
                                (group/pattstr (or/pattstr "\\d\\d" "[a-f][a-f]"))
                                "x"
                                (group/pattstr (or/pattstr "\\d\\d" "[a-f][a-f]"))))])
    (check-equal? (capt "dax29")
                  (list "dax29" "da" "29"))))
