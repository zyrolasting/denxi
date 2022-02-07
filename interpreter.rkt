#lang racket/base

; Add transaction semantics

(require racket/contract
         racket/exn
         "machine.rkt"
         "message.rkt")


(define-message $interpreter ())
(define-message $interpreter:done $interpreter ())
(define-message $interpreter:redundant $interpreter ())


(define transaction-mark-key
  (string->uninterned-symbol "transaction"))
   

(define-subprogram (interpret ils sleep-time make-mind make-subprogram)
  (if (continuation-mark-set-first transaction-mark-key)
      ($fail ($interpreter:redundant))
      (with-continuation-mark transaction-mark-key #t
        (subprogram
         (λ (messages)
           (define mind (make-mind))
           (define-values (mind* messages*)
             (run-subprogram (make-subprogram mind) messages))
           (values mind*
                   (cons ($transaction:done)
                         messages*)))))))


; Interprocess locks are hard. Gate process using explicit, prescribed bytes.
(define-subprogram (assert-external-consent path key)
  (if (or (not (file-exists? path))
          (with-input-from-file path
            (λ ()
              (define data (read-bytes (bytes-length key)))
              (and (not (eof-object? data))
                   (bytes=? data key)))))
      (with-output-to-file (interprocess-lock-path ils) #:mode 'truncate/replace
        (λ ()
          (write-bytes (interprocess-lock-key ils))
          ($use (void))))
      ($fail ($locked))))

