#lang racket/base

; Define a setting as a Racket value from one of the following
; sources (where a later source overrides an earlier one):
;
; 1. A hard-coded default
; 2. A runtime configuration file
; 3. An environment variable
; 4. A command line argument (not handled here)
;
; Protect each setting with a contract.
;
; Assuming the name of a setting is "Use Widgets", the following
; must hold:
;
; - The envvar's name is `ZCPKG_USE_WIDGETS`.
;
; - (file->value "etc/zcpkg/ZCPKG_USE_WIDGETS") produces a value, where
;   the path is relative to this file.
;
; - The command line flag is --use-widgets "...", where "..." is a
;   (read)able value. If the setting is a boolean, omit the "...".

(provide make-setting
         load-setting)

(require racket/contract
         racket/file
         "logging.rkt")

(define (load-setting id make-rcfile-path default)
  (define str (symbol->string id))
  (or (with-handlers ([exn:fail? (λ (e) #f)])
        (file->value (make-rcfile-path str)))
      (getenv str)
      default))

(define (make-setting id cnt [initial (void)])
  ; - The guard does not check the initial value.
  (make-parameter initial (make-guard id cnt)))

(define (make-guard id cnt)
  (λ (v)
    (log-message zcpkg-logger
                 'debug
                 (format "setting ~a: ~s" id v)
                 (vector id v (current-continuation-marks)))
    (with-handlers ([exn:fail? (λ (e) (raise (rewrite-contract-error-message e id)))])
      (invariant-assertion cnt v))))

(define (rewrite-contract-error-message e id)
  (struct-copy exn:fail:contract e
               [message #:parent exn
                        (regexp-replace #rx"^[^\n]+"
                                        (exn-message e)
                                        (format "Invalid value for ~a" id))]))
