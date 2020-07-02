#lang racket/base

(provide config-command)

(require racket/file
         racket/path
         racket/pretty
         "common.rkt")

(define HELP-FMT #<<EOF
Usage: raco zcpkg config <subcommand>

Change or review package manager configuration.

raco zcpkg config set <key> <value>
raco zcpkg config get <key>
raco zcpkg config dump

EOF
)


(define (show-help . _)
  (displayln HELP-FMT))

(define (make-fail-thunk str)
  (λ ()
    (eprintf "There is no setting called ~a.~n" str)
    (exit 1)))

(define (set-config)
  (command-line
   #:program "set-config"
   #:args (key val)
   (define dump (dump-configuration))
   (define sym (string->symbol key))
   (define param (hash-ref dump sym (make-fail-thunk key)))

   (define to-write
     (with-handlers ([exn:fail? (λ (e)
                                  (eprintf "Rejecting invalid value for ~a. Error follows.~n" sym)
                                  (raise e))])
       (param (read (open-input-string val)))
       (param)))

   (define target (ws/ "etc/zcpkg" key))
   (make-directory* (path-only target))
   (write-to-file #:exists 'truncate/replace
                  (param) target)

   (printf "Saved ~a~n" target)))

(define (get-config)
  (command-line
   #:program "get-config"
   #:args (key)
   (define dump (dump-configuration))
   (define param (hash-ref dump (string->symbol key) (make-fail-thunk key)))
   (pretty-write #:newline? #t (param))))

(define (dump-config)
  (command-line
   #:program "dump-config"
   #:args ()
   (pretty-write #:newline? #t
                 (for/hash ([(k v) (dump-configuration)])
                   (values k (v))))))

(define (config-command)
  (act-on-next-argument "config"
                        show-help
                        ["set" set-config]
                        ["get" get-config]
                        ["dump" dump-config]))
