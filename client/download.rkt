#lang racket/base

(provide download-command)

(require racket/cmdline
         racket/match
         net/url
         "../download.rkt")


(define HELP-FMT #<<EOF
Usage: raco zcpkg download <subcommand> [args] ...

Downloads files to local cache.

  download file:   Download and verify tgz archive from URL
  download github: Download archive from GitHub
  download clear:  Clear download cache

EOF
)

(define (show-help)
  (displayln HELP-FMT))

(define (unrecognized-command action)
  (printf "Unrecognized command: ~a~n~n" action)
  (show-help)
  (exit 1))

(define (download-command)
  (parse-command-line "zcpkg" (current-command-line-arguments)
                      '()
                      (λ (flags action . args)
                        (parameterize ([current-command-line-arguments (list->vector args)])
                          ((match action
                             ["file"   download-file-command]
                             ["github" download-github-archive]
                             ["clear"  clear-download-cache!]
                             [_ (λ () (unrecognized-command action))]))))
                      '("subcommand" "args")
                      void))

(define (download-file-command)
  (command-line #:args (str)
                (displayln (download-archive str))))

(define (download-github-archive)
  (command-line
   #:args (un+repo [ref "master"])
   (define dest
     (download-file
      (string->url
       (format "https://github.com/~a/archive/~a.zip" un+repo ref))))
   (printf "Downloaded ZIP to ~a~n" dest)))
