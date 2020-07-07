#lang racket/base

(provide new-command
         make-skeleton-package)

(require racket/cmdline)


(define (write-info.rkt package-name)
  (displayln "#lang info\n")
  (writeln `(define provider-name "you"))
  (writeln `(define package-name ,package-name))
  (writeln `(define edition-name  "draft"))
  (writeln `(define revision-number 0))
  (displayln "(define revision-names '(\"initial\"))")
  (writeln `(define installer "installer.rkt"))
  (displayln "(define dependencies '())"))


(define (write-README.md package-name)
  (printf #<<EOF
# ~a

EOF
package-name))


(define (write-installer.rkt package-name)
  (displayln "#lang racket/base\n")
  (displayln "(define (set-up!)\n  (void))\n")
  (displayln "(define (tear-down!)\n  (void))"))


(define (make-skeleton-package name)
  (make-directory name)
  (define (make-file file-name proc)
    (call-with-output-file (build-path name file-name)
      (λ (o) (parameterize ([current-output-port o])
               (proc name)))))
  (make-file "info.rkt" write-info.rkt)
  (make-file "README.md" write-README.md)
  (make-file "installer.rkt" write-installer.rkt))


(define (new-command)
  (command-line
   #:program "new"
   #:args (name)
   (make-skeleton-package name)))
