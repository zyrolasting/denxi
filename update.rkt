#lang racket/base

; Pre-condition: package-name is installed, and zcp-source
; points to a different version of the same package

(require racket/contract)

(provide
 (contract-out
  [zcp-update (->i ([path   zcp-path/c]
                    [source zcp-source/c])
                   [r (path source)
                      (installed? source)])]))

(define (zcp-update package-name zcp-source)
  (uninstall package-name)
  (install zcp-source))

(define uninstall void)
(define install void)
