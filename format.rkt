#lang racket/base


(provide (all-defined-out)
         (all-from-out racket/format))

(require racket/format
         "dependency.rkt"
         "string.rkt"
         "zcpkg-info.rkt")

(define (~a* . args)
  (apply ~a (map (λ (s) (~a s "\n")) args)))

(define (indent-lines lines)
  (map (λ (s) (~a "  " s)) lines))

(define (join-lines lines)
  (string-join lines "\n"))

(define (print-zcpkg-info-table unsorted-infos)
  (define infos
    (sort unsorted-infos
          #:key (λ (info) (dependency->string (zcpkg-info->dependency info)))
          string<?))

  (define (get-cell-printer strs)
    (define min-width (apply max (map string-length strs)))
    (λ args (apply ~a #:min-width min-width args)))


  (define print-provider-name (get-cell-printer (map zcpkg-info-provider-name infos)))
  (define print-package-name  (get-cell-printer (map zcpkg-info-package-name infos)))
  (define print-edition-name  (get-cell-printer (map zcpkg-info-edition-name infos)))
  (define print-revision-num  (get-cell-printer (map (compose ~a zcpkg-info-revision-number) infos)))

  (define row-fmt "~a\t~a\t~a\t~a")
  (printf (~a (format row-fmt
                      (print-package-name "Package")
                      (print-provider-name "Provider")
                      (print-edition-name "Edition")
                      (print-revision-num "Revision"))
              "~n~a~n~n")
          (string-join
           (for/list ([info (in-list infos)])
             (format row-fmt
                     (print-package-name (zcpkg-info-package-name info))
                     (print-provider-name (zcpkg-info-provider-name info))
                     (print-edition-name (zcpkg-info-edition-name info))
                     (print-revision-num (zcpkg-info-revision-number info))))
           "\n")))
