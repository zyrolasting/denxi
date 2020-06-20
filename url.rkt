#lang racket/base


(require racket/contract
         net/url)

(provide (all-from-out net/url)
         (contract-out
          [find-directory-path (-> url? (or/c #f path?))]
          [find-catalog-name (-> url? (or/c #f ))]))

(define (find-directory-path u)
  (with-handlers ([exn? (const #f)])
    (define scheme (url-scheme u))
    (and (or (equal? scheme "file")
             (not scheme))
         (indicates-fs-path? u)
         (simplify-path
          (apply build-path
                 (if (url-path-absolute? u)
                     (car (filesystem-root-list))
                     (current-directory))
                 (map path/param-path (url-path u)))))))

(define (find-catalog-name u)
  (with-handlers ([exn? (const #f)])
    (and (not (or (url-scheme u)
                  (indicates-fs-path? u)))
         (apply make-artifact-name (map path/param-path (url-path u))))))

(define (indicates-fs-path? u)
  (define leading (get-leading-path-element u))
  (or (eq? 'up leading)
      (eq? 'same leading)
      (equal? "" leading) ; For UNC path (Windows)
      (url-path-absolute? u)))

(define (get-leading-path-element u)
  (define path/params (url-path u))
  (if (null? path/params)
      (raise-argument-error 'get-leading-path-element "URL with a path" u)
      (path/param-path (car path/params))))
