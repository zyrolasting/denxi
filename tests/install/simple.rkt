#lang racket/base

(require "../workspace.rkt"
         racket/file
         zcpkg/operations
         zcpkg/zcpkg-info
         rackunit
         racket/runtime-path)

(define-runtime-path ./no-deps "./no-deps")

(module+ test
  (test-case "Can work on zero-dependency packages"
    (with-new-workspace
      (copy-directory/files ./no-deps "no-deps")
      (install "./no-deps")
      (define info (getinfo/zcpkg-info "./no-deps"))
      (define location (zcpkg-info->install-path info))
      (test-true "Install package"
                 (directory-exists? location))
      (uninstall location)
      (test-false "Uninstalled package"
                  (directory-exists? location)))))
