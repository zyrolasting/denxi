#lang racket/base

(provide (struct-out artifact-info)
         read-artifact-info
         write-artifact-info)

(require "metadata.rkt"
         "zcpkg-info.rkt")

(struct artifact-info zcpkg-info (integrity signature ctime) #:transparent)

(declare-info-i/o artifact-info)
