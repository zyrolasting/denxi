#lang racket/base

(struct exn:fail:xiden exn:fail ())

(struct exn:fail:xiden:unreadable-setting exn:fail:xiden ())
(struct exn:fail:xiden:invalid-revision-interval exn:fail:xiden ())
(struct exn:fail:xiden:invalid-setting-value exn:fail:xiden ())
(struct exn:fail:xiden:no-viable-sources exn:fail:xiden ())
