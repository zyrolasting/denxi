#lang racket/base

;----------------------------------------------------------------------
; Define a garbage collector for the filesystem.
;
; The GC:
;
; - Deletes every record of a link that does not actually exist on the filesystem.
; - Deletes every file/directory in the store referenced by no link.
;
; This means that whenever we change the database, the GC needs to do
; another pass in search of paths to delete.


(provide xiden-collect-garbage)

(require racket/stream
         "file.rkt"
         "localstate.rkt"
         "workspace.rkt")

(struct gc-state
  (needs-new-pass?
   paths-inbox
   delete-inbox
   bytes-recovered
   directories-deleted
   files-deleted
   links-deleted)
  #:transparent)

(define-syntax-rule (gc-update st fields ...)
  (struct-copy gc-state st fields ...))

(define (xiden-collect-garbage)
  (gc-pass (gc-state #f (gc-path-stream) empty-stream 0 0 0 0)))


(define (gc-pass state)
  (define paths (gc-state-paths-inbox state))
  (define no-paths-to-audit?  (stream-empty? paths))
  (define no-paths-to-delete? (stream-empty? (gc-state-delete-inbox state)))

  (cond [(and no-paths-to-audit?
              no-paths-to-delete?)
         (values (gc-state-bytes-recovered state)
                 (gc-state-directories-deleted state)
                 (gc-state-files-deleted state)
                 (gc-state-links-deleted state))]

        [no-paths-to-audit?
         (if (gc-state-needs-new-pass? state)
             (gc-pass (gc-update state
                                 [needs-new-pass? #f]
                                 [paths-inbox (gc-path-stream)]))
             (gc-collect state))]

        [else
         (gc-audit-path
          (gc-update state [paths-inbox (stream-rest paths)])
          (stream-first paths))]))


(define (gc-path-stream)
  (sequence->stream (in-list (directory-list (build-object-path)))))


;----------------------------------------------------------------------
; The following bindings build a list of paths to delete from the disk,
; and delete any link records that do not have an actual existing link
; on the filesystem.

(define (gc-audit-path state path)
  (let ([path-record-or-#f (find-exactly-one (path-record #f path #f))])
    (if path-record-or-#f
        (gc-check-links state
                        (in-path-links path-record-or-#f)
                        path-record-or-#f
                        0)
        (gc-pass (gc-update state
                            [delete-inbox
                             (stream-cons path (gc-state-delete-inbox state))])))))


; For a given path, check every recorded symlink to see if there is at least
; one symbolic link that actually refers to the given path. If it does, then
; we don't want to delete whatever is at the original path.
;
; If no links refer to the path, don't delete the original filesystem entry
; yet. Just delete the record of the path and set the GC to do another pass.
; It will notice the lack of a record in gc-audit-path.
(define (gc-check-links state links path-record-inst references-found)
  (if (stream-empty? links)
      (if (eq? references-found 0)
          (begin (delete-record path-record-inst)
                 (gc-pass (gc-update state
                                     [needs-new-pass? #t])))
          (gc-pass state))
      (gc-check-link state
                     (stream-first links)
                     (stream-rest links)
                     references-found
                     path-record-inst)))


; Checks a singular link record to see if it actually refers to the given path
(define (gc-check-link state link-record-inst links references-found path-record-inst)
  (define link-path-record (find-exactly-one (path-record (link-record-link-path-id link-record-inst) #f #f)))
  (if (link-references-path? path-record-inst link-path-record)
      (gc-check-links state
                      links
                      path-record-inst
                      (add1 references-found))
      (begin (delete-record link-record-inst)
             (delete-record link-path-record)
             (gc-check-links (gc-update state [needs-new-pass? #t])
                             links
                             path-record-inst
                             references-found))))


(define (valid-link? link-path-record-or-#f)
  (and (path-record? link-path-record-or-#f)
       (let ([link-path (path-record-path link-path-record-or-#f)])
         (link-exists?
          (if (complete-path? link-path)
              link-path
              (build-workspace-path link-path))))))


(define (link-references-path? path-record-inst link-path-record-or-#f)
  (and (valid-link? link-path-record-or-#f)
       (equal? (file-or-directory-identity (path-record-path link-path-record-or-#f))
               (file-or-directory-identity (path-record-path path-record-inst)))))


;----------------------------------------------------------------------
; The following bindings delete gathered paths from the disk.

(define (gc-collect-directory state path)
  (gc-collect
   (gc-update state
              [delete-inbox
               (stream-append (gc-state-delete-inbox state)
                              (sequence->stream (in-list (directory-list path #:build? #t))))]
              [directories-deleted
               (add1 (gc-state-directories-deleted state))])))

(define (gc-collect-file state path)
  (define bytes-recovered* (+ (file-size path) (gc-state-bytes-recovered state)))
  (delete-file path)
  (gc-collect
   (gc-update state
    [bytes-recovered bytes-recovered*]
    [files-deleted (add1 (gc-state-files-deleted state))])))

; Assume links have negligible size. People don't run cleanup operations to know that they saved 4-20 bytes.
(define (gc-collect-link state path)
  (delete-file path)
  (gc-collect
   (gc-update state
    [needs-new-pass? #t] ; Because deleting a link may make new files eligible for collection
    [links-deleted (add1 (gc-state-links-deleted state))])))


(define (gc-collect-path state path)
  (apply (cond [(directory-exists? path)
                gc-collect-directory]
               [(link-exists? path)
                gc-collect-link]
               [(file-exists? path)
                gc-collect-file]
               [else (λ () (gc-collect-path state))])
         state path))


(define (gc-collect state)
  (if (stream-empty? (gc-state-delete-inbox state))
      (gc-pass state)
      (gc-collect-path (gc-update state
                        [delete-inbox (stream-rest (gc-state-delete-inbox state))])
                       (stream-first (gc-state-delete-inbox state)))))
