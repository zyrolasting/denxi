#lang racket/base

(provide capture-workspace
         restore-workspace)

(require racket/match
         racket/path
         racket/port
         "file.rkt"
         "message.rkt"
         "team.rkt"
         "verify.rkt"
         "config.rkt"
         "workspace.rkt")

(define (capture-workspace)
  (call-with-output-file "capture.rktd"
    (λ (o)
      (parameterize ([current-directory (workspace-directory)])
        (write-to-file (make-capture) o)))))

(define (restore-workspace path)
  (rename-file-or-directory (workspace-directory)
                            (path-replace-extension (workspace-directory) ".bak"))
  (call-with-temporary-directory
   (λ (tmp-dir)
     (parameterize ([workspace-directory tmp-dir])
       (reproduce-workspace (file->value path))
       (copy-directory/files tmp-dir (workspace-directory))))))


(struct capture-entry (path digest reproduction) #:prefab)
(struct fs-entry (id kind make-digest) #:transparent)


(define (capture-directory? d)
  (member d (map build-path (list "etc" (ZCPKG_INSTALL_RELATIVE_PATH)))))

(define (make-fstab)
  (for/hash ([p (in-directory)])
    (define kind
      (cond [(link-exists? p) 'link]
            [(directory-exists? p) 'directory]
            [(file-exists? p) 'file]))
    (values p
            (fs-entry (file-or-directory-identity p (eq? kind 'link))
                      kind
                      (λ () (and (eq? kind 'file)
                                 (let-values ([(exit-code dig) (make-digest p)])
                                   dig)))))))

(define (make-capture)
  (define fstab (make-fstab))
  (append (capture-rcfiles fstab)
          (capture-links fstab)
          (capture-commands fstab)))

(define (capture-rcfiles fstab)
  (parameterize ([current-directory (workspace-directory)])
    (for/hash ([p (in-directory "etc")])
      (define entry (hash-ref fstab p))
      (capture-entry (path->string p)
                     #f
                     (file->bytes p)))))

(define (capture-links fstab)
  (define-values (by-id links-only)
    (for/fold ([lookup (hash)]
               [links null])
              ([(path entry) (in-hash fstab)])
      (values (hash-set lookup (fs-entry-id entry) path)
              (if (eq? (fs-entry-kind entry) 'link)
                  (cons path links)
                  links))))

  (for/list ([link-path (in-list links-only)])
    (capture-entry (path->string link-path)
                   #f
                   (hash-ref by-id (file-or-directory-identity link-path)))))

(define (capture-commands fstab)
  ; TODO: Seek out info.rkt files with source information.
  (list))

(define (restore-file entry)
  (match-define (capture-entry path _ repro) entry)
  (make-directory* (path-only path))
  (call-with-output-file path
    (λ (out)
      (copy-port (open-input-bytes repro) out))))

(define (restore-link entry)
  (match-define (capture-entry path _ repro) entry)
  (make-directory* (path-only path))
  (make-file-or-directory-link repro path))

(define (reproduce-workspace entries [commands null])
  (if (null? entries)
      (process-jobs commands)
      (reproduce-workspace
       (cdr entries)
       (let* ([entry (car entries)]
              [repro (capture-entry-reproduction entry)])
         (cond [(bytes? repro)
                (restore-file entry)]
               [(string? repro)
                (restore-link entry)]
               [(vector? repro)
                (cons repro commands)])))))
