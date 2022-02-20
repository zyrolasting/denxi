#lang racket/base

(require racket/contract)
(provide (struct-out file-source)
         (struct-out file-sink)
         (contract-out
          [make-file-sink
           (->* (file-name?
                 transfer-policy?)
                (complete-path?
                 #:exists symbol?
                 #:permissions (integer-in 0 65535))
                file-sink?)]))

(require racket/file
         racket/match
         racket/path
         "io.rkt"
         "machine.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt")


(define (file-name? path)
  (and (path-string? path)
       (let ([exploded (explode-path path)])
         (and (= (length exploded) 1)
              (not (member (format "~a" path)
                           '(".." ".")))))))
         

(define (make-file-sink file-name
                        policy
                        [directory-path (current-directory)]
                        #:handle-directory [handle-directory void]
                        #:exists [exists 'error]
                        #:permissions [permissions #o666])
  (file-sink directory-path
             file-name
             policy
             handle-directory
             exists
             permissions))


(struct file-source (path)
  #:methods gen:source
  [(define (source-tap source)
     (machine-rule (open-input-file (file-source-path source))))
   (define (source-measure source)
     (machine-rule (file-size (file-source-path source))))])


(struct file-sink (directory-path
                   file-name
                   policy
                   handle-directory
                   exists
                   permissions)
  #:methods gen:sink
  [(define (sink-drain sink source)
     (mdo policy := (machine-rule (file-sink-policy sink))
          est-size    := (source-measure source)
          from-source := (source-tap source)
          (machine
           (λ (state)
             (match-define (file-sink directory-path
                                      file-name
                                      handle-directory
                                      exists
                                      permissions
                                      _)
               sink)

             (define output-path
               (simple-form-path
                (build-path directory-path file-name)))

             (handle-directory directory-path)
             (call-with-output-file* output-path
               #:exists exists
               #:permissions permissions
               (λ (to-file)
                 (transfer from-source to-file est-size policy)
                 (close-input-port from-source)
                 output-path))))))])
