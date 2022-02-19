#lang racket/base

(require racket/contract)
(provide (except-out (struct-out file-sink)
                     file-sink)
         (struct-out file-source)
         (contract-out
          (rename make-file-sink file-sink
                  (->* (file-name?
                        transfer-policy?)
                       (complete-path?
                        #:exists symbol?
                        #:permissions (integer-in 0 65535))
                       file-sink?))))

(require racket/file
         racket/match
         racket/path
         "io.rkt"
         "machine.rkt"
         "message.rkt"
         "port.rkt")

(define-message $file-open (path))


(define (file-name? path)
  (and (path-string? path)
       (let ([exploded (explode-path path)])
         (and (= (length exploded) 1)
              (not (member (format "~a" path)
                           '(".." ".")))))))
         

(define (make-file-sink file-name
                        policy
                        [directory-path (current-directory)]
                        #:exists [exists 'error]
                        #:permissions [permissions #o666])
  (file-sink directory-path
             file-name
             policy
             exists
             permissions
             #f))


(struct file-source (path)
  #:methods gen:source
  [(define (source-tap source)
     (machine-rule (open-input-file (file-source-path source))))
   (define (source-measure source)
     (machine-rule (file-size (file-source-path source))))])


(struct file-sink (directory-path file-name policy exists permissions [port #:mutable])
  #:methods gen:source
  [(define (sink-source sink)
     (machine
      (λ (state)
        (if (opened-output-port? (file-sink-port sink))
            (state-set-value state (file-source (file-sink-path sink)))
            (state-halt-with state ($file-open (file-sink-path sink)))))))
   (define (sink-open sink)
     (machine
      (λ (state)
        (match-define (file-sink directory-path _ _ exists permissions _) sink)
        (make-directory* directory-path)
        (define out
          (open-output-file (file-sink-path sink)
                            #:exists exists
                            #:permissions permissions))
        (set-file-sink-port! sink out)
        (state-set-value state out))))
   (define (sink-close sink)
     (machine-effect
      (let ([out (file-sink-port sink)])
        (when (opened-output-port? out)
          (flush-output out)
          (close-output-port out)
          (set-file-sink-port! sink #f)))))])


(define (opened-output-port? out)
  (and (output-port? out)
       (not (port-closed? out))))


(define (file-sink-path sink)
  (simple-form-path
   (build-path (file-sink-directory-path sink)
               (file-sink-file-name sink))))
