#lang racket/base

; Define high-level interface for cryptographic hash functions.
;
; Explicitly distinguish CHF names and implementations to allow a more
; flexibile trust profile.

(require racket/contract)

(provide
 (struct-out $chf-unavailable)
 (contract-out
  [chf-bind-trust (-> chf-trust/c predicate/c)]
  [chf-alias/c flat-contract?]
  [chf-fold-trust
   (-> (-> symbol? chf-implementation/c)
       (listof symbol?)
       chf-trust/c)]
  [chf-implementation/c chaperone-contract?]
  [chf-names/c flat-contract?]
  [chf-trust/c chaperone-contract?]
  [current-chfs (parameter/c chf-trust/c)]
  [get-default-chf (->* () (chf-trust/c) (or/c #f symbol?))]
  [make-digest
   (->* ((or/c input-port? path? bytes?))
        ((or/c #f symbol?)
         #:expect (or/c #f bytes?))
        bytes?)]
  [find-chf-canonical-name
   (->* (symbol?) (chf-trust/c) (or/c #f symbol?))]
  [MAX_EXPECTED_DIGEST_LENGTH exact-positive-integer?]))

(require (only-in "../message.rkt" define-message))

(define-message $chf-unavailable (name))

(define MAX_EXPECTED_DIGEST_LENGTH 64) ; SHA-512


; A CHF may have many aliases, and they are sometimes confusing when
; they resemble CHF _suite_ names.  Allow the user to define patterns
; for comparison.
(define chf-alias/c
  (rename-contract (or/c symbol? string? regexp? byte-regexp?)
                   'chf-alias/c))

; If we add a symbolic canonical name to the aliases, we can create a
; better-defined relationship with other systems.
(define chf-names/c
  (rename-contract (cons/c symbol? (listof chf-alias/c))
                   'chf-names/c))


; This definition implies that an implementation can trivially
; short-circuit an integrity check by returning the second argument.
; This seems scary, but these procedures can only be installed by
; privileged code, and it can be helpful to know an expected digest
; when accounting for collidable inputs across multiple
; implementations of the same CHF.
(define chf-implementation/c
  (-> input-port?      ; Where content comes from
      (or/c #f bytes?) ; Expected digest (in context of integrity check)
      bytes?))         ; Output digest


(define chf-profile/c
  (list/c chf-names/c chf-implementation/c))


(define chf-trust/c
  (listof chf-profile/c))


(define current-chfs
  (make-parameter null))


(define ((chf-bind-trust [table (current-chfs)]) sym)
  (and (symbol? sym)
       (find-chf-implementation table sym)
       #t))



(define (chf-fold-trust f trusted-chf-names)
  (for/fold ([wip null] #:result (reverse wip))
            ([name trusted-chf-names])
    (cons (list (list name
                      (pregexp (format "^(?i:~a)$" name)))
                (f name))
          wip)))


(define (get-default-chf [table (current-chfs)])
  (and (not (null? table))
       (caaar table)))


(define (chf-match? needle haystack [needle-string (format "~a" needle)])
  (if (null? haystack)
      #f
      (let ([next (car haystack)]
            [try (λ (test)
                   (or test
                       (chf-match? needle
                                   (cdr haystack)
                                   needle-string)))])
        (cond [(symbol? next)
               (try (eq? needle next))]
              [(string? next)
               (try (string=? needle-string next))]
              [(or (regexp? next) (byte-regexp? next))
               (try (regexp-match? next needle-string))]))))



(define (find-chf-implementation [table (current-chfs)] [sym (get-default-chf table)])
  (if (null? table)
      #f
      (let* ([chf-profile (car table)]
             [chf-names (car chf-profile)]
             [chf-implementation (cadr chf-profile)])
        (if (chf-match? sym chf-names)
            chf-implementation
            (find-chf-implementation (cdr table) sym)))))


(define (find-chf-canonical-name sym [table (current-chfs)])
  (if (null? table)
      #f
      (let* ([chf-profile (car table)]
             [chf-names (car chf-profile)])
        (if (chf-match? sym chf-names)
            (car chf-names)
            (find-chf-canonical-name sym (cdr table))))))


(define (make-digest #:expect [expect #f] variant [algorithm (get-default-chf)])
  (cond [(not algorithm)
         (raise ($chf-unavailable algorithm))]
        [(bytes? variant)
         (make-digest (open-input-bytes variant) algorithm)]
        [(path-string? variant)
         (call-with-input-file*
           variant
           (λ (from-file)
             (make-digest from-file algorithm)))]
        [else
         (let ([f (find-chf-implementation (current-chfs) algorithm)])
           (if f
               (f variant expect)
               (raise ($chf-unavailable algorithm))))]))


(define (chf-correct-digest-length? digest chf)
  (equal? (bytes-length digest)
          (bytes-length (make-digest #"_" chf))))


(module+ test
  (provide
   (contract-out
    [test-digest-data bytes?]
    [test-digests (non-empty-listof (cons/c symbol? string?))]))

  (require rackunit)

  (define test-digest-data #"the rain in spain falls mainly on the plain\n")

  (define test-digests
    '((sha3-384 . "C5cwjJB4SzrDF0DDwfxIQeTwulzTMGSpKSZmW1BSzEs4GLQJy5emOmigwBAO5DUY")
      (sha256 . "2AQkOghwLerAkbcbvdWrtocTeBXV5UnbPA56MsyClqk=")
      (sha1 . "89mymPTvJIenhzZpwZ7uDKUANd8=")
      (md5 . "K0/4xUnpQ4mZjePs8CJjfw==")
      (sha2-224 . "J6HLdwlQa7vmdBXI+0rIbmk0Q0phlLYoHUO7Fw==")
      (sha2-384 . "V5oG04vbEMtqMkN7yHQk3Rw53O9PE3P7UrN7Fr5NV2hnRQkfotR06+D/XWh58dVK")
      (sha3-224 . "4UEGL2680oczBfoD+4SoH3k82OeevWq8b7seDw==")
      (sha3-256 . "dHGOrfdKJB1Agz8mMafKamWzJg7Cu1Cg1G3XM3vWFJc=")
      (sha3-384 . "C5cwjJB4SzrDF0DDwfxIQeTwulzTMGSpKSZmW1BSzEs4GLQJy5emOmigwBAO5DUY")
      (sha3-512 . "UQqbIueWYFqdA9ibHPS2O0l+gX9DPySh4KPIv97XDBHPlPZB5MMzN4mXamWIuTLxLaZO8r2euZx0tfoksyevaQ==")))
  
  (define empty-aliases '(#rx#"" #px#"" #px"" #rx"" "" ||))

  (for ([alias (in-list empty-aliases)])
    (check-pred chf-alias/c alias))

  (check-pred chf-names/c '(sym))
  (check-pred chf-names/c '(sym . ,empty-aliases))

  (check-false (chf-match? '|| null))
  (check-true (chf-match? 'a '(a))) 
  (check-true (chf-match? "b" '(a "b")))

  (check-false ((chf-bind-trust) 'anything))

  (let ([check* (λ (v) (check-true (chf-match? v '(sha1 #px"^(?i:sha-?1)$"))))])
    (check* 'sha1)
    (check* 'sha-1)
    (check* 'SHA1)
    (check* 'SHA-1)
    (check* 'Sha1))

  (check-equal?
   (chf-fold-trust (λ (s) s) '(a b c))
   `(([a #px"^(?i:a)$"] a)
     ([b #px"^(?i:b)$"] b)
     ([c #px"^(?i:c)$"] c)))

  (parameterize ([current-chfs `(([sha1 #px"s1"] ,(λ _ #"")))])
    (let ([trust? (chf-bind-trust (current-chfs))])
      (check-true (trust? 'sha1))
      (check-true (trust? 's1))
      (check-true (trust? 'as1b))
      (check-false (trust? 's))
      (check-false (trust? '||))
      (check-false (trust? 'md5)))
    (check-eq? (get-default-chf (current-chfs)) 'sha1)
    (check-eq? (get-default-chf) 'sha1)
    (check-eq? (find-chf-canonical-name 's1) 'sha1)
    (check-pred procedure? (find-chf-implementation))
    (check-pred procedure? (find-chf-implementation (current-chfs) 's1))
    (check-pred procedure? (find-chf-implementation (current-chfs) 'sha1))))
