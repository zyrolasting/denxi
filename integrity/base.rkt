#lang racket/base

; Define integrity checking primitives

(require racket/contract)

(provide
 (struct-out $chf-unavailable)
 (struct-out chf)
 (struct-out integrity)
 (contract-out
  [check-integrity
   (-> #:trust-bad-digest any/c
       (-> symbol? any/c)
       symbol?
       bytes?
       bytes?
       symbol?)]
  [chf-bind-trust
   (-> (listof chf?) predicate/c)]
  [chf-find
   (->* ((listof chf?))
        (symbol?)
        (or/c #f chf?))]
  [chf-fold-trust
   (-> (-> symbol? chf-impl/c)
       (listof symbol?)
       (listof chf?))]
  [chf-impl/c
   chaperone-contract?]
  [current-chfs
   (parameter/c (listof chf?))]
  [get-default-chf
   (->* () ((listof chf?)) (or/c #f symbol?))]
  [make-digest
   (->* ((or/c input-port? path? bytes?))
        ((or/c #f symbol?))
        bytes?)]
  [raw-integrity?
   flat-contract?]
  [MAX_EXPECTED_DIGEST_LENGTH exact-positive-integer?]))


;--------------------------------------------------------------------------------
; Implementation

(require racket/match
         "../message.rkt"
         "../string.rkt")


; A claim of data integrity without a verifying implementation
(struct integrity (chf-symbol digest)
  #:transparent)

(define raw-integrity?
  (struct/c integrity symbol? bytes?))

(define-message $chf-unavailable
  (name))


; Assuming SHA-512
(define MAX_EXPECTED_DIGEST_LENGTH 64)


; Return explicit outcomes for transparency.
(define (check-integrity #:trust-bad-digest trust-bad-digest
                         trust-chf?
                         chf
                         digest
                         other-digest)
  (if trust-bad-digest
      'skip
      (if (trust-chf? chf)
          (if (equal? digest other-digest)
              'pass
              'fail)
          'curb)))


(struct chf (canonical-name alias-pattern implementation)
  #:property prop:procedure
  (λ (self in [expected #f])
    ((chf-implementation self) in expected))
  #:methods gen:equal+hash
  [(define (equal-proc a b ?)
     (eq? (chf-canonical-name a)
          (chf-canonical-name b)))
   (define (hash-proc c ?)
     (string-hash-code (symbol->string (chf-canonical-name c))))
   (define (hash2-proc c ?)
     (string-secondary-hash-code (symbol->string (chf-canonical-name c))))])


(define chf-impl/c
  (-> input-port? bytes?))


(define current-chfs
  (make-parameter null))


; For checking if a user trusts a CHF by name alone.
(define ((chf-bind-trust [haystack (current-chfs)]) needle)
  (and (symbol? needle)
       (chf-find haystack needle)
       #t))


; For building a new (current-chfs) value using only names and an
; implementation lookup procedure.
(define (chf-fold-trust select-implementation trusted-chf-names)
  (map (λ (name)
         (chf name
              (pregexp (format "^(?i:~a)$" name))
              (select-implementation name)))
       trusted-chf-names))


; Convention: First element is the default, because I cannot make a
; forward-compatible claim for a reasonable default CHF.
(define (get-default-chf [trusted (current-chfs)])
  (and (not (null? trusted))
       (chf-canonical-name (car trusted))))


(define (chf-find [haystack (current-chfs)] [needle (get-default-chf haystack)])
  (if (null? haystack)
      #f
      (match-let* ([maybe-needle (car haystack)]
                   [(chf cname alias impl) maybe-needle])
        (if (or (eq? needle cname)
                (and alias
                     (regexp-match? alias (symbol->string needle))))
            maybe-needle
            (chf-find (cdr haystack) needle)))))


; `expect` is an expected digest in an integrity check. This allows a
; user to trivially pass the checks, but also a way to compare
; different implementations of the same algorithm.
(define (make-digest variant [chf (get-default-chf)])
  (cond [(not chf)
         (raise ($chf-unavailable chf))]
        [(bytes? variant)
         (make-digest (open-input-bytes variant) chf)]
        [(path-string? variant)
         (call-with-input-file*
           variant
           (λ (from-file)
             (make-digest from-file chf)))]
        [else
         (let ([chf-instance (chf-find (current-chfs) chf)])
           (if chf-instance
               (chf-instance variant)
               (raise ($chf-unavailable chf))))]))


(module+ test
  (provide
   (contract-out
    [test-digest-data bytes?]
    [test-digests (non-empty-listof (cons/c symbol? string?))]))

  (require rackunit)

  (check-eq? (check-integrity #:trust-bad-digest #t (λ _ #f) '_ #"x" #"x") 'skip)
  (check-eq? (check-integrity #:trust-bad-digest #f (λ _ #f) '_ #"x" #"x") 'curb)
  (check-eq? (check-integrity #:trust-bad-digest #f (λ _ #t) '_ #"x" #"y") 'fail)
  (check-eq? (check-integrity #:trust-bad-digest #f (λ _ #t) '_ #"x" #"x") 'pass)

  (check-true  (raw-integrity? (integrity '|| #"")))
  (check-false (raw-integrity? (integrity #f #"")))
  (check-false (raw-integrity? (integrity '|| #f)))
  (check-false (raw-integrity? (integrity #f #f)))

  ; For other tests
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

  (check-false ((chf-bind-trust) 'anything))

  (let ([check*
         (λ (v) (check-pred chf?
                            (chf-find
                             (list (chf 'sha1
                                        #px"^(?i:sha-?1)$"
                                        values)))))])
    (check* 'sha1)
    (check* 'sha-1)
    (check* 'SHA1)
    (check* 'SHA-1)
    (check* 'Sha1))

  (check-equal?
   (chf-fold-trust (λ (s) s) '(a b c))
   (list (chf 'a #px"^(?i:a)$" values)
         (chf 'b #px"^(?i:b)$" values)
         (chf 'c #px"^(?i:c)$" values)))

  (parameterize ([current-chfs (list (chf 'sha1 #px"s1" (λ _ #"")))])
    (let ([trust? (chf-bind-trust (current-chfs))])
      (check-true (trust? 'sha1))
      (check-true (trust? 's1))
      (check-true (trust? 'as1b))
      (check-false (trust? 's))
      (check-false (trust? '||))
      (check-false (trust? 'md5)))
    (check-eq? (get-default-chf (current-chfs)) 'sha1)
    (check-eq? (get-default-chf) 'sha1)))
