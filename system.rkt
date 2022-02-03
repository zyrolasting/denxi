#lang racket/base


(require racket/contract
         racket/generic
         racket/port
         racket/system
         syntax/parse
         "machine.rkt"
         "message.rkt")


(provide (all-from-out racket/system)
         (struct-out $subprocess)
         (struct-out $subprocess:report)
         (struct-out $subprocess:command-not-found)
         (contract-out
          [run (->* (path-string?)
                    (#:expected-exit-codes (listof (integer-in 0 255))
                     #:fail-on-stderr? any/c
                     #:cwd path-string?
                     #:controller any/c
                     #:timeout exact-positive-integer?
                     #:stdin (or/c #f (and/c output-port? file-stream-port?)))
                    #:rest (listof path-string?)
                    machine?)]))


(define-generics subprocess-controller
  [get-status subprocess-controller handle]
  [find-exe subprocess-controller cmd]
  [start subprocess-controller stdout stdin stderr group cmd args]
  [stop subprocess-controller handle])


(define-message $subprocess ())
(define-message $subprocess:report $subprocess (cmd args wd max-runtime actual-runtime expected-exit-codes actual-exit-code stderr?))
(define-message $subprocess:command-not-found $subprocess (cmd))


(struct subprocess-controller/production ()
  #:methods gen:subprocess-controller
  [(define (get-status s handle)
     (subprocess-status handle))

   (define (find-exe s cmd)
     (if (file-exists? cmd)
         cmd
         (find-executable-path cmd)))

   (define (stop s handle)
     (subprocess-kill handle)
     (or (sync/timeout 3 handle)
         (subprocess-kill handle #t)))

   (define (start s stdout stdin stderr group cmd args)
     (apply subprocess stdout stdin stderr group cmd args))])


(struct subprocess-controller/mock (runtime code proc)
  #:methods gen:subprocess-controller
  [(define (get-status s handle)
     (if (thread-running? handle)
         'running
         (subprocess-controller/mock-code s)))

   (define (find-exe s cmd)
     cmd)

   (define (stop s handle)
     (kill-thread handle))

   (define (start s stdout stdin stderr group cmd args)
     (define-values (stdout< stdin> stderr<)
       ((subprocess-controller/mock-proc s) stdout stdin stderr group cmd args))

     (values (thread (λ () (sync (alarm-evt (+ (current-inexact-milliseconds)
                                               (subprocess-controller/mock-runtime s))))))
             stdout<
             stdin>
             stderr<))])


(define (run #:expected-exit-codes [expected-exit-codes '(0)]
             #:timeout [timeout (* 60 60)]
             #:stdin [user-stdin #f]
             #:cwd [wd (current-directory)]
             #:fail-on-stderr? [fail-on-stderr? #t]
             #:controller [controller (subprocess-controller/production)]
             cmd . args)
  (machine
   (λ (state)
     (let/cc return
       (define cmd-actual (find-exe controller cmd))

       (unless cmd-actual
         (return (state-halt-with state ($subprocess:command-not-found cmd))))

       (define-values (handle stdout stdin stderr)
         (parameterize ([current-directory wd])
           (start controller (current-output-port) (current-input-port) #f #f cmd-actual args)))

       (when user-stdin
         (copy-port user-stdin stdin)
         (flush-output stdin))

       (define stderr? #f)

       (define stderr-pump
         (thread
          (λ ()
            (unless (eof-object? (peek-byte stderr))
              (set! stderr? #t)
              (copy-port stderr (current-error-port))))))

       (define start-s (current-seconds))

       (define runtime
         (if (sync/timeout timeout handle)
             (- (current-seconds) start-s)
             (begin (stop controller handle)
                    timeout)))

       (thread-wait stderr-pump)

       (define exit-code (get-status controller handle))
       (define report ($subprocess:report cmd-actual args wd timeout runtime expected-exit-codes exit-code stderr?))
       (define state* (state-add-message state report))

       (state-set-value state*
                        (if (and (or (null? expected-exit-codes)
                                     (list? (member exit-code expected-exit-codes)))
                                 (or (not fail-on-stderr?)
                                     (not stderr?))
                                 (< runtime timeout))
                            (void)
                            halt))))))


(module+ test
  (require rackunit)

  (define from-nothing (open-input-bytes #""))
  (define to-nowhere (open-output-nowhere))

  (define-syntax-rule (test-subprocess msg #:should-fail? should-fail? m expected-report-pattern)
    (test-case msg
      (define state
        (parameterize ([current-output-port to-nowhere]
                       [current-error-port to-nowhere])
          (m)))
      (define value (car state))
      (define messages (cdr state))
      (if should-fail?
          (check-equal? value halt)
          (check-pred void? value))
      (check-match (car messages) expected-report-pattern)))

  (define (noop . _)
    (values from-nothing to-nowhere from-nothing))

  (define (include-stderr . _)
    (values from-nothing to-nowhere (open-input-bytes #"uh oh")))

  (define (exited-quickly? v)
    (< v 1))

  (test-subprocess "Run subprocess without error in default case"
                   #:should-fail? #f
                   (run #:controller (subprocess-controller/mock 0 0 noop) "abc" "1" "2" "3")
                   ($subprocess:report "abc" '("1" "2" "3") _ 3600 (? exited-quickly? _) '(0) 0 #f))

  (test-subprocess "Detect unexpected exit codes"
                   #:should-fail? #t
                   (run #:expected-exit-codes '(2)
                        #:controller (subprocess-controller/mock 0 3 noop)
                        "mismatch")
                   ($subprocess:report _ _ _ _ _ '(2) 3 _))

  (test-subprocess "Allow any exit code"
                   #:should-fail? #f
                   (run #:expected-exit-codes null
                        #:controller (subprocess-controller/mock 0 3 noop)
                        "mismatch")
                   ($subprocess:report _ _ _ _ _ '() 3 _))

  (test-subprocess "Stop runaway processes"
                   #:should-fail? #t
                   (run #:timeout 0.5
                        #:controller (subprocess-controller/mock 2000 0 noop)
                        "forever")
                   ($subprocess:report _ _ _ 0.5 0.5 _ _ _))

  (test-subprocess "Fail on non-empty STDERR"
                   #:should-fail? #t
                   (run #:timeout 3
                        #:fail-on-stderr? #t
                        #:controller (subprocess-controller/mock 0 1 include-stderr)
                        "stderr")
                   ($subprocess:report _ _ _ _ _ _ _ #t))

  (test-subprocess "Allow non-empty STDERR when asked"
                   #:should-fail? #f
                   (run #:timeout 3
                        #:fail-on-stderr? #f
                        #:controller (subprocess-controller/mock 0 0 include-stderr)
                        "stderr")
                   ($subprocess:report _ _ _ _ _ _ _ #t))

  (test-subprocess "Fail if command was not found"
                   #:should-fail? #t
                   (run #:controller (subprocess-controller/mock 0 0 noop) #f)
                   ($subprocess:command-not-found #f))
  
  ; This test case forwards user-defined STDIN to STDERR in a mock
  ; process.  That way, checking if STDERR is non-empty confirms that
  ; standard input was routed correctly.
  ;
  ; TODO: This test fails silently. Find out why.
  ;
  #;(define-values (i o) (make-pipe))
  #;(define pi (peeking-input-port i))
  #;(test-subprocess "Allow user-defined standard input"
                     #:should-fail? #f
                     (run #:fail-on-stderr? #f
                          #:stdin pi
                          #:controller
                          (subprocess-controller/mock 0 0
                                                      (λ (so si . _)
                                                        (check-eq? pi i)
                                                        (write-bytes #"abc" o)
                                                        (flush-output o)
                                                        (close-output-port o)
                                                        (values from-nothing
                                                                to-nowhere
                                                                i)))
                          "stdin")
                     ($subprocess:report _ _ _ _ _ _ _ #t)))
