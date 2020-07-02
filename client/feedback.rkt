#lang racket/base

(provide feedback-command)

(require racket/cmdline
         racket/date
         racket/file
         racket/format
         racket/port
         net/url
         "../service/feedback.rkt"
         "../service/server.rkt")

(define (send-feedback in)
  (post-pure-port feedback-endpoint
                  (string->bytes/utf-8 (read-string FEEDBACK-CHAR-LIMIT in))
                  '("Content-Type: text/plain; charset=utf-8")))

(define POLICY (format #<<EOF

/!\ ONLY USE THE FEEDBACK COMMAND IF YOU CONSENT TO THESE TERMS. /!\

When you send feedback, it's submitted as clear text to a server.
While the connection is encrypted, you should not send any private
information.

To prevent spam, the service only accepts ~a characters per
submission, and limits each IP to one submission every twenty
minutes. The component of the service that collects feedback will
remember your IP for that long to enforce this.

EOF
FEEDBACK-CHAR-LIMIT))

(define (feedback-command)
  (define src (current-input-port))
  (command-line
   #:usage-help
   "Send feedback to the developer. READ --policy FIRST."
   #:once-any
   [("-?" "--policy")
    "Show rules on how feedback is accepted"
    (set! src #f)]
   [("-i" "--stdin")
    "Read feedback from STDIN (default)"
    (set! src (current-input-port))]
   [("-f" "--file")
    feedback-file
    "Read feedback from file"
    (set! src (open-input-file feedback-file))]
   [("-s" "--string")
    feedback-str
    "Read feedback from string"
    (set! src (open-input-string feedback-str))]
   #:args ()
   (cond
     [(input-port? src)
      (copy-port (send-feedback src) (current-output-port))
      (close-input-port src)]
     [else
      (displayln POLICY)])))
