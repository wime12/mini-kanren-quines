(include "~~lib/_gambit#.scm")

(define (make-read-sharp-single-char res)
  (lambda (re next start-pos)
    (macro-read-char (macro-readenv-port re))
    (let ((rstr (##build-delimited-string re #\space 0)))
      (if (string=? rstr "")
        res
	(##raise-datum-parsing-exception 'invalid-token re)))))

(define (mk-enable-read-syntax)
  (##readtable-char-sharp-handler-set! (##current-readtable) #\s
				       (make-read-sharp-single-char 'succeed))
  (##readtable-char-sharp-handler-set! (##current-readtable) #\u
				       (make-read-sharp-single-char 'fail)))
