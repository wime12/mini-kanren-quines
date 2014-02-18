(define-macro (check . forms)
  `(combine-results
    ,@(map (lambda (form)
	     (let ((test? (car form))
		   (form (cadr form))
		   (expected (caddr form)))
	       `(report-result ,test? ',test? ',form ,expected ,form)))
	   forms)))

(define-macro (combine-results . forms)
  (let ((result (gensym)))
    `(let ((,result #t))
       ,@(map (lambda (f) `(if (not ,f) (set! ,result #f)))
	      forms)
       ,result)))

(define-macro (define-test def . body)
  (let ((test-name (car def)))
    `(define ,def
       (parameterize ((*test-name* (append (*test-name*) (list ',test-name))))
		     ,@body))))
