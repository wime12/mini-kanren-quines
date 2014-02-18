(declare
  (block)
  (not safe)
  (standard-bindings)
  (extended-bindings)
  (fixnum))

(define *test-name* (make-parameter '()))

(define (report-result test? test?-form form expected result)
  (let ((success? (test? result expected)))
    (if success?
	(display "pass")
	(display "FAIL"))
    (display " ... ")
    (display (*test-name*))
    (if success?
      (newline)
      (begin
        (display ": ") (newline) (display "  ") (pp form)
        (display "  Test:     ") (pp test?-form)
        (display "  Expected: ") (pp expected)
        (display "  Result:   ") (pp result)))
    success?))
