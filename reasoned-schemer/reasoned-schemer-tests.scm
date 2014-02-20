(include "../test-framework/test-framework#.scm")

(define-test (reasoned-schemer-tests)
  (combine-results
    (chapter-1)
    (chapter-2)))


;;; vim: set lispwords+=define-test:
