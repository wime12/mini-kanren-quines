(define-macro (deftest name expr expected)
  `(define-test (,name)
     (check (equal? ,expr ,expected))))
