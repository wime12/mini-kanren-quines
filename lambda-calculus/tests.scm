(include "../test-framework/test-framework#.scm")
(include "../test-framework/simple-tests#.scm")
(include "../mk#.scm")

(define-test (lambda-calculus-tests)
  (combine-results
        (sec-3)))

;;; Section 3

(define-test (sec-3)
  (combine-results
    (ex-3-1)
    (ex-3-2)
    (ex-3-3)
    (ex-3-4)))

(deftest ex-3-1
  (run* (q)
    (lookupo 'y '((x . foo) (y . bar)) q))
  '(bar))

(deftest ex-3-2
  (run* (q)
    (lookupo 'w '((x . foo) (y . bar)) q))
  '())

(deftest ex-3-3
  (run 5 (q)
    (fresh (e v)
      (eval-expo e '() v)
      (== `(,e -> ,v) q)))
  '((((lambda (_.0) _.1) -> (closure _.0 _.1 ())) (sym _.0))
    ((((lambda (_.0) _.0) (lambda (_.1) _.2))
      -> (closure _.1 _.2 ()))
     (sym _.0 _.1))
    ((((lambda (_.0) (lambda (_.1) _.2)) (lambda (_.3) _.4))
      -> (closure _.1 _.2 ((_.0 closure _.3 _.4 ()))))
     (=/= ((_.0 lambda)))
     (sym _.0 _.1 _.3))
    ((((lambda (_.0) (_.0 _.0)) (lambda (_.1) _.1))
      -> (closure _.1 _.1 ()))
     (sym _.0 _.1))
    ((((lambda (_.0) (_.0 _.0)) (lambda (_.1) (lambda (_.2) _.3)))
      -> (closure _.2 _.3 ((_.1 closure _.1 (lambda (_.2) _.3) ()))))
     (=/= ((_.1 lambda)))
     (sym _.0 _.1 _.2))))

(deftest ex-3-4
  (run 5 (q)
    (eval-expo q '() '(closure y x ((x . (closure z z ()))))))
  '(((lambda (x) (lambda (y) x)) (lambda (z) z))
    ((lambda (x) (x (lambda (y) x))) (lambda (z) z))
    (((lambda (x) (lambda (y) x))
      ((lambda (_.0) _.0) (lambda (z) z)))
     (sym _.0))
    ((((lambda (_.0) _.0) (lambda (x) (lambda (y) x)))
      (lambda (z) z))
     (sym _.0))
    (((lambda (_.0) _.0)
      ((lambda (x) (lambda (y) x)) (lambda (z) z)))
     (sym _.0))))

;;; vim: set lispwords+=define-test,deftest,run,run*,fresh:
