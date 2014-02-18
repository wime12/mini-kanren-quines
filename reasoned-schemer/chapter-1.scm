(include "../test-framework/test-framework#.scm")
(include "../test-framework/simple-tests#.scm")
(include "../mk#.scm")

(define-test (chapter-1)
  (combine-results
   (box-1.10)
   (box-1.11)
   (box-1.12)
   (box-1.13)
   (box-1.14)
   (box-1.15)
   (box-1.16)
   (box-1.17)
   (box-1.18)
   (box-1.22)
   (box-1.23)
   (box-1.26)
   (box-1.27)
   (box-1.28)
   (box-1.29)
   (box-1.30)
   (box-1.31)
   (box-1.32)
   (box-1.33)
   (box-1.34)
   (box-1.35)
   (box-1.36)
   (box-1.37)
   (box-1.38)
   (box-1.39)
   (box-1.40a)
   (box-1.40b)
   (box-1.41)
   
   ))

(deftest box-1.10
  (run* (q)
    fail)
  '())

(deftest box-1.11
  (run* (q)
    (== #t q))
  '(#t))

(deftest box-1.12
  (run* (q)
    fail
    (== #t q))
  '())

(deftest box-1.13
  (run* (q)
    succeed
    (== #t q))
  '(#t))

(deftest box-1.14
  (run* (q)
    succeed
    (== #t q))
  '(#t))

(deftest box-1.15
  (run* (r)
    succeed
    (== 'corn r))
  '(corn))

(deftest box-1.16
  (run* (r)
    succeed
    (== 'corn r))
  '(corn))

(deftest box-1.17
  (run* (r)
    fail
    (== 'corn r))
  '())

(deftest box-1.18
  (run* (q)
    succeed
    (== #f q))
  '(#f))

(deftest box-1.22
  (run* (x)
    (let ((x #f))
      (== #t x)))
  '())

(deftest box-1.23
  (run* (q)
    (fresh (x)
      (== #t x)
      (== #t q)))
  '(#t))

(deftest box-1.26
  (run* (q)
    (fresh (x)
      (== x #t)
      (== #t q)))
  '(#t))

(deftest box-1.27
  (run* (q)
    (fresh (x)
      (== x #t)
      (== q #t)))
  '(#t))

(deftest box-1.28
  (run* (x)
    succeed)
  '(_.0))

(deftest box-1.29
  (run* (x)
    (let ((x #f))
      (fresh (x)
	(== #t x))))
  '(_.0))

(deftest box-1.30
  (run* (r)
    (fresh (x y)
      (== (cons x (cons y '())) r)))
  '((_.0 _.1)))

(deftest box-1.31
  (run* (s)
    (fresh (t u)
      (== (cons t (cons u '())) s)))
  '((_.0 _.1)))

(deftest box-1.32
  (run* (r)
    (fresh (x)
      (let ((y x))
	(fresh (x)
	  (== (cons y (cons x (cons y '()))) r)))))
  '((_.0 _.1 _.0)))

(deftest box-1.33
  (run* (r)
    (fresh (x)
      (let ((y x))
	(fresh (x)
	  (== (cons x (cons y (cons x '()))) r)))))
  '((_.0 _.1 _.0)))

(deftest box-1.34
  (run* (q)
    (== #f q)
    (== #t q))
  '())

(deftest box-1.35
  (run* (q)
    (== #f q)
    (== #f q))
  '(#f))

(deftest box-1.36
  (run* (q)
    (let ((x q))
      (== #t x)))
  '(#t))

(deftest box-1.37
  (run* (r)
    (fresh (x)
      (== x r)))
  '(_.0))

(deftest box-1.38
  (run* (q)
    (fresh (x)
      (== #t x)
      (== x q)))
  '(#t))

(deftest box-1.39
  (run* (q)
    (fresh (x)
      (== x q)
      (== #t x)))
  '(#t))

(deftest box-1.40a
  (run* (q)
    (fresh (x)
      (== (eq? x q) q)))
  '(#f)) 

(deftest box-1.40b
  (run* (q)
    (let ((x q))
      (fresh (q)
        (== (eq? x q) x))))
  '(#f))

(deftest box-1.41
  (cond
    (#f #t)
    (else #f))
  #f)

(deftest box-1.43
  (run* (q)
    (cond
      (#f succeed)
      (fail)))
  '())

(deftest box-1.44
  (run* (q)
    (conde
      (fail succeed)
      (fail)))
  '())

(deftest box-1.45
  (run* (q)
    (conde
      (fail fail)
      (succeed)))
  '(_.0))

(deftest box-1.46
  (run* (q)
    (conde
      (succeed succeed)
      (fail)))
  '(_.0))

(deftest box-1.47
  (run* (x)
    (conde
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)
      (fail)))
  '(olive oil))

(deftest box-1.49
  (run 1 (x)
    (conde
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)
      (fail)))
  '(olive))

(deftest box-1.50
  (run* (x)
    (conde
      ((== 'virgin x) fail)
      ((== 'olive x) succeed)
      (succeed succeed)
      ((== 'oil x) succeed)
      (fail)))
  '(olive _.0 oil))

(deftest box-1.52
  (run 2 (x)
    (conde
      ((== 'extra x) succeed)
      ((== 'virgin x) fail)
      ((== 'olive x) succeed)
      ((== 'oil x) succeed)
      (fail)))
  '(extra olive))


;;; vim: set lispwords+=deftest,define-test,fresh,run,run* :
