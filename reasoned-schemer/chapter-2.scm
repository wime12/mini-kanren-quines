(include "../test-framework/test-framework#.scm")
(include "../test-framework/simple-tests#.scm")
(include "../mk#.scm")

(define-test (chapter-2)
  (combine-results
   (box-2.1)
   (box-2.2)
   (box-2.3)
   (box-2.4)
   (box-2.5)
   (box-2.6)
   (box-2.7)
   (box-2.8)
   (box-2.10)
   (box-2.11)
   (box-2.13)
   (box-2.14)
   (box-2.15)
   (box-2.17)
   (box-2.18)
   (box-2.19)
   (box-2.20)
   (box-2.21)
   (box-2.22)
   (box-2.25)
   (box-2.26)
   (box-2.27)
   (box-2.29)
   (box-2.30)
   (box-2.31)
   (box-2.32)
   (box-2.33)
   (box-2.34)
   (box-2.36)
   (box-2.37)
   (box-2.38)
   (box-2.39)
   (box-2.43)
   (box-2.44)
   (box-2.48)
   (box-2.49)
   (box-2.51)
   (box-2.52)
   (box-2.54)
   (box-2.55)
   (box-2.56)
   (box-2.57)
   (box-2.58)

   ))

(deftest box-2.1
  (let ((x (lambda (a) a))
	(y 'c))
    (x y))
  'c)

(deftest box-2.2
  (run* (r)
    (fresh (y x)
      (== `(,x ,y) r)))
  '((_.0 _.1)))

(deftest box-2.3
  (run* (r)
    (fresh (v w)
      (== (let ((x v) (y w)) `(,x ,y)) r)))
  '((_.0 _.1)))

(deftest box-2.4
  (car '(grape raisin pear))
  'grape)

(deftest box-2.5
  (car '(a c o r n))
  'a)

(deftest box-2.6
  (run* (r)
    (caro '(a c o r n) r))
  '(a))

(deftest box-2.7
  (run* (q)
    (caro '(a c o r n) 'a)
    (== #t q))
  '(#t))

(deftest box-2.8
  (run* (r)
    (fresh (x y)
      (caro `(,r ,y) x)
      (== 'pear x)))
  '(pear))

;;; Definition box-2.9
(define (caro p a)
  (fresh (d)
    (== (cons a d) p)))

(deftest box-2.10
  (cons
   (car '(grape raisin pear))
   (car '((a) (b) (c))))
  '(grape a))

(deftest box-2.11
  (run* (r)
    (fresh (x y)
      (caro '(grape raisin pear) x)
      (caro '((a) (b) (c)) y)
      (== (cons x y) r)))
  '((grape a)))

(deftest box-2.13
  (cdr '(grape raisin pear))
  '(raisin pear))

(deftest box-2.14
  (car (cdr '(a c o r n)))
  'c)

(deftest box-2.15
  (run* (r)
    (fresh (v)
      (cdro '(a c o r n) v)
      (caro v r)))
  '(c))

;;; Definition box-2.16
(define (cdro p d)
  (fresh (a)
    (== (cons a d) p)))

(deftest box-2.17
  (cons
   (cdr '(grape raisin pear))
   (car '((a) (b) (c))))
  '((raisin pear) a))

(deftest box-2.18
  (run* (r)
    (fresh (x y)
      (cdro '(grape raisin pear) x)
      (caro '((a) (b) (c)) y)
      (== (cons x y) r)))
  '(((raisin pear) a)))

(deftest box-2.19
  (run* (q)
    (cdro '(a c o r n) '(c o r n))
    (== #t q))
  '(#t))

(deftest box-2.20
  (run* (x)
    (cdro '(c o r n) `(,x r n)))
  '(o))

(deftest box-2.21
  (run* (l)
    (fresh (x)
      (cdro l '(c o r n))
      (caro l x)
      (== 'a x)))
  '((a c o r n)))

(deftest box-2.22
  (run* (l)
    (conso '(a b c) '(d e) l))
  '(((a b c) d e)))

(deftest box-2.23
  (run* (x)
    (conso x '(a b c) '(d a b c)))
  '(d))

(deftest box-2.24
  (run* (r)
    (fresh (x y z)
      (== `(e a d ,x) r)
      (conso y `(a ,z c) r)))
  '((e a d c)))

(deftest box-2.25
  (run* (x)
    (conso x `(a ,x c) `(d a ,x c)))
  '(d))

(deftest box-2.26
  (run* (l)
    (fresh (x)
      (== `(d a ,x c) l)
      (conso x `(a ,x c) l)))
  '((d a d c)))

(deftest box-2.27
  (run* (l)
    (fresh (x)
      (conso x `(a ,x c) l)
      (== `(d a ,x c) l)))
  '((d a d c)))

;;; Definition box-2.28
(define (conso a d p)
  (== (cons a d) p))

(deftest box-2.29
  (run* (l)
    (fresh (d x y w s)
      (conso w '(a n s) s)
      (cdro l s)
      (caro l x)
      (== 'b x)
      (cdro l d)
      (caro d y)
      (== 'e y)))
  '((b e a n s)))

(deftest box-2.30
  (null? '(graphe raisin pear))
  #f)

(deftest box-2.31
  (null? '())
  #t)

(deftest box-2.32
  (run* (q)
    (nullo '(grape raisin pear))
    (== #t q))
  '())

(deftest box-2.33
  (run* (q)
    (nullo '())
    (== #t q))
  '(#t))

(deftest box-2.34
  (run* (x)
    (nullo x))
  '(()))

;;; Definition box-2.35
(define (nullo x)
  (== '() x))

(deftest box-2.36
  (eq? 'pear 'plum)
  #f)

(deftest box-2.37
  (eq? 'plum 'plum)
  #t)

(deftest box-2.38
  (run* (q)
    (eqo 'pear 'plum)
    (== #t q))
  '())

(deftest box-2.39
  (run* (q)
    (eqo 'plum 'plum)
    (== #t q))
  '(#t))

;;; Definition 2.40
(define (eqo x y)
  (== x y))

(deftest box-2.43
  (pair? '((split) . pea))
  #t)

(deftest box-2.44
  (pair? '())
  #f)

(deftest box-2.48
  (car '(pear))
  'pear)

(deftest box-2.49
  (cdr '(pear))
  '())

(deftest box-2.51
  (cons '(split) 'pea)
  '((split) . pea))

(deftest box-2.52
  (run* (r)
    (fresh (x y)
      (== (cons x (cons y 'salad)) r)))
  '((_.0 _.1 . salad)))

;;; Definition box-2.53
(define (pairo p)
  (fresh (a d)
    (conso a d p)))

(deftest box-2.54
  (run* (q)
    (pairo (cons q q))
    (== #t q))
  '(#t))

(deftest box-2.55
  (run* (q)
    (pairo '())
    (== #t q))
  '())

(deftest box-2.56
  (run* (q)
    (pairo 'pair)
    (== #t q))
  '())

(deftest box-2.57
  (run* (x)
    (pairo x))
  '((_.0 . _.1)))

(deftest box-2.58
  (run* (r)
    (pairo (cons r 'pear)))
  '(_.0))

