(include "mk#.scm")

(define (build-num n)
  (cond
    ((odd? n)
     (cons 1 (build-num (/ (- n 1) 2))))
    ((and (not (zero? n)) (even? n))
     (cons 0 (build-num (/ n 2))))
    ((zero? n) '())))

(define (zeroo n)
  (== '() n))

(define (poso n)
  (fresh (a d)
    (== (cons a d) n)))

(define (>1o n)
  (fresh (a ad dd)
    (== (cons a (cons ad dd)) n)))

(define (full-addero b x y r c)
  (conde
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))

(define (addero d n m r)
  (conde
    ((== 0 d) (== '() m) (== n r))
    ((== 0 d) (== '() n) (== m r) (poso m))
    ((== 1 d) (== '() m) (addero 0 n '(1) r))
    ((== 1 d) (== '() n) (poso m) (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
     (fresh (a c)
       (== (list a c) r)
       (full-addero d 1 1 a c)))
    ((== '(1) n) (gen-addero d n m r))
    ((== '(1) m) (>1o n) (>1o r) (addero d '(1) n r))
    ((>1o n) (gen-addero d n m r))))

(define (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== (cons a x) n)
    (== (cons b y) m) (poso y)
    (== (cons c z) r) (poso z)
    (full-addero d a b c e)
    (addero e x y z)))

(define (+o n m k)
  (addero 0 n m k))

(define (-o n m k)
  (+o m k n))

(define (*o n m p)
  (conde
    ((== '() n) (== '() p))
    ((poso n) (== '() m) (== '() p))
    ((== '(1) n) (poso m)  (== m p))
    ((>1o n) (== '(1) m)  (== n p))
    ((fresh (x z)
       (== (cons 0 x) n) (poso x)
       (== (cons o y) m) (poso z)
       (>1o m)
       (*o x m z)))
    ((fresh (x y)
       (== (cons 1 x) n) (poso x)
       (== (cons 0 y) m) (poso y)
       (*o m n p)))
    ((fresh (x y)
       (== (cons 1 x) n) (poso x)
       (== (cons 1 y) m) (poso y)
       (odd-*o x n m p)))))

(define (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (+o (cons o q) m p)))

(define (bound-*o q p n m)
  (conde
    ((== '() q) (poso p))
    ((fresh (a0 a1 a2 a3 x y z)
       (== (cons a0 x) q)
       (== (cons a1 y) p)
       (conde
         ((== '() n) (== (cons a2 z) m)
          (bound-*o x y z '()))
         ((== (cons a3 z) n) (bound-*o x y z m)))))))


;;; vim: set lispwords+=fresh,conde,conda,condu,run,run*
