(include "../test-framework/test-framework#.scm")
(include "../test-framework/simple-tests#.scm")
(include "../mk#.scm")

(define-test (mini-kanren-quines-tests)
  (combine-results
        (sec-2.1)
        (sec-2.2)))

;;; Section 2.1

(define-test (sec-2.1)
  (combine-results
    (ex-2.1-1)
    (ex-2.1-2)
    (ex-2.1-3)
    (ex-2.1-4)
    (ex-2.1-5)
    (ex-2.1-6)
    (ex-2.1-7)
    (ex-2.1-8)
    (ex-2.1-9a)
    (ex-2.1-10a)
    (ex-2.1-11)
    (ex-2.1-12)))

(deftest ex-2.1-1
         (run 1 (q)
           (fresh (x y z)
             (== x z)
             (== 3 y)))
         '(_.0))

(deftest ex-2.1-2
         (run 1 (q)
           (fresh (x y)
             (== x q)
             (== 3 y)))
         '(_.0))

(deftest ex-2.1-3
         (run 1 (y)
           (fresh (x z)
             (== x z)
             (== 3 y)))
         '(3))

(deftest ex-2.1-4
         (run 1 (q)
           (fresh (x z)
             (== x z)
             (== 3 z)
             (== q x)))
         '(3))

(deftest ex-2.1-5
         (run 1 (y)
           (fresh (x y)
             (== 4 x)
             (== x y))
           (== 3 y))
         '(3))

(deftest ex-2.1-6
         (run 1 (x)
           (== 4 3))
         '())

(deftest ex-2.1-7
         (run 1 (x)
           (== 5 x) (== 6 x))
         '())

(deftest ex-2.1-8
         (run 2 (q)
           (fresh (w x y)
             (conde
               ((== `(,x ,w ,x) q) (== y w))
               ((== `(,w ,x ,w) q) (== y w)))))
         '((_.0 _.1 _.0) (_.0 _.1 _.0)))

;; (deftest ex-2.1-9
;;   (run* (q)
;;     (let loop ()
;;       (conde
;; 	((== #f q))
;; 	((== #t q))
;; 	((loop)))))
;;   +infinity+)

(deftest ex-2.1-9a
         (run 10 (q)
           (let loop ()
             (conde
               ((== #f q))
               ((== #t q))
               ((loop)))))
         '(#f #t #f #t #f #t #f #t #f #t))

(define (anyo g)
  (conde
    (g)
    ((anyo g))))

;; (deftest ex-2.1-10
;;   (run* (q)
;;     (conde
;;       ((anyo (== #f q)))
;;       ((== #t q))))
;;   +infinity+)

(deftest ex-2.1-10a
         (run 5 (q)
           (conde
             ((anyo (== #f q)))
             ((== #t q))))
         '(#t #f #f #f #f))

(deftest ex-2.1-11
         (run 10 (q)
           (anyo
             (conde
               ((== 1 q))
               ((== 2 q))
               ((== 3 q)))))
         '(1 2 3 1 2 3 1 2 3 1))

(deftest ex-2.1-12
         (run 3 (q)
           (let ((nevero (anyo (== #f #t))))
             (conde
               ((== 1 q))
               (nevero)
               ((conde
                  ((== 2 q))
                  (nevero)
                  ((== 3 q)))))))
         '(1 2 3))

;;; Section 2.2

(define-test (sec-2.2)
  (combine-results
    (ex-2.2-1)
    (ex-2.2-2)
    (ex-2.2-3)
    (ex-2.2-2a)
    (ex-2.2-3a)
    (ex-2.2-4)
    (ex-2.2-5)
    (ex-2.2-6a)
    (ex-2.2-6b)
    (ex-2.2-6c)
    (ex-2.2-7)
    (ex-2.2-8a)
    (ex-2.2-8b)
    (ex-2.2-9)
    (ex-2.2-10)
    (ex-2.2-11)
    (ex-2.2-12))) 

(deftest ex-2.2-1
         (run* (q)
           (symbolo q))
         '((_.0 (sym _.0))))

(deftest ex-2.2-2
         (run* (q)
           (symbolo q)
           (== 4 q))
         '())

(deftest ex-2.2-3
         (run* (q)
           (symbolo q)
           (numbero q))
         '())

(deftest ex-2.2-2a
         (run* (q)
           (numbero q)
           (== 4 q))
         '(4))

(deftest ex-2.2-3a
         (run* (q)
           (numbero q)
           (numbero q))
         '((_.0 (num _.0))))

(deftest ex-2.2-4
         (run* (p)
           (=/= p 1))
         '((_.0 (=/= ((_.0 1))))))

(deftest ex-2.2-5
         (run* (p)
           (=/= 1 p)
           (== 1 p))
         '())

(deftest ex-2.2-6a
         (run* (q)
           (fresh (p r)
             (=/= '(1 2) `(,p ,r))
             (== `(,p ,r) q)))
         '(((_.0 _.1) (=/= ((_.0 1) (_.1 2))))))

(deftest ex-2.2-6b
         (run* (q)
           (fresh (p r)
             (=/= '((1) (2)) `((,p) (,r)))
             (== `(,p ,r) q)))
         '(((_.0 _.1) (=/= ((_.0 1) (_.1 2))))))

(deftest ex-2.2-6c
         (run* (q)
           (fresh (p r)
             (=/= `((1) (,r)) `((,p) (2)))
             (== `(,p ,r) q)))
         '(((_.0 _.1) (=/= ((_.0 1) (_.1 2))))))

(deftest ex-2.2-7
         (run* (q)
           (fresh (p r)
             (=/= '(1 2) `(,p ,r))
             (== 1 p)
             (== `(,p ,r) q)))
         '(((1 _.0) (=/= ((_.0 2))))))

(deftest ex-2.2-8a
         (run* (q)
           (fresh (p r)
             (=/= '(1 2) `(,p ,r))
             (== 1 p)
             (== 2 r)
             (== `(,p ,r) q)))
         '())

(deftest ex-2.2-8b
         (run* (q)
           (fresh (p r)
             (=/= '(1 2) `(,p ,r))
             (== 1 p)
             (symbolo r)
             (== `(,p ,r) q)))
         '(((1 _.0) (sym _.0))))

(deftest ex-2.2-9
         (run* (q)
           (fresh (x y)
             (== `(jackal (,y leopard ,x)) q)
             (absento 'panda q)))
         '(((jackal (_.0 leopard _.1))
            (absent panda _.0)
            (absent panda _.1))))

(deftest ex-2.2-10
         (run* (q)
           (fresh (x y)
             (== `(jackal (,y leopard ,x)) q)
             (absento 'panda q)
             (== 'panda x)))
         '())

(deftest ex-2.2-11
         (run* (q)
           (fresh (x y)
             (== `(jackal (,y leopard ,x)) q)
             (absento 'panda q)
             (symbolo x)))
         '(((jackal (_.0 leopard _.1))
            (=/= ((_.1 panda)))
            (absent panda _.0)
            (sym _.1))))

(deftest ex-2.2-12
         (run* (q)
           (fresh (x y z)
             (== `(jackal (,y leopard ,x)) q)
             (absento 'panda q)
             (symbolo x)
             (== `(c ,z d) y)
             (== 'panda z)))
         '())


;;; vim: set lispwords+=define-test,deftest,run,run*,fresh:
