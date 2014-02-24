(define (conso a d l)
  (== (cons a d) l))

(define (caro a l)
  (fresh (d)
    (conso a d l)))

(define (cdro d l)
  (fresh (a)
    (conso a d l)))

(define (nullo l)
  (== '() l))

(define (membero x l)
  (conde
   ((caro x l))
   ((fresh (d)
      (cdro d l)
      (membero x d)))))

(define (remo x l out)
  (conde
   ((caro x l) (cdro out l))
   ((fresh (a d z)
      (conso a d l)
      (remo x d z)
      (conso a z out)))))

(define (seto vars)
  (fresh (a d)
    (conso a d vars)
    (disjo a d)
    (conda
     ((nullo d))
     ((seto d)))))

(define (disjo x l)
  (conde
   ((nullo l))
   ((fresh (a d)
      (conso a d l)
      (=/= x a)
      (disjo x d)))))

(define (val-numo x)
  (conde
   ((== 1 x))
   ((== 2 x))
   ((== 3 x))
   ((== 4 x))))

(define (val-numso l)
  (conde
   ((nullo l))
   ((fresh (a d)
      (conso a d l)
      (val-numo a)
      (val-numso d)))))

(define (test)
  (run 1 (q)
    (fresh (a1 a2 a3 a4
	    b1 b2 b3 b4
	    c1 c2 c3 c4
	    d1 d2 d3 d4)
      (val-numso (list a1 a2 a3 a4
		       b1 b2 b3 b4
		       c1 c2 c3 c4
		       d1 d2 d3 d4))
      ;; restrictions
      ;; (== 1 a1)
      ;; (== 2 a2)
      ;; (== 3 a3)
      ;; (== 4 a4)
      ;; (== 3 b1)
      ;; (== 2 c1)
      ;; rows
      (seto (list a1 a2 a3 a4))
      ;; (seto (list b1 b2 b3 b4))
      ;; (seto (list c1 c2 c3 c4))
      ;; (seto (list d1 d2 d3 d4))
      ;; columns
      ;; (seto (list a1 b1 c1 d1))
      ;; (seto (list a2 b2 c2 d2))
      ;; (seto (list a3 b3 c3 d3))
      ;; (seto (list a4 b4 c4 d4))
      ;; squares
      ;; (seto (list a1 a2 b1 b2))
      ;; (seto (list a3 a4 b3 b4))
      ;; (seto (list c1 c2 d1 d2))
      ;; (seto (list c3 c4 d3 d4))
      ;; result
      (== q (list a1 a2 a3 a4
		  b1 b2 b3 b4
		  c1 c2 c3 c4
		  d1 d2 d3 d4)))))
