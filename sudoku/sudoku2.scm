(declare
  (block)
  (not safe)
  (fixnum)
  (standard-bindings)
  (extended-bindings))

(include "../mk#.scm")

(define (nullo l)
  (== '() l))

(define (conso a d l)
  (== (cons a d) l))

(define (caro a l)
  (fresh (d)
    (conso a d l)))

(define (cdro d l)
  (fresh (a)
    (conso a d l)))

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

(define full-set '(1 2 3 4 5 6 7 8 9))

(define (disjo x in out)
  (membero x in)
  (remo x in out))

(define (set-mapo vars vals)
  (fresh (a d t)
    (caro a vars)
    (cdro d vars)
    (disjo a vals t)
    (conda
      ((nullo d))
      ((set-mapo d t)))))

(define (test)
  (run* (q)
    (fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9
            b1 b2 b3 b4 b5 b6 b7 b8 b9
            c1 c2 c3 c4 c5 c6 c7 c8 c9
            d1 d2 d3 d4 d5 d6 d7 d8 d9
            e1 e2 e3 e4 e5 e6 e7 e8 e9
            f1 f2 f3 f4 f5 f6 f7 f8 f9
            g1 g2 g3 g4 g5 g6 g7 g8 g9
            h1 h2 h3 h4 h5 h6 h7 h8 h9
            i1 i2 i3 i4 i5 i6 i7 i8 i9)
      (== a2 1) (== a3 8) (== a5 7) (== a7 4)
      (== b1 7) (== b2 6) (== b3 5) (== b6 4)
      (== c1 2) (== c5 8) (== c7 5) (== c9 7)
      (== d1 1) (== d5 4) (== d6 3)
      (== e1 8) (== e4 9) (== e6 7) (== e9 4)
      (== f4 8) (== f5 5) (== f9 3)
      (== g1 3) (== g3 2) (== g5 1) (== g9 5)
      (== h4 5) (== h6 2) (== h7 8) (== h8 7) (== h9 1)
      (== i2 7) (== i7 9) (== i8 3) (== i9 2)
      ;; rows
      (set-mapo (list a1 a2 a3 a4 a5 a6 a7 a8 a9) full-set)
      (set-mapo (list b1 b2 b3 b4 b5 b6 b7 b8 b9) full-set)
      (set-mapo (list c1 c2 c3 c4 c5 c6 c7 c8 c9) full-set)
      (set-mapo (list d1 d2 d3 d4 d5 d6 d7 d8 d9) full-set)
      (set-mapo (list e1 e2 e3 e4 e5 e6 e7 e8 e9) full-set)
      (set-mapo (list f1 f2 f3 f4 f5 f6 f7 f8 f9) full-set)
      (set-mapo (list g1 g2 g3 g4 g5 g6 g7 g8 g9) full-set)
      (set-mapo (list h1 h2 h3 h4 h5 h6 h7 h8 h9) full-set)
      (set-mapo (list i1 i2 i3 i4 i5 i6 i7 i8 i9) full-set)
      ;; cols
      (set-mapo (list a1 b1 c1 d1 e1 f1 g1 h1 i1) full-set)
      (set-mapo (list a2 b2 c2 d2 e2 f2 g2 h2 i2) full-set)
      (set-mapo (list a3 b3 c3 d3 e3 f3 g3 h3 i3) full-set)
      (set-mapo (list a4 b4 c4 d4 e4 f4 g4 h4 i4) full-set)
      (set-mapo (list a5 b5 c5 d5 e5 f5 g5 h5 i5) full-set)
      (set-mapo (list a6 b6 c6 d6 e6 f6 g6 h6 i6) full-set)
      (set-mapo (list a7 b7 c7 d7 e7 f7 g7 h7 i7) full-set)
      (set-mapo (list a8 b8 c8 d8 e8 f8 g8 h8 i8) full-set)
      (set-mapo (list a9 b9 c9 d9 e9 f9 g9 h9 i9) full-set)
      ;; squares
      (set-mapo (list a1 a2 a3 b1 b2 b3 c1 c2 c3) full-set)
      (set-mapo (list a4 a5 a6 b4 b5 b6 c4 c5 c6) full-set)
      (set-mapo (list a7 a8 a9 b7 b8 b9 c7 c8 c9) full-set)
      (set-mapo (list d1 d2 d3 e1 e2 e3 f1 f2 f3) full-set)
      (set-mapo (list d4 d5 d6 e4 e5 e6 f4 f5 f6) full-set)
      (set-mapo (list d7 d8 d9 e7 e8 e9 f7 f8 f9) full-set)
      (set-mapo (list g1 g2 g3 h1 h2 h3 i1 i2 i3) full-set)
      (set-mapo (list g4 g5 g6 h4 h5 h6 i4 i5 i6) full-set)
      (set-mapo (list g7 g8 g9 h7 h8 h9 i7 i8 i9) full-set)
      ;; answer
      (== (list (list a1 a2 a3 a4 a5 a6 a7 a8 a9)
                (list b1 b2 b3 b4 b5 b6 b7 b8 b9)
                (list c1 c2 c3 c4 c5 c6 c7 c8 c9)
                (list d1 d2 d3 d4 d5 d6 d7 d8 d9)
                (list e1 e2 e3 e4 d5 d6 e7 e8 e9)
                (list f1 f2 f3 f4 f5 f6 f7 f8 f9)
                (list g1 g2 g3 g4 g5 g6 g7 g8 g9)
                (list h1 h2 h3 h4 h5 h6 h7 h8 h9)
                (list i1 i2 i3 i4 i5 i6 i7 i8 i9))
          q))))

(define (test2)
  (run* (q)
    (fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9
            b1 b2 b3 b4 b5 b6 b7 b8 b9
            c1 c2 c3 c4 c5 c6 c7 c8 c9
            d1 d2 d3 d4 d5 d6 d7 d8 d9
            e1 e2 e3 e4 e5 e6 e7 e8 e9
            f1 f2 f3 f4 f5 f6 f7 f8 f9
            g1 g2 g3 g4 g5 g6 g7 g8 g9
            h1 h2 h3 h4 h5 h6 h7 h8 j9
            i1 i2 i3 i4 i5 i6 i7 i8 i9)
      (== 1 a1)
      (== 2 a2)
      (== 3 a3)
      (== 4 a4)
      (== 3 b1)
      (== 2 c1)
      ;; rows
      (set-mapo (list a1 a2 a3 a4) full-set)
      (set-mapo (list b1 b2 b3 b4) full-set)
      (set-mapo (list c1 c2 c3 c4) full-set)
      (set-mapo (list d1 d2 d3 d4) full-set)
      ;; cols
      (set-mapo (list a1 b1 c1 d1) full-set)
      (set-mapo (list a2 b2 c2 d2) full-set)
      (set-mapo (list a3 b3 c3 d3) full-set)
      (set-mapo (list a4 b4 c4 d4) full-set)
      ;; squares
      (set-mapo (list a1 a2 b1 b2) full-set)
      (set-mapo (list a3 a4 b3 b4) full-set)
      (set-mapo (list c1 c2 d1 d2) full-set)
      (set-mapo (list c3 c4 d3 d4) full-set)
      ;; answer
      (== (list (list a1 a2 a3 a4)
                (list b1 b2 b3 b4)
                (list c1 c2 c3 c4)
                (list d1 d2 d3 d4))
          q))))

(pp (test2))

;;; :vim set lispwords+=fresh,conde,conda,condu,run,run*:
