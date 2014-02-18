(include "../mk#.scm")

(define (lookupo x env t)
  (fresh (y v rest)
    (== (cons (cons y v) rest) env)
    (conde
      ((== y x) (== v t))
      ((=/= y x) (lookupo x rest t)))))

(define (eval-expo exp env val)
  (conde
    ((fresh (rator rand x body env2 a)
       (== (list rator rand) exp)
       (eval-expo rator env (list 'closure x body env2))
       (eval-expo rand env a)
       (eval-expo body (cons (cons x a) env2) val)))
    ((fresh (x body)
       (== (list 'lambda (list x) body) exp)
       (symbolo x)
       (== (list 'closure x body env) val)
       (not-in-envo 'lambda env)))
    ((symbolo exp) (lookupo exp env val)) )) 

(define (not-in-envo x env)
  (conde
    ((== '() env))
    ((fresh (y v rest)
       (== (cons (cons y v) rest) env)
       (=/= y x)
       (not-in-envo x rest)))))
