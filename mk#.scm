;; (##namespace ("mk#"
;; 	      run
;; 	      run*
;; 	      fresh
;; 	      conde
;; 	      project
;; 	      conda
;; 	      condu
;; 	      symbolo
;; 	      numbero
;; 	      booleano
;; 	      =/=
;; 	      ==
;; 	      absento
;; 	      succeed
;; 	      fail
;; 	      onceo))

(define-macro (lambdag@ args e)
  (cond
    ((and (list? args) (= (length args) 1))
     `(lambda ,args ,e))
    ((and (list? args) (= (length args) 6) (eq? (cadr args) ':))
     (let ((c (car args))
           (S (caddr args))
           (D (caddr (cdr args)))
           (A (caddr (cddr args)))
           (T (caddr (cdddr args))))
       `(lambda (,c)
          (let ((,S (c->S ,c))
                (,D (c->D ,c))
                (,A (c->A ,c))
                (,T (c->T ,c)))
            ,e))))
    (else (##ill-formed-special-form 'lambdag@ `(,args ,e)))))

(define-macro (lambdaf@ args e)
  (if (null? args)
    `(lambda () ,e)
    (##ill-formed-special-form 'lambdaf@ `(,args ,e))))

(define-macro (inc e)
  `(lambdaf@ () ,e))

(define-macro (case-inf e c1 c2 c3 c4)
  (if (and (list? c1) (= (length c1) 2) (null? (car c1))
	   (list? c2) (= (length c2) 2)
	   (list? (car c2)) (= (length (car c2)) 1)
	   (list? c3) (= (length c3) 2)
	   (list? (car c3)) (= (length (car c3)) 1)
	   (list? c4) (= (length c4) 2)
	   (list? (car c4)) (= (length (car c4)) 2))
    (let ((c-inf (gensym))
	  (e0 (cadr c1))
	  (f^ (caar c2))
	  (e1 (cadr c2))
	  (c^ (caar c3))
	  (e2 (cadr c3))
	  (c (caar c4))
	  (f (cadar c4))
	  (e3 (cadr c4))) 
      `(let ((,c-inf ,e))
	 (cond
	   ((not ,c-inf) ,e0)
	   ((procedure? ,c-inf) (let ((,f^ ,c-inf)) ,e1))
	   ((not (and (pair? ,c-inf)
		      (procedure? (cdr ,c-inf))))
	    (let ((,c^ ,c-inf)) ,e2))
	   (else (let ((,c (car ,c-inf)) (,f (cdr ,c-inf)))
		   ,e3)))))
    (##ill-formed-special-form 'case-inf `(,e ,c1 ,c2 ,c3 ,c4))))

(define-macro (run n x g0 . g)
  (if (and (list? x) (= (length x) 1))
      (let ((final-c (gensym)) (z (gensym)))
	`(take ,n
	       (lambdaf@ ()
		 ((fresh ,x ,g0 ,@g
		    (lambdag@ (,final-c)
		      (let ((,z ((reify ,@x) ,final-c)))
			(choice ,z empty-f))))
		  empty-c))))
      (##ill-formed-special-form 'run `(,n ,x ,g0 ,@g))))

(define-macro (run* x . gs)
  (if (and (list? x) (= (length x) 1))
    `(run #f ,x ,@gs)
    (##ill-formed-special-form 'run* `(,x ,@gs))))

(define-macro (fresh xs g0 . gs)
  (let ((c (gensym)))
    `(lambdag@ (,c)
       (inc (let ,(map (lambda (x) `(,x (var ',x))) xs)
	      (bind* (,g0 ,c) ,@gs))))))

(define-macro (bind* e . gs)
  (if (null? gs)
      e
      `(bind* (bind ,e ,(car gs)) ,@(cdr gs))))

(define-macro (conde f . fs)
  (define (good-form? f)
    (and (list? f) (>= (length f) 1)))
  (define (good-forms? fs)
    (if (null? fs)
	#t
	(and (good-form? (car fs)) (good-forms? (cdr fs)))))
  (if (and (good-form? f) (good-forms? fs))
      (let ((c (gensym)))
	`(lambdag@ (,c)
	   (inc (mplus* (bind* (,(car f) ,c) ,@(cdr f))
			,@(map (lambda (f) `(bind* (,(car f) ,c) ,@(cdr f)))
			       fs)))))
      (##ill-formed-special-form 'conde `(,f ,@fs))))

(define-macro (mplus* e . es)
  (if (null? es)
      e
      `(mplus ,e (lambdaf@ () (mplus* ,@es)))))

(define-macro (case-value u f1 f2 f3)
  (if (and
       (list? f1) (= (length f1) 2) (list? (car f1)) (= (length (car f1)) 1)
       (list? f2) (= (length f2) 2) (list? (car f2)) (= (length (car f2)) 2)
       (list? f3) (= (length f3) 2) (list? (car f3)) (= (length (car f3)) 1))
      (let ((t (gensym))
	    (t1 (caar f1)) (e0 (cadr f1))
	    (at (caar f2)) (dt (cadar f2)) (e1 (cadr f2))
	    (t2 (caar f3)) (e2 (cadr f3)))
	`(let ((,t ,u))
	   (cond ((var? ,t) (let ((,t1 ,t)) ,e0))
		 ((pair? ,t) (let ((,at (car ,t)) (,dt (cdr ,t))) ,e1))
		 (else (let ((,t2 ,t)) ,e2)))))))

(define-macro (project xs g . gs)
  (if (list? xs)
      (let ((c (gensym))
	    (S (gensym))
	    (D (gensym))
	    (A (gensym))
	    (T (gensym)))
	`(lambdag@ (,c : ,S ,D ,A ,T)
	   (let (,(map (lambda (x) `(,x (walk* ,x ,S))) xs))
	     ((fresh () ,g ,@gs) ,c))))
      (##ill-formed-special-form 'project `(,xs ,g ,@gs))))

(define-macro (conda gs . other-gs)
  (let* ((c (gensym))
	 (pair-up (lambda (gs)
		    (map (lambda (g) `(,g ,c)) gs))))
    `(lambdag@ (,c)
       (inc
         (ifa ,(pair-up gs) ,@(map pair-up other-gs))))))

(define-macro (ifa . args)
  (cond
    ((null? args) '(mzero))
	((and (>= (length args) 1)
	      (list? (car args)) (>= (length (car args)) 1))
	 (let ((c-inf (gensym))
	       (f (gensym))
	       (a (gensym))
	       (loop (gensym))
	       (e (caar args))
	       (gs (cdar args))
	       (bs (cdr args)))
	   `(let ,loop ((,c-inf ,e))
		 (case-inf ,c-inf
		   (() (ifa ,@bs))
		   ((,f) (inc (,loop (,f))))
		   ((,a) (bind* ,c-inf ,@gs))
		   ((,a ,f) (bind* ,c-inf ,@gs))))))
	(else (##ill-formed-special-form 'ifa args))))

(define-macro (condu gs . other-gs)
  (let* ((c (gensym))
	 (pair-up (lambda (gs)
		    (map (lambda (g) `(,g ,c)) gs))))
    `(lambdag@ (,c)
       (inc
	(ifu
	 ,(pair-up gs) ,@(map pair-up other-gs))))))

(define-macro (ifu . args)
  (cond ((null? args) '(mzero))
	((and (>= (length args) 1)
	      (list? (car args)) (>= (length (car args)) 1))
	 (let ((c-inf (gensym))
	       (f (gensym))
	       (c (gensym))
	       (loop (gensym))
	       (e (caar args))
	       (gs (cdar args))
	       (bs (cdr args)))
	   `(let ,loop ((,c-inf ,e))
		 (case-inf ,c-inf
		   (() (ifu ,@bs))
		   ((,f) (inc (,loop (,f))))
		   ((,c) (bind* ,c-inf ,@gs))
		   ((,c ,f) (bind* (unit ,c) ,@gs))))))
	(else (##ill-formed-special-form 'ifu args))))
