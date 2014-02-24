(declare
  (block)
  (not safe)
  (standard-bindings)
  (extended-bindings)
  (fixnum))

(include "../mk#.scm")

(define (caro a p)
  (fresh (d)
    (== (cons a d) p)))

(define (cdro d p)
  (fresh (a)
    (== (cons a d) p)))

(define (conso a d l)
  (== (cons a d) l))

(define (nullo l)
  (== '() l))

(define (listo l)
  (conde
    ((nullo l))
    ((fresh (d)
       (cdro d l)
       (listo d)))))

(define (valid-numbero n)
  (conde
    ((== 1 n))
    ((== 2 n))
    ((== 3 n))
    ((== 4 n))
    ((== 5 n))
    ((== 6 n))
    ((== 7 n))
    ((== 8 n))
    ((== 9 n))))

(define (all-diffo l)
  (conde
    ((nullo l))
    ((fresh (a d)
       (conso a d l)
       (not-membero a d)
       (all-diffo d)))))

(define (not-membero x l)
  (conde
    ((nullo l))
    ((fresh (a d)
       (conso a d l)
       (=/= x a)
       (not-membero x d)))))

(define (valid-numbero* l)
  (conde
    ((nullo l))
    ((fresh (a d)
       (conso a d l)
       (valid-numbero a)
       (valid-numbero* d)))))

(define (ensembleo l)
  (fresh ()
    (all-diffo l)
    (valid-numbero* l)))

(define (fieldo f)
  (fresh (x11 x12 x13 x14 x15 x16 x17 x18 x19
          x21 x22 x23 x24 x25 x26 x27 x28 x29
          x31 x32 x33 x34 x35 x36 x37 x38 x39
          x41 x42 x43 x44 x45 x46 x47 x48 x49
          x51 x52 x53 x54 x55 x56 x57 x58 x59
          x61 x62 x63 x64 x65 x66 x67 x68 x69
          x71 x72 x73 x74 x75 x76 x77 x78 x79
          x81 x82 x83 x84 x85 x86 x87 x88 x89
          x91 x92 x93 x94 x95 x96 x97 x98 x99)
    (ensembleo (list x11 x12 x13 x14 x15 x16 x17 x18 x19))
    (ensembleo (list x21 x22 x23 x24 x25 x26 x27 x28 x29))
    (ensembleo (list x31 x32 x33 x34 x35 x36 x37 x38 x39))
    (ensembleo (list x41 x42 x43 x44 x45 x46 x47 x48 x49))
    (ensembleo (list x51 x52 x53 x54 x55 x56 x57 x58 x59))
    (ensembleo (list x61 x62 x63 x64 x65 x66 x67 x68 x69))
    (ensembleo (list x71 x72 x73 x74 x75 x76 x77 x78 x79))
    (ensembleo (list x81 x82 x83 x84 x85 x86 x87 x88 x89))
    (ensembleo (list x91 x92 x93 x94 x95 x96 x97 x98 x99))
    
    (ensembleo (list x11 x21 x31 x41 x51 x61 x71 x81 x91))
    (ensembleo (list x12 x22 x32 x42 x52 x62 x72 x82 x92))
    (ensembleo (list x13 x23 x33 x43 x53 x63 x73 x83 x93))
    (ensembleo (list x14 x24 x34 x44 x54 x64 x74 x84 x94))
    (ensembleo (list x15 x25 x35 x45 x55 x65 x75 x85 x95))
    (ensembleo (list x16 x26 x36 x46 x56 x66 x76 x86 x96))
    (ensembleo (list x17 x27 x37 x47 x57 x67 x77 x87 x97))
    (ensembleo (list x18 x28 x38 x48 x58 x68 x78 x88 x98))
    (ensembleo (list x19 x29 x39 x49 x59 x69 x79 x89 x99))

    (ensembleo (list x11 x12 x13 x21 x22 x23 x31 x32 x33))
    (ensembleo (list x14 x15 x16 x24 x25 x26 x34 x35 x36))
    (ensembleo (list x17 x18 x19 x27 x28 x29 x37 x38 x39))
    (ensembleo (list x41 x42 x43 x51 x52 x53 x61 x62 x63))
    (ensembleo (list x44 x45 x46 x54 x55 x56 x64 x65 x66))
    (ensembleo (list x47 x48 x49 x57 x58 x59 x67 x68 x69))
    (ensembleo (list x71 x72 x73 x81 x82 x83 x91 x92 x93))
    (ensembleo (list x74 x75 x76 x84 x85 x86 x94 x95 x96))
    (ensembleo (list x77 x78 x79 x87 x88 x89 x97 x98 x99))
   
    (== (list (list x11 x12 x13 x14 x15 x16 x17 x18 x19)
              (list x21 x22 x23 x24 x25 x26 x27 x28 x29)
              (list x31 x32 x33 x34 x35 x36 x37 x38 x39)      
              (list x41 x42 x43 x44 x45 x46 x47 x48 x49)
              (list x51 x52 x53 x54 x55 x56 x57 x58 x59)
              (list x61 x62 x63 x64 x65 x66 x67 x68 x69)
              (list x71 x72 x73 x74 x75 x76 x77 x78 x79)
              (list x81 x82 x83 x84 x85 x86 x87 x88 x89)
              (list x91 x92 x93 x94 x95 x96 x97 x98 x99))
        f)))

(define (example)
  (run 1 (q)
    (fresh (x13 x14 x15 x18 x19
            x22 x23 x24 x26 x27
            x32 x33 x34 x35 x36 x37
            x45 x47 x48 x49
            x52 x53 x55 x57 x58 x59
            x61 x62 x63 x64 x65 x69
            x73 x74 x76 x77 x78 x79
            x82 x83 x84 x86 x88
            x91 x92 x96 x97)
      (== q (list (list  4   3  x13 x14 x15  8   7  x18 x19)
                  (list  7  x22 x23 x24  4  x26 x27  1   8)
                  (list  5  x32 x33 x34 x35 x36 x37  3   9)
                  (list  9   8   2   7  x45  4  x47 x48 x49)
                  (list  6  x52 x53  3  x55  5  x57 x58 x59)
                  (list x61 x62 x63 x64 x65  2   1   9  x69)
                  (list  1   9  x73 x74  3  x76 x77 x78 x79)
                  (list  2  x82 x83 x84  6  x86  3  x88  1)
                  (list x91 x92  3   2   5  x96 x97  7   6)))
      (fieldo q))))

;;; vim: set lispwords+=run,run*,fresh,conde,conda,condu:
