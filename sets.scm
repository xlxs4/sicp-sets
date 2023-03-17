(use-modules (srfi srfi-1))

(define (u/element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (u/adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (u/intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

(define (u/union-set set1 set2)
  (fold-right adjoin-set set2 set1))
