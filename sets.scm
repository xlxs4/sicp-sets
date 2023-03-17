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
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (u/union-set set1 set2)
  (fold-right adjoin-set set2 set1))

(define (um/element-of-set? x set)
  (u/element-of-set? x set))

(define (um/adjoin-set x set)
  (cons x set))

(define (um/intersection-set set1 set2)
  (u/intersection-set set1 set2))

(define (um/union-set set1 set2)
  (append set1 set2))

(define (o/element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (o/adjoin-set x set)
  (cond ((or? (null? set) (< x (car set)))
         (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (o/intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1)
                                           set2))
              ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(define (o/union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (o/union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (o/union-set (cdr set1) set2)))
                 (else
                  (cons x2 (o/union-set set1 (cdr set2)))))))))
