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

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tr/element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (tr/adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (tr/intersection-set set1 set2)
  (let ((lset1 (tree->list set1))
        (lset2 (tree->list set2))
        (lintersection (o/intersection-set lset1 lset2))
        (intersection (list->tree lintersection)))
    intersection))

(define (tr/union-set set1 set2)
  (let ((lset1 (tree->list set1))
        (lset2 (tree->list set2))
        (lunion (o/union-set lset1 lset2))
        (union (list->tree lunion)))
    union))
