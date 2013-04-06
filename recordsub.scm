(load "lib.scm")

(define typ
  (lambda (ty)
    (fresh (r)
      (== `(rcd ,r) ty)
      (typ-rcd r))))

(define typ-rcd
  (lambda (r)
    (conde
      ((== r ∅))
      ((fresh (l ty rr)
         (== r (set rr `(,l ,ty)))
         (symbolo l)
         (typ ty)
         (label-!in l rr)
         (typ-rcd rr))))))

(define label-!in
  (lambda (l r)
    (conde
      ((== r ∅))
      ((fresh (ol oty rr)
         (== r (set rr `(,ol ,oty)))
         (=/= ol l)
         (label-!in l rr))))))

'(
   (run 100 (q) (typ-rcd q))
 )

(define sub
  (lambda (ty1 ty2)
    (conde
      ((== ty1 ty2)
       (typ ty1))
      ((=/= ty1 ty2)
       (fresh (r1 r2)
         (== `(rcd ,r1) ty1)
         (== `(rcd ,r2) ty2)
         (sub-rcd r1 r2))))))

(define sub-rcd
  (lambda (r1 r2)
    (conde
      ((== r2 ∅)
       (typ-rcd r1))
      ((fresh (l ty2 rr2 ty1 rr1)
         (== r2 (set rr2 `(,l ,ty2)))
         (== r1 (set rr1 `(,l ,ty1)))
         (symbolo l)
         (label-!in l rr2)
         (label-!in l rr1)
         (sub ty1 ty2)
         (sub-rcd rr1 rr2))))))

'(
   (run 10 (q)
     (fresh (ty1 ty2)
       (=/= ty1 ty2)
       (== q `(,ty1 ,ty2))
       (sub ty1 ty2)
       (typ ty1)
       (typ ty2)))
 )
