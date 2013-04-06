(load "lib.scm")

(define typ
  (lambda (ty)
    (conde
      ((== 'top ty))
      ((fresh (ty1 ty2)
         (== `(arr ,ty1 ,ty2) ty)
         (typ ty1)
         (typ ty2)))
      ((fresh (r)
         (== `(rcd ,r) ty)
         (typ-rcd r))))))

(define typ-rcd
  (lambda (r)
    (conde
      ((== r ∅))
      ((fresh (l ty rr)
         (== r (set rr `(,l ,ty)))
         (label-!ino l rr)
         (symbolo l)
         (typ ty)
         (typ-rcd rr))))))

(define label-!ino
  (lambda (l r)
    (conde
      ((== r ∅))
      ((fresh (ol oty rr)
         (== r (set rr `(,ol ,oty)))
         (!ino `(,ol ,oty) rr)
         (=/= ol l)
         (label-!ino l rr))))))

'(
   (run 100 (q) (typ-rcd q))
 )

(define labelso
  (lambda (r ls)
    (conde
      ((== r ∅) (== ls ∅))
      ((fresh (l ty rr lsr)
         (== r (set rr `(,l ,ty)))
         (== ls (set lsr l))
         (!ino `(,l ,ty) rr)
         (!ino l lsr)
         (labelso rr lsr))))))

(define sub
  (lambda (ty1 ty2)
    (conde
      ((== ty1 ty2)
       (typ ty1))
      ((=/= ty1 ty2)
       (conde
         ((== 'top ty2)
          (typ ty1))
         ((fresh (tya1 tyb1 tya2 tyb2)
            (== `(arr ,tya1 ,tyb1) ty1)
            (== `(arr ,tya2 ,tyb2) ty2)
            (sub tya2 tya1)
            (sub tyb1 tyb2)))
         ((fresh (r1 r2)
            (== `(rcd ,r1) ty1)
            (== `(rcd ,r2) ty2)
            (sub-rcd r1 r2))))))))

(define sub-rcd
  (lambda (r1 r2)
    (fresh (ls1 ls2)
      (subseto ls2 ls1)
      (labelso r1 ls1)
      (labelso r2 ls2)
      (conde
        ((== r2 ∅)
         (typ-rcd r1))
        ((fresh (l ty1 rr1 ty2 rr2)
           (== r1 (set rr1 `(,l ,ty1)))
           (== r2 (set rr2 `(,l ,ty2)))
           (symbolo l)
           (sub ty1 ty2)
           (label-!ino l rr1)
           (label-!ino l rr2)
           (sub-rcd rr1 rr2)))))))

'(
   (run 10 (q)
     (fresh (ty1 ty2)
       (=/= ty1 ty2)
       (=/= ty2 `(rcd ,∅))
       (== q `(,ty1 ,ty2))
       (sub ty1 ty2)))
 )
