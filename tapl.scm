(load "mk.scm")
(load "test-check.scm")

(define map-seto
  (lambda (fo s out)
    (conde
     ((== s ∅)
      (== out ∅))
     ((fresh (se sr oe or)
        (== s (set sr se))
        (!ino se sr)
        (== out (set or oe))
        (!ino oe or)
        (fo se oe)
        (map-seto fo sr or))))))

(test-check
 "map-seto"
 (run 1 (q) (map-seto (lambda (x out) (== `(,x) out))
                      (set ∅ 'a 'b 'c)
                      q))
 '((set ∅ (a) (b) (c))))

(test-check
 "map-seto-converges"
 (length
  (run* (q) (map-seto (lambda (x out) (== `(,x) out))
                      (set ∅ 'a 'b 'c)
                      q)))
 6)

(define cartesiano
  (lambda (a b out)
    (conde
     ((== a ∅)
      (== out ∅))
     ((fresh (ae ar o1 or)
        (== a (set ar ae))
        (!ino ae ar)
        (map-seto (lambda (be oe) (== oe `(,ae ,be))) b o1)
        (uniono o1 or out)
        (cartesiano ar b or))))))

(test-check
 "cartesiano"
 (run 1 (q) (cartesiano (set ∅ 'a 'b 'c) (set ∅ 1 2 3) q))
 '((set ∅ (a 1) (a 2) (a 3) (b 1) (b 2) (b 3) (c 1) (c 2) (c 3))))

(test-check
 "cartesiano-converges"
 (length (run* (q) (cartesiano (set ∅ 'a 'b) (set ∅ 1 2) q)))
 128)

;; 3.2.3 of TAPL
(define S
  (lambda (i s)
    (conde
     ((== i 'z) (== s ∅))
     ((fresh (i-1 S-1 S-11 S-111 S-succ S-pred S-iszero S-if s1 s2 s3)
         (== i `(s ,i-1))
         (S i-1 S-1)
         (map-seto (lambda (e o) (== o `(succ ,e))) S-1 S-succ)
         (map-seto (lambda (e o) (== o `(pred ,e))) S-1 S-pred)
         (map-seto (lambda (e o) (== o `(iszero ,e))) S-1 S-iszero)
         (cartesiano S-1 S-1 S-11)
         (cartesiano S-1 S-11 S-111)
         (map-seto (lambda (e o) (fresh (e1 e2 e3)
                               (== e `(,e1 (,e2 ,e3)))
                               (== o `(if ,e1 ,e2 ,e3)))) S-111 S-if)
         (uniono (set ∅ 'true 'false 'zero) S-succ s1)
         (uniono s1 S-pred s2)
         (uniono s2 S-iszero s3)
         (uniono s3 S-if s))))))

(define mini-S
  (lambda (i s)
    (conde
     ((== i 'z) (== s ∅))
     ((fresh (i-1 S-1 S-11 S-succ S-plus s1)
         (== i `(s ,i-1))
         (mini-S i-1 S-1)
         (map-seto (lambda (e o) (== o `(succ ,e))) S-1 S-succ)
         (cartesiano S-1 S-1 S-11)
         (map-seto (lambda (e o) (fresh (e1 e2)
                               (== e `(,e1 ,e2))
                               (== o `(plus ,e1 ,e2)))) S-11 S-plus)
         (uniono (set ∅ 'true 'false 'zero) S-succ s1)
         (uniono s1 S-plus s))))))

(test-check
 "S0"
 (run 1 (q) (S 'z q))
 '(∅))

(test-check
 "S1"
 (run 1 (q) (S '(s z) q))
 '((set ∅ false true zero)))

(test-check
 "mini-S2"
 (run 1 (q) (mini-S '(s (s z)) q))
 '((set ∅
    (plus false false) (plus false true) (plus false zero)
    (plus true false) (plus true true) (plus true zero)
    (plus zero false) (plus zero true) (plus zero zero)
    (succ false) (succ true) (succ zero) false true zero)))
