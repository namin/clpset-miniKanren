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

