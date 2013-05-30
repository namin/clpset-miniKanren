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
 ;; running with 1, because we get dups :-(
 (run 1 (q) (map-seto (lambda (x out) (== `(,x) out))
                      (set ∅ 'a 'b 'c)
                      q))
 '((set ∅ (a) (b) (c))))

