(load "mk.scm")

(define subseto
  (lambda (r1 r2)
    (fresh (ri)
      (uniono r1 ri r2))))

(define !subseto
  (lambda (r1 r2)
    (fresh (x)
      (ino x r1)
      (!ino x r2))))

