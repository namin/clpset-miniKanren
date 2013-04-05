(load "lib.scm")
(load "test-check.scm")

(test-check "subseto-1"
  (run 1 (q)
    (subseto (set ∅ 1 2) (set ∅ 1 2 3)))
  '(_.0))

(test-check "subseto-1-not"
  (run 1 (q)
    (subseto (set ∅ 1 2 3) (set ∅ 1 2)))
  '())

(test-check "not-subseto-1"
  (run 1 (q)
    (!subseto (set ∅ 1 2 3) (set ∅ 1 2)))
  '(_.0))

(test-check "not-subseto-1-not"
  (run 1 (q)
    (!subseto (set ∅ 1 2) (set ∅ 1 2 3)))
  '())



