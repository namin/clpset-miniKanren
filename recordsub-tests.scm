(load "recordsub.scm")
(load "test-check.scm")

(test-check "sub-reflexivity-1"
  (run 1 (q) (sub `(rcd ,(set ∅ `(a (rcd ,∅)))) `(rcd ,(set ∅ `(a (rcd ,∅))))))
  '(_.0))

(test-check "sub-1"
  (run 1 (q) (sub `(rcd ,(set ∅ `(a (rcd ,∅)) `(b (rcd ,∅)))) `(rcd ,(set ∅ `(a (rcd ,∅))))))
  '(_.0))

(test-check "sub-empty-x-is-empty"
  (run* (q)
    (sub `(rcd ,∅) q))
  '((rcd ∅)))

(test-check "sub-rcd-empty-x-is-empty"
  (run* (q)
    (sub-rcd ∅ q))
  '(∅))

(test-check "sub-1-x-is"
 (run* (q)
    (sub `(rcd ,(set ∅ `(a (rcd ,∅)))) q))
  '((rcd (set ∅ (a (rcd ∅)))) (rcd ∅)))

(test-check "fail-sub-1"
  (run* (q) (sub `(rcd ,(set ∅ `(a (rcd ,∅)))) `(rcd ,(set ∅ `(a (rcd ,∅)) `(b (rcd ,∅))))))
  '())
