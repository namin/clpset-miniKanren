(load "recordsub.scm")
(load "test-check.scm")

(test-check "sub-reflexivity-1"
  (run 1 (q) (sub `(rcd ,(set ∅ `(a (rcd ,(set ∅))))) `(rcd ,(set ∅ `(a (rcd ,(set ∅)))))))
  '(_.0))

(test-check "sub-1"
  (run 1 (q) (sub `(rcd ,(set ∅ `(a (rcd ,(set ∅))) `(b (rcd ,(set ∅))))) `(rcd ,(set ∅ `(a (rcd ,(set ∅)))))))
  '(_.0))

(test-disable "slow-exhaustive-sub-search-1"
  ;; TODO: why does it stall?
  (run 1 (q) (sub `(rcd ,(set ∅ `(a (rcd ,(set ∅))))) `(rcd ,(set ∅ `(a (rcd ,(set ∅))) `(b (rcd ,(set ∅)))))))
  '())
