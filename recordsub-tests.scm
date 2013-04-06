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

(test-check "tc-id"
  (run* (q) (tc `(lambda (x) x) ∅ q))
  `((arr _.0 _.0)))

(test-check "tc-lam-2"
  (car (run* (q) (tc `(lambda (x) (lambda (y) x)) ∅ q)))
  '(arr _.0 (arr _.1 _.0)))

(test-check "tc-lam-3"
  (car (run* (q) (tc `(lambda (x) (lambda (y) y)) ∅ q)))
  '(arr _.0 (arr _.1 _.1)))

(test-check "tc-app-1"
  (car (run* (q) (tc `(lambda (f) (lambda (x) (app f x))) ∅ q)))
  '(arr (arr _.0 _.1) (arr _.0 _.1)))

(test-check "tc-new-0"
  (run* (q) (tc `(new ,∅) ∅ q))
  '((rcd ∅)))

(test-check "tc-new-1"
  (run* (q) (tc `(new ,(set ∅ `(foo (lambda (x) x)))) ∅ q))
  '((rcd (set ∅ (foo (arr _.0 _.0))))))
