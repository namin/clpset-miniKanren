(load "mk.scm")
(load "test-check.scm")

;;; feature complete wrt to paper!

(test-check "set-run-eq-1"
  (run* (q) (== q (set q 1)))
  '(((set _.0 1) : (set _.0))))

(test-check "set-run-eq-2"
  (run* (q) (== q (set q 1)) (== q (set q 2)))
  ;; TODO: why 5 times, in {log}, it's only 4
  '(((set _.0 1 2) : (set _.0))
    ((set _.0 1 2) : (set _.0))
    ((set _.0 1 2) : (set _.0))
    ((set _.0 1 2) : (set _.0))
    ((set _.0 1 2) : (set _.0))))

(test-check "set-run-eq-3"
  (run* (q) (fresh (x y r s) (== q `(,x ,y ,r ,s)) (== (set r x) (set s y))))
  '(((_.0 _.0 _.1 _.1) : (set _.1))
    ((_.0 _.0 _.1 (set _.1 _.0)) : (set _.1))
    ((_.0 _.0 (set _.1 _.0) _.1) : (set _.1))
    ((_.0 _.1 (set _.2 _.1) (set _.2 _.0)) : (set _.2))))

(test-check "set-run-eq-4"
  (run* (q) (fresh (z) (== q (set q (set z 1)))))
  '(((set _.0 (set _.1 1)) : (set  _.0 _.1))))

(test-check "set-run-eq-5"
  (run* (q) (== (set q 2) (set q 1)))
  '(((set _.0 1 2) : (set _.0))))

(test-check "set-run-ino-1"
  (run* (q) (fresh (x y z) (== q `(,x ,y ,z)) (ino 'a (set z x 'b y))))
  '(((a _.0 _.1) : (set _.1))
    ((_.0 a _.1) : (set _.1))
    ((_.0 _.1 (set _.2 a)) : (set _.2))))

(test-check "set-run-neq-1"
  (run* (q) (fresh (x y)
              (== q `(,x ,y))
              (=/= `(f a ,(set ∅ 'b 'c)) `(f ,x ,(set ∅ x y)))))
  '(((_.0 _.1) : (=/= (_.0 a)))
    ((_.0 _.1) : (=/= (_.0 b) (_.1 b)))
    ((_.0 _.1) : (=/= (_.0 b) (_.0 c)))
    ((_.0 _.1) : (=/= (_.0 c) (_.1 c)))
    ((_.0 _.1) : (=/= (_.1 b) (_.1 c)))))

(test-check "set-run-neq-2"
  (run* (q) (=/= (set q 'c) (set ∅ 'b 'c)))
  '((_.0 : (set _.0) (!in (b _.0)))
    ((set _.0 _.1) : (set _.0) (=/= (_.1 b) (_.1 c)))))

(test-check "set-run-union-1"
  (run* (q) (fresh (x y z v)
              (== q `(,x ,y ,z ,v))
              (uniono (set ∅ x) (set z y) v)))
  '(((_.0 _.1 _.2 (set _.2 _.0 _.1)) :
      (set _.2)
      (=/= (_.1 _.0))
      (!in (_.0 _.2)))
    ((_.0 _.0 _.1 (set _.1 _.0)) :
      (set _.1)
      (!in (_.0 _.1)))
    ((_.0 _.0 (set _.1 _.0) (set _.1 _.0)) :
      (set _.1)
      (!in (_.0 _.1)))
    ((_.0 _.1 (set _.2 _.0) (set _.2 _.0 _.1)) :
      (set _.2)
      (=/= (_.1 _.0))
      (!in (_.0 _.2)))))

(test-check "set-run-union-cats-and-dogs-1"
  (length
    (run* (q)
      (fresh (x y z v)
        (== q `(,x ,y ,z ,v))
        (uniono (set ∅ 'cat x y) (set ∅ 'dog 'bird z) v))))
  57)

(test-check "set-run-disj-1"
  (run* (q)
    (fresh (x y z)
      (== q `(,x ,y ,z))
      (disjo (set ∅ x y) (set z 'a))))
  '(((_.0 _.1 _.2) :
      (set _.2)
      (=/= (_.0 a) (_.1 a))
      (!in (_.0 _.2) (_.1 _.2)))))

(test-check "set-run-not-union-1"
  (run* (q)
    (fresh (x y)
      (== q `(,x ,y))
      (!uniono x y (set ∅ 'a 'b))))
  '(((_.0 _.1) : (set _.0 _.1) (!in (a _.0) (a _.1)))
    ((_.0 _.1) : (set _.0 _.1) (!in (b _.0) (b _.1)))
    (((set _.0 _.1) _.2) : (set _.0 _.2) (=/= (_.1 a) (_.1 b)))
    ((_.0 (set _.1 _.2)) : (set _.0 _.1) (=/= (_.2 a) (_.2 b)))))

(test-check "set-run-not-disj-1"
  (run* (q) (!disjo (set ∅ 'a) (set ∅ q 'b)))
  '(a))

(test-check "set-run-union-neq-1"
  (run* (q)
    (fresh (x y z)
      (== q `(,x ,y ,z))
      (uniono x y z)
      (=/= z empty-set)))
  '((((set _.0 _.1) _.2 (set _.3 _.1)) :
      (set _.0 _.2 _.3)
      (!in (_.1 _.0) (_.1 _.3))
      (union [_.0 _.2 _.3]))
   ((_.0 (set _.1 _.2) (set _.3 _.2)) :
     (set _.0 _.1 _.3)
     (!in (_.2 _.1) (_.2 _.3))
     (union [_.0 _.1 _.3]))
   (((set _.0 _.1) (set _.2 _.1) (set _.3 _.1)) :
     (set _.0 _.2 _.3)
     (!in (_.1 _.0) (_.1 _.2) (_.1 _.3))
     (union [_.0 _.2 _.3]))
   (((set _.0 _.1) _.2 (set _.3 _.1)) :
     (set _.0 _.2 _.3)
     (!in (_.1 _.0) (_.1 _.3))
     (union [_.0 _.2 _.3]))
   ((_.0 (set _.1 _.2) (set _.3 _.2)) :
     (set _.0 _.1 _.3)
     (!in (_.2 _.1) (_.2 _.3))
     (union [_.0 _.1 _.3]))
   (((set _.0 _.1) (set _.2 _.1) (set _.3 _.1)) :
     (set _.0 _.2 _.3)
     (!in (_.1 _.0) (_.1 _.2) (_.1 _.3))
     (union [_.0 _.2 _.3]))))

(test-check
 "intersectiono-related"
 (run* (q)
   (fresh (x y z)
     (uniono x z (set ∅ 1))
     (uniono y z (set ∅ 1))
     (disjo x y)
     (== q `(,x ,y ,z))))
 '((∅ ∅ (set ∅ 1))
   ((set ∅ 1) ∅ (set ∅ 1))
   (∅ (set ∅ 1) (set ∅ 1))))

;;; problematic (?) tests, reproduced in {log} too

(test-check "redundant-answers-=/=-pair-case-1"
  (run* (q)
    (fresh (va vb r l v)
      (== q (set ∅ `(a ,va) `(b ,vb)))
      (== q (set r `(,l ,v)))
      (!ino `(,l ,v) r)))
  '((set ∅ (a _.0) (b _.1))
    ((set ∅ (a _.0) (b _.1)) : (=/= (_.1 _.0))) ;; subsumed
    (set ∅ (a _.0) (b _.1)) ;; redundant
    ((set ∅ (a _.0) (b _.1)) : (=/= (_.0 _.1))) ;; subsumed
     ))

;;; useful features beyond the paper

;;; symbolo

(test-check "set-run-symbolo-0"
  (run* (q) (symbolo q))
  '((_.0 : (sym _.0))))

(test-check "set-run-symbolo-1"
  (run* (q)
    (symbolo q)
    (== q 'hello))
  '(hello))

(test-check "set-run-symbolo-2"
  (run* (q)
    (symbolo q)
    (== q empty-set))
  '())

(test-check "set-run-symbolo-3"
  (run* (q)
    (symbolo q)
    (== q (set q 'x)))
  '())

(test-check "set-run-symbolo-3"
  (run* (q)
    (symbolo q)
    (seto q))
  '())

;;; implementation tests

(test-check "normalize-set-1"
  (normalize-set empty-set '() empty-s)
  empty-set)

(test-check "normalize-set-2"
  (normalize-set (set empty-set 1 2) '() empty-s)
  (set empty-set 1 2))

(test-check "normalize-set-3"
  (normalize-set (set (set empty-set 1) 2) '() empty-s)
  (set empty-set 1 2))

(test-check "normalize-set-4"
  (normalize-set (set (set (var 'x) 1) 2) '() empty-s)
  (set (var 'x) 1 2))
