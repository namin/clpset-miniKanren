(load "mk.scm")
(load "test-check.scm")

(test-check "normalize-set-1"
  (normalize-set empty-set '() empty-s)
  empty-set)

(test-check "normalize-set-2"
  (normalize-set `#(,empty-set 1 2) '() empty-s)
  `#(,empty-set 1 2))

(test-check "normalize-set-3"
  (normalize-set `#(#(,empty-set 1) 2) '() empty-s)
  `#(,empty-set 1 2))

(test-check "normalize-set-4"
  (normalize-set `#(#(,(var 'x) 1) 2) '() empty-s)
  `#(,(var 'x) 1 2))

;;; feature complete wrt to paper!

(test-check "set-run-eq-1"
  (run* (q) (== q `#(,q 1)))
  '((#(_.0 1) : (set _.0))))

(test-check "set-run-eq-2"
  (run* (q) (== q `#(,q 1)) (== q `#(,q 2)))
  ;; TODO: why 5 times, in {log}, it's only 4
  '((#(_.0 1 2) : (set _.0))
    (#(_.0 1 2) : (set _.0))
    (#(_.0 1 2) : (set _.0))
    (#(_.0 1 2) : (set _.0))
    (#(_.0 1 2) : (set _.0))))

(test-check "set-run-eq-3"
  (run* (q) (fresh (x y r s) (== q `(,x ,y ,r ,s)) (== `#(,r ,x) `#(,s ,y))))
  '(((_.0 _.0 _.1 _.1) : (set _.1))
    ((_.0 _.0 _.1 #(_.1 _.0)) : (set _.1))
    ((_.0 _.0 #(_.1 _.0) _.1) : (set _.1))
    ((_.0 _.1 #(_.2 _.1) #(_.2 _.0)) : (set _.2))))

(test-check "set-run-eq-4"
  (run* (q) (fresh (z) (== q `#(,q #(,z 1)))))
  '((#(_.0 #(_.1 1)) : (set  _.0 _.1))))

(test-check "set-run-ino-1"
  (run* (q) (fresh (x y z) (== q `(,x ,y ,z)) (ino 'a `#(,z ,x b ,y))))
  '(((a _.0 _.1) : (set _.1))
    ((_.0 a _.1) : (set _.1))
    ((_.0 _.1 #(_.2 a)) : (set _.2))))

(test-check "set-run-neq-1"
  (run* (q) (fresh (x y)
              (== q `(,x ,y))
              (=/= `(f a #(,empty-set b c)) `(f ,x #(,empty-set ,x ,y)))))
  '(((_.0 _.1) : (=/= (_.0 a)))
    ((_.0 _.1) : (=/= (_.0 b) (_.1 b)))
    ((_.0 _.1) : (=/= (_.0 b) (_.0 c)))
    ((_.0 _.1) : (=/= (_.0 c) (_.1 c)))
    ((_.0 _.1) : (=/= (_.1 b) (_.1 c)))))

(test-check "set-run-neq-2"
  (run* (q) (=/= `#(,q c) `#(,empty-set b c)))
  '((_.0 : (set _.0) (!in (b _.0)))
    (#(_.0 _.1) : (set _.0) (=/= (_.1 b) (_.1 c)))))

(test-check "set-run-union-1"
  (run* (q) (fresh (x y z v)
              (== q `(,x ,y ,z ,v))
              (uniono `#(,empty-set ,x) `#(,z ,y) v)))
  '(((_.0 _.1 _.2 #(_.2 _.0 _.1)) :
      (set _.2)
      (=/= (_.1 _.0))
      (!in (_.0 _.2)))
    ((_.0 _.0 _.1 #(_.1 _.0)) :
      (set _.1)
      (!in (_.0 _.1)))
    ((_.0 _.0 #(_.1 _.0) #(_.1 _.0)) :
      (set _.1)
      (!in (_.0 _.1)))
    ((_.0 _.1 #(_.2 _.0) #(_.2 _.0 _.1)) :
      (set _.2)
      (=/= (_.1 _.0))
      (!in (_.0 _.2)))))

(test-check "set-run-union-cats-and-dogs-1"
  (length
    (run* (q)
      (fresh (x y z v)
        (== q `(,x ,y ,z ,v))
        (uniono `#(,empty-set cat ,x ,y) `#(,empty-set dog bird ,z) v))))
  57)

(test-check "set-run-disj-1"
  (run* (q)
    (fresh (x y z)
      (== q `(,x ,y ,z))
      (disjo `#(,empty-set ,x ,y) `#(,z a))))
  '(((_.0 _.1 _.2) :
      (set _.2)
      (=/= (_.0 a) (_.1 a))
      (!in (_.0 _.2) (_.1 _.2)))))

(test-check "set-run-not-union-1"
  (run* (q)
    (fresh (x y)
      (== q `(,x ,y))
      (!uniono x y `#(,empty-set a b))))
  '(((_.0 _.1) : (set _.0 _.1) (!in (a _.0) (a _.1)))
    ((_.0 _.1) : (set _.0 _.1) (!in (b _.0) (b _.1)))
    ((#(_.0 _.1) _.2) : (set _.0 _.2) (=/= (_.1 a) (_.1 b)))
    ((_.0 #(_.1 _.2)) : (set _.0 _.1) (=/= (_.2 a) (_.2 b)))))

(test-check "set-run-not-disj-1"
  (run* (q) (!disjo `#(,empty-set a) `#(,empty-set ,q b)))
  '(a))

(test-check "set-run-union-neq-1"
  (run* (q)
    (fresh (x y z)
      (== q `(,x ,y ,z))
      (uniono x y z)
      (=/= z empty-set)))
  '(((#(_.0 _.1) _.2 #(_.3 _.1)) :
      (set _.0 _.2 _.3)
      (!in (_.1 _.0) (_.1 _.3))
      (union [_.0 _.2 _.3]))
   ((_.0 #(_.1 _.2) #(_.3 _.2)) :
     (set _.0 _.1 _.3)
     (!in (_.2 _.1) (_.2 _.3))
     (union [_.0 _.1 _.3]))
   ((#(_.0 _.1) #(_.2 _.1) #(_.3 _.1)) :
     (set _.0 _.2 _.3)
     (!in (_.1 _.0) (_.1 _.2) (_.1 _.3))
     (union [_.0 _.2 _.3]))
   ((#(_.0 _.1) _.2 #(_.3 _.1)) :
     (set _.0 _.2 _.3)
     (!in (_.1 _.0) (_.1 _.3))
     (union [_.0 _.2 _.3]))
   ((_.0 #(_.1 _.2) #(_.3 _.2)) :
     (set _.0 _.1 _.3)
     (!in (_.2 _.1) (_.2 _.3))
     (union [_.0 _.1 _.3]))
   ((#(_.0 _.1) #(_.2 _.1) #(_.3 _.1)) :
     (set _.0 _.2 _.3)
     (!in (_.1 _.0) (_.1 _.2) (_.1 _.3))
     (union [_.0 _.2 _.3]))))

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
    (== q `#(,q x)))
  '())

(test-check "set-run-symbolo-3"
  (run* (q)
    (symbolo q)
    (seto q))
  '())
