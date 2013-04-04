(load "mk.scm")

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (cout "Testing " title nl)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define nl (string #\newline))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define errorf
  (lambda (tag . args)
    (printf "Failed: ~s: ~%" tag)
    (apply printf args)
    (error 'WiljaCodeTester "That's all, folks!")))

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

(test-check "set-run-eq-1"
  (run* (q) (== q `#(,q 1)))
  '((#(_.0 1) : (set _.0))))

(test-check "set-run-eq-2"
  (run* (q) (== q `#(,q 1)) (== q `#(,q 2)))
  ;; TODO: why 5 times, in {log}, it's only 4
  '((#(_.0 2 1) : (set _.0))
    (#(_.0 2 1) : (set _.0))
    (#(_.0 2 1) : (set _.0))
    (#(_.0 2 1) : (set _.0))
    (#(_.0 2 1) : (set _.0))))

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
  '(((_.0 _.1 _.2 #(_.2 _.1 _.0)) :
      (set _.2)
      (=/= (_.1 _.0))
      (!in (_.0 _.2)))
    ((_.0 _.0 _.1 #(_.1 _.0)) :
      (set _.1)
      (!in (_.0 _.1)))
    ((_.0 _.0 #(_.1 _.0) #(_.1 _.0)) :
      (set _.1)
      (!in (_.0 _.1)))
    ((_.0 _.1 #(_.2 _.0) #(_.2 _.1 _.0)) :
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
