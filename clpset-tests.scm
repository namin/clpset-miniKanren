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
  '(#(_.0 1)))

(test-check "set-run-eq-2"
  (run* (q) (== q `#(,q 1)) (== q `#(,q 2)))
  ;; TODO: why 5 times, in {log}, it's only 4
  '(#(_.0 2 1) #(_.0 2 1) #(_.0 2 1) #(_.0 2 1) #(_.0 2 1)))

(test-check "set-run-eq-3"
  (run* (q) (fresh (x y r s) (== q `(,x ,y ,r ,s)) (== `#(,r ,x) `#(,s ,y))))
  '((_.0 _.0 _.1 _.1)
    (_.0 _.0 _.1 #(_.1 _.0))
    (_.0 _.0 #(_.1 _.0) _.1)
    (_.0 _.1 #(_.2 _.1) #(_.2 _.0))))


