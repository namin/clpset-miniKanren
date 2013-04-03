(define s->S (lambda (s) (car s)))
(define s->C (lambda (s) (cadr s)))
(define S->s (lambda (S) (with-S empty-s S)))
(define with-S (lambda (s S) (list S (s->C s))))
(define empty-s '(() ()))

(define empty-set '#())
(define empty-set? (lambda (x) (and (vector? x) (= (vector-length x) 0))))
(define non-empty-set? (lambda (x) (and (vector? x) (> (vector-length x) 1))))
(define set? (lambda (x) (and (empty-set? x) (non-empty-set? x))))
(define join
  (lambda (a b)
    (cond
      ((null? a) b)
      ((member (car a) b) (join (cdr a) b))
      (else (cons (car a) (join (cdr a) b))))))
(define normalize-set
  (lambda (x es s)
    (cond
      ((empty-set? x) (if (null? es) empty-set `#(,empty-set ,@es)))
      ((non-empty-set? x) (let ((v (vector->list x)))
                            (normalize-set
                              (walk (car v) s)
                              (join (cdr v) es)
                              s)))
      ((and (var? x) (not (null? es))) `#(,x ,@es))
      ((and (symbol? x) (not (null? es))) `#(,x ,@es)))))
(define set-tail
  (lambda (x)
    (cond
      ((empty-set? x) #f)
      ((non-empty-set? x) (vector-ref x 0))
      ((var? x) t))))
(define with-set-tail
  (lambda (t x)
    (cond
      ((non-empty-set? x)
       (let ((v (vector->list x)))
         `#(,t ,@(cdr v))))
      ((var? x) t))))
(define non-empty-set-first
  (lambda (x)
    (vector-ref x 1)))
(define non-empty-set-rest
  (lambda (x)
    (if (= 2 (vector-length x))
      (vector-ref x 0)
      (let ((v (vector->list x)))
        `#(,(car v) ,@(cddr v))))))
(define (rem-index i lst)
  (if (= i 0)
    (cdr lst)
    (cons (car lst) (rem-index (- i 1) (cdr lst)))))
(define non-empty-set-at
  (lambda (j x)
    (vector-ref x (+ j 1))))
(define non-empty-set-except-at
  (lambda (j x)
    (let ((r (rem-index (+ 1 j) (vector->list x))))
      (if (null? (cdr r))
        (car r)
        `#(,@r)))))
(define non-empty-set-size
  (lambda (x)
    (- (vector-length x) 1)))
(define setc (lambda (x) succeed)) ;; TODO

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (p) e) (lambda (p) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define-syntax rhs
  (syntax-rules ()
    ((_ x) (cdr x))))

(define-syntax lhs
  (syntax-rules ()
    ((_ x) (car x))))

(define-syntax size-S
  (syntax-rules ()
    ((_ x) (length (s->S x)))))

(define-syntax var
  (syntax-rules ()
    ((_ x) (vector x))))

(define-syntax var?
  (syntax-rules ()
    ((_ x) (and (vector? x) (= (vector-length x) 1)))))

(define walk
  (lambda (u s)
    (cond
      ((and (var? u) (assq u (s->S s))) =>
       (lambda (pr) (walk (rhs pr) s)))
      (else u))))

(define ext-s
  (lambda (x v s)
    (with-S s (cons `(,x . ,v) (s->S s)))))

(define unify
  (lambda (ou ov s)
    (let ((u (walk ou s))
          (v (walk ov s)))
      (cond
        ((and (var? v) (not (var? u))) (unify ov ou s))
        ((eq? u v) s)
        ((and (var? u) (not (occurs-check u v s))) (ext-s u v s))
        ((non-empty-set? v)
          (let* ((vns (normalize-set v '() s))
                 (x (set-tail vns)))
            (cond
              ((and (var? u) (eq? x u))
               ((fresh (n)
                  (== u (with-set-tail n vns))
                  (setc n))
                 s))
              ((non-empty-set? u)
               (let ((uns (normalize-set u '() s)))
                 (if (not (eq? (set-tail uns) (set-tail vns)))
                   (let ((tu (non-empty-set-first uns))
                         (ru (non-empty-set-rest uns))
                         (tv (non-empty-set-first vns))
                         (rv (non-empty-set-rest vns)))
                     ((conde
                        ((== tu tv) (== ru rv))
                        ((== tu tv) (== uns rv))
                        ((== tu tv) (== ru vns))
                        ((fresh (n)
                           (== ru `#(,n ,tv))
                           (== rv `#(,n ,tu))
                           (setc n))))
                       s))
                   ((let ((t0 (non-empty-set-first uns))
                          (ru (non-empty-set-rest uns))
                          (maxj (non-empty-set-size vns)))
                      (let loopj ((j 0))
                        (if (< j maxj)
                          (let ((tj (non-empty-set-at j vns))
                                (rj (non-empty-set-except-at j vns)))
                            (conde
                              ((== t0 tj) (== ru rj))
                              ((== t0 tj) (== uns rj))
                              ((== t0 tj) (== ru vns))
                              ((fresh (n)
                                 (== x `#(,n ,t0))
                                 (== (with-set-tail n ru) (with-set-tail n vns))
                                 (setc n)))
                              ((loopj (+ j 1)))))
                          fail))) s))))
              (else (fail s)))))
        ((and (pair? u) (pair? v))
         (let ((s (unify
                    (car u) (car v) s)))
           (and s (unify
                    (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))

(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (ext-s x v s)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v)
         (or
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        ((non-empty-set? v)
          (occurs-check x (vector->list v) s))
        (else #f)))))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        ((non-empty-set? v)
          (let ((ns (normalize-set v '()  s)))
            `#(,@(walk* (vector->list ns) s))))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (size-S s)) s))
        ((pair? v) (reify-s (cdr v)
                     (reify-s (car v) s)))
        ((non-empty-set? v)
         (reify-s (vector->list v) s))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v s)
    (let ((v (walk* v s)))
      (walk* v (reify-s v empty-s)))))

(define-syntax mzero
  (syntax-rules () ((_) #f)))

(define-syntax inc
  (syntax-rules () ((_ e) (lambdaf@ () e))))

(define-syntax unit
  (syntax-rules () ((_ a) a)))

(define-syntax choice
  (syntax-rules () ((_ a f) (cons a f))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((a^) e2) ((a f) e3))
     (let ((a-inf e))
       (cond
         ((not a-inf) e0)
         ((procedure? a-inf)  (let ((f^ a-inf)) e1))
         ((not (and (pair? a-inf)
                    (procedure? (cdr a-inf))))
          (let ((a^ a-inf)) e2))
         (else (let ((a (car a-inf)) (f (cdr a-inf)))
                 e3)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ...
            (lambdag@ (s)
              (cons (reify x s) '())))
          empty-s))))))

(define take
  (lambda (n f)
    (if (and n (zero? n))
      '()
      (case-inf (f)
        (() '())
        ((f) (take n f))
        ((a) a)
        ((a f)
         (cons (car a)
           (take (and n (- n 1)) f)))))))

(define ==
  (lambda (u v)
    (lambdag@ (s)
      (unify u v s))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s)
       (inc
         (let ((x (var 'x)) ...)
           (bind* (g0 s) g ...)))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((a) (g a))
      ((a f) (mplus (g a) (lambdaf@ () (bind (f) g)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
       (inc
         (mplus*
           (bind* (g0 s) g ...)
           (bind* (g1 s) g^ ...) ...))))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0
                    (lambdaf@ () (mplus* e ...))))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((a) (choice a f))
      ((a f^) (choice a (lambdaf@ () (mplus (f) f^)))))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
       (inc
         (ifa ((g0 s) g ...)
              ((g1 s) g^ ...) ...))))))

(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
       (inc
         (ifu ((g0 s) g ...)
              ((g1 s) g^ ...) ...))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* (unit a) g ...)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (s)
       (let ((x (walk* x s)) ...)
         ((fresh () g g* ...) s))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
      (g succeed)
      ((== #f #f) fail))))
