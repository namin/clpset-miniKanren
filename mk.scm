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

(define s->S (lambda (s) (car s)))
(define s->C (lambda (s) (cadr s)))
(define C->set (lambda (C) (car C)))
(define C->=/= (lambda (C) (cadr C)))
(define C->!in (lambda (C) (caddr C)))
(define C->union (lambda (C) (cadddr C)))
(define C->disj (lambda (C) (cadddr (cdr C))))
(define C->symbol (lambda (C) (cadddr (cddr C))))
(define S->s (lambda (S) (with-S empty-s S)))
(define with-S (lambda (s S) (list S (s->C s))))
(define with-C (lambda (s C) (list (s->S s) C)))
(define with-C-set (lambda (C cs) (list cs (C->=/= C) (C->!in C) (C->union C) (C->disj C) (C->symbol C))))
(define with-C-=/= (lambda (C cs) (list (C->set C) cs (C->!in C) (C->union C) (C->disj C) (C->symbol C))))
(define with-C-!in (lambda (C cs) (list (C->set C) (C->=/= C) cs (C->union C) (C->disj C) (C->symbol C))))
(define with-C-union (lambda (C cs) (list (C->set C) (C->=/= C) (C->!in C) cs (C->disj C) (C->symbol C))))
(define with-C-disj (lambda (C cs) (list (C->set C) (C->=/= C) (C->!in C) (C->union C) cs (C->symbol C))))
(define with-C-symbol (lambda (C cs) (list (C->set C) (C->=/= C) (C->!in C) (C->union C) (C->disj C) cs)))
(define empty-s '(() (() () () () () ())))

(define empty-set '#())
(define ∅ empty-set)
(define make-set (lambda (base mems) `#(,base ,mems)))
(define set-map vector-map)
(define set-base (lambda (x) (vector-ref x 0)))
(define set-mems (lambda (x) (vector-ref x 1)))
(define set (lambda (base e0 . es) (make-set base (cons e0 es))))
(define empty-set? (lambda (x) (and (vector? x) (= (vector-length x) 0))))
(define non-empty-set? (lambda (x) (and (vector? x) (= (vector-length x) 2))))
(define set? (lambda (x) (or (empty-set? x) (non-empty-set? x))))
(define normalize-set
  (lambda (x es s)
    (cond
      ((empty-set? x) (if (null? es) empty-set (make-set empty-set es)))
      ((non-empty-set? x)
       (normalize-set (walk (set-base x) s)
                      (append (set-mems x) es)
                      s))
      ((and (var? x) (not (null? es))) (make-set x es))
      ((and (symbol? x) (not (null? es))) (make-set x es)))))
(define set-tail
  (lambda (x)
    (cond
      ((empty-set? x) #f)
      ((non-empty-set? x) (set-base x))
      ((var? x) x))))
(define with-set-tail
  (lambda (t x)
    (cond
      ((non-empty-set? x) (make-set t (set-mems x)))
      ((var? x) t))))
(define non-empty-set-first
  (lambda (x) (car (set-mems x))))
(define non-empty-set-rest
  (lambda (x)
    (let ((rest-mems (cdr (set-mems x))))
      (if (null? rest-mems)
          (set-base x)
          (make-set (set-base x) rest-mems)))))
(define set->list (lambda (x) (cons (set-base x) (set-mems x))))
(define list->set (lambda (x) (make-set (car x) (cdr x))))
(define check-constraints
  (lambda (C->c with-C-c apply-c)
    (lambda (s)
      (let loop ((cs (C->c (s->C s))) (s (with-C s (with-C-c (s->C s) '()))))
        (cond
          ((not s) #f)
          ((null? cs) s)
          (else (loop (cdr cs) (bind s (apply-c (car cs))))))))))
(define tree-collect
  (lambda (predicate tree acc)
    (let ((acc (if (predicate tree)
                 (cons tree acc)
                 acc)))
      (cond
        ((pair? tree)
         (tree-collect predicate (cdr tree)
           (tree-collect predicate (car tree) acc)))
        ((non-empty-set? tree)
         (tree-collect predicate (set->list tree) acc))
        (else acc)))))
(define infer-sets
  (lambda (vs)
    (lambda (s)
      (let loop ((vs (tree-collect non-empty-set? vs '())) (s s))
        (cond
          ((not s) #f)
          ((null? vs) s)
          (else (loop (cdr vs) (bind s (seto (car vs))))))))))
(define run-constraints
  (lambda (s . vs)
    (let* ((s (with-C s (walk* (s->C s) s)))
           (s (bind s (infer-sets vs)))
           (s (bind s (check-constraints C->set with-C-set seto)))
           (s (bind s (check-constraints C->symbol with-C-symbol symbolo)))
           (s (bind s (check-constraints C->union with-C-union (lambda (args) (apply uniono args)))))
           (s (bind s (check-constraints C->disj with-C-disj (lambda (args) (apply disjo args)))))
           (s (bind s (check-constraints C->!in with-C-!in (lambda (args) (apply !ino args)))))
           (s (bind s (check-constraints C->=/= with-C-=/= (lambda (args) (apply =/= args))))))
      s)))

(define walk
  (lambda (u s)
    (cond
      ((and (var? u) (assq u (s->S s))) =>
       (lambda (pr) (walk (rhs pr) s)))
      (else u))))

(define ext-s
  (lambda (x v s)
    (run-constraints (with-S s (cons `(,x . ,v) (s->S s))) v)))

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
          (occurs-check x (set->list v) s))
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
           (list->set (walk* (set->list ns) s))))
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
         (reify-s (set->list v) s))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define join
  (lambda (a b)
    (cond
      ((null? a) b)
      ((member (car a) b) (join (cdr a) b))
      (else (join (cdr a) (cons (car a) b))))))
(define reified-sort
  (lambda (x)
    (sort (lambda (s1 s2) (string<? (format "~a" s1) (format "~a" s2))) (join x '()))))
(define reify-some-constraints
  (lambda (tag C->c)
    (lambda (s r others)
      (let ((cs (filter (lambda (x) (null? (tree-collect (lambda (v) (var? v)) x '()))) (map (lambda (x) (walk* x r)) (C->c (s->C s))))))
        (if (null? cs)
          others
          (cons (cons tag (reified-sort cs))
            others))))))
(define reify-symbol-constraints
  (reify-some-constraints 'sym C->symbol))
(define reify-set-constraints
  (reify-some-constraints 'set C->set))
(define reify-neq-constraints
  (reify-some-constraints '=/= C->=/=))
(define reify-!in-constraints
  (reify-some-constraints '!in C->!in))
(define reify-union-constraints
  (reify-some-constraints 'union C->union))
(define reify-disj-constraints
  (reify-some-constraints 'disj C->disj))
(define reify-constraints
  (lambda (s r)
    (reify-symbol-constraints s r
      (reify-set-constraints s r
        (reify-neq-constraints s r
          (reify-!in-constraints s r
            (reify-union-constraints s r
              (reify-disj-constraints s r '()))))))))
(define normalize-all-reified-sets
  (lambda (x)
    (cond
      ((empty-set? x) '∅)
      ((non-empty-set? x)
       (let ((v (set-map normalize-all-reified-sets x)))
         (let ((v (normalize-set (set-base v) (set-mems v) empty-s)))
           `(set ,(set-base v) ,@(reified-sort (set-mems v))))))
      ((pair? x)
       (cons
         (normalize-all-reified-sets (car x))
         (normalize-all-reified-sets (cdr x))))
      (else x))))
(define reify
  (lambda (v s)
    (let* ((v (walk* v s))
           (r (reify-s v empty-s))
           (vr (walk* v r))
           (Cr (reify-constraints s r)))
      (normalize-all-reified-sets
        (if (null? Cr)
          vr
          `(,vr : ,@Cr))))))

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
            run-constraints
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

(define unify
  (lambda (ou ov s)
    (let ((u (walk ou s))
          (v (walk ov s)))
      (cond
        ((and (var? v) (not (var? u))) (unify v u s))
        ((eq? u v) s)
        ((and (var? u) (not (occurs-check u v s))) (ext-s u v s))
        ((and (pair? u) (pair? v))
         (let ((s (unify
                    (car u) (car v) s)))
           (bind s (== (cdr u) (cdr v)))))
        ((non-empty-set? v)
          (let* ((vns (normalize-set v '() s))
                 (x (set-tail vns)))
            (cond
              ((and (var? u) (eq? x u)
                    (not (occurs-check u (set-mems vns) s)))
                (bind s
                  (fresh (n)
                    (== u (with-set-tail n vns))
                    (seto n))))
              ((non-empty-set? u)
               (let ((uns (normalize-set u '() s)))
                 (if (not (eq? (set-tail uns) (set-tail vns)))
                   (let ((tu (non-empty-set-first uns))
                         (ru (non-empty-set-rest uns))
                         (tv (non-empty-set-first vns))
                         (rv (non-empty-set-rest vns)))
                     (bind s
                       (conde
                         ((== tu tv) (== ru rv) (seto ru))
                         ((== tu tv) (== uns rv))
                         ((== tu tv) (== ru vns))
                         ((fresh (n)
                            (== ru (set n tv))
                            (== (set n tu) rv)
                            (seto n))))))
                   ((let ((t0 (non-empty-set-first uns))
                          (ru (non-empty-set-rest uns)))
                      (bind s
                        (let ((b (set-base vns)))
                          (let loopj ((j (set-mems vns))
                                      (acc '()))
                            (if (null? j)
                                fail
                                (let ((cj (cdr j)))
                                  (let ((tj (car j))
                                        (rj (if (and (null? acc) (null? cj))
                                                b
                                                (make-set b (append acc cj)))))
                                    (conde
                                     ((== t0 tj) (== ru rj) (seto ru))
                                     ((== t0 tj) (== uns rj))
                                     ((== t0 tj) (== ru vns))
                                     ((fresh (n)
                                             (== x (set n t0))
                                             (== (with-set-tail n ru) (with-set-tail n vns))
                                             (seto n)))
                                     ((loopj cj (cons tj acc)))))))))))))))
              (else #f))))
        ((equal? u v) s)
        (else #f)))))

(define ==
  (lambda (u v)
    (lambdag@ (s)
      (unify u v s))))

(define ino
  (lambda (t x)
    (lambda (s)
      (let ((s (bind s (seto x))))
        (if (not s)
          #f
          (let ((x (walk x s)))
            (cond
              ((empty-set? x) #f)
              ((non-empty-set? x)
                (let ((tx (non-empty-set-first x))
                      (rx (non-empty-set-rest x)))
                  (bind s
                    (conde
                      ((== tx t))
                      ((ino t rx))))))
              ((var? x)
                (bind s
                  (fresh (n)
                    (== x (set n t))
                    (seto n))))
              (else #f))))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (vector? x)))))

(define =/=
  (lambda (ou ov)
    (lambda (s)
      (let ((u (walk ou s))
            (v (walk ov s)))
        (cond
          ((and (var? v) (not (var? u)))
           (bind s (=/= v u)))
          ((and (pair? u) (pair? v))
            (bind s
              (conde
                ((=/= (car u) (car v)))
                ((=/= (cdr u) (cdr v))))))
          ((eq? u v) #f)
          ((and (atom? u) (atom? v))
           (if (equal? u v)
             #f
             s))
          ((and (var? u) (occurs-check u v s))
           (if (non-empty-set? v)
             (let* ((vns (normalize-set v '() s))
                    (vts (set-mems vns)))
               (if (occurs-check u vts s)
                 s
                 (begin
                   (assert (eq? u (set-base vns)))
                   (bind s
                     (let loop ((vts vts))
                       (if (null? vts)
                         fail
                         (conde
                           ((!ino (car vts) u))
                           ((loop (cdr vts))))))))))
             s))
          ((and (non-empty-set? u) (non-empty-set? v))
           (bind s
             (fresh (n)
               (conde
                 ((ino n u) (!ino n v))
                 ((ino n v) (!ino n u))))))
          ((and ;; cases seem to be missing from Fig. 5 (?)
             (not (var? u))
             (not (var? v))
             (or
               (not (eq? (empty-set? u) (empty-set? v)))
               (not (eq? (non-empty-set? u) (non-empty-set? v)))
               (not (eq? (atom? u) (atom? v)))
               (not (eq? (pair? u) (pair? v)))))
            s)
          ;; NOTE handling cases (6) and (7) of uniono (Fig. 7)
          ((and (var? u)
             (exists
               (lambda (c)
                 (and
                   (exists (lambda (x) (eq? u (walk x s))) c)
                   (not (eq? (walk (car c) s) (walk (cadr c) s)))))
               (C->union (s->C s))))
            (bind s
              (fresh (n)
                (conde
                  ((ino n u) (!ino n v))
                  ((ino n v) (!ino n u))
                  ((== u empty-set) (=/= v empty-set))))))
          (else
            (with-C s (with-C-=/= (s->C s) (cons `(,u ,v) (C->=/= (s->C s)))))))))))

(define !ino
  (lambda (t x)
    (lambda (s)
      (let ((s (bind s (seto x))))
        (if (not s)
          #f
          (let ((x (walk x s)))
            (cond
              ((empty-set? x) s)
              ((non-empty-set? x)
                (let ((tx (non-empty-set-first x))
                      (rx (non-empty-set-rest x)))
                  (bind s
                    (fresh ()
                      (=/= tx t)
                      (!ino t rx)))))
              ((and (var? x) (occurs-check x t s))
                s)
              (else
                (with-C s (with-C-!in (s->C s) (cons `(,t ,x) (C->!in (s->C s)))))))))))))

(define uniono
  (lambda (x y z)
    (lambda (s)
      (let ((s (((fresh () (seto x) (seto y) (seto z)) s))))
        (if (not s)
          #f
          (let ((x (walk x s))
                (y (walk y s))
                (z (walk z s)))
            (cond
              ((eq? x y) (bind s (== x z)))
              ((empty-set? z) (bind s (fresh () (== x z) (== y z))))
              ((empty-set? x) (bind s (== y z)))
              ((empty-set? y) (bind s (== x z)))
              ((non-empty-set? z)
               (let ((tz (non-empty-set-first z)))
                 (bind s
                   (fresh (n)
                    (== z (set n tz))
                    (!ino tz n)
                    (conde
                      ((fresh (n1)
                         (== x (set n1 tz))
                         (!ino tz n1)
                         (uniono n1 y n)))
                      ((fresh (n1)
                         (== y (set n1 tz))
                         (!ino tz n1)
                         (uniono x n1 n)))
                      ((fresh (n1 n2)
                         (== x (set n1 tz))
                         (!ino tz n1)
                         (== y (set n2 tz))
                         (!ino tz n2)
                         (uniono n1 n2 n))))))))
              ((or (non-empty-set? x) (non-empty-set? y))
               (let-values (((x y) (if (non-empty-set? x) (values x y) (values y x))))
                 (let ((tx (non-empty-set-first x)))
                   (bind s
                     (fresh (n n1)
                       (== x (set n1 tx))
                       (!ino tx n1)
                       (== z (set n tx))
                       (!ino tx n)
                       (conde
                         ((!ino tx y) (uniono n1 y n))
                         ((fresh (n2)
                            (== y (set n2 tx))
                            (!ino tx n2)
                            (uniono n1 n2 n)))))))))
              ;; NOTE cases (6) and (7) in Fig. 7 are handled in =/=
              (else (with-C s (with-C-union (s->C s) (cons `(,x ,y ,z) (C->union (s->C s)))))))))))))

(define disjo
  (lambda (x y)
    (lambda (s)
      (let ((s (((fresh () (seto x) (seto y)) s))))
        (if (not s)
          #f
          (let ((x (walk x s))
                (y (walk y s)))
            (cond
              ((empty-set? x) s)
              ((empty-set? y) s)
              ((and (var? x) (eq? x y)) (bind s (== x empty-set)))
              ((or (and (non-empty-set? x) (var? y))
                   (and (non-empty-set? y) (var? x)))
               (let-values (((x y) (if (non-empty-set? x) (values x y) (values y x))))
                 (let ((tx (non-empty-set-first x))
                       (rx (non-empty-set-rest x)))
                   (bind s
                     (fresh ()
                       (!ino tx y)
                       (disjo y rx))))))
              ((and (non-empty-set? x) (non-empty-set? y))
               (let ((tx (non-empty-set-first x))
                     (rx (non-empty-set-rest x))
                     (ty (non-empty-set-first y))
                     (ry (non-empty-set-rest y)))
                 (bind s
                   (fresh ()
                     (=/= tx ty)
                     (!ino tx ry)
                     (!ino ty rx)
                     (disjo rx ry)))))
              (else
                (assert (and (var? x) (var? y)))
                (with-C s (with-C-disj (s->C s) (cons `(,x ,y) (C->disj (s->C s)))))))))))))

(define !uniono
  (lambda (x y z)
    (lambda (s)
      (let ((s (((fresh () (seto x) (seto y) (seto z)) s))))
        (if (not s)
          #f
          (let ((x (walk x s))
                (y (walk y s))
                (z (walk z s)))
            (bind s
              (fresh (n)
                (conde
                  ((ino n z) (!ino n x) (!ino n y))
                  ((ino n x) (!ino n z))
                  ((ino n y) (!ino n z)))))))))))

(define !disjo
  (lambda (x y)
    (lambda (s)
      (let ((s (((fresh () (seto x) (seto y)) s))))
        (if (not s)
          #f
          (let ((x (walk x s))
                (y (walk y s)))
            (bind s
              (fresh (n)
                (ino n x)
                (ino n y)))))))))

(define seto
  (lambda (x)
    (lambda (s)
      (let ((x (walk x s)))
        (cond
          ((var? x)
            (if (exists
                  (lambda (y) (eq? x (walk y s)))
                  (C->symbol (s->C s)))
              #f
              (with-C s (with-C-set (s->C s) (cons x (C->set (s->C s)))))))
          ((empty-set? x) s)
          ((non-empty-set? x) (bind s (seto (vector-ref x 0))))
          (else #f))))))

(define symbolo
  (lambda (x)
    (lambda (s)
      (let ((x (walk x s)))
        (cond
          ((var? x)
            (if (exists
                  (lambda (y) (eq? x (walk y s)))
                  (C->set (s->C s)))
              #f
              (with-C s (with-C-symbol (s->C s) (cons x (C->symbol (s->C s)))))))
          ((symbol? x) s)
          (else #f))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
      (g succeed)
      ((== #f #f) fail))))
