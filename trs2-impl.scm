;;; Copyright © 2018 Daniel P. Friedman, William E. Byrd, Oleg Kiselyov, and Jason Hemann
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Implementation of the language used in 'The Reasoned Schemer,
;;; Second Edition,' by Friedman, Byrd, Kiselyov, and Hemann (MIT
;;; Press, 2018).

;;; Definitions are presented in the order in which they appear in
;;; Chapter 10 and Appendix A.


;;; Here are the key parts of Chapter 10

(define sub1
  (lambda (n)
    (- n 1)))

(define var (lambda (x) (vector x)))
(define var? (lambda (x) (vector? x)))

(define empty-s '())

(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (cond
      ((pair? a) (walk (cdr a) s))
      (else v))))

(define (ext-s x v s)
  (cond
    ((occurs? x v s) #f)
    (else (cons `(,x . ,v) s))))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eqv? v x))
      ((pair? v) 
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s)))
      (else #f))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((eqv? u v) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s
           (unify (cdr u) (cdr v) s))))
      (else #f))))

(define (== u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s `(,s) '()))))

(define succeed
  (lambda (s)
    `(,s)))
 
(define fail
  (lambda (s)
    '()))

(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf) 
     (cons (car s-inf)
       (append-inf (cdr s-inf) t-inf)))
    (else (lambda () 
            (append-inf t-inf (s-inf))))))

(define (take-inf n s-inf)
  (cond
    ((and n (zero? n)) '())
    ((null? s-inf) '())
    ((pair? s-inf) 
     (cons (car s-inf)
       (take-inf (and n (sub1 n))
         (cdr s-inf))))
    (else (take-inf n (s-inf)))))

(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
       (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))

(define (call/fresh name f)
  (f (var name)))

(define (reify-name n)
  (string->symbol
    (string-append "_"
      (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons
         (walk* (car v) s)
         (walk* (cdr v) s)))
      (else v))))

; 'project' is defined in the frame 10:98 on page 166.
(define-syntax project
  (syntax-rules ()
    ((project (x ...) g ...)
     (lambda (s)
       (let ((x (walk* x s)) ...)
         ((conj g ...) s))))))

(define (reify-s v r)
  (let ((v (walk v r)))
    (cond
      ((var? v)
       (let ((n (length r)))
         (let ((rn (reify-name n)))
           (cons `(,v . ,rn) r))))
      ((pair? v)
       (let ((r (reify-s (car v) r)))
         (reify-s (cdr v) r)))
      (else r))))

(define (reify v)
  (lambda (s)
    (let ((v (walk* v s)))
      (let ((r (reify-s v empty-s)))
        (walk* v r)))))

(define (run-goal n g)
  (take-inf n (g empty-s)))

(define (ifte g1 g2 g3)
  (lambda (s)
    (let loop ((s-inf (g1 s)))
      (cond
        ((null? s-inf) (g3 s))
        ((pair? s-inf)
         (append-map-inf g2 s-inf))
        (else (lambda ()
                (loop (s-inf))))))))

(define (once g)
  (lambda (s)
    (let loop ((s-inf (g s)))
      (cond
        ((null? s-inf) '())
        ((pair? s-inf)
         (cons (car s-inf) '()))
        (else (lambda ()
                (loop (s-inf))))))))


;;; Here are the key parts of Appendix A

(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj2 g0 (disj g ...)))))

(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))

(define-syntax run
  (syntax-rules ()
    ((run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== `(,x0 ,x ...) q) g ...)))
    ((run n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
         (run-goal n (conj g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))

(define-syntax fresh
  (syntax-rules ()
    ((fresh () g ...) (conj g ...))
    ((fresh (x0 x ...) g ...)
     (call/fresh 'x_0
       (lambda (x0)
         (fresh (x ...) g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))

(define-syntax conda
  (syntax-rules ()
    ((conda (g0 g ...)) (conj g0 g ...))
    ((conda (g0 g ...) ln ...)
     (ifte g0 (conj g ...) (conda ln ...)))))

(define-syntax condu
  (syntax-rules ()
    ((condu (g0 g ...) ...)
     (conda ((once g0) g ...) ...))))

;;; Here are the simple difinitions of chapter 1

(defrel (teacupo t)
  (disj (== 'tea t) (== 'cup t)))

;;; Here are the simple difinitions of chapter 2

(defrel (caro p a)
  (fresh (d)
         (== (cons a d) p)))

(defrel (cdro p d)
  (fresh (a)
         (== (cons a d) p)))

(defrel (conso a d p)
  (caro p a)
  (cdro p d))

(defrel (conso-2nd a d p)
  (== `(,a . ,d) p))

(defrel (nullo x)
  (== '() x))

(defrel (pairo p)
  (fresh (a d)
         (conso a d p)))

(define (singleton? l)
  (cond
    ((pair? l) (null? (cdr l)))
    (else #f)))

(defrel (singletono l)
  (conde
   ((pairo l)
    (fresh (d)
           (cdro l d)
           (nullo d)))
    (succeed fail)))

(defrel (singletono-2nd l)
  (fresh (d)
         (cdro l d)
         (nullo d)))

;;; Here are the simple difinitions of chapter 3

(define (mylist? l)
  (cond
    ((null? l) #t)
    ((pair? l) (list? (cdr l)))
    (#t #f)))

(defrel (listo l)
  (conde
   ((nullo l) succeed)
   ((pairo l)
    (fresh (d)
           (cdro l d)
           (listo d)))
   (succeed fail)))

(defrel (listo-2nd l)
  (conde
   ((nullo l))
   ((fresh (d)
           (cdro l d)
           (listo d)))))

(define (lol? l)
  (cond
    ((null? l) #t)
    ((list? (car l))
     (lol? (cdr l)))
    (#t #f)))

(defrel (lolo l)
  (conde
   ((nullo l))
   ((fresh (a)
           (caro l a)
           (listo a))
    (fresh(d)
          (cdro l d)
          (lolo d)))))

(defrel (singletono-3rd l)
  (fresh (a)
         (== `(,a) l)))

(defrel (loso l)
  (conde
   ((nullo l))
   ((fresh (a)
           (caro l a)
           (singletono a))
    (fresh (d)
           (cdro l d)
           (loso d)))))

(define (member? x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) #t)
    (#t (member? x (cdr l)))))

(defrel (membero x l)
  (conde
   ((caro l x))
   ((fresh (d)
           (cdro l d)
           (membero x d)))))

(defrel (proper-membero x l)
  (conde
   ((caro l x)
    (fresh (d)
           (cdro l d)
           (listo d)))
   ((fresh (d)
           (cdro l d)
           (proper-membero x d)))))

;;; Here are the simple difinitions of chapter 4

(define (append2 l t)
  (cond
    ((null? l) t)
    (#t (cons (car l)
              (append2 (cdr l) t)))))

(defrel (appendo l t out)
  (conde
   ((nullo l) (== t out))
   ((fresh (a d res)
           (conso a d l)
           (appendo d t res)
           (conso a res out)))))

(defrel (appendo2 l t out)
  (conde
   ((nullo l) (== t out))
   ((fresh (a d res)
           (conso a d l)
           (conso a res out)
           (appendo d t res)))))

(defrel (swappendo l t out)
  (conde
   ((fresh (a d res)
           (conso a d l)
           (conso a res out)
           (swappendo d t res)))
   ((nullo l) (== t out))))

(define (unwrap x)
  (cond
    ((pair? x) (unwrap (car x)))
    (#t x)))

(defrel (unwrapo x out)
  (conde
   ((fresh (a)
           (caro x a)
           (unwrapo a out)))
   ((== x out))))

;;; Here are the simple difinitions of chapter 5

(define (mem x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) l)
    (#t (mem x (cdr l)))))

(defrel (memo x l out)
  (conde
   ((caro l x) (== l out))
   ((fresh (d)
           (cdro l d)
           (memo x d out)))))

(define (rember x l)
  (cond
    ((null? l) '())
    ((equal? (car l) x) (cdr l))
    (#t (cons (car l) (rember x (cdr l))))))

(defrel (rembero x l out)
  (conde
   ((nullo l) (== '() out))
   ((conso x out l))
   ((fresh (a d res)
           (conso a d l)
           (conso a res out)
           (rembero x d res)))))

