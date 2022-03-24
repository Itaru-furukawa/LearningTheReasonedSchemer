; 1-7
(run* q
        fail)

; 1-18
(run* q
        succeed)

; 1-19
(run* q (== 'pea 'pea))

; 1-20
(run* q (== q q))

; 1-23
(run* q
        (fresh (x)
               (== 'pea x)))

; 1-25
(run* q
        (fresh (x)
               (== (cons x '()) q)))

; 1-38
(run* q
      (fresh (x)
              (fresh (y)
                    (== `(,q ,y) `((,x ,y) ,x)))))

; 1-50
(run* q
        (conj succeed succeed))

; 1-52
(run* q
        (conj fail (== 'corn q)))

; 1-53
(run* q
        (conj (== 'corn q) (== 'meal q)))

; 1-54
(run* q
        (conj (== 'corn q) (== 'corn q)))

; 1-56
(run* q
        (disj (== 'olive q) fail))

; 1-57
(run* q
        (disj fail (== 'oil q)))

; 1-65
(run* x
        (disj
         (conj (== 'virgin x) fail)
         (disj
          (== 'olive x)
          (disj
           succeed
           (== 'oil x)))))

; 1-67
(run* r
        (fresh (x)
               (fresh (y)
                      (conj
                       (== 'split x)
                       (conj
                        (== 'pea y)
                        (== `(,x ,y) r))))))

; 1-82
(run* x
        (teacupo x))

; 1-86
(run* (x y)
       (teacupo x)
       (teacupo x))

; 1-88
(run* (x y)
        (disj
         (conj (== 'split x) (== 'pea y))
         (conj (== 'red x) (== 'bean y))))

(run* (x y)
        (conde
         ((== 'split x) (== 'pea y))
         ((== 'red x) (== 'bean y))))

; 1-91
(run* (x y)
        (conde
         ((== 'split x) (== 'pea y))
         ((== 'red x) (== 'bean y))
         ((== 'green x) (== 'lentil y))))

; 2-4
(run* x
        (caro '(a c o r n) 'a))

; 2-8
(run* r
        (fresh (x y)
               (caro '(grape raisin pear) x)
               (caro '((a) (b) (c)) y)
               (== (cons x y) r)))

; 2-16
(run* q
        (cdro '(a c o r n) '(c o r n)))

; 2-22
(run* x
        (conso x `(a ,x c) `(d a ,x c)))

(run* x
        (conso-2nd x `(a ,x c) `(d a ,x c)))

; 2-27
(run* l
        (fresh (d t x y w)
               (conso w '(n u s) t)
               (cdro l t)
               (caro l x)
               (== 'b x)
               (cdro l d)
               (caro d x)
               (== 'o y)))

; 2-32
(run* x
        (nullo x))

; 2-50
(run* q
        (pairo q))

; 2-52
(run* r
        (pairo (cons r '())))

; 3-12
(run* x
        (listo `(a b ,x d)))

; 3-23
(run* q
        (fresh (x y)
               (lolo `((a b) (,x c) (d ,y)))))

; 3-24
(run 1 l
       (lolo l))

; 3-27
(run* x
        (lolo `((a b) (c d) . ,x)))

; (3-32)
(run* q
        (singletono '(a)))

(run* q
       (singletono-3rd '((g) (e tofu))))

; 3-34
(run* z
        (loso `((g) . ,z)))

; 3-48
(run* q
      (membero 'olive '(virgin olive oil)))

; 3-53
(run* y
        (membero y '(hummuns with pita)))

; 3-55
(run* y
        (membero y '(pear grape . peached)))

; 3-56
(run* x
        (membero 'e `(pasta ,x fagioli)))

; 3-63
(run* q
        (fresh (x y)
               (== `(pasta ,x fagioli ,y) q)
               (membero 'e q)))

; 3-37
(run* q
        (proper-membero q '(a b c)))
