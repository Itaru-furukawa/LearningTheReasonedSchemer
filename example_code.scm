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
