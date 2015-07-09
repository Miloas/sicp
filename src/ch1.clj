;ch1 solutions

;test fn
(defn tt [x] (println x))

;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
            (* 3 (- 6 2) (- 2 7))) ; -47/150

;1.3
(defn sum-max2 [a b c]
  (- (+ a b c) (min a b c)))

;1.3 test
(tt (sum-max2 3 1 2))

;1.7
(defn sqrt [x]
  (letfn [(good-enough? [last-guess this-guess] (< (Math/abs (- last-guess this-guess)) 0.0000001))
          (average [x y] (/ (+ x y) 2))
          (improve [guess] (average guess (/ x guess)))
          (help [guess]
            (let [new-guess (improve guess)]
              (if (good-enough? guess new-guess) new-guess (help new-guess))))]
    (help 1.0)))

;1.7 test
(tt (sqrt 16))

;1.8
(defn cube-root [x]
  (letfn [(good-enough? [last-guess this-guess] (< (Math/abs (- last-guess this-guess)) 0.0000001))
          (improve [guess] (/ (+ (/ x (* guess guess)) (* 2 guess))) 3)
          (help [guess]
            (let [new-guess (improve guess)]
              (if (good-enough? guess new-guess) new-guess (help new-guess))))]
    (help 1.0)))

;1.8 test
(tt (cube-root 27))

;1.11
(defn f-rec [n]
  (if (< n 3) n
    (+ (f-rec (- n 1))
       (f-rec (- n 2))
       (f-rec (- n 3)))))

(defn f-iter [n]
  (letfn [(help [a b c cnt]
            (if (= cnt n) (+ a b c) (help b c (+ a b c) (+ cnt 1))))]
    (if (< n 3) n (help 0 1 2 3))))

;1.11test
(tt (f-rec 2))
(tt (f-rec 5))
(tt (f-iter 2))
(tt (f-iter 5))

;1.12
(defn pascal [r c]
  (cond (or (<= r 0) (<= c 0) (> c r)) 0
        (or (= r 1) (= r 2) (= r c)) 1
        :else (+ (pascal (- r 1) c) (pascal (- r 1) (- c 1)))
    ))
(tt (pascal 4 2))
