;ch1 solutions

;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
            (* 3 (- 6 2) (- 2 7))) ; -47/150

;1.3
(defn sum-max2 [a b c]
  (- (+ a b c) (min a b c)))
;1.3 test
(println (sum-max2 3 1 2))

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
(println (sqrt 16))

