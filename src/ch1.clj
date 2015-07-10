;ch1 solutions

;test fn
(defn tt [x] (println x))
(use 'clojure.test)

;help fn
(defn square [x] (* x x))
(defn mydouble [x] (+ x x))
(defn halve [x] (/ x 2))

;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
            (* 3 (- 6 2) (- 2 7))) ; -47/150

;1.3
(defn sum-max2 [a b c]
  (- (+ a b c) (min a b c)))

;1.3 test
(deftest test-1-3
  (is (= 5 (sum-max2 3 1 2))))
(test-1-3)

;1.7
(defn sqrt [x]
  (letfn [(good-enough? [last-guess this-guess] (< (Math/abs (- last-guess this-guess)) 0.0000001))
          (average [x y] (halve (+ x y)))
          (improve [guess] (average guess (/ x guess)))
          (help [guess]
            (let [new-guess (improve guess)]
              (if (good-enough? guess new-guess) new-guess (help new-guess))))]
    (help 1.0)))

;1.7 test
(deftest test-1-7
  (is (= 4.0 (sqrt 16))))
(test-1-7)

;1.8
(defn cube-root [x]
  (letfn [(good-enough? [last-guess this-guess] (< (Math/abs (- last-guess this-guess)) 0.0000001))
          (improve [guess] (/ (+ (/ x (square guess)) (* 2 guess))) 3)
          (help [guess]
            (let [new-guess (improve guess)]
              (if (good-enough? guess new-guess) new-guess (help new-guess))))]
    (help 1.0)))

;1.8 test
(deftest test-1-8
  (is (= 3 (cube-root 27))))
(test-1-8)

;1.11
(defn f-rec [n]
  (if (< n 3) n
    (+ (f-rec (- n 1))
       (f-rec (- n 2))
       (f-rec (- n 3)))))

(defn f-iter [n]
  (letfn [(help [a b c cnt]
            (if (= cnt n) (+ a b c) (help b c (+ a b c) (inc cnt))))]
    (if (< n 3) n (help 0 1 2 3))))

;1.11 test
(deftest test-1-11
  (is (= 2 (f-rec 2)))
  (is (= 11 (f-rec 5)))
  (is (= 2 (f-iter 2)))
  (is (= 11 (f-iter 5))))
(test-1-11)

;1.12
(defn pascal [r c]
  (cond (or (<= r 0) (<= c 0) (> c r)) 0
        (or (= r 1) (= r 2) (= r c)) 1
        :else (+ (pascal (- r 1) c) (pascal (- r 1) (- c 1)))
    ))

;1.12 test
(deftest test-1-12
  (is (= 3 (pascal 4 2))))
(test-1-12)

;1.16
(defn fast-exp-iter [b n]
  (letfn [(help [a b n]
          (cond (= n 0) a
                (even? n) (help a (square b) (halve n))
                :else (help (* a b) b (dec n))))]
    (help 1 b n)))

;1.16 test
(deftest test-1-16
  (is (= 1024 (fast-exp-iter 2 10))))
(test-1-16)

;1.17
(defn fast-mult-rec [a b]
  (cond (or (= a 0) (= b 0)) 0
        (= a 1) b
        (= b 1) a
        (even? b) (mydouble (fast-mult-rec a (halve b)))
        :else (+ a (fast-mult-rec a (- b 1)))))

;1.17 test
(deftest test-1-17
  (is (= (* 3 4) (fast-mult-rec 3 4))))
(test-1-17)

;1.18
(defn fast-mult-iter [a b]
  (letfn [(help [a b result]
            (cond (or (= b 1) (= b 0) (= a 0)) result
              (even? b) (help (mydouble a) (halve b) result)
              :else (help a (- b 1) (+ result a))))]
    (help a b 0)))

;1.18
(deftest test-1-18
  (is (= (* 3 4) (fast-mult-iter 3 4))))

;1.19
(defn fib-iter [a b p q count]
  (cond (= count 0) b
        (even? count) (fib-iter a b (square q) (+ (square q) (* 2 p q)) (halve count))
        :else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b q) (* a q)) p q (dec count))))
(defn fib [n]
  (fib-iter 1 0 0 1 n))

;1.19 test
(deftest test-1-19
  (is (= (+ (fib 3) (fib 4)) (fib 5))))

;1.22
(defn divides? [a b]
  (= (rem b a) 0))
(defn smallest-divisor [n]
  (letfn [(find-divisor [n test-divisor]
            (cond (> (square test-divisor) n) n
              (divides? test-divisor n) test-divisor
              :else (find-divisor n (inc test-divisor))))]
    (find-divisor n 2)))
(defn prime? [n]
  (= n (smallest-divisor n)))
(defn prime-interval [l r]
  "If can't find return r+1"
  (cond (= l (inc r)) (inc r)
        (= l 2) 2
        (and (odd? l) (prime? l)) l
        :else (prime-interval (inc l) r)))

;1.22 test
(deftest test-1-22
  (is (= 2 (prime-interval 2 3))
      (= 23 (prime-interval 20 30))))
(test-1-22)

;1.23
(defn smallest-divisor2 [n]
  (letfn [(next [x]
            (if (= x 2) 3 (+ x 2)))
          (find-divisor [n test-divisor]
            (cond (> (square test-divisor) n) n
              (divides? test-divisor n) test-divisor
              :else (find-divisor n (next test-divisor))))]
    (find-divisor n 2)))
(defn prime2? [n]
  (= n (smallest-divisor2 n)))
(defn prime-interval2 [l r]
  "If can't find return r+1"
  (cond (= l (inc r)) (inc r)
    (= l 2) 2
    (and (odd? l) (prime2? l)) l
    :else (prime-interval2 (inc l) r)))

;1.24
(defn expmod [a n m]
  (cond (= n 0) 1
        (even? n) (rem (square (expmod a (halve n) m)) m)
        :else (rem (* a (expmod a (dec n) m)) m)))
(defn fermat-test [x]
  (letfn [(try-it [a]
            (= (expmod a x x) a))]
    (try-it (inc (rand-int (dec x))))))
(defn fast-prime? [x times]
  (cond (= times 0) true
        (fermat-test x) (fast-prime? x (dec times))
        :else false))

;1.24 test
(deftest test-1-24
  (is (= true (fast-prime? 101 3))
      (= false (fast-prime? 4 1))))
(test-1-24)

;1.27


