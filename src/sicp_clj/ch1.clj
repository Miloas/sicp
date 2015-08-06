;ch1 solutions
(ns sicp-clj.ch1)

;test fn
(def tt println)

;help fn
(defn square [x] (* x x))
(defn mydouble [x] (+ x x))
(defn halve [x] (/ x 2))
(defn cube [x] (* x x x))

;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ; -47/150

;1.3
(defn sum-max2 [a b c]
  (- (+ a b c) (min a b c)))

;1.7
(defn sqrt [x]
  (letfn [(good-enough? [last-guess this-guess] (< (Math/abs (- last-guess this-guess)) 0.0000001))
          (average [x y] (halve (+ x y)))
          (improve [guess] (average guess (/ x guess)))
          (help [guess]
                (let [new-guess (improve guess)]
                  (if (good-enough? guess new-guess) new-guess (help new-guess))))]
    (help 1.0)))

;1.8
(defn cube-root [x]
  (letfn [(good-enough? [last-guess this-guess] (< (Math/abs (- last-guess this-guess)) 0.0000001))
          (improve [guess] (/ (+ (/ x (square guess)) (* 2 guess))) 3)
          (help [guess]
                (let [new-guess (improve guess)]
                  (if (good-enough? guess new-guess) new-guess (help new-guess))))]
    (help 1.0)))

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


;1.12
(defn pascal [r c]
  (cond (or (<= r 0) (<= c 0) (> c r)) 0
        (or (= r 1) (= r 2) (= r c)) 1
        :else (+ (pascal (- r 1) c) (pascal (- r 1) (- c 1)))
        ))

;1.16
(defn fast-exp-iter [b n]
  ((fn help [a b n]
     (cond (= n 0) a
           (even? n) (help a (square b) (halve n))
           :else (help (* a b) b (dec n))))
    1 b n))


;1.17
(defn fast-mult-rec [a b]
  (cond (or (= a 0) (= b 0)) 0
        (= a 1) b
        (= b 1) a
        (even? b) (mydouble (fast-mult-rec a (halve b)))
        :else (+ a (fast-mult-rec a (- b 1)))))

;1.18
(defn fast-mult-iter [a b]
  ((fn help [a b result]
     (cond (or (= b 0) (= a 0)) result
           (= b 1) a
           (even? b) (help (mydouble a) (halve b) result)
           :else (help a (- b 1) (+ result a))))
    a b 0))

;1.19
(defn fib [n]
  (letfn [(help [a b p q count]
                    (cond (= count 0) b
                          (even? count) (help a b
                                                  (+ (* p p) (* q q))
                                                  (+ (* q q) (* 2 p q))
                                                  (/ count 2))
                          :else (help (+ (* b q) (* a q) (* a p))
                                          (+ (* b p) (* a q))
                                          p
                                          q
                                          (- count 1))))]
    (help 1 0 0 1 n)))

;1.22
(defn divides? [a b]
  (= (rem b a) 0))
(defn smallest-divisor [n]
  ((fn find-divisor [n test-divisor]
     (cond (> (square test-divisor) n) n
           (divides? test-divisor n) test-divisor
           :else (find-divisor n (inc test-divisor))))
    n 2))
(defn prime? [n]
  (if (= n 1)
    false
    (= n (smallest-divisor n))))
(defn prime-interval [l r]
  "If can't find return r+1"
  (cond (= l (inc r)) (inc r)
        (= l 2) 2
        (and (odd? l) (prime? l)) l
        :else (prime-interval (inc l) r)))

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
  ((fn [a] (= (expmod a x x) a))
    (inc (rand-int (dec x)))))
(defn fast-prime? [x times]
  (cond (= times 0) true
        (fermat-test x) (fast-prime? x (dec times))
        :else false))

;1.27
(defn carmichael? [x]
  (and (not (prime? x))
       (every? #(= (expmod % x x) %) (range 1 x))))

;1.28
(defn miller-robin-expmod [a n m]
  (cond (= n 0) 1
        (even? n) (let [itr (miller-robin-expmod a (halve n) m)
                        sqr (square itr)]
                    (if (and (not= itr 1)
                             (not= itr (dec m))
                             (= 1 (rem sqr m)))
                      0
                      (rem sqr m)))
        :ekse (rem (* a (miller-robin-expmod a (dec n) m)) m)))
(defn miller-robin-test [n]
  (= (miller-robin-expmod (inc (rand-int (dec n))) (dec n) n) 1))
(defn miller-robin-prime? [n times]
  (if (= times 0) true
                  (if (miller-robin-test n) (miller-robin-prime? n (dec times)) false)))

;1.29
(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))
(defn simp-integral [f a b n]
  (let [h (/ (- b a) n)]
    (letfn [(g [x] (f (+ a (* x h))))
            (myterm [x]
                    (* (g x) (if (even? x) 2 4)))]
      (/ (* h (+ (f a) (f b) (sum myterm 1 inc (dec n)))) 3))))

;(tt (simp-integral cube 0 1 100))

;1.30
(defn sum-iter [term a next b]
  (loop [a a
         acc 0]
    (if (> a b) acc
                (recur (next a) (+ acc (term a))))))

;1.31
(defn product [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))
(defn product-iter [term a next b]
  (loop [a a
         result 1]
    (if (> a b) result
                (recur (next a) (* result (term a))))))
(defn factorial [n]
  (product-iter identity 1 inc n))
(defn cal-pi [n]
  (letfn [(next [x] (+ x 2))]
    (let [a (/ (mydouble (product-iter square 4 next (mydouble (inc n)))) (mydouble (inc n)))
          b (product-iter square 3 next (inc (mydouble n)))]
      (* 4 (/ a b)))))

;1.32
(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(defn accumulate-iter [combiner null-value term a next b]
  (loop [a a
         result null-value]
    (if (> a b)
      result
      (recur (next a) (combiner result (term a))))))
(defn sum-acc [term a next b]
  (accumulate-iter + 0 term a next b))
(defn product-acc [term a next b]
  (accumulate-iter * 1 term a next b))

;1.33
(defn sum-prime-interval [a b]
  (reduce + (filter prime? (range a b))))
(defn gcd [a b]
  (if (= b 0) a (gcd b (rem a b))))
(defn sum-prime-smaller [n]
  (reduce + (filter prime? (range n))))

;1.35
(def tolerance 0.00001)
(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
                         (< (Math/abs (- v1 v2)) tolerance))
          (try-guess [guess]
                     (let [next (f guess)]
                       (if (close-enough? guess next)
                         next
                         (try-guess next))))]
    (try-guess first-guess)))
(def phi (fixed-point (fn [x] (inc (/ 1 x))) 1.0))

;1.36
(defn fixed-point2 [f first-guess]
  (letfn [(close-enough? [v1 v2]
                         (< (Math/abs (- v1 v2)) tolerance))
          (try-guess [guess]
                     (println "guess: " guess)
                     (let [next (f guess)]
                       (if (close-enough? guess next)
                         next
                         (try-guess next))))]
    (try-guess first-guess)))

(def fixed-point-1-36 (fixed-point #(/ (Math/log 1000) (Math/log %)) 1.1))

;1.37
(defn cont-frac [n d k]
  ((fn help [cnt]
     (if (= cnt k)
       (/ (n k) (d k))
       (/ (n cnt) (+ (d cnt) (help (inc cnt))))))
    1))
;(tt (cont-frac (fn [_] 1.0) (fn [_] 1.0) 10))
(defn cont-frac-iter [n d k]
  ((fn help [itr result]
     (if (= itr 1)
       result
       (help (dec itr) (/ (n (dec itr)) (+ (d (dec itr)) result)))))
    k (/ (n k) (d k))))
;(tt (cont-frac-iter (fn [_] 1.0) (fn [_] 1.0) 10))

;1.38
(def e
  (+ 2 (cont-frac-iter (fn [_] 1.0)
                       (fn [n]
                         (cond (= n 1) 1
                               (= n 2) 2
                               (and (= (rem n 3) 0) (= (rem n 3) 1)) 1
                               :else (- n (quot n 3)))) 100)))

;1.39
(defn tan-cf [x k]
  (letfn [(n [n]
             (if (= n 1) x (- (square x))))
          (d [n]
             (inc (mydouble (dec n))))]
    (cont-frac-iter n d k)))

;1.40
(def dx 0.00001)
(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))
(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn cubic [a b c]
  (fn [x]
    (+ (cube x)
       (* a x x)
       (* b x)
       c)))
;(tt (newtons-method (cubic 1 1 1) 1.0))

;1.41
(defn double-excute [f]
  (fn [x] (f (f x))))

;1.42
(defn compose [f g]
  (fn [x] (f (g x))))

;1.43
(defn repeated [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))

;1.44
(defn smooth [f]
  (fn [x]
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(defn n-smooth [f n]
  (repeated (smooth f) n))

;1.45
(defn average [a b]
  (halve (+ a b)))
(defn average-damp [f]
  (fn [x] (average x (f x))))
(defn log2 [n]
  (int (quot (Math/log n) (Math/log 2))))
(defn n-root [n x]
  (fixed-point ((repeated average-damp (log2 n))
                 (fn [y] (/ x (fast-exp-iter y (dec n))))) 1.0))

;1.46
(defn iterative-improve [good-enough? improve]
  (fn [guess]
    (loop [guess guess]
      (if (good-enough? guess)
        guess
        (recur (improve guess))))))

(defn sqrt2 [x]
  ((iterative-improve
     (fn [guess] (< (Math/abs (- (square guess) x)) tolerance))
     (fn [guess] (average guess (/ x guess))))
    1.0))
;(tt (sqrt2 16))

(defn fixed-point3 [f first-guess]
  ((iterative-improve
     (fn [guess] (< (Math/abs (- (f guess) guess)) tolerance))
     (fn [guess] (f guess))) first-guess))
;(tt (fixed-point3 #(Math/cos %) 1.0))

