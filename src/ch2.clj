;ch2 solutions
(ns sicp.ch2)

;test fn
(defn tt [x] (println x))
(use 'clojure.test)


;2.1
(defn gcd [a b]
  (if (= b 0) a (gcd b (rem a b))))
(defn make-rat [n d]
  (let [v1 (Math/abs (gcd n d))
        v2 (if (neg? d) (- v1) v1)]
    [(/ n v2) (/ d v2)]))

;2.1 test
(deftest test-2-1
  (is (= [-3 4] (make-rat -6 8))))
(test-2-1)

;2.2
(defn make-point [x y]
  [x y])
(defn get-x-coordinate [point]
  (first point))
(defn get-y-coordinate [point]
  (second point))

(defn make-segment [start end]
  [start end])
(defn get-start-point [segment]
  (first segment))
(defn get-end-point [segment]
  (second segment))

(defn print-point [point]
  (println (format "(%d,%d)"
             (get-x-coordinate point)
             (get-y-coordinate point))))

(defn midpoint-segment [segment]
  (let [start-point (get-start-point segment)
        end-point (get-end-point segment)
        mid-x (/ (+ (get-x-coordinate start-point) (get-x-coordinate end-point)) 2)
        mid-y (/ (+ (get-y-coordinate start-point) (get-y-coordinate end-point)) 2)]
    (make-point mid-x mid-y)))

;2.2 test
(deftest test-2-2
  (is (= [2 2] (midpoint-segment (make-segment [1 2] [3 2])))))
(test-2-2)

;2.3

; y ^
;   |   -----------
;   |  |           |
;   |  |           |
;   |  |           |
;   |   -----------
;   |
;   O----------------->
;                     x

(defn make-rec [p1 p2]
  "p1:left-lower point , p2:right-upper point"
  [p1 p2])
(defn get-width [rec]
  (Math/abs (- (get-x-coordinate (second rec)) (get-x-coordinate (first rec)))))
(defn get-height [rec]
  (Math/abs (- (get-y-coordinate (second rec)) (get-y-coordinate (first rec)))))

;(defn make-rec [left lower]
;  "left:left segment, lower:lower segment"
;  [left lower])
;(defn get-width [rec]
;  (Math/abs (- (get-x-coordinate (get-end-point (second rec))) (get-x-coordinate (get-start-point (second rec))))))
;(defn get-height [rec]
;  (Math/abs (- (get-y-coordinate (get-end-point (first rec))) (get-y-coordinate (get-start-point (first rec))))))


(defn circumference-rec [rec]
  (* 2 (+ (get-width rec) (get-height rec))))
(defn area-rec [rec]
  (* (get-width rec) (get-height rec)))

;2.3 test
(deftest test-2-3
  (is (= 4 (circumference-rec (make-rec [1 1] [2 2])))
  (is (= 1 (area-rec (make-rec [1 1] [2 2]))))))
(test-2-3)

;(deftest test-2-3
;  (is (= 4 (circumference-rec (make-rec (make-segment [1 1] [1 2]) (make-segment [1 1] [2 1]))))
;  (is (= 1 (area-rec (make-rec (make-segment [1 1] [1 2]) (make-segment [1 1] [2 1])))))
;    ))
;(test-2-3)

;2.4
(defn cons1 [x y]
  (fn [m] (m x y)))
(defn car1 [z]
  (z (fn [p q] p)))
(defn cdr1 [z]
  (z (fn [p q] q)))

;2.4 test
(deftest test-2-4
  (is (= 2 (car1 (cons1 2 3)))
  (is (= 3 (cdr1 (cons1 2 3))))))
(test-2-4)

;2.5
(defn cons2 [x y]
  (* (Math/pow 2 x)
     (Math/pow 3 y)))
(defn divides-cnt [n d]
  (loop [cnt 0
         n (int n)]
    (if (not= (rem n d) 0) cnt
      (recur (inc cnt) (/ n d)))))
(defn car2 [z]
  (divides-cnt z 2))
(defn cdr2 [z]
  (divides-cnt z 3))

;2.5 test
(deftest test-2-5
  (is (= 2 (car2 (cons2 2 3)))
  (is (= 3 (cdr2 (cons2 2 3))))))
(test-2-5)

;2.6
(def one-church
  (fn [f] (fn [x] (f x))))
(def two-church
  (fn [f] (fn [x] (f (f x)))))
(defn add-church [m n]
  (fn [f] (fn [x] ((m f) ((n f) x)))))
(defn mul-church [m n]
  (fn [f] (fn [x] ((m (n f)) x))))
(defn pow-church [m n]
  (fn [f] (fn [x] (((m n) f) x))))

;2.7
(defn make-interval [x y]
  [x y])
(defn upper-bound [interval]
  (second interval))
(defn lower-bound [interval]
  (first interval))

;2.7 test
(deftest test-2-7
  (is (= 3 (upper-bound (make-interval 2 3))))
  (is (= 2 (lower-bound (make-interval 2 3)))))
(test-2-7)

;2.8
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;2.9
(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(defn div-interval [x y]
  (mul-interval x
    (let [a (upper-bound y)
          b (lower-bound y)]
      (if (> (* a b) 0)
        (make-interval (/ 1.0 a) (/ 1.0 b))
        (throw (Exception. "除数不能为0"))))))

;2.12
;What's the 2.12 meaning ?
()

;2.17
(defn last-pair-iter [lst]
  (loop [[x & xs] lst]
    (if xs (recur xs) (list x))))
(defn last-pair-recur [lst]
  (if (next lst) (recur (rest lst)) (list (first lst))))
(defn last-pair [lst]
  (list (last lst)))

;2.17 test
(deftest test-2-17
  (is (= (last-pair '(1 2 3)) (last-pair-iter '(1 2 3))))
  (is (= (last-pair '(3 4 1)) (last-pair-recur '(3 4 1)))))
(test-2-17)

;2.18
(defn myreverse [lst]
  (loop [lst lst
         acc  '()]
    (if (last lst) (recur (rest lst) (conj acc (first lst))) acc)))


;2.18 test
(deftest test-2-18
  (is (= '(3 2 1) (myreverse '(1 2 3)))))
(test-2-18)




