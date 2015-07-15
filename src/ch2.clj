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


