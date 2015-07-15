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

