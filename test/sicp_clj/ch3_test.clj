(ns sicp-clj.ch3-test
  (:require [clojure.test :refer :all]
            [sicp-clj.ch3 :refer :all]))

;3.1 test
(deftest test-3-1
  (let [A (make-accumulator 5)]
    (is (= 15 (A 10)))
    (is (= 25 (A 10)))))

;3.2 test
(deftest test-3-2
  (let [s (make-monitored #(Math/sqrt %))]
    (is (= 10.0 (s 100)))
    (is (= 1 (s 'how-many-calls?)))
    (is (= 0 (s 'reset-count)))))

;3.3 and 3.4 test
(deftest test-3-3and4
  (let [acc (make-acount 100 '123456)]
    (is (= 60 ((acc '123456 'withdraw) 40)))
    (is (= 100 ((acc '123456 'deposit) 40)))))

;test algorithm
(deftest lazy-prime-test
  (is (= '(2 3 5 7 11) (take 5 prime))))

;3.54 test
(deftest test-3-54
  (is (= '(1 2 6 24 120) (take 5 (factorials)))))

;3.55 test
(deftest test-3-55
  (is (= '(1 3 6 10 15) (take 5 (partial-sums (iterate inc 1))))))
