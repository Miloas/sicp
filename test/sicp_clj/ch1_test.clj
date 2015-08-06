(ns sicp-clj.ch1-test
  (:require [clojure.test :refer :all]
            [sicp-clj.ch1 :refer :all]))

;1.3 test
(deftest test-1-3
  (is (= 5 (sum-max2 3 1 2))))

;1.7 test
(deftest test-1-7
  (is (= 4.0 (sqrt 16))))

;1.8 test
(deftest test-1-8
  (is (= 3 (cube-root 27))))

;1.11 test
(deftest test-1-11
  (is (= 2 (f-rec 2)))
  (is (= 11 (f-rec 5)))
  (is (= 2 (f-iter 2)))
  (is (= 11 (f-iter 5))))

;1.12 test
(deftest test-1-12
  (is (= 3 (pascal 4 2))))

;1.16 test
(deftest test-1-16
  (is (= 1024 (fast-exp-iter 2 10))))

;1.17 test
(deftest test-1-17
  (is (= (* 3 4) (fast-mult-rec 3 4))))

;1.18 test
(deftest test-1-18
  (is (= (* 3 4) (fast-mult-iter 3 4))))

;1.19 test
(deftest test-1-19
  (is (= (+ (fib 3) (fib 4)) (fib 5))))

;1.22 test
(deftest test-1-22
  (is (= 2 (prime-interval 2 3))
      (is (= 23 (prime-interval 20 30)))))

;1.24 test
(deftest test-1-24
  (is (= true (fast-prime? 101 3))
      (is (= false (fast-prime? 4 3)))))

;1.27 test
(deftest test-1-27
  (is (= true (carmichael? 561))
      (is (= false (carmichael? 562)))))

;1.28 test
(deftest test-1-28
  (is (= false (miller-robin-prime? 561 3))
      (= true (miller-robin-prime? 100 3))))

;1.30 test
(deftest test-1-30
  (is (= (sum cube 1 inc 10) (sum-iter cube 1 inc 10))))

;1.31 test
(deftest test-1-31
  (is (= 120 (factorial 5))
      (is (= (product-iter identity 1 inc 5) (product identity 1 inc 5)))))

;1.32 test
(deftest test-1-32
  (is (= 55 (sum-acc identity 1 inc 10))
      (is (= 120 (product-acc identity 1 inc 5)))))

;1.33 test
(deftest test-1-33
  (is (= 17 (sum-prime-interval 1 10))
      (is (= 17 (sum-prime-smaller 10)))))

;1.41 test
(deftest test-1-41
  (is (= 3 ((double-excute inc) 1))))

;1.43 test
(deftest test-1-43
  (is (= 625 ((repeated square 2) 5))))
