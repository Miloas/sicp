(ns sicp-clj.ch2-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [sicp-clj.ch2 :refer :all]))

;2.1 test
(deftest test-2-1
  (is (= [-3 4] (make-rat -6 8))))

;2.2 test
(deftest test-2-2
  (is (= [2 2] (midpoint-segment (make-segment [1 2] [3 2])))))

;2.3 test
(deftest test-2-3
  (is (= 4 (circumference-rec (make-rec [1 1] [2 2])))
      (is (= 1 (area-rec (make-rec [1 1] [2 2]))))))

;2.4 test
(deftest test-2-4
  (is (= 2 (car1 (cons1 2 3)))
      (is (= 3 (cdr1 (cons1 2 3))))))

;2.5 test
(deftest test-2-5
  (is (= 2 (car2 (cons2 2 3)))
      (is (= 3 (cdr2 (cons2 2 3))))))

;2.7 test
(deftest test-2-7
  (is (= 3 (upper-bound (make-interval 2 3))))
  (is (= 2 (lower-bound (make-interval 2 3)))))

;2.17 test
(deftest test-2-17
  (is (= (last-pair '(1 2 3)) (last-pair-iter '(1 2 3))))
  (is (= (last-pair '(3 4 1)) (last-pair-recur '(3 4 1)))))

;2.18 test
(deftest test-2-18
  (is (= '(3 2 1) (myreverse '(1 2 3)))))

;2.20 test
(deftest test-2-20
  (is (= '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))))

;2.21 test
(deftest test-2-21
  (is (= (square-list1 '(1 2 3)) (square-list2 '(1 2 3)))))

;2.27 test
(deftest test-2-27
  (is (= '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))))

;2.28 test
(deftest test-2-28
  (let [x '((1 2) (3 4))]
    (is (= '(1 2 3 4 1 2 3 4) (fringe2 (list x x))))))

;2.30 test
(deftest test-2-30
  (is (= '(1 (4 (9 16) 25)) (square-tree (list 1 (list 2 (list 3 4) 5)))))
  (is (= '(1 (4 (9 16) 25)) (square-tree2 (list 1 (list 2 (list 3 4) 5))))))

;2.31 test
(deftest test-2-31
  (is (= '(1 (4 (9 16) 25)) (square-tree3 (list 1 (list 2 (list 3 4) 5))))))

;2.32 test
(deftest test-2-32
  "set A = set B means : A is the subset of B and B is the subset of A .
   In Clojure , method 'set' can translate list and vector to set data
   structure"
  (let [test-data '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))]
    (is (= true (subset? test-data (set (subsets '(1 2 3))))))
    (is (= true (subset? (subsets '(1 2 3)) (set test-data))))))

;2.33 test
(deftest test-2-33
  (is (= (map square '(1 2 3)) (map-by-accumulate square '(1 2 3))))
  (is (= (concat '(2 7 8) '(3 2 1)) (append-by-accumulate '(2 7 8) '(3 2 1))))
  (is (= (count '(1 2 3)) (length-by-accumulate '(1 2 3)))))

;2.34 test
(deftest test-2-34
  (is (= 79 (horner-eval 2 '(1 3 0 5 0 1)))))

;2.35 test
;
;     (1((1 2)1))
;      /       \
;     1     ((1 2)1)
;            /    \
;         (1 2)    1
;         /   \
;        1     2
;
(deftest test-2-35
  (is (= 4 (count-leaves '(1 ((1 2) 1))))))

;2.36 test
(deftest test-2-36
  (is (= '(22 26 30) (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))))

;2.37 test
(deftest test-2-37
  (let [m '((1 2 3) (4 5 6) (7 8 9))]
    (is (= '((30 36 42) (66 81 96) (102 126 150)) (matrix-*-matrix m m)))))

;2.39 test
(deftest test-2-39
  (let [test-data '(1 7 2 9)]
    (is (= (reverse test-data) (reverse-foldl test-data)))
    (is (= (reverse test-data) (reverse-foldr test-data)))))

;2.40 test
(deftest test-2-40
  (is (= '((1 1) (1 2) (2 2) (1 3) (2 3) (3 3) (1 4) (2 4) (3 4) (4 4)) (unique-pairs 4))))

;2.41 test
(deftest test-2-41
  (is (= '((1 2 3) (1 2 4) (1 3 4) (2 3 4)) (unique-triples 4)))
  (is (= '((1 2 3)) (solver-2-41 4 6))))

;2.42 test
(deftest test-2-42
  (is (= 92 (count (queens 8)))))

;2.54 test
(deftest test-2-54
  (is (= true (equal? '(this is a list) '(this is a list))))
  (is (= false (equal? '(this is a list) '(this (is a) list))))
  (is (= false (equal? '(this is a list) '(this is a vector)))))

;2.56 test
(deftest test-2-56
  (is (= '(* 9 (** x 2)) (deriv '(* 3 (** x 3)) 'x)))
  (is (= 3 (deriv '(+ x x x) 'x))))

;2.59 test
(deftest test-2-59
  (is (= (set '(1 2 3 4)) (set (union-set '(1 2 3) '(3 4))))))

;2.61 test
(deftest test-2-61
  (is (= '(1 2 3 4) (adjoin-sorted-set 3 '(1 2 4))))
  (is (= '(1 2 3 4) (adjoin-sorted-set 4 '(1 2 3)))))

;2.62 test
(deftest test-2-62
  (is (= '(1 2 3 4) (union-sorted-set '(1 2 3) '(2 3 4)))))

;2.67 test
(deftest test-2-67
  (is (= '(A D A B B C A) (decode sample-message sample-tree))))

;2.68 test
(deftest test-2-68
  (let [message '(A D A B B C A)]
    (is (= message (decode (encode message sample-tree) sample-tree)))))

;2.69 test
(deftest test-2-69
  (is (= sample-tree (generate-huffman-tree (list (make-leaf 'A 4) (make-leaf 'B 2) (make-leaf 'D 1) (make-leaf 'C 1))))))
