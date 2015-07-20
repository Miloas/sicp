;ch2 solutions
(ns sicp.ch2)

;test fn
(defn tt [x] (println x))
(use 'clojure.test)
(use 'clojure.set)


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
(defn last-pair-iter [items]
  (loop [[x & xs] items]
    (if xs (recur xs) (list x))))
(defn last-pair-recur [items]
  (if (next items) (recur (rest items)) (list (first items))))
(defn last-pair [items]
  (list (last items)))

;2.17 test
(deftest test-2-17
  (is (= (last-pair '(1 2 3)) (last-pair-iter '(1 2 3))))
  (is (= (last-pair '(3 4 1)) (last-pair-recur '(3 4 1)))))
(test-2-17)

;2.18
(defn myreverse [items]
  (loop [items items
         acc  '()]
    (if items (recur (next items) (conj acc (first items))) acc)))

;2.18 test
(deftest test-2-18
  (is (= '(3 2 1) (myreverse '(1 2 3)))))
(test-2-18)

;2.19
(defn first-denomination [items]
  (first items))
(defn except-first-denomination [items]
  (rest items))
(defn no-more? [items]
  (next items))

;2.20
(defn same-parity [x & xs]
  (cons x (filter (if (even? x) even? odd?) xs)))

;2.20 test
(deftest test-2-20
  (is (= '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))))
(test-2-20)

;2.21
(defn square [x] (* x x))
(defn square-list1 [items]
  (if (nil? items)
    nil
    (cons (square (first items)) (square-list1 (next items)))))
(defn square-list2 [items]
  (map square items))

;2.21 test
(deftest test-2-21
  (is (= (square-list1 '(1 2 3)) (square-list2 '(1 2 3)))))
(test-2-21)

;2.23
(defn for-each [f items]
  (doseq [x items] (f x)))

;2.27
(defn deep-reverse [items]
  (if (list? items)
    (map deep-reverse (reverse items))
    items))

;2.27 test
(deftest test-2-27
  (is (= '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))))
(test-2-27)

;2.28
(defn fringe [tree]
  (flatten tree))
(defn fringe2 [x & tree]
  "apply can expand nested list.
   example: (max [1 2 3]) will error,
   but (apply max [1 2 3]) works"
  (concat (if (seq? x) (apply fringe2 x) (list x))
          (if (nil? tree) nil (apply fringe2 tree))))

;2.28 test
(deftest test-2-28
  (let [x '((1 2) (3 4))]
    (is (= '(1 2 3 4 1 2 3 4) (fringe2 (list x x))))))
(test-2-28)

;2.29
(defn make-mobile [left right]
  (list left right))
(defn make-branch [length structure]
  (list length structure))
; a)
(defn left-branch [mobile]
  (first mobile))
(defn right-branch [mobile]
  (second mobile))
(defn branch-length [branch]
  (first branch))
(defn branch-structure [branch]
  (second branch))
; b)
(defn total-weight [mobile]
  (if (list? mobile)
    (let [l-structure (branch-structure (left-branch mobile))
          r-structure (branch-structure (right-branch mobile))]
      (+ (if (list? l-structure) (total-weight l-structure) l-structure)
         (if (list? r-structure) (total-weight r-structure) r-structure)))
    mobile))
; c)
(defn mobile-balanced? [mobile]
  (if (list? mobile)
    (let [l (left-branch mobile)
          r (right-branch mobile)]
      (and (= (* (branch-length l) (total-weight (branch-structure l)))
              (* (branch-length r) (total-weight (branch-structure r))))
           (mobile-balanced? (branch-structure l))
           (mobile-balanced? (branch-structure r))))))


;2.30
(defn square-tree [tree]
  (if (list? tree)
    (cons
      (square-tree (first tree))
      (square-tree (next tree)))
    (if (nil? tree) '() (square tree))))

(defn square-tree2 [tree]
  (if (list? tree)
    (map square-tree2 tree)
    (square tree)))

;2.30 test
(deftest test-2-30
  (is (= '(1 (4 (9 16) 25)) (square-tree (list 1 (list 2 (list 3 4) 5)))))
  (is (= '(1 (4 (9 16) 25)) (square-tree2 (list 1 (list 2 (list 3 4) 5))))))
(test-2-30)

;2.31
(defn tree-map [f tree]
  (if (list? tree)
    (map #(tree-map f %) tree)
    (f tree)))
(defn square-tree3 [tree]
  (tree-map square tree))

;2.31 test
(deftest test-2-31
  (is (= '(1 (4 (9 16) 25)) (square-tree3 (list 1 (list 2 (list 3 4) 5))))))
(test-2-31)

;2.32
(defn subsets [s]
  (if (nil? s) '(())
    (let [rest-s (subsets (next s))]
      (concat rest-s (map #(cons (first s) %) rest-s)))))

;2.32 test
(deftest test-2-32
  "set A = set B means : A is the subset of B and B is the subset of A .
   In Clojure , method 'set' can translate list and vector to set data
   structure"
  (let [test-data '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))]
    (is (= true (subset? test-data (set (subsets '(1 2 3))))))
    (is (= true (subset? (subsets '(1 2 3)) (set test-data))))))
(test-2-32)

;2.33
;It's interesting. accumulate method in Haskell is fold,
;but in Clojure is reduce
(defn map-by-accumulate [f coll]
  "Method 'cons' require two arguments, it append first argument in
   second argument from head"
  "Method 'conj' acquire any arguments,
   In list: It head insert into first arugument from second argument to end argument
   In vector: It tail insert into ..."
  (reduce #(cons (f %2) %1) '() (reverse coll)))
(defn append-by-accumulate [seq1 seq2]
  (reduce #(cons %2 %1) seq2 (reverse seq1)))
(defn length-by-accumulate [coll]
  (reduce (fn [acc item] (inc acc)) 0 coll))
;2.33 test
(deftest test-2-33
  (is (= (map square '(1 2 3)) (map-by-accumulate square '(1 2 3))))
  (is (= (concat '(2 7 8) '(3 2 1)) (append-by-accumulate '(2 7 8) '(3 2 1))))
  (is (= (count '(1 2 3)) (length-by-accumulate '(1 2 3)))))
(test-2-33)

;2.34
(defn horner-eval [x coefficient-list]
  (let [temp (reverse coefficient-list)]
    (reduce #(+ %2 (* x %1)) (first temp) (rest temp))))

;2.34 test
(deftest test-2-34
  (is (= 79 (horner-eval 2 '(1 3 0 5 0 1)))))
(test-2-34)

