;ch2 solutions
(ns sicp-clj.ch2)

;test fn
(def tt println)
(use 'clojure.set)


;2.1
(defn gcd [a b]
  (if (= b 0) a (gcd b (rem a b))))
(defn make-rat [n d]
  (let [v1 (Math/abs (gcd n d))
        v2 (if (neg? d) (- v1) v1)]
    [(/ n v2) (/ d v2)]))

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
                    (throw (Exception. "��������Ϊ0"))))))

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

;2.18
(defn myreverse [items]
  (loop [items items
         acc  '()]
    (if items (recur (next items) (conj acc (first items))) acc)))

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

;2.21
(defn square [x] (* x x))
(defn square-list1 [items]
  (if (nil? items)
    nil
    (cons (square (first items)) (square-list1 (next items)))))
(defn square-list2 [items]
  (map square items))

;2.23
(defn for-each [f items]
  (doseq [x items] (f x)))

;2.27
(defn deep-reverse [items]
  (if (list? items)
    (map deep-reverse (reverse items))
    items))

;2.28
(defn fringe [tree]
  (flatten tree))
(defn fringe2 [x & tree]
  "apply can expand nested list.
   example: (max [1 2 3]) will error,
   but (apply max [1 2 3]) works"
  (concat (if (seq? x) (apply fringe2 x) (list x))
          (if (nil? tree) nil (apply fringe2 tree))))

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

;2.31
(defn tree-map [f tree]
  (if (list? tree)
    (map #(tree-map f %) tree)
    (f tree)))
(defn square-tree3 [tree]
  (tree-map square tree))

;2.32
(defn subsets [s]
  (if (nil? s) '(())
               (let [rest-s (subsets (next s))]
                 (concat rest-s (map #(cons (first s) %) rest-s)))))

;2.33
;It's interesting. accumulate method in Haskell is fold,
;but in Clojure is reduce (Haskell foldl)
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

;2.34
(defn horner-eval [x coefficient-list]
  (let [temp (reverse coefficient-list)]
    (reduce #(+ %2 (* x %1)) (first temp) (rest temp))))

;2.35
(defn count-leaves [tree]
  (reduce + 0 (map #(if (list? %) (count-leaves %) 1) tree)))

;2.36
(defn accumulate [op init seqs]
  "In Clojure , method 'reduce' is foldl like Haskell.
   We implement function 'accumulate' and make it work as foldr.
          foldr: f y1 (f y2 (... (f yk x) ...))
          foldl: f (... (f (f x y1) y2) ...) yk
   "
  (if (nil? seqs)
    init
    (op (first seqs)
        (accumulate op init (next seqs)))))
(defn accumulate-n [op init seqs]
  (if (nil? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map next seqs)))))

;2.37
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))
(defn matrix-*-vector [m v]
  (map #(dot-product v %) m))
(defn transpose [mat]
  (accumulate-n cons nil mat))
(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [row] (map #(dot-product row %) cols)) m)))

;2.39
(defn reverse-foldl [seqs]
  (reduce #(cons %2 %1) nil seqs))
(defn reverse-foldr [seqs]
  "(concat nil x) equal x"
  (accumulate #(concat %2 (list %1)) nil seqs))

;2.40
(defn unique-pairs [n]
  (mapcat (fn [x] (map #(list % x) (range 1 (inc x)))) (range 1 (inc n))))

;2.41
(defn unique-triples [n]
  (for [x (range 1 (inc n))
        y (range (inc x) (inc n))
        z (range (inc y) (inc n))]
    (list x y z)))
(defn solver-2-41 [n s]
  (filter #(= s (reduce + 0 %)) (unique-triples n)))

;2.42
(def empty-board nil)
(defn safe? [k pos]
  (let [cols (range 1 (inc k))
        a (map + pos cols)
        b (map - pos cols)
        sz (count pos)]
    (and (= (count (distinct pos)) sz)
         (= (count (distinct a)) sz)
         (= (count (distinct b)) sz))))
(defn adjoin-position [row col queens]
  (cons row queens))
(defn queens [board-size]
  (defn queen-cols [k]
    (if (= k 0)
      (list empty-board)
      (filter
        #(safe? k %)
        (mapcat
          (fn [rest-of-queeens]
            (map (fn [new-rows]
                   (adjoin-position new-rows k rest-of-queeens))
                 (range 1 (inc board-size))))
          (queen-cols (dec k))))))
  (queen-cols board-size))

;2.54
(defn equal? [seq1 seq2]
  (if (and (not (list? seq1)) (not (list? seq2))) (= seq1 seq2)
                                                  (if (and (list? seq1) (list? seq2))
                                                    (and (equal? (first seq1) (first seq2)) (equal? (next seq1) (next seq2)))
                                                    false)))

;2.56 and 2.57
(def variable? symbol?)
(def same-variable? #(and (variable? %1)
                          (variable? %2)
                          (= %1 %2)))
(defn =number? [exp num]
  (and (number? exp)
       (= exp num)))

(def sum? #(and (seq? %) (= (first %) '+)))
(def addend second)
(defn augend [seqs]
  (if (= (count seqs) 3) (nth seqs 2) (concat '(+) (next (next seqs)))))
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(def product? #(and (seq? %) (= (first %) '*)))
(def multipiler addend)
(def multiplicand augend)
(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (and (number? m1) (product? m2)) (make-product (* m1 (multipiler m2)) (multiplicand m2))
        (and (product? m1) (number? m2)) (make-product (* (multipiler m1) m2) (multiplicand m1))
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(def exponentiation? #(and (seq? %) (= (first %) '**)))
(def base addend)
(def exponent augend)
(defn make-exponentiation [b n]
  (cond (=number? n 1) b
        (=number? n 0) 0
        :else (list '** b n)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multipiler exp) (deriv (multiplicand exp) var))
                                 (make-product (deriv (multipiler exp) var) (multiplicand exp)))
        (exponentiation? exp) (make-product (exponent exp)
                                            (make-product (make-exponentiation (base exp) (dec (exponent exp)))
                                                          (deriv (base exp) var)))
        :else (throw (Exception. (format "unknow expression type %s DERUV" exp)))))

;2.59
(defn element-of-set? [x set]
  (some #(= x %) set))
(defn adjoin-set [x set]
  (if (element-of-set? x set) set (cons x set)))
(defn union-set [set1 set2]
  (reduce #(adjoin-set %2 %1) set1 set2))

;2.60
(defn element-of-multiset? [x multiset]
  (some #(= x %) set))
(defn adjoin-multiset [x multiset]
  (cons x multiset))
(defn union-multiset [multiset1 multiset2]
  (reduce #(adjoin-multiset %2 %1) multiset1 multiset2))
(defn intersection-multiset [multiset1 multiset2]
  (filter #(element-of-multiset? % multiset1) multiset2))

;2.61
(defn adjoin-sorted-set [x set]
  (loop [xx '()
         yy set]
    (cond (nil? yy) (conj xx x)
          (= x (first yy)) (concat xx yy)
          (< x (first yy)) (concat xx [x] yy)
          :else (recur (conj (vec xx) (first yy)) (next yy)))))


;2.62
(defn union-sorted-set [[x & xs :as set1] [y & ys :as set2]]
  (cond (nil? set1) set2
        (nil? set2) set1
        (= x y) (cons x (union-sorted-set xs ys))
        (> x y) (cons y (union-sorted-set set1 ys))
        :else   (cons x (union-sorted-set xs set2))))

;2.65
(defn entry [tree] (first tree))
(defn left-branch-tree [tree] (second tree))
(defn right-branch-tree [tree] (nth tree 3))
(defn make-tree [entry left right]
  (list entry left right))
(defn element-of-tree-set [x set]
  (cond (nil? set) false
        (= x (entry set)) true
        (< x (entry set)) (element-of-tree-set x (left-branch-tree set))
        :else (element-of-tree-set x (right-branch-tree set))))
(defn adjoin-tree-set [x set]
  (cond (nil? set) (make-tree x nil nil)
        (= x (entry set)) set
        (< x (entry set)) (make-tree (entry set) (adjoin-tree-set x (left-branch-tree set)) (right-branch-tree set))
        :else (make-tree (entry set) (left-branch-tree set) (adjoin-tree-set x (right-branch-tree set)))))
(defn union-tree-set [set1 set2]
  "Transform set to an sorted list ,and union, last transform unioned list to tree,
   all of needed method on the book."
  ())
(defn intersection-tree-set [set1 set2]
  "Same as 'union-tree-set' "
  ())

;2.66
;Same as method 'elemnt-of-tree-set' ,but we should change the tree structure,
;((key value) left-branch right-branch), compare the key and return value

;2.67
(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))
(defn leaf? [obj]
  (= (first obj) 'leaf))
(def symbol-leaf (comp first rest))
(def weight-leaf (comp second rest))
(def left-huffman-branch first)
(def right-huffman-branch second)
(defn symbols [tree]
  "Don't use 'nth' in lazy sequence will throw a exception,
   'concat' will generate a lazy sequence, be careful."
  (if (leaf? tree) (list (symbol-leaf tree))
                   (second (rest tree))))
(defn weight [tree]
  (if (leaf? tree) (weight-leaf tree)
                   (second (rest (rest tree)))))
(defn make-code-tree [left right]
  (list left right (concat (symbols left) (symbols right)) (+ (weight left) (weight right))))
(defn choose-branch [bit branch]
  (cond (= bit 0) (left-huffman-branch branch)
        (= bit 1) (right-huffman-branch branch)
        :else (throw (IllegalArgumentException. (format "bad bit %d CHOOSE-BRANCH" bit)))))
(defn decode [bits tree]
  (loop [bits bits
         branch tree
         acc []]
    (if (nil? bits) acc
                    (let [next-branch (choose-branch (first bits) branch)]
                      (if (leaf? next-branch)
                        (recur (next bits) tree (conj acc (symbol-leaf next-branch)))
                        (recur (next bits) next-branch acc))))))
(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;2.68
(defn encode-symbol [symbol tree]
  (loop [symbol symbol
         tree tree
         acc []]
    (if (leaf? tree)
      (if (some #(= symbol %) (symbols tree))
        acc
        (throw (RuntimeException. "Symbol not in tree.")))
      (let [left-symbols (symbols (left-huffman-branch tree))
            right-symbols (symbols (right-huffman-branch tree))]
        (cond
          (some #(= symbol %) left-symbols) (recur symbol (left-huffman-branch tree) (conj acc 0))
          (some #(= symbol %) right-symbols) (recur symbol (right-huffman-branch tree) (conj acc 1))
          :else (throw (RuntimeException. "Symbol not in tree.")))))))
(defn encode [message tree]
  (reduce #(concat %1 (encode-symbol %2 tree)) '() message))

;2.69
(defn adjoin-huff-set [set x]
  (cond (empty? set) (list x)
        (< (weight x) (-> set first weight)) (cons x set)
        :else (cons (first set) (adjoin-huff-set (rest set) x))))
(defn make-leaf-set [pairs]
  (reduce #(adjoin-huff-set %1 %2)
          '()
          pairs))
(defn successive-merge [leaves]
  (let [[l r & set] leaves]
    (if set
      (recur (adjoin-huff-set set (make-code-tree l r)))
      (make-code-tree l r))))
(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))


