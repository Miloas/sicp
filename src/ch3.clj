;ch3 solutions
(ns sicp.ch3)

;test fn
(def tt println)
(use 'clojure.test)

(def balance (atom 100))
(defn withdraw [amount]
  (if (>= @balance amount)
      ;(reset! balance (- @balance amount))
      (swap! balance #(- % amount))
    (throw (RuntimeException. "Insufficient funds."))))

;3.1
(defn make-accumulator [initial]
  (let [acc (atom initial)]
    (fn [value]
      (swap! acc #(+ % value)))))

;3.1 test
(deftest test-3-1
  (let [A (make-accumulator 5)]
    (is (= 15 (A 10)))
    (is (= 25 (A 10)))))
(test-3-1)

;3.2
(defn make-monitored [f]
  (let [count (atom 0)]
    (fn [x]
    (cond (= x 'reset-count) (reset! count 0)
          (= x 'how-many-calls?) @count
          :else (do
                  (swap! count inc)
                  (f x))))))

;3.2 test
(deftest test-3-2
  (let [s (make-monitored #(Math/sqrt %))]
    (is (= 10.0 (s 100)))
    (is (= 1 (s 'how-many-calls?)))
    (is (= 0 (s 'reset-count)))))
(test-3-2)


