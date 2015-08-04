;ch3 solutions
(ns sicp.ch3)

;test fn
(def tt println)
(use 'clojure.test)

;try atom
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

;3.3 and 3.4
(defn make-acount [amount password]
  (let [balance (atom amount)
        count (atom 0)]
    (fn [secret-password op]
      (fn [x]
        (let [call-the-cops (fn [] (println "Cops comming."))]
        (if (= secret-password password)
          (cond (= op 'withdraw) (if (>= x @balance)
                                   (throw (RuntimeException. "Insufficient funds"))
                                   (swap! balance #(- % x)))
                (= op 'deposit) (swap! balance #(+ % x))
                :else (throw (RuntimeException. "Unknow request")))
          (do
            (swap! count inc)
            (when (= @count 7) call-the-cops)
            (throw (RuntimeException. "Password incorrect")))))))))

;3.3 and 3.4 test
(deftest test-3-3and4
  (let [acc (make-acount 100 '123456)]
    (is (= 60 ((acc '123456 'withdraw) 40)))
    (is (= 100 ((acc '123456 'deposit) 40)))))
(test-3-3and4)

;3.5
(defn monte-carlo [trials experiment]
  (loop [trials-remaining trials
         trials-passed 0]
    (cond (= trials-remaining 0) (/ trials-passed trials)
          (experiment) (recur (dec trials-remaining) (inc trials-passed))
          :else (recur (dec trials-remaining) trials-passed))))
(defn rand-in-range [low high]
  (+ low (* (Math/random) (- high low))))
(defn estimate-integral [P x1 y1 x2 y2 trials]
  (let [area (* (- x2 x1) (- y2 y1))]
    (* area (monte-carlo trials #(P (rand-in-range x1 x2)
                                    (rand-in-range y1 y2))))))

;(tt (/ (estimate-integral (fn [x y] (<= (+ (* (- x 5) (- x 5)) (* (- y 7) (- y 7))) 9)) 2 4 8 10 10000) 9))

;3.7
(defn make-joint [account password new-password]
  (do ((account password 'withdraw) 0)
    (fn [p op]
      (if (= p new-password)
        (account password op)
        (account 'invalid-password op)))))


