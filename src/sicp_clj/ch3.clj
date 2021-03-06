;ch3 solutions
(ns sicp-clj.ch3)

;test fn
(def tt println)

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

;3.2
(defn make-monitored [f]
  (let [count (atom 0)]
    (fn [x]
      (cond (= x 'reset-count) (reset! count 0)
            (= x 'how-many-calls?) @count
            :else (do
                    (swap! count inc)
                    (f x))))))

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

;3.24
;If use Clojure's 'assoc' , this problem is too complicated.
;Let it go . QAQ
;(swap! xx f arg1 arg2) = (reset! xx (ff xx arg1 arg2))
(defn make-table []
  (let [local-table (atom {})]
    (letfn [(lookup [key-1 key-2]
                    (let [subtable (get @local-table key-1)]
                      (when subtable
                        (get subtable key-2))))
            (insert! [key-1 key-2 value]
                     (let [subtable (get @local-table key-1)]
                       (swap! local-table
                              assoc key-1 (assoc subtable key-2 value))))]
      (let [dispatch {:lookup lookup
                      :insert insert!}]
        #(% dispatch)))))

;(def tab (make-table))
;(tt ((tab :insert) 1 2 3))

;.3.47
;Follow <Operating Systems:Internals and Design Principles,Fifth Edition> P.217
;Book author: William Stallings
;P and V are atom operations,according to problem description,we only should
;implement it with 'test-and-set!' method.
(defn P [n]
  (do
    (swap! n dec)
    (if (< @n 0) false true)))
(defn V [n]
  (swap! n inc))
(defn make-semaphore [n]
  (let [n (atom n)]
    (letfn [(the-semaphore [op]
                           (condp = op
                             :acquire (when (= (P n) false) (the-semaphore n))
                             :release (V n)))]
      the-semaphore)))

;3.50
(defn stream-map [proc & argstreams]
  (lazy-seq
    (if (empty? (first argstreams)) '()
                                    (cons
                                      (apply proc (map first argstreams))
                                      (apply stream-map (cons proc (map rest argstreams)))))))

;sieve of Eratosthenes in Clojure,
(defn sieve [stream]
  (lazy-seq
    (cons
      (first stream)
      (sieve (filter #(not= (mod % (first stream)) 0) (rest stream))))))

(def prime (sieve (iterate inc 2)))

;3.54
(defn factorials
  ([] (factorials 1 2))
  ([n m] (cons n (lazy-seq (factorials (* n m) (inc m))))))

;3.55
(defn partial-sums [streams]
  (letfn [(help
            ([] (help (first streams) (rest streams)))
            ([n m] (cons n (lazy-seq (help (+ n (first m)) (rest m))))))]
    (help)))

