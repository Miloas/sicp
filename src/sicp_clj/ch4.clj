(ns sicp-clj.ch4)

;test fn
(def tt println)

;eval and apply
(defn self-evelating [exp]
  (or (number? exp) (string? exp)))
(def variable? symbol?)

(defn tagged-list? [exp tag]
  (when (seq? exp)
    (= tag (first exp))))

(defn quoted? [exp]
  (tagged-list? exp 'quote))
(defn text-of-quotation [exp]
  (second exp))

(defn assignment? [exp]
  (tagged-list? exp 'set!))
(defn assignment-variable [exp]
  (second exp))
(defn assignment-value [exp]
  (nth exp 2))

(defn lambda? [exp]
  (tagged-list? exp 'lambda))
(defn lambda-parameters [exp]
  (second exp))
(defn lambda-body [exp]
  (-> exp rest rest))
(defn make-lambda [parameters body]
  (list 'lambda parameters body))

(defn definition? [exp]
  (tagged-list? exp 'define))
(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    (-> exp second first)))
(defn definition-value [exp]
  (if (symbol? (second exp))
    (nth exp 2)
    (make-lambda (-> exp second rest)
                 (-> exp rest rest))))

(defn if? [exp]
  (tagged-list? exp 'if))
(defn if-predicate [exp]
  (second exp))
(defn if-consequent [exp]
  (nth exp 2))
(defn if-alternative [exp]
  (if (not (nil? (-> exp next next next)))
    (-> exp next next next)
    'false))
(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn begin? [exp]
  (tagged-list? exp 'begin))
(defn begin-action [exp]
  (rest exp))
(defn last-exp? [seq]
  (-> seq next nil?))
(defn first-exp [seq]
  (first seq))
(defn rest-exps [seq]
  (rest seq))
(defn make-begin [seq]
  (cons 'begin seq))
(defn sequence->exp [seq]
  (cond (nil? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

(defn application? [exp]
  (list? exp))
(defn operator [exp]
  (first exp))
(defn operands [exp]
  (next exp))
(defn no-operands? [ops]
  (nil? ops))
(defn first-operands [ops]
  (first ops))
(defn rest-operands [ops]
  (rest ops))









