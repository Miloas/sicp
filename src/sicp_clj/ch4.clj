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
  (-> exp next next))
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
    (make-lambda (-> exp second next)
                 (-> exp next next))))

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

(defn cond? [exp]
  (tagged-list? exp 'cond))
(defn cond-clauses [exp]
  (next exp))
(defn cond-preducate [clause]
  (first clause))
(defn cond-actions [clause]
  (next clause))
(defn cond-else-clauses? [clause]
  (= (cond-preducate clause) 'else))
(defn expand-clauses [clauses]
  (if (nil? clauses)
    'false
    (let [f (first clauses)
          r (next clauses)]
      (if (cond-else-clauses? f)
        (if (nil? r)
          (sequence->exp (cond-actions f))
          (throw (RuntimeException. (format "ELSE clause isn't last %s COND->IF" clauses))))
        (make-if (cond-preducate f)
                 (sequence->exp (cond-actions r))
                 (expand-clauses r))))))
(defn cond->if [exp]
  (-> exp cond-clauses expand-clauses))

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))
(defn primitive-implementation [proc]
  (second proc))
(declare apply')
(defn apply-primitive-procedure [proc args]
  (apply' (primitive-implementation proc) args))











