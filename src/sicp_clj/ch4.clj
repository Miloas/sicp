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








