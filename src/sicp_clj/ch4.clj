(ns sicp-clj.ch4)

;test fn
(def tt println)

;4.1
(declare no-operands?)
(declare first-operand)
(declare eval')
(declare rest-operands)
;left->right
(defn list-of-values' [exps env]
  (if (no-operands? exps)
    '()
    (let [x (eval' (first-operand exps) env)]
      (cons x (list-of-values' (rest-operands exps) env)))))
;right->left
(defn list-of-values'' [exps env]
  (if (no-operands? exps)
    '()
    (let [x (list-of-values'' (rest-operands exps) env)]
      (cons (eval' (first-operand exps) env) x))))

