(ns sicp-clj.ch4)

;test fn
(def tt println)

;eval and apply
(defn self-evelating? [exp]
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

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))
(defn compound-procedure? [p]
  (tagged-list? p 'procedure))
(defn procedure-parameters [p]
  (second p))
(defn procedure-body [p]
  (nth p 2))
(defn procedure-enviroment [p]
  (nth p 3))

(defn enclosing-enviroment [env]
  (next env))
(defn first-frame [env]
  (first env))
(def the-empty-env '())
(defn make-frame [variables values]
  (atom (zipmap variables values)))
(defn frame-variables [frame]
  (keys frame))
(defn frame-values [frame]
  (vals frame))
(defn add-binding-to-frame! [var val frame]
  (swap! frame assoc var val))
(defn extend-enviroment [vars vals base]
  (when (= (count vars) (count vals))
    (cons (make-frame vars vals) base)))
(defn lookup-variable-value [var env]
  (some (comp #(get % var) deref) env))
(defn find-first-frame-containing [var env]
  (some #(when (contains? (deref %) var) %) env))
(defn set-variable-value! [var val env]
  (when-let [frame (find-first-frame-containing var env)]
    (add-binding-to-frame! var val frame)))
(defn define-variable! [var val env]
  (add-binding-to-frame! var val
                         (or (find-first-frame-containing var env)
                             (first-frame env))))

(declare eval')
(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval' (assignment-value exp) env)
                       env))
(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (eval' (definition-value exp) env)
                    env))
(defn eval-if [exp env]
  (if (eval' (if-predicate exp) env)
    (eval' (if-consequent exp) env)
    (eval' (if-alternative exp) env)))
(defn eval-sequence [exps env]
  (if (last-exp? exps)
    (eval' (first-exp exps) env)
    (do
      (eval' (first-exp exps) env)
      (recur (rest-exps exps) env))))
(defn apply' [procedure arguments]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence
                                          (procedure-body procedure)
                                          (extend-enviroment (procedure-parameters procedure)
                                                             arguments
                                                             (procedure-enviroment procedure)))
        :else (throw (RuntimeException. (format "Unknow procedure type %s APPLY" procedure)))))
(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval' (first-operands exps) env)
          (list-of-values (rest-operands exps) env))))

(defn eval' [exp env]
  (cond (self-evelating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (lambda? exp) (make-procedure (lambda-parameters exp)
                                      (lambda-body exp)
                                      env)
        (begin? exp) (eval-sequence exp env)
        (cond? exp) (eval' (cond->if exp) env)
        (application? exp) (apply' (eval' (operator exp) env) (list-of-values (operands exp) env))
        :else (throw (RuntimeException. (format "Unknow expression type %s EVAL" exp)))))










