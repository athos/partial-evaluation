(ns partial-evaluation.flowchart.interpreter
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :refer [match]]))

;; Program Structure:
;; {:vars {:<var1> <val1>, :<var2> <val2>, ...}
;;  :commands
;;  {<label1> [<command1>, ...],
;;   <label2> [<command2>, ...],
;;   ...}}

;; <command> :=
;;   [goto <label>]
;; | [set! <var> <expr>]
;; | [if <expr> goto <label> else <label>]
;; | [return <expr>]

;; <expr> :=
;;   <constant>
;; | <var>
;; | [+ <expr> <expr>]
;; | [- <expr> <expr>]
;; | [* <expr> <expr>]
;; | [/ <expr> <expr>]
;; | [= <expr> <expr>]
;; | [< <expr> <expr>]
;; | [> <expr> <expr>]

(defn lookup [env var]
  (env var))

(defn update [env var val]
  (assoc env var val))

(defn pred->op [pred]
  (fn [& args]
    (if (apply pred args)
      1
      0)))

(defn primitive-implementation [op]
  (let [fs {'+ +, '- -, '* *, '/ /,
            '= (pred->op =), '< (pred->op <), '> (pred->op >)}]
    (fs op)))

(defn eval [env expr]
  (cond (symbol? expr) (lookup expr env)
        (coll? expr)
        #_=> (let [[op & args] expr]
               (if-let [f (primitive-implementation op)]
                 (apply f (map #(eval env %) args))))
        :else expr))

(defn run [commands env program]
  (loop [[command & commands] commands, env env]
    (match command
      ['goto l] (recur (program l) env)
      ['set! v e] (recur commands (update env v (eval env e)))
      ['if test 'goto l1 'else l2]
      #_=> (if (= (eval env test) 0)
             (recur (program l2) env)
             (recur (program l1) env))
      ['return e] (eval env e))))
