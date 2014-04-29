(ns partial-evaluation.flowchart-lang
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :refer [match]]))

;; Program Structure:
;; {:vars {:<var1> <val1>, :<var2> <val2>, ...}
;;  :commands
;;  {1 [<command1>, ...],
;;   2 [<command2>, ...],
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

(defn lookup [var env]
  (env var))

(defn eval [env expr]
  (cond (symbol? expr) (lookup expr env)
        (coll? expr)
        #_=> (let [[op & args] expr
                   fs {'+ +, '- -, '* *, '/ /, '= =, '< <, '> >}]
               (if-let [f (fs op)]
                 (apply f (map #(eval env %) args))))
        :else expr))
