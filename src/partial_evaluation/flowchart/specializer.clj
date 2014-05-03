(ns partial-evaluation.flowchart.specializer
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :refer [match]]
            [partial-evaluation.flowchart.interpreter :refer [eval] :as interp])
  (:import clojure.lang.PersistentQueue))

(defn static? [division var]
  (= (division var) :static))

(defn constant? [exp]
  (number? exp))

(defn fold [senv exp]
  (cond (symbol? exp) (or (senv exp) exp)
        (coll? exp)
        #_=> (let [[op & args] exp
                   args' (map #(fold senv %) args)]
               (if (every? constant? args')
                 (apply (interp/primitive-implementation op) args')
                 (cons op args')))
        :else exp))

(defn specialize [division senv program]
  (loop [[[pp senv :as state] :as pending] (conj PersistentQueue/EMPTY [1 senv]),
         visited #{}
         residual {}]
    (if (empty? pending)
      residual
      (let [visited (conj visited state)
            specialize-block
            (fn [block]
              (loop [[command & commands] block,
                     senv senv,
                     code [],
                     pending (pop pending)]
                (if (nil? command)
                  [code pending]
                  (match command
                    ['set! v e]
                    #_=> (if (static? division v)
                           (recur commands (assoc senv v (eval senv e)) code pending)
                           (recur commands senv (conj code ['set! v (fold senv e)]) pending))
                    ['goto l]
                    #_=> (recur (program l) senv code pending)
                    ['if test 'goto l1 'else l2]
                    #_=> (if (static? division test) ; FIXME: test may be an arbitrary expression
                           (if (= (eval senv test) 1)
                             (recur (program l1) senv code pending)
                             (recur (program l2) senv code pending))
                           [(conj code command)
                            (into pending (remove visited [[l1 senv] [l2 senv]]))])
                    ['return e]
                    #_=> [(conj code ['return (fold senv e)]) pending]))))
            [code pending] (specialize-block (program pp))]
        (recur pending visited (assoc residual state code))))))
