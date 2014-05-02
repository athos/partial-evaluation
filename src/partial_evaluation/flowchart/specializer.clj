(ns partial-evaluation.flowchart.specializer
  (:require [clojure.core.match :refer [match]])
  (:import clojure.lang.PersistentQueue))

(defn specialize [division senv program]
  (loop [[[pp senv :as state] :as pending] (conj PersistentQueue/EMPTY [1 senv]),
         visited #{}
         residual {}]
    (if (empty? pending)
      residual
      (let [visited (conj visited state)
            specialize-block
            (fn [block]
              (loop [[command & commands] block, code [], pending (pop pending)]
                (if (nil? command)
                  [code pending]
                  (match command
                    ['set! v e]
                    #_=> (recur commands (conj code command) pending)
                    ['goto l]
                    #_=> (recur (program l) code pending)
                    ['if test 'goto l1 'else l2]
                    #_=> [(conj code command)
                          (into pending (remove visited [[l1 senv] [l2 senv]]))]
                    ['return e]
                    #_=> [(conj code command) pending]))))
            [code pending] (specialize-block (program pp))]
        (recur pending visited (assoc residual state code))))))
