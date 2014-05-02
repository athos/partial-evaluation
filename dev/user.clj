(ns user
  (:require clojure.repl
            [clojure.tools.namespace.repl :refer [refresh refresh-all]]
            [partial-evaluation.flowchart.interpreter :as flow]))

(def env
  '{n 10, a 0, b 1, t 0})

(def program
  '{1 [[if (= n 0) goto 2 else 3]]
    2 [[return a]]
    3 [[set! t a]
       [set! a b]
       [set! b (+ b t)]
       [set! n (- n 1)]
       [goto 1]]})
