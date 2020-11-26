(ns stuartstein777.math-evaluator
  (:gen-class)
  (:require [clojure.string :as str]
            [stuartstein777.tokenizer :as tkr]
            [stuartstein777.rpn-convertor :as rpnc]
            [stuartstein777.rpn-evaluator :as rpne]))


(->> (tkr/tokenize "100+40*(2*3)---3")
     (rpnc/to-rpn)
     (rpne/evaluate-rpn))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))