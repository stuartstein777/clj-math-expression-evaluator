(ns stuartstein777.math-evaluator
  (:gen-class)
  (:require [clojure.string :as str]
            [stuartstein777.tokenizer :as tkr]
            [stuartstein777.rpn-convertor :as rpnc]
            [stuartstein777.rpn-evaluator :as rpne]))

(defn evaluate [expr]
  (->> (tkr/tokenize expr)
       (rpnc/to-rpn)
       (rpne/evaluate-rpn)))

(defn -main
  [& args]
  (println "Evaluating" (first args))
  (println (evaluate (first args))))