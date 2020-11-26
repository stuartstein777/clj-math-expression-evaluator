(ns stuartstein777.math-evaluator
  (:gen-class)
  (:require [clojure.string :as str]))

(defn is-operator? [token]
  (boolean (#{\+ \/ \* \-} token)))

(defn dash-type [expr loc]
  (let [prev (nth expr (dec loc) \-)]
    (if (or (= \) prev) (Character/isDigit ^char prev))
      :subtraction
      :negation)))

(defn get-number [expr loc]
  (->> (drop loc expr)
       (take-while #(or (Character/isDigit ^char %) (= % \.)))
       (apply str)))

(defn tokenize [expr]
  (let [expr (str/replace expr #" " "")]
    (loop [tokens []
           loc 0]
      (if (= loc (count expr))
        tokens
        (let [token (nth expr loc)]
          (cond
            ; token is a number
            ; so we need to keep taking digits till we don't hit a digit. Then parse it to a double.
            ; we then to move the location to the end of the number.
            (Character/isDigit ^char token)
            (let [num (get-number expr loc)]
              (recur (conj tokens (Double/parseDouble num)) (+ loc (count num))))

            ; otherwise token is an operator
            ; add it to the operators list and increase the location for the next token.
            (is-operator? token)
            (recur (conj tokens token) (inc loc))))))))



(-> "5*+3"
    (str/replace #"\s" "")
    (str/replace #"(--)+-" "-")
    (str/replace #"--" "+")
    (str/replace #"\+-\+" "-")
    (str/replace #"\++" "+")
    (str/replace #"/\+" "/")
    (str/replace #"\*\+" "*")
    (str/replace #"\-+" "-")
    (str/replace #"\+-" "-")
    (str/replace "(+" "("))

(tokenize "5*+3")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
