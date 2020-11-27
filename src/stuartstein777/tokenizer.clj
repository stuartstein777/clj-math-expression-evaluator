(ns stuartstein777.tokenizer
  (:require [clojure.string :as str]))

(defn is-operator? [token]
  (boolean (#{\+ \/ \* \- \( \)} token)))

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
    (loop [tokens [] loc 0]
      (if (= loc (count expr))
        tokens
        (let [token (nth expr loc)]
          (cond (Character/isDigit ^char token)
                (let [num (get-number expr loc)]
                  (recur (conj tokens (Double/parseDouble num)) (+ loc (count num))))
                :else
                (recur (conj tokens
                             (if (= \- token)
                               (if (= :subtraction (dash-type expr loc)) \- \~)
                               token))
                       (inc loc))))))))