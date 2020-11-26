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
    (loop [tokens []
           loc 0]
      (if (= loc (count expr))
        tokens
        (let [token (nth expr loc)]
          (cond (Character/isDigit ^char token)
                (let [num (get-number expr loc)]
                  (recur (conj tokens (Double/parseDouble num)) (+ loc (count num))))

                (= \- token)
                (let [dash-type (dash-type expr loc)]
                  (cond (= :subtraction dash-type)
                        (recur (conj tokens \-) (inc loc))

                        (and (= :negation dash-type) (Character/isDigit ^char (nth expr (inc loc))))
                        (let [num (get-number expr (inc loc))]
                          (recur (conj tokens (- (Double/parseDouble num))) (+ loc 1 (count num))))

                        :else
                        (recur (conj tokens \~) (inc loc))))

                (is-operator? token)
                (recur (conj tokens token) (inc loc))))))))