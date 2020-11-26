(ns stuartstein777.rpn-evaluator)

(defn get-function [i]
  (cond (= i \+) +
        (= i \-) -
        (= i \*) *
        (= i \/) /))

(defn rpn-reducer [operand-stack i]
  (cond (number? i)
        (concat [i] operand-stack)

        (= \~ i)
        (concat [(- (first operand-stack))] (drop 1 operand-stack))

        :else
        (let [op1 (first operand-stack)
            op2 (second operand-stack)
            res ((get-function i) op2 op1)]
          (concat [res] (drop 2 operand-stack)))))

(defn evaluate-rpn [rpn]
  (reduce rpn-reducer [] rpn))