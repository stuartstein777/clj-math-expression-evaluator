(ns stuartstein777.rpn-convertor)

(def operator-precedence
  {\( 1 \) 1 \+ 2 \- 2 \* 3 \/ 3 \~ 4})

;; Returns precedence of op1 compared to op2.
;; e.g. (precedence \* \+) => :higher
(defn precedence [op1 op2]
  (cond (< (operator-precedence op1) (operator-precedence op2)) :lower
        (> (operator-precedence op1) (operator-precedence op2)) :higher
        :else :same))

(defn pop-brackets [{:keys [output op-stack]}]
  (let [ops (take-while #(not= % \() op-stack)
        output (apply conj output ops)]
    {:output output :op-stack (vec (drop (inc (count ops)) op-stack))}))

(defn pop-lower-or-same-precedence [{:keys [output op-stack]} t]
  (if (or (empty? op-stack) (= (precedence t (first op-stack)) :higher))
    {:output output :op-stack (concat [t] op-stack)}
    (let [ops (take-while #(not= :higher (precedence t %)) op-stack)
          output (apply conj output ops)
          op-stack (concat [t] (drop (count ops) op-stack))]
      {:output output :op-stack op-stack})))

(defn to-rpn [tokens]
  (let [acc (reduce (fn [acc t]
                      (cond (number? t)
                            (update acc :output conj t)

                            (or (= t \() (= t \~))
                            (update acc :op-stack #(concat [t] %))

                            (= t \))
                            (pop-brackets acc)

                            :else
                            (if (= :higher (operator-precedence t (first (acc :op-stack))))
                              (update acc :op-stack #(concat [t] %))
                              (pop-lower-or-same-precedence acc t))))
                    {:output [] :op-stack []}
                    tokens)]
    (apply conj (:output acc) (:op-stack acc))))