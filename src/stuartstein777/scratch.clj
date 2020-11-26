(ns stuartstein777.scratch)

(defn is-digit? [token]
  (Character/isDigit ^char token))

(defn is-operator? [token]
  (not (nil? (#{\+ \/ \* \( \)} token))))

(defn get-number [expr loc]
  (->> (drop loc expr)
       (take-while (fn [t]
                     (or (is-digit? t) (= t \.))))
       (apply str)))

(defn type-of-dash [expr loc]
  (if (= 0 loc)
    (if (is-digit? (second expr))
      :negation
      :subtraction)
    (let [prev (nth expr (dec loc))
          next (nth expr (inc loc))]
      (cond (and (= \) prev) (is-digit? next)) :subtraction
            (and (is-operator? prev) (is-digit? next)) :negation
            (and (= \) prev) (= \( next)) :subtraction
            (and (= \- prev) (is-digit? next)) :negation
            (and (= \- prev) (= \- next)) :negation
            (and (is-digit? prev) (is-digit? next)) :subtraction
            (and (is-digit? prev) (= \- next)) :subtraction
            (and (is-digit? prev) (= \( next)) :subtraction
            (and (= \) prev) (= \- next)) :subtraction))))

(defn tokenize [expr]
  (let [expr (->> (str/replace expr #" " "")
                  #_(fix-zeros))
        expr (if (str/starts-with? expr "-(") (str "0" expr) expr)]
    (loop [tokens []
           loc 0]
      (if (= loc (count expr))
        tokens
        (let [token (nth expr loc)]
          (cond
            ; token is a number
            ; so we need to keep taking digits till we don't hit a digit. Then parse it to an int.
            ; we then to move the location to the end of the number.
            (is-digit? token)
            (let [num (get-number expr loc)]
              (recur (conj tokens (Double/parseDouble num)) (+ loc (count (str num)))))

            ; token is an operator
            ; add it to the operators list and increase the location for the next token.
            (is-operator? token)
            (recur (conj tokens token) (inc loc))

            ; special case :: token is -
            ; this can mean either subtraction or negation.
            (= token \-)
            (let [dash-type (type-of-dash expr loc)]
              (cond (= dash-type :subtraction)
                    (recur (conj tokens token) (inc loc))
                    :else
                    ; its negation - so if the next char is a digit, then we have a negative number!
                    ; otherwise just add the - and carry on.
                    (if (is-digit? (nth expr (inc loc)))
                      (let [num (get-number expr (inc loc))]
                        (recur (conj tokens (- (Double/parseDouble num))) (+ loc (inc (count (str num))))))
                      (recur (conj tokens \~) (inc loc)))))

            ; ignore trash
            :else
            (recur tokens (inc loc))))))))

(defn precedence-check [op1 op2]
  (cond (or (= op1 \-) (= op1 \+)) (cond (= op2 \-) :same
                                         (= op2 \+) :same
                                         (= op2 \/) :lower
                                         (= op2 \*) :lower
                                         (= op2 \_) :lower)

        (or (= op1 \*) (= op1 \/)) (cond (= op2 \_) :lower
                                         (= op2 \-) :higher
                                         (= op2 \+) :higher
                                         (= op2 \/) :same
                                         (= op2 \*) :same)

        (or (= op1 \_)) :higher))


(defn get-lower-or-same-precedence-operators [token op-stack]
  (take-while (fn [t] (let [precedence (precedence-check token t)]
                        (cond (or (= t \() (= t \))) false
                              (= :same precedence) true
                              (= :lower precedence) true)))
              op-stack))

(defn to-rpn [tokens]
  (loop [tokens tokens
         output []
         operators []]
    (if (empty? tokens)
      (apply conj output operators)
      (let [token (first tokens)]
        (cond (number? token)
              (recur (rest tokens) (conj output token) operators)

              (= \( token)
              (recur (rest tokens) output (concat [token] operators))

              (= \) token)
              (let [paren-ops (take-while #(not= % \() operators)]
                (recur (rest tokens)
                       (apply conj output paren-ops)
                       (drop (inc (count paren-ops)) operators)))

              (or (= \+ token) (= \- token) (= \* token) (= \/ token))
              (let [popped-tokens (get-lower-or-same-precedence-operators token operators)]
                (recur (rest tokens)
                       (apply conj output popped-tokens)
                       (concat [token] (drop (count popped-tokens) operators))))

              (= \~ token)
              (recur (rest tokens)
                     output
                     (concat [token] operators))

              :else
              (recur (rest tokens) output operators))))))

(defn get-function [i]
  (cond (= i \+) +
        (= i \-) -
        (= i \*) *
        (= i \/) /))

(defn run-rpn [rpn]
  (loop [rpn rpn
         operand-stack []]
    (let [current (first rpn)]
      (if (empty? rpn)
        (first operand-stack)
        (if (number? current)
          (recur (rest rpn) (concat [current] operand-stack))
          (if (= \~ current)
            ; its negation
            (let [op (first operand-stack)]
              (recur (rest rpn) (concat [(- op)] (rest operand-stack))))
            (let [right (first operand-stack)
                  left (second operand-stack)
                  op-res ((get-function current) left right)]
              (recur (rest rpn) (concat [op-res] (drop 2 operand-stack))))))))))

(defn calc [expression]
  (->> expression
       (tokenize)
       (to-rpn)
       (run-rpn)))
