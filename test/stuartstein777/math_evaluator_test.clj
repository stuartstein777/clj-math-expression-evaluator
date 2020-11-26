(ns stuartstein777.math-evaluator-test
  (:require [clojure.test :refer :all]
            [stuartstein777.math-evaluator :refer :all]
            [stuartstein777.tokenizer :refer :all]
            [stuartstein777.rpn-evaluator :refer :all]
            [stuartstein777.rpn-convertor :refer :all]))

(deftest is-operator?-tests
  (is (true? (is-operator? \*)))
  (is (true? (is-operator? \+)))
  (is (true? (is-operator? \-)))
  (is (true? (is-operator? \/))))

(deftest dash-type-tests
  (is (= :subtraction (dash-type "5-2" 1)))
  (is (= :negation (dash-type "5--2" 2)))
  (is (= :subtraction (dash-type "5--2" 1)))
  (is (= :subtraction (dash-type "5---2" 1)))
  (is (= :negation (dash-type "5---2" 2)))
  (is (= :negation (dash-type "5---2" 3)))
  (is (= :subtraction (dash-type "5-(--2)" 1)))
  (is (= :negation (dash-type "-5" 0)))
  (is (= :negation (dash-type "-5--5" 0)))
  (is (= :subtraction (dash-type "-5--5" 2)))
  (is (= :negation (dash-type "-5--5" 3)))
  (is (= :negation (dash-type "5/-5" 2)))
  (is (= :subtraction (dash-type "(5)-5" 3)))
  (is (= :subtraction (dash-type "(5)-(5)" 3)))
  (is (= :subtraction (dash-type "5-(5)" 1)))
  (is (= :subtraction (dash-type "(5)--(5)" 3)))
  (is (= :negation (dash-type "(5)--(5)" 4)))
  (is (= :negation (dash-type "5+-5" 2)))
  (is (= :negation (dash-type "5*-5" 2))))

(deftest get-number-tests
  (is (= "1" (get-number "1" 0)))
  (is (= "1" (get-number "1+2+12345+4234" 0)))
  (is (= "2" (get-number "1+2+12345+4234" 2)))
  (is (= "4234" (get-number "1+2+12345+4234" 10)))
  (is (= "12345" (get-number "1+2+12345+4234" 4)))
  (is (= "12345.6789" (get-number "1+2+12345.6789+4234" 4))))

(deftest is-operator?-tests
  (is (true? (is-operator? \+)))
  (is (true? (is-operator? \-)))
  (is (true? (is-operator? \*)))
  (is (true? (is-operator? \/)))
  (is (false? (is-operator? \a)))
  (is (false? (is-operator? \=)))
  (is (false? (is-operator? \$)))
  (is (false? (is-operator? \space))))

(deftest tokenizing-tests
  (testing "basic tests"
    (is (= [100.0] (tokenize "100")))
    (is (= [100.0 \+ 100.0] (tokenize "100 + 100")))
    (is (= [100.0 \- 100.0] (tokenize "100 - 100")))
    (is (= [100.0 \/ 100.0] (tokenize "100 / 100")))
    (is (= [100.0 \* 100.0] (tokenize "100 * 100")))
    (is (= [100.0 \* 100.0 \- 50.0 \+ 30.0 \/ 2.0] (tokenize "100 * 100 -50 +30 /2")))
    (is (= [1.0 \+ \~ \( 2.0 \) \/ -4.0 \* 5.0 \* \( 7.0 \/ -2.5 \)] (tokenize "1 + -(2) / -4 * 5*(7 / -2.5)")))
    (is (= [100.0 \- \~ \~ -100.0] (tokenize "100 ---- 100")))))

(deftest precedence-tests
  (is (= :higher (precedence \* \+)))
  (is (= :higher (precedence \* \-)))
  (is (= :same (precedence \* \*)))
  (is (= :same (precedence \* \/)))
  (is (= :lower (precedence \* \~)))

  (is (= :higher (precedence \/ \+)))
  (is (= :higher (precedence \/ \-)))
  (is (= :same (precedence \/ \*)))
  (is (= :same (precedence \/ \/)))
  (is (= :lower (precedence \/ \~)))

  (is (= :same (precedence \+ \+)))
  (is (= :same (precedence \+ \-)))
  (is (= :lower (precedence \+ \*)))
  (is (= :lower (precedence \+ \/)))
  (is (= :lower (precedence \+ \~)))

  (is (= :same (precedence \- \+)))
  (is (= :same (precedence \- \-)))
  (is (= :lower (precedence \- \*)))
  (is (= :lower (precedence \- \/)))
  (is (= :lower (precedence \- \~)))

  (is (= :higher (precedence \~ \+)))
  (is (= :higher (precedence \~ \-)))
  (is (= :higher (precedence \~ \*)))
  (is (= :higher (precedence \~ \/)))
  (is (= :same (precedence \~ \~))))

(deftest pop-brackets-tests
  (is (= {:output [1.0 2.0 \* \+] :op-stack [\/]}
         (pop-brackets {:output [1.0 2.0] :op-stack [\* \+ \( \/]}))))

