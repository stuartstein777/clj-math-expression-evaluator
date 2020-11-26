# math-evaluator

Solves simple math expressions.

Supported Operators (+ * / - ), plus parenthesis and negative numbers.

e.g.

```clojure

(solve "(123.45*(678.90 / (-2.5+ 11.5)-(((80 -(19))) *33.25)) / 20) - (123.45*(678.90 / (-2.5+ 11.5)-(((80 -(19))) *33.25)) / 20) + (13 - 2)/ -(-11)")
=> 1.0
```

Works by parsing the input expression into tokens.
Converts those tokens to Reverse Polish Notation (to handle precedence)
Evaluates the RPN.
