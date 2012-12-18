## About

1. It parses a string like `x + y + 2*3 + 2x + x^2 + y^2`, and simplify it into the form `x^2 + 3x + (y^2+y+6)`.
2. Use function`get_ans : string -> string` to get the simplifed string directly.
3. Or get the parse tree with function `parse : string -> expr`.
4. type `expr` define the parse tree.
5. type `term` define each term of a polynomial, e.g. 2xy is represented by Group (2, 1, 1).

## Before using it

1. Run `chmod +x ./build.sh && ./build.sh` first to generate lexer and parser.
2. Copy and paste `main.ml` or use `#use "main.ml"` in OCaml intepreter.

## Notes

1. Cannot handle `-` operator.
2. Only support two variable `x` and `y`.
3. It only simplify the polynomial in descending order of x.
