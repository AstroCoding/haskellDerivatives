# Haskell Derivative Calculator
Derivative Calculator Code for Haskell.

## How does it work?
The Haskell Derivative Calculator uses the MathExpr custom data typeto represent certain polynomials.
This calculator has 4 functions:
  * value
  * simp
  * diff
  * readDiffWrite

### value
value takes a MathExpr and an n value as an input and returns the value of that input.
For Example: 
```
value (Sum X (Coef 2.0)) 3.0 == 5.0
```

### simp
simp takes a MathExpr and simplifies the expression down to its most simplified form.
For Example: 
```
simp (Sum (Coef 0.0) (Prod (Coef 2.0) (Coef 1.0))) == Coef 2.0
```

### diff
diff takes a MathExpr calculates the derivative expression.
For Example: 
```
diff (Log (Sum (Prod (X) (X)) (Coef (-1.0)))) == (Quot (Sum (Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))) (Coef 0.0)) (Log (Sum (Prod X X) (Coef (-1.0)))))
```

### readDiffWrite
readDiffWrite reads a input file in which has a series of MathExprs, each on their own line. They are ran through diff and simp and then are written to an output file.
