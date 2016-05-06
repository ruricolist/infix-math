# Infix-Math

Infix-Math is a library that provides a special syntax for
transcribing mathematical formulas into Lisp. Bitter experience has
taught me that the more the formula on screen resembles the formula on
paper, the better. The more the formula on screen resembles the
formula on paper, the easier it is to prevent bugs from transcription
errors. The easier it is to prevent transcription errors, the easier
it is to trace the source of any bugs that do occur – because
sometimes the formula is wrong.

Even if you end up rewriting the formula for speed or numerical
stability, having the specification in an executable form is
invaluable for reference and testing.

(Having to transcribe formulas from crooked, blurry scans of ancient
pre-LaTeX typescripts is bad enough without having to parse operator
precedence and do common subexpression elimination in your head.)

## Examples

The macro `$` is the entry point into Infix-Math.

    ($ 2 + 2)     => 4
    ($ 1 + 2 * 3) => 7

Common subexpression elimination is automatic and aggressive. All
forms are assumed to be pure. Math does not have side effects.

    (macroexpand '($ 2 ^ 2x * 2 ^ 2x)
    => ‘(let ((#:subexp11325 (^ 2 (* 2 x))))
          (* #:subexp11325 #:subexp11325))

The parser automatically descends into function argument lists.

    ($ (tan pi * (p - 1/2))) ≡ ($ (tan ($ pi * (p - 1/2))))

Infix-Math knows about the following arithmetic and bitwise operators,
in descending order of precedence.

- unary -, sqrt
- expt, log
- *, /, rem, mod, floor, ffloor, ceiling, fceiling, truncate,
  ftruncate, round, fround, scale-float, gcd, lcm, atan
- +, -
- ash
- logand, logandc1, logandc2, lognand
- logxor, logeqv
- logior, logorc1, logorc2, lognor
- min, max
- over

Operator precedence parsing in Infix-Math is reliable – it uses
Dijkstra’s [shunting yard][] algorithm.

Operations at the same level of precedence are always evaluated
left-to-right.

    (+ 0.1d0 (+ 0.2d0 0.3d0)) => 0.6d0
    (+ (+ 0.1d0 0.2d0) 0.3d0) => 0.6000000000000001D0
    ($ 0.1d0 + 0.2d0 + 0.3d0) => 0.6000000000000001D0

Parentheses can be used for grouping.

    ($ 0.1d0 + (0.2d0 + 0.3d0)) => 0.6d0

Infix-Math exports just four symbols: `$`, `^`, `over`, and
`declare-operator`.

(If you want more math symbols, the package `infix-math/symbols`
provides a few more.)

The symbol `^` is just a shorthand for `expt`.

    ($ 1 + 2 * 3 ^ 4) => 163

(`^` is from Dylan.)

The symbol `over` represents the same operation as `/`, but at a much
lower priority. Using `over` lets you avoid introducing parentheses
for grouping when transcribing fractions.

    (setf x 5)
    ($ x * 2 / x * 3)     ≡ (* (/ (* x 2) x) 3) => 6
    ($ (x * 2) / (x * 3)) ≡ (/ (* x 2) (* x 3)) => 2/3
    ($ x * 2 over x * 3)  ≡ (/ (* x 2) (* x 3)) => 2/3

You can also spell `over` with a series of dashes or underscores.

    ($ x * 2
       -----
       x * 3)
    => 2/3

Variables can be written with literal numbers as coefficients.

    ($ 2x)  => 10
    ($ -2x) => 10

Literal coefficients have very high priority.

    ($ 2 ^ 2 * x) ≡ (* (expt 2 2) x)     => 20
    ($ 2 ^ 2x)    ≡ (* (expt 2 (* 2 x))) => 1024

A literal coefficient of 1 can be omitted.

    ($ -x) ≡ (- x)

Literal coefficients are parsed as decimals, rather than floats.

    ($ 1.5x) ≡ (* 3/2 x)

You can use fractions as literal coefficients.

    ($ 1/3x) ≡ (* 1/3 x)

Literal coefficients are very nice for units of measurement.

(The idea for literal coefficients comes from Julia.)

## Extending

Infix-Math is easily to extend. In fact, you may not even need to
extend it.

Any symbol that consists entirely of operator characters (anything but
dashes, underscores, whitespace or alphanumeric characters) is
interpreted as an infix operator, with the highest non-unary priority.

    (defun <*> (x y)
      "Matrix multiplication, maybe."
      ...)

    (macroexpand '($ x * y <*> z)) => (* x (<*> y z))

(This approach is taken from Haskell.)

You can use any function as an infix operator by surrounding its name
with dots.

    (defun choose (n k)
      "Binomial coefficient, maybe."
      ...)

    (macroexpand '($ n .choose. k)) => '(choose n k)

Again, the operator has the highest non-unary priority.

(This approach is taken from Haskell and Fortran.)

If you need more flexibility, use `declare-operator`.

To copy the precedence of another operator:

    (declare-operator <*> :from *)

To declare a unary operator:

    (declare-operator √ :from -)

To declare an operator right-associative:

    (declare-operator ?
      :from *
      :right-associative t)

## Future work

It might be nice to have the macro rewrite unstable expressions into
more stable forms. It would be easy to, say, automatically rewrite `($
(exp a) – 1)` into `(exp-1 a)`. But just doing it *sometimes* would
inspire false confidence. You would have to catch everything.

It might also be nice to recognize polynomials and rewrite them into
Horner form, possibly with preconditioning for exact coefficients.

In principle, if a Lisp implementation compiled `x * y +z` using
[FMA][], CSE would have to be careful to avoid lifting out the
multiplication in expressions like `(x * y + a) * (x * y + b)`.
However, to be best of my knowledge, no Lisp does that.

[FMA]: https://en.wikipedia.org/wiki/Fused_multiply%E2%80%93add
[Julia]: http://julialang.org
[shunting yard]: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
