# Infix-Math

Infix-Math is a library that provides a special-purpose syntax for
transcribing mathematical formulas into Lisp.

Bitter experience has taught me that the more the formula on screen
resembles the formula on paper, the better. The more the formula on
screen resembles the formula on paper, the easier it is to prevent
bugs from transcription errors. The easier it is to prevent
transcription errors, the easier it is to trace the source of any bugs
that do occur – because sometimes the formula is wrong.

(Having to transcribe formulas from crooked, blurry scans of ancient
pre-LaTeX typescripts is bad enough without having to parse operator
precedence in your head.)

Even if you end up rewriting the formula for speed or numerical
stability, having the specification in an executable form is
invaluable for reference and testing.

## Examples

The macro `$` is the entry point into Infix-Math.

    ($ 2 + 2)     => 4
    ($ 1 + 2 * 3) => 7

Operator precedence parsing in Infix-Math is reliable – it uses
Dijkstra’s [shunting yard][] algorithm.

The parser automatically descends into function argument lists, which
means that the total number of parentheses is never greater than it
would be in a purely infix language.

    ($ (tan pi * (p - 1/2)))
    ≡ (tan (* pi (- p 1/2)))
    ≅ tan(pi*(p-0.5))

Common subexpression elimination is automatic and aggressive. All
forms are assumed to be pure. Math does not have side effects.

    (macroexpand '($ 2 ^ 2x * 2 ^ 2x)
    => ‘(let ((#:subexp11325 (^ 2 (* 2 x))))
          (* #:subexp11325 #:subexp11325))

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

Operations at the same level of precedence are always evaluated
left-to-right.

    (+ 0.1d0 (+ 0.2d0 0.3d0)) => 0.6d0
    (+ (+ 0.1d0 0.2d0) 0.3d0) => 0.6000000000000001D0
    ($ 0.1d0 + 0.2d0 + 0.3d0) => 0.6000000000000001D0

Parentheses can be used for grouping.

    ($ 0.1d0 + (0.2d0 + 0.3d0)) => 0.6d0

Variables can be written with literal numbers as coefficients.

    ($ 2x)  => 10
    ($ -2x) => 10

Literal coefficients have very high priority.

    ($ 2 ^ 2 * x) ≡ (* (expt 2 2) x)     => 20
    ($ 2 ^ 2x)    ≡ (expt 2 (* 2 x))     => 1024

A literal coefficient of 1 can be omitted.

    ($ -x) ≡ ($ -1x) ≡ (* -1 x)

Literal coefficients are parsed as decimals, rather than floats.

    ($ 1.5x) ≡ (* 3/2 x)

You can also use fractions as literal coefficients.

    ($ 1/3x) ≡ (* 1/3 x)

Among other things, literal coefficients are very convenient for units
of measurement.

(The idea for literal coefficients comes from Julia.)

## Symbols

Infix-Math exports only five symbols: `$`, `^`, `over`, and two macros
for declaring operators: `declare-unary-operator` and
`declare-binary-operator`.

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

If you want more math symbols, the package `infix-math/symbols`
provides a few more.

## Calculator

You can use Infix-Math to turn your REPL into a calculator.

First, load the `infix-math/calc` system:

    (asdf:load-system "infix-math/calc")

Then, at the REPL, start the calculator:

    (infix-math/calc:calc)

This will put you at a calculator prompt. You can type in mathematical expressions directly:

    $> 2 + 2
    4

A single form entered at the REPL is interpreted as ordinary CL.

    $> *package*
    :infix-math/calc-user

You can assign to variables using the `<-` operator.

    $> x <- 2 + 2
    4
    $> x
    4

Certain one-letter variables are provided for you to assign to, such as `x`, `y`, and `z`. You can see the full list by evaluating `:v` at the calculator prompt.

To quit, use `:q`. The value of the last expression evaluated will be returned.

    $> 2 + 2
    4
    $> :q
    4
    CL-USER> *
    4

## Extending

Infix-Math is easily to extend. In fact, you may not even need to
extend it.

Any symbol that consists entirely of operator characters is
interpreted as an infix operator, with the highest non-unary priority.
Operator characters are anything but dashes, underscores, whitespace
or alphanumeric characters.

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

If you need more flexibility, declare the operators using
`declare-binary-operator` or `declare-unary-operator`.

To declare a unary operator:

    (declare-unary-operator √)

To copy the precedence of another operator:

    (declare-binary-operator <*> :from *)

To declare an operator right-associative:

    (declare-binary-operator ?
      :from *
      :right-associative t)

[FMA]: https://en.wikipedia.org/wiki/Fused_multiply%E2%80%93add
[Julia]: http://julialang.org
[shunting yard]: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
