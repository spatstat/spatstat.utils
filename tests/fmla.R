#'   tests/fmla.R
#'
#'   Manipulation of formulae

require(spatstat.utils)

#'  code coverage of special cases
identical.formulae(y ~ x, NULL)
termsinformula(NULL)
offsetsinformula(y ~ x)
lhs.of.formula(~x)
rhs.of.formula(y ~ x, tilde=FALSE)
f <- ~x
lhs.of.formula(f) <- quote(y)
print(f)
can.be.formula("y ~ x")

