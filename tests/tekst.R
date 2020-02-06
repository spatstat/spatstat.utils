#'  spatstat.utils/tests/tekst.R

require(spatstat.utils)

#' tests of R/utiltext.R

a <- paren(character(0))
a <- paren("hello", "")
a <- paren("hello", "{")

strsplitretain("hello, world")

truncline(c("Now is the time for all good people",
            "to come to the aid of the Party"),
          15)

is.blank(c("a", " ", "b"))

onetwo <- c("one", "two")
padtowidth(onetwo, 10, "left")
padtowidth(onetwo, 10, "right")
padtowidth(onetwo, 10, "centre")

splat("Hello world", indent="zzz")

choptext("Hello\nWorld")
exhibitStringList("Letters", letters)
exhibitStringList("Letters", letters[1:4])

numalign(42, 1e4)
singlestring(1:5)

x <- c("TRUE", "unknown", "not known")
verbalogic(x, "and")
verbalogic(x, "or")
verbalogic(x, "not")
x[1] <- "FALSE"
verbalogic(x, "and")

sensiblevarname("$@wtf%!", "variablenumberone")
nzpaste(c("Hello", "", "World"))
substringcount("v", "vavavoom")
huh <- c("42", "y <- x", "$%^%$")
is.parseable(huh)
make.parseable(huh)
paste.expr(expression(y == x))
pasteFormula(y ~ x + z)
gsubdot("cbind(est,theo)", ". ~ r")

simplenumber(0)
simplenumber(1/3)
simplenumber(2/3)
simplenumber(-2)
simplenumber(0, unit="km")
simplenumber(1/3, unit="km")
simplenumber(2/3, unit="km")
simplenumber(-2, unit="km")

romansort(c("Eb", "Au"))
makeCutLabels(0:3)
