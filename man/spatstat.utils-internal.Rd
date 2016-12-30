\name{spatstat.utils-internal} 
\title{Internal spatstat.utils functions}
\alias{adjustthinrange}
\alias{apply23sum}
\alias{articlebeforenumber}
\alias{as2vector}
\alias{asNumericMatrix}
\alias{assignDFcolumn}
\alias{badprobability}
\alias{blockdiagarray}
\alias{blockdiagmatrix}
\alias{cat.factor}
\alias{check.1.integer}
\alias{check.1.real}
\alias{check.1.string}
\alias{check.finite}
\alias{check.in.range}
\alias{check.named.list}
\alias{check.named.thing}
\alias{check.named.vector}
\alias{check.nvector}
\alias{check.range}
\alias{commasep}
\alias{complaining}
\alias{dont.complain.about}
\alias{dotexpr.to.call}
\alias{dropifsingle}
\alias{dround}
\alias{ensure2vector}
\alias{ensure3Darray}
\alias{equispaced}          
\alias{evenly.spaced}
\alias{exceedsMaxArraySize}
\alias{fakecallstring}
\alias{fastFindInterval}
\alias{fave.order}
\alias{fillseq}
\alias{findfirstfactor}
\alias{firstfactor}
\alias{flat.deparse}
\alias{fontify}
\alias{forbidNA}
\alias{geomseq}
\alias{getdataobjects}
\alias{good.names}
\alias{gsubdot}
\alias{ifelse0NA}
\alias{ifelse1NA}
\alias{ifelseAB}
\alias{ifelseAX}
\alias{ifelseNegPos}
\alias{ifelseXB}
\alias{ifelseXY}
\alias{indexCartesian}
\alias{inject.expr}
\alias{insertinlist}     
\alias{inside.range}     
\alias{intersect.ranges}     
\alias{is.blank}
\alias{is.parseable}
\alias{lty2char}
\alias{make.parseable}
\alias{mapstrings}
\alias{matchNameOrPosition}
\alias{matcolall}
\alias{matcolany}
\alias{matcolsum}
\alias{matrixsample}
\alias{matrowall}
\alias{matrowany}
\alias{matrowsum}
\alias{natozero}
\alias{niceround}
\alias{numalign}
\alias{nzpaste}
\alias{orderstats}
\alias{orderwhich}
\alias{ordinal}
\alias{ordinalsuffix}
\alias{\%orifnull\%}  %DoNotExport
%NAMESPACE export("%orifnull%")
\alias{padtowidth}
\alias{paren}
\alias{paste.expr}
\alias{pasteN}
\alias{prange}
\alias{prettydiscrete}
\alias{prettyinside}
\alias{prolongseq}
\alias{ratiotweak}
\alias{revcumsum}
\alias{romansort}
\alias{samefunction}
\alias{sensiblevarname}
\alias{short.deparse}
\alias{simplenumber}              
\alias{singlestring}              
\alias{startinrange}
\alias{strsplitretain}
\alias{substringcount}
\alias{there.is.no.try}
\alias{trap.extra.arguments}
\alias{truncline}
\alias{unparen}
\alias{uptrimat}
\alias{validposint}
\alias{variablesintext}
\alias{verbalogic}
\alias{warn.ignored.args}

\description{
  Internal spatstat functions.
}
\usage{
adjustthinrange(ur,vstep,vr)
apply23sum(x)
articlebeforenumber(k)
as2vector(x)
asNumericMatrix(x)
assignDFcolumn(x, name, value, \dots)
badprobability(x, NAvalue)
blockdiagarray(\dots)
blockdiagmatrix(\dots)
cat.factor(\dots, recursive=FALSE)
check.1.integer(x, context, fatal)
check.1.real(x, context, fatal)
check.1.string(x, context, fatal)
check.finite(x, context, xname, fatal, usergiven)
check.in.range(x, r, fatal)
check.named.list(x, nam, context, namopt, onError)
check.named.thing(x, nam, namopt, xtitle, valid, type, context, fatal)
check.named.vector(x, nam, context, namopt, onError)
check.nvector(v, npoints, fatal, things, naok, warn, vname, oneok)
check.range(x, fatal)
commasep(x, join, flatten)
complaining(whinge, fatal, value)
dont.complain.about(\dots)
dotexpr.to.call(expr, dot, evaluator)
dropifsingle(x)
dround(x)
ensure2vector(x)
ensure3Darray(x)
equispaced(z, reltol)
evenly.spaced(x, tol)
exceedsMaxArraySize(\dots)
fakecallstring(fname, parlist)
fastFindInterval(x, b, labels, reltol)
fave.order(x)
fillseq(x, step)
findfirstfactor(x)
firstfactor(x)
flat.deparse(x)
fontify(x, font)
forbidNA(x, context, xname, fatal, usergiven)
geomseq(from, to, length.out)
getdataobjects(nama, envir, datalist, fatal)
good.names(nama, defaults, suffices)
gsubdot(replacement, x)
ifelse0NA(test)
ifelse1NA(test)
ifelseAB(test, a, b)
ifelseAX(test, a, x)
ifelseNegPos(test, x)
ifelseXB(test, x, b)
ifelseXY(test, x, y)
indexCartesian(nn)
inject.expr(base, expr)
insertinlist(x, i, y)
inside.range(x, r)
intersect.ranges(a,b,fatal)
is.blank(s)
is.parseable(x)
lty2char(i)
make.parseable(x)
mapstrings(x, map)
matchNameOrPosition(expected, avail)
matcolall(x)
matcolany(x)
matcolsum(x)            
matrixsample(mat, newdim, phase, scale, na.value)
matrowall(x)
matrowany(x)
matrowsum(x)
natozero(x)
niceround(x, m)
numalign(i, nmax, zero)
nzpaste(\dots, sep, collapse)
orderstats(x, k, decreasing)
orderwhich(x, k, decreasing)
ordinal(k)
ordinalsuffix(k)
a \%orifnull\% b
padtowidth(a, b, justify)
paren(x, type)
paste.expr(x)
pasteN(\dots)
prange(x)
prettydiscrete(x, n)
prettyinside(x, \dots)
prolongseq(x, newrange, step)
ratiotweak(a, b, overzero, zerozero)
revcumsum(x)
romansort(x)
samefunction(f, g)
sensiblevarname(guess, fallback, maxlen)
short.deparse(x, maxlen)
simplenumber(x, unit, multiply, tol)
singlestring(s, coll)
startinrange(x0, dx, r)
strsplitretain(x, split)
substringcount(x,y)
there.is.no.try(\dots)
trap.extra.arguments(\dots, .Context, .Fatal)
truncline(x, nc)
unparen(x)
uptrimat(x)
validposint(n, caller, fatal)
variablesintext(x)
verbalogic(x, op)
warn.ignored.args(\dots, context)
}
\details{
  These internal \pkg{spatstat.utils} functions are not usually called
  directly by the user. Their names and capabilities may change
  without warning from one version of \pkg{spatstat.utils} to the next.
}
\keyword{internal}