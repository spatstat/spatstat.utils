\name{spatstat.utils-internal} 
\title{Internal Functions of spatstat.utils Package}
\alias{adjustthinrange}
\alias{apply23sum}
\alias{as2vector}
\alias{asNumericMatrix}
\alias{assignDFcolumn}
\alias{badprobability}
\alias{blockdiagarray}
\alias{blockdiagmatrix}
\alias{check.finite}
\alias{choptext}
\alias{choptextline}
\alias{complaining}
\alias{distpl}               
\alias{distppl}
\alias{distppll}
\alias{distppllmin}
\alias{dont.complain.about}
\alias{dotexpr.to.call}
\alias{dropifsingle}
\alias{dround}
\alias{ensure2vector}
\alias{ensure3Darray}
\alias{equispaced}          
\alias{eratosthenes}
\alias{evenly.spaced}
\alias{exceedsMaxArraySize}
\alias{exhibitStringList}
\alias{explain.ifnot}
\alias{fakecallstring}
\alias{fastFindInterval}
\alias{fave.order}
\alias{fillseq}
\alias{findfirstfactor}
\alias{firstfactor}
\alias{flat.deparse}
\alias{fontify}
\alias{forbidNA}
\alias{getSpatstatLocatorQueue}
\alias{geomseq}
\alias{getdataobjects}
\alias{good.names}
\alias{graphicsPars}            
\alias{gsubdot}
\alias{indexCartesian}
\alias{inject.expr}
\alias{insertinlist}     
\alias{is.blank}
\alias{is.parseable}
\alias{lty2char}
\alias{makeCutLabels}
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
\alias{NNdist2segments}
\alias{numalign}
\alias{nzpaste}
\alias{orderstats}
\alias{orderwhich}
\alias{\%orifnull\%}  %DoNotExport
%NAMESPACE export("%orifnull%")
\alias{padtowidth}
\alias{passthrough}
\alias{paste.expr}
\alias{pasteFormula}
\alias{pasteN}
\alias{prettydiscrete}
\alias{prettyinside}
\alias{prolongseq}
\alias{putSpatstatLocatorQueue}
\alias{ratiotweak}
\alias{romansort}
\alias{samefunction}
\alias{sensiblevarname}
\alias{short.deparse}
\alias{singlestring}
\alias{sortunique}
\alias{startinrange}
\alias{strsplitretain}
\alias{substringcount}
\alias{there.is.no.try}
\alias{trap.extra.arguments}
\alias{truncline}
\alias{uptrimat}
\alias{validposint}
\alias{variablesintext}
\alias{warn.ignored.args}
%%
\alias{inside.triangle}
\alias{overlap.trapezium}
%%
\alias{xysegXcircle}
\alias{xysegMcircle}
\alias{xysegPcircle}
%%
\alias{matchIntegerDataFrames}
\description{
  Internal utility functions of the \code{spatstat.utils} package.
}
\usage{
adjustthinrange(ur,vstep,vr)
apply23sum(x)
as2vector(x)
asNumericMatrix(x)
assignDFcolumn(x, name, value, \dots)
badprobability(x, NAvalue)
blockdiagarray(\dots)
blockdiagmatrix(\dots)
check.finite(x, context, xname, fatal, usergiven)
choptext(\dots, prefix, indent)
choptextline(st, w, prefix, indent)
complaining(whinge, fatal, value)
distpl(p, l)               
distppl(p, l)
distppll(p, l, mintype, method, listit)
distppllmin(p, l, big)
dont.complain.about(\dots)
dotexpr.to.call(expr, dot, evaluator)
dropifsingle(x)
dround(x)
ensure2vector(x)
ensure3Darray(x)
equispaced(z, reltol)
eratosthenes(nmax, startset)
evenly.spaced(x, tol)
exhibitStringList(prefix, strings)
exceedsMaxArraySize(\dots)
explain.ifnot(expr, context)
fakecallstring(fname, parlist)
fastFindInterval(x, b, labels, reltol, dig.lab)
fave.order(x)
fillseq(x, step)
findfirstfactor(x)
firstfactor(x)
flat.deparse(x)
fontify(x, font)
forbidNA(x, context, xname, fatal, usergiven)
getSpatstatLocatorQueue()
geomseq(from, to, length.out)
getdataobjects(nama, envir, datalist, fatal)
good.names(nama, defaults, suffices)
graphicsPars(key)
gsubdot(replacement, x)
indexCartesian(nn)
inject.expr(base, expr)
insertinlist(x, i, y)
is.blank(s)
is.parseable(x)
lty2char(i)
makeCutLabels(breaks, dig.lab, right, include.lowest)
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
NNdist2segments(xp, yp, x0, y0, x1, y1, bigvalue)
numalign(i, nmax, zero)
nzpaste(\dots, sep, collapse)
orderstats(x, k, decreasing)
orderwhich(x, k, decreasing)
a \%orifnull\% b
padtowidth(a, b, justify)
passthrough(.Fun, \dots, .Fname)
paste.expr(x)
pasteFormula(f)
pasteN(\dots)
prettydiscrete(x, n)
prettyinside(x, \dots)
prolongseq(x, newrange, step)
putSpatstatLocatorQueue(x)
ratiotweak(a, b, overzero, zerozero)
romansort(x)
samefunction(f, g)
sensiblevarname(guess, fallback, maxlen)
short.deparse(x, maxlen)
singlestring(s, coll)
sortunique(x)
startinrange(x0, dx, r)
strsplitretain(x, split)
substringcount(x,y)
there.is.no.try(\dots)
trap.extra.arguments(\dots, .Context, .Fatal)
truncline(x, nc)
uptrimat(x)
validposint(n, caller, fatal)
variablesintext(x)
warn.ignored.args(\dots, context)
%% xypolygon %%%
inside.triangle(x, y, xx, yy)
overlap.trapezium(xa, ya, xb, yb, verb = FALSE)
%%
xysegXcircle(xcentres, ycentres, radii, x0, y0, x1, y1, check)
xysegMcircle(xcentres, ycentres, radmat, x0, y0, x1, y1, check)
xysegPcircle(xc, yc, rc, x0, y0, x1, y1, check)
%%
matchIntegerDataFrames(X,Y,sort)
}
\details{
  These internal \pkg{spatstat.utils} functions are not usually called
  directly by the user. Their names and capabilities may change
  without warning from one version of \pkg{spatstat.utils} to the next.
}
\keyword{internal}
