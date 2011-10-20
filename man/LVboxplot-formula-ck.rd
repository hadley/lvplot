\name{LVboxplot.formula}
\alias{LVboxplot.formula}
\title{Side-by-side LV boxplots}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
An extension of standard boxplots which draws k letter statistics
}
\usage{LVboxplot.formula(formula,alpha=0.95, k=NULL, perc=NULL,horizontal=TRUE,col="grey",...)}
\arguments{
\item{formula}{the formula has to be of the form $y \tilde x$, where $x$ is a qualitative variable. The values of $y$ will be split into groups according to their values on $x$ and separate letter value box plots of $y$ are drawn side by side in the same display.}
\item{alpha}{significance level, if neither \code{k} nor \code{perc} is specified, \code{alpha} is used to determine how many letter values are to be used.}
\item{k}{percentage of data points to be shown individually (as outliers) outside the letter-value boxes. \code{perc} is only used, if \code{k} is not specified.  If used, $k$ is determined in such a way, that confidence intervals around each letter value statistics will not include neighboring letter value statistics at a significance level of \code{alpha}.}
\item{perc}{number of letter statistics to compute and draw}
\item{horizontal}{if defined, aim for \code{perc} percent outliers}
\item{col}{display horizontally (TRUE) or vertically (FALSE)}
\item{...}{specify base colour to use}
\item{}{unused}
}

\details{Conventional boxplots (Tukey 1977) are useful displays for conveying rough information
about the central 50\% of the data and the extent of the data.

For moderate-sized data sets ($n < 1000$), detailed estimates of tail behavior beyond
the quartiles may not be trustworthy, so the information provided by boxplots is
appropriately somewhat vague beyond the quartiles, and the expected number of
``outliers'' and ``far-out'' values for a Gaussian sample of size $n$ is often less
than 10 (Hoaglin, Iglewicz, and Tukey 1986). Large data sets ($n \approx
10,000-100,000$) afford more precise estimates of quantiles in the tails beyond the
quartiles and also can be expected to present a large number of ``outliers'' (about 0.4
+ 0.007$n$).

The letter-value box plot addresses both these shortcomings: it conveys more detailed
information in the tails using letter values, only out to the depths where the letter
values are reliable estimates of their corresponding quantiles (corresponding to tail
areas of roughly $2^{-i}$); ``outliers'' are defined as a function of the most extreme
letter value shown. All aspects shown on the letter-value boxplot are actual
observations, thus remaining faithful to the principles that governed Tukey's original
boxplot.}
\seealso{\code{\link{LVboxplot.numeric}}}
\examples{n <- 10
oldpar <- par()
par(mfrow=c(4,2), mar=c(3,3,3,3))
for (i in 1:4) {
x <- rexp(n*10^i)
boxplot(x,col="grey", horizontal=TRUE)
title(paste("Exponential, n=",length(x)))
LVboxplot(x,col="grey", xlab="")
}}
\keyword{hplot}
