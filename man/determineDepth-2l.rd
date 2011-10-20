\name{determineDepth}
\alias{determineDepth}
\title{Determine depth}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Determine number of letter values needed for n observations
}
\usage{determineDepth(n, k, alpha,  perc)}
\arguments{
\item{n}{number of observation to be shown in the LV boxplot}
\item{k}{number of letter value statistics used}
\item{alpha}{if defined, depth k is calculated such that confidence intervals of an LV statistic do not extend into neighboring LV statistics}
\item{perc}{if defined, depth k is adjusted such that \code{perc} percent outliers are shown}
}

\details{}

\examples{}
\keyword{internal}
