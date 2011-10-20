\name{LVboxplot.numeric}
\alias{LVboxplot.numeric}
\title{Single LV boxplot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Produces a single lettervalue boxplot for the specified data.
}
\usage{LVboxplot.numeric(x,alpha=0.95, k=NULL, perc=NULL,horizontal=TRUE,col="grey",...)}
\arguments{
\item{x}{alpha level for significance level: alpha 100\% confidence intervals do not touch neighboring LV statistics}
\item{alpha}{number of letter statistics to compute and draw}
\item{k}{if defined, aim for \code{perc} percent outliers}
\item{perc}{display horizontally (TRUE) or vertically (FALSE)}
\item{horizontal}{specify base colour to use}
\item{col}{unused}
\item{...}{}
}

\details{}
\seealso{\code{\link{LVboxplot.formula}}}
\examples{}
\keyword{hplot}
