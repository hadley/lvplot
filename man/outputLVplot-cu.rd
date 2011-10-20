\name{outputLVplot}
\alias{outputLVplot}
\title{LV summary table}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create letter value summary table
}
\usage{outputLVplot(x,qu,k,out,depth,alpha)}
\arguments{
\item{x}{numeric vector}
\item{qu}{quantiles to compute}
\item{k}{number of letter statistics}
\item{out}{list of outliers}
\item{depth}{depth of the corresponding LV statistic (i.e. how far from the outside do we have to go into the sorted data values?)}
\item{alpha}{significance level}
}
\value{
 \item{letter.val: letter value statistic, distinguishes between upper and lower LV statistic for all statistics but the median}
 \item{conf.int: confidence interval of corresponding letter value statistic}
 \item{out: list of defined outliers}
}
\details{}

\examples{}
\keyword{internal}
