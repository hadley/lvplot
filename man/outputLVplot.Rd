\name{outputLVplot}
\alias{outputLVplot}
\title{Compute letter value summary table.}
\usage{
  outputLVplot(x, qu, k, out, depth, alpha)
}
\arguments{
  \item{x}{numeric vector}

  \item{qu}{quantiles to compute}

  \item{out}{binary vector of outliers (\code{TRUE} for
  outlier, \code{FALSE} otherwise)}

  \item{depth}{depth of the corresponding LV statistic
  (i.e. how far from the outside do we have to go into the
  sorted data values?)}

  \item{k}{number of letter value statistics used}

  \item{alpha}{if supplied, depth k is calculated such that
  confidence intervals of width \code{alpha} of an LV
  statistic do not extend into neighboring LV statistics.}
}
\value{
  \item{letter.val}{letter value statistic, distinguishes
  between upper and lower LV statistic for all statistics
  but the median} \item{conf.int}{confidence interval of
  corresponding letter value statistic} \item{out}{list of
  defined outliers}
}
\description{
  Compute letter value summary table.
}
\keyword{internal}

