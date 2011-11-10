\name{determineDepth}
\alias{determineDepth}
\title{Determine depth of letter values needed for n observations.}
\usage{
  determineDepth(n, k = NULL, alpha = NULL, perc = NULL)
}
\arguments{
  \item{n}{number of observation to be shown in the LV
  boxplot}

  \item{k}{number of letter value statistics used}

  \item{alpha}{if supplied, depth k is calculated such that
  confidence intervals of width \code{alpha} of an LV
  statistic do not extend into neighboring LV statistics.}

  \item{perc}{if supplied, depth k is adjusted such that
  \code{perc} percent outliers are shown}
}
\description{
  Determine depth of letter values needed for n
  observations.
}
\details{
  Supply one of \code{k}, \code{alpha} or \code{perc}.
}

