\name{LVboxplot.numeric}
\alias{LVboxplot.numeric}
\title{Draw a single LV boxplot.}
\usage{
  \method{LVboxplot}{numeric} (x, alpha = 0.95, k = NULL,
    perc = NULL, horizontal = TRUE, xlab = NULL,
    ylab = NULL, col = "grey30", bg = "grey90", ...)
}
\arguments{
  \item{x}{numeric vector of data}

  \item{alpha}{if supplied, depth k is calculated such that
  confidence intervals of width \code{alpha} of an LV
  statistic do not extend into neighboring LV statistics.}

  \item{k}{number of letter value statistics used}

  \item{perc}{if supplied, depth k is adjusted such that
  \code{perc} percent outliers are shown}

  \item{horizontal}{display horizontally (TRUE) or
  vertically (FALSE)}

  \item{xlab}{x axis label}

  \item{ylab}{y axis label}

  \item{col}{vector of colours to use}

  \item{bg}{background colour}

  \item{...}{passed onto \code{\link{plot}}}
}
\description{
  Draw a single LV boxplot.
}
\seealso{
  Other letter.value.boxplots: \code{\link{LVboxplot}},
  \code{\link{LVboxplot.formula}}
}

