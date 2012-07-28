
#' Compute letter value summary table.
#' 
#' @param x numeric vector
#' @param qu quantiles to compute
#' @param out binary vector of outliers (\code{TRUE} for outlier, 
#'   \code{FALSE} otherwise)
#' @inheritParams determineDepth
#' @keywords internal
#' @return
#'  \item{letter.val}{letter value statistic, distinguishes between upper and
#'    lower LV statistic for all statistics but the median}
#'  \item{conf.int}{confidence interval of corresponding letter value
#'    statistic}
#'  \item{out}{list of defined outliers}
outputLVplot <- function(x,qu,k,out,alpha) {
  n <- length(x)
  
  depth <- getDepth(k,n)
  
  LV <- cbind(depth,lower=qu[k:1],upper=qu[k-1+1:k])
  conf <- confintLV(x, k, alpha=alpha)

  dimnames <- nameLV(k)
  row.names(LV) <- dimnames[[1]]
  row.names(conf) <- dimnames[[2]]

  result <- list(letter.val = LV, conf.int= conf,outliers = which(out))
  return(result)
}

#' Draw an LV plot.
#'
#' @param x x positions
#' @param y y positions
#' @param k number of letter value statistics used
#' @param out indices of outliers
#' @param qu quantiles
#' @param horizontal display horizontally (TRUE) or vertically (FALSE)
#' @param col vector of colours to use
#' @param width height/width of box
#' @keywords internal
drawLVplot <- function(x,y,k,out,qu,horizontal,col,width = 0.9,...) {
  i <- seq_len(k)
  offset <- (i / (2 * k)) * width
  y <- rep(y, length(x))
  
  lower <- i
  upper <- rev(seq_len(k) + k - 1)
  
  if (horizontal) { 
    points(x[out], y[out], pch = 1, cex=0.7)        
    rect(qu[lower], y[i] + offset, qu[upper], y[i] - offset, col = col)
  } else { # draw vertical plot
    points(y[out], x[out], pch = 1, cex=0.7)                         
    rect(y[i] + offset, qu[lower], y[i] - offset, qu[upper], col = col)
  }
}
