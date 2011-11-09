
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
#' @keywords internal
drawLVplot <- function(x,y,k,out,qu,horizontal,col,...) {
  if (horizontal) { 
	points(x[out],rep(y,length(x[out])),pch=8)		
	# draw boxes:
	for (i in 1:k) 
		rect(qu[i], y+i/(2*k),qu[2*k-i+1], y-i/(2*k), col=col[i])
  } else { # draw vertical plot
	points(rep(y,length(x[out])),x[out],pch=8)						 
	# draw boxes:
	for (i in 1:k) 
		rect(y+i/(2*k),qu[i], y-i/(2*k), qu[2*k-i+1], col=col[i])
  }
}
