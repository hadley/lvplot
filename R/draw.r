# Compute table of k letter values for vector x
# alpha determines confidence level
lvtable <- function(x, k, alpha=0.95) {
  n <- length(x)
  if (2^k > n) k <- floor(log(n, base=2))
  
# depths for letter values 
  depth <- getDepth(k, n)

# letter value
  qu <- calcLV(x,k)
  
  tab <- matrix(c(c(rev(depth), depth[-1]), qu), ncol=2, dimnames=list(nameLV(k)[[2]], c("depth","LV")))

# confidence limits
  conf <- confintLV(x, k, alpha=alpha)
  
  cbind(tab, conf)
}


confintLV <- function(x, k, alpha=0.95) {
# confidence interval for k letter values
  n <- length(x)
  y <- sort(x)
  
  depth <- getDepth(k,n)
  extend <- ceiling(0.5 *sqrt(2*depth-1) * qnorm(alpha+(1-alpha)/2))
  low <- depth - extend
  high <- depth + extend
  clow <- pmax(0,ceiling(low))
  flow <- pmax(0,floor(low))
  chigh <- pmin(n, ceiling(high))
  fhigh <- pmin(n,floor(high))

  lvllow <- rev(rowMeans(cbind(y[clow],y[flow]), na.rm=T))
  if (length(lvllow) == 0) lvllow <- NA
  lvlhigh <- rev(rowMeans(cbind(y[chigh],y[fhigh]), na.rm=T))
  if (length(lvlhigh) == 0) lvlhigh <- NA
# no 1 is the median - that's the last element in lvl
  lvulow <- rowMeans(cbind(y[n-clow],y[n-flow]), na.rm=T)[-1]
  lvuhigh <- rowMeans(cbind(y[n-chigh],y[n-fhigh]), na.rm=T)[-1]
#  conf <- cbind(c(y[rev(low[-1])],y[n-high]),c(y[rev(high[-1])],y[n-low]))
  conf <- cbind(c(lvllow, lvulow), c(lvlhigh, lvuhigh))	

  colnames(conf) <- c(paste((1-alpha)/2*100,"%",sep=""),paste((alpha+(1-alpha)/2)*100,"%",sep=""))
  conf
}

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
