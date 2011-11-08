#' Compute letter value summary table.
#' 
#' @param x numeric vector
#' @param qu quantiles to compute
#' @param out binary vector of outliers (\code{TRUE} for outlier, 
#'   \code{FALSE} otherwise)
#' @param depth depth of the corresponding LV statistic (i.e. how far from the
#'    outside do we have to go into the sorted data values?)
#' @inheritParams determineDepth
#' @return
#'  \item{letter.val}{letter value statistic, distinguishes between upper and
#'    lower LV statistic for all statistics but the median}
#'  \item{conf.int}{confidence interval of corresponding letter value
#'    statistic}
#'  \item{out}{list of defined outliers}
outputLVplot <- function(x,qu,k,out,depth,alpha) {
  n <- length(x)

  extend <- ceiling(0.5 *sqrt(2*depth-1) * qnorm(alpha+(1-alpha)/2))
  low <- depth - extend
  high <- depth + extend
  clow <- pmax(0,ceiling(low))
  flow <- pmax(0,floor(low))
  chigh <- pmin(n, ceiling(high))
  fhigh <- pmin(n,floor(high))
  
  LV <- cbind(depth,lower=qu[k:1],upper=qu[k+1:k])
  y <- sort(x)
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
  if (k > 1) {
    which <- (((k-1):1 + (6-k)) %% 26) + 1
    row.names(LV) <- c('M',toupper(letters[which]))
    row.names(conf) <- c(paste(toupper(letters[rev(which)]),"l",sep=""),'M',paste(toupper(letters[which]),"u",sep=""))
  } 
  if (k == 1) {
    row.names(LV) <- 'M'
    row.names(conf) <- 'M'
  }

  result <- list(letter.val = LV, conf.int= conf,outliers = x[out])
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
