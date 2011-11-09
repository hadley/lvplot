# Get a vector of data depths for k letter values
getDepth <- function(k, n) {
   # compute letter values based on depth
     depth <- rep(0,k)
     depth[1] <- (1+n)/2
     if (k > 1) {
       for (j in 2:k) depth[j] <- (1+floor(depth[j-1]))/2
     }
  depth
}

# Calculate first k letter values for vector x
calcLV <- function(x, k) {
# compute letter values based on depth  
  n <- length(x)
  depth    <- getDepth(k,n)
  
  y <- sort(x)
  d <- c(rev(depth),n-depth+1)
  qu <- (y[floor(d)] + y[ceiling(d)])/2 	
    # floor and ceiling is the same for .0 values
  	# .5 values yield average of two neighbours

# k, k+1 is the median 
# report only once
  qu[-k]
}

# Determine names of first k letter values
# output is (1) list of names starting with 'M'edian, and
# (2) a vector of letter values ordered by rank from lower kth letter value to upper k letter value
nameLV <- function(k) {
	# list of 
	#	k letter values (starting with median)
	# 	lower/upper letter values ordered from lowest to highest

	idx <- (((k-1):1 + (6-k)) %% 26) + 1
	lvs <- toupper(letters[idx])
    LV <- c('M',lvs)
    conf <- c(paste(rev(lvs),"l",sep=""),
             'M',paste(lvs,"u",sep=""))
	list(LV, conf)
}

#' Determine depth of letter values needed for n observations.
#' 
#' @details Supply one of \code{k}, \code{alpha} or \code{perc}.
#'
#' @param n number of observation to be shown in the LV boxplot
#' @param k number of letter value statistics used 
#' @param alpha if supplied, depth k is calculated such that confidence
#'   intervals of width \code{alpha} of an LV statistic do not extend into
#'   neighboring LV statistics. 
#' @param perc if supplied, depth k is adjusted such that \code{perc} percent
#'   outliers are shown 
#' @export
determineDepth <- function(n, k, alpha, perc) {
  if (!is.null(perc)) {
  	# we're aiming for perc percent of outlying points
  	k <- ceiling((log2(n))+1) - ceiling((log2(n*perc*0.01))+1)+1
  }
  if (is.null(k)) { 
  	# confidence intervals around an LV statistic 
  	# should not extend into surrounding LV statistics

  	k <- ceiling((log2(n))-log2(2*qnorm(alpha+(1-alpha)/2)^2))  
  }
  if (k < 1) k <- 1	
 
  return (k)
}

#' Compute table of k letter values for vector x
#' 
#' @param x input numeric vector
#' @param k number of letter values to compute
#' @param alpha alpha-threshold for confidence level
#' @export
lvtable <- function(x, k, alpha=0.95) {
  n <- length(x)
  if (2^k > n) k <- floor(log(n, base=2))
  
  # depths for letter values 
  depth <- getDepth(k, n)

  # letter value
  qu <- calcLV(x,k)
  
  tab <- matrix(c(c(rev(depth), depth[-1]), qu), ncol = 2,
    dimnames = list(nameLV(k)[[2]], c("depth","LV")))

  # confidence limits
  conf <- confintLV(x, k, alpha = alpha)
  
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
