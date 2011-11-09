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
determineDepth <- function(n, k, alpha,  perc) {
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
