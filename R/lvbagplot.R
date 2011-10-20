determine2dDepth <- function(n,k,alpha,perc) {
# by default, subtracts 3 levels from one dimensional solution
# --- no mathematical foundation for doing this so far

	k <- determineDepth(n,k,alpha,perc)-3	
    if (k < 1) 
        k <- 1
    return(k)
}

LVbagplot <- function(x, ...) UseMethod("LVbagplot",x)

LVbagplot.formula <- function(formula,alpha=0.95, k=NULL, perc=NULL,horizontal=T,col="grey", method="depth", ...) {
    deparen <- function(expr) {
        while (is.language(expr) && !is.name(expr) && deparse(expr[[1]]) == 
            "(") expr <- expr[[2]]
        expr
    }
    bad.formula <- function() stop("invalid formula; use format y ~ x")
    bad.lengths <- function() stop("incompatible variable lengths")
    
    formula <- deparen(formula)
    if (!inherits(formula, "formula")) 
        bad.formula()
    z <- deparen(formula[[2]])
    x <- deparen(formula[[3]])
    rhs <- deparen(formula[[3]])
    if (is.language(rhs) && !is.name(rhs) && (deparse(rhs[[1]]) == 
        "*" || deparse(rhs[[1]]) == "+")) {
        bad.formula()
    }
    z.name <- deparse(z)
    z <- eval(z,  parent.frame())
    x.name <- deparse(x)
    x <- eval(x,  parent.frame())

	LVbagplot.numeric(x,z, alpha, k, perc, col, method, ...)
}


LVbagplot.numeric <- function(x,y, alpha=0.95, k=NULL, perc=NULL, col="grey", method="depth", ...) {
    win <- function(dx, dy) {
        atan2(y = dy, x = dx)
    }

  if (missing(y)) {
  	print("don't have y")
  }
	
  n <- length(x)
  if (length(y) != n) stop("x and y do not have the same length")
  
  k <- determine2dDepth(n,k,alpha,perc) 
  src.col <- col 

  if (! is.na(src.col)) { 
	   		colrgb <- col2rgb(src.col)
	   		colhsv <- rgb2hsv(colrgb)
			if (colhsv[2,1] == 0) {
	   			val <- seq(0.9,colhsv[3,1], length.out=k)
	   			colrgb <- col2rgb(hsv(colhsv[1,1], colhsv[2,1], val))
			} else {
	   			sat <- seq(0.1,colhsv[2,1], length.out=k)
	   			colrgb <- col2rgb(hsv(colhsv[1,1], sat, colhsv[3,1]))
			}
	   		col <- rgb(colrgb[1,],colrgb[2,],colrgb[3,], maxColorValue=255)
	   		col <- rev(col)	
  }
  else { col <- rep("grey",k) }

  xy <- cbind(x,y)

if (method=="mine") {
# compute halfspace depth
  i <- 1

  m <- nrow(xy)
  res <- numeric(0)

  while ((!is.null(m)) && (m > 0)) {
    pts <- chull(xy)
    res <- rbind(res, cbind(xy[pts,], rep(i,length(pts))))
    xy <- xy[-pts,]
    m <- dim(xy)[1]
    i <- i+1
  }
} 
if (method=="apl") {

}
if (method=="depth") {
	require(depth)
	dep <- vector(length=nrow(xy))
	for (i in 1:nrow(xy))
		dep[i] <- depth(xy[i,],xy)
	res <- cbind(xy,round(dep*nrow(xy)))
}
# compute median as average of points with maximal halfspace depth
	med <- res[which(res[,3]==max(res[,3])),]
	medx <- mean(med[,1])
	medy <- mean(med[,2])

# draw LV polygons
	plot(x,y, type="n")
	for (i in k:1) {
		dd <- 2^i
		Q <- res[n/dd,3]
		tp <- res[which(res[,3]==Q),1:2]
		angle <- win(tp[,1]-medx, tp[,2]-medy)
		ord <- order(angle)
		polygon(tp[ord,], col=col[i], density=-1)
	}
	points(medx, medy, pch=20)
	Qmin <- res[n/2^k,3]

	points(res[which(res[,3]>=Qmin),1:2], pch=".")
	points(res[which(res[,3]<Qmin),1:2], ...)
}