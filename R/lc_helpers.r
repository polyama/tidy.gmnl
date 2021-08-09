###########################################################
# A series of functions (helpers) for LC-MNL using gmnl.
# By: Mauricio Sarrias
###########################################################

shares <- function(obj){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  Q <- length(cons_class)
  shares <- exp(cons_class) / sum(exp(cons_class))
  names(shares) <- paste("share q", 1:Q, sep = "=")  
  return(shares)
}


plot_ci_lc <- function(obj, var = NULL, mar = c(2, 5, 2, 2),
                       cex.pts = 0.9, cex.var = 0.8, 
                       var.las = 2, pch.pts = 20, col.pts = 1, ...){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  se   <- sqrt(diag(vcov(obj)))
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  name.x <- if (is.null(var)) names(obj$mf)[-1] else var
  Q <- length(cons_class)
  lc.names <- c()
  for (i in 1:length(name.x)) {
    lc.names <- c(lc.names, paste("class", 1:Q, name.x[i], 
                                  sep = "."))
  }
  bhat <- bhat[lc.names]
  se   <- se[lc.names]
  
  u <-  bhat + 1.96 * se
  l <-  bhat - 1.96 * se
  n.c <- length(bhat)
  idx <- seq(1, n.c)
  k <- 1 / n.c
  
  par(mar = mar)
  plot(c(l, u), c(idx + k, idx - k), 
       type = "n", axes = F, main = "" , xlab = "", 
       ylab = "", ...)
  axis(3)
  axis(2, n.c:1, names(bhat)[n.c:1], las = var.las, 
       tck = FALSE, lty = 0, cex.axis = cex.var)
  abline(v = 0, lty = 2)
  points(bhat, idx, pch = pch.pts, cex = cex.pts, 
         col = col.pts)
  segments(l, idx, u, idx, lwd = 2, 
           col = "red")
}