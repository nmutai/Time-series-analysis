## AR(1) simulation function
ar1sim <- function(nrep = 100, n = 100, ar = 0.5, intercept = 0, sd = 1, burnin = 20) {
  ## empty matrix
  y <- matrix(NA, nrow = n + burnin, ncol = nrep)

  ## initialization
  y[1,] <- intercept

  ## AR(1) simulation for every column (= replication) through for() loop
  ## filter() would be a faster alternative
  for(j in 1:nrep) {
    for(i in 2:(n + burnin)) {
      y[i, j] <- intercept + ar * y[i-1, j] + rnorm(1, sd = sd)
    }
  }

  ## drop burnin
  y <- tail(y, -burnin)

  ## return with class attribute
  rval <- list(
    series = y,
    parameters = c(ar = ar, intercept = intercept, sd = sd)
  )
  class(rval) <- "ar1sim"
  return(rval)
}

## compute relevant summary statistics
summary.ar1sim <- function(object, ...) {
  empmean <- apply(object$series, 1, mean)
  empvar <- apply(object$series, 1, var)
  rval <- list(
    empirical_mean = mean(empmean),
    empirical_mean_function = empmean,
    empirical_var = mean(empvar),
    empirical_var_function = empvar,
    theoretical_mean = object$parameters["intercept"] /(1 - object$parameters["ar"]),
    theoretical_var = object$parameters["sd"]^2/(1 - object$parameters["ar"]^2)
  )
  class(rval) <- "summary.ar1sim"
  return(rval)
}

## print properties of generated AR(1) series
print.ar1sim <- function(x, ...) {
  cat("Simulated AR(1) series\n\n")
  cat("Number of replications:", ncol(x$series), "\n")
  cat("Length of series:", nrow(x$series), "\n\n")
  cat("Parameters:\n")
  print(x$parameters)
  invisible(x)
}

## print summary statistics (omit *_function and rest in 2x2 table)
print.summary.ar1sim <- function(x, ...) {
  tab <- matrix(c(
    x$theoretical_mean, x$empirical_mean,
    sqrt(x$theoretical_var), sqrt(x$empirical_var)
  ), ncol = 2)
  rownames(tab) <- c("theoretical", "empirical")
  colnames(tab) <- c("mean", "sd")
  print(tab, ...)
  invisible(x)
}

## visualization
plot.ar1sim <- function(object, col = NULL, lwd = 2, ...) {
  ## obtain summary statistics
  s <- summary(object)
  
  ## default colors: semi-transparent gray, black, red
  if(is.null(col)) col <- c(gray(0.1, alpha = 10/ncol(object$series)),
    "blue", "red")
  
  ## AR(1) replications
  matplot(object$series, type = "l", ylab = "AR(1) series", xlab = "Time", col = col[1], lty = 1, lwd = lwd, ...)

  ## empirical vs. theoretical statistics
  critval <- qnorm(c(0.025, 0.975))
  lines(s$empirical_mean_function, col = col[3])
  lines(s$empirical_mean_function + critval[1] * sqrt(s$empirical_var_function), col = col[3])
  lines(s$empirical_mean_function + critval[2] * sqrt(s$empirical_var_function), col = col[3])
  abline(h = s$empirical_mean, col = col[3], lwd = lwd)
  abline(h = s$empirical_mean + critval * sqrt(s$empirical_var), col = col[3], lwd = lwd)
  abline(h = s$theoretical_mean, lwd = lwd, col = col[2])
  abline(h = s$theoretical_mean + critval * sqrt(s$theoretical_var), lwd = lwd, col = col[2])
}  

