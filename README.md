# DEBUGGING
tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim = dim(x))
  for(j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] & tukey.outlier(x[,j])
  }
  Outlier.vec <- vector(length=nrow(x))
  for(i in 1:nrow(x))
  { Outlier.vec[i] <-all(outliers[i])} 
  return(Outlier.vec)}

tukey.outlier <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  return(x < q3 - 1.5 *iqr | x > q3 + 1.5 * iqr)
}


x <- matrix(rnorm(10), nrow = 5, ncol = 2)
debug(tukey_multiple)
tukey_multiple(x)
