#' Shapiro Lambda
#'
#' @param Data Numeric vector.
#' @param lambda Optimization interval.
#'
#' @return The lambda and the added value such that the p value of the shapiro test for normality is maximum.
#' @importFrom stats optimize
#' @importFrom stats shapiro.test
#' @export
#'
SHLambda=function(Data, lambda = c(-10,10)){
  if(is.numeric(Data)){
    if(sum(Data == 0) + sum(Data == 1) != length(Data)){
      added = 0
      if(min(Data)<=0){
        added = ceiling(abs(min(Data)))
        Data = (Data + added)
      }
      Data = sample(Data, min(length(Data),4999))
      f=function(x){shapiro.test(jitter(((Data^x)-1/x), factor = 1e-06))$p.value}
      opt = optimize(f, lower = lambda[1], upper = lambda[2], maximum = TRUE)$objective
      return(list(lambda = opt, Added.value = added))
    }else{stop("Box cox transformation is useless for binary data.")}
  }else{stop("Input should be a numeric vector")}
}
