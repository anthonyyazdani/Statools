#' Mean Median Lambda
#'
#' @param Data Numeric vector.
#' @param lambda Optimization interval.
#'
#' @return The lambda and the added value such that the mean is equal to the median.
#' @importFrom stats uniroot
#' @importFrom stats optimize
#' @importFrom stats median
#' @export
#'
MMLambda=function(Data, lambda = c(-10,10)){
  if(is.numeric(Data)){
    if(sum(Data == 0) + sum(Data == 1) != length(Data)){
    added = 0
    if(min(Data)<=0){added = ceiling(abs(min(Data))); Data = (Data + added)}
    f=function(x){mean(((Data^x)-1)/x)-median(((Data^x)-1)/x)}
      root = uniroot(f, lower = lambda[1], upper = lambda[2], extendInt = c("yes"))$root
      return(list(lambda = root, Added.value = added))
    }else{stop("Box cox transformation is useless for binary data.")}
  }else{stop("Input should be a numeric vector")}
}
