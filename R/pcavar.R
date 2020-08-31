#' Variance objective PCA
#'
#' @param Data Numeric or complex matrix whose SVD decomposition is to be computed. Logical matrices are coerced to numeric.
#' @param Minvar Minimum variance to be explained.
#'
#' @return Scores, transpose of the eigen vectors matrix and the approximated data with the required explained variance.
#' @export
#'
pcavar=function(Data, Minvar = 99.99999){
  if(Minvar>=1 & Minvar<=100){
    TEMP = svd(Data)
    Ind = max(2,which((cumsum(TEMP$d^2)/sum(TEMP$d^2))>=(Minvar/100))[1])
    Scores = TEMP$u[,1:Ind] %*% diag(TEMP$d)[1:Ind,1:Ind]
    EigenT = t(TEMP$v[,1:Ind])
    return(list("Scores" = Scores, "EigenT" = EigenT, Approximation = Scores%*%EigenT))
  }else{stop("'Minvar' must belong to the interval [1,100]")}
}
