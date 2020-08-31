#' Dimension objective PCA
#'
#' @param Data Numeric or complex matrix whose SVD decomposition is to be computed. Logical matrices are coerced to numeric.
#' @param Dim Exact sublinear dimension required.
#'
#' @return Scores, transpose of the eigen vectors matrix and the approximated data with the sublinear dimension required.
#' @export
#'
pcadim=function(Data, Dim){
  if(Dim>=1 & Dim<=dim(Data)[2]){
    TEMP = svd(Data)
    Scores = TEMP$u[,1:Dim] %*% diag(TEMP$d)[1:Dim,1:Dim]
    EigenT = t(TEMP$v[,1:Dim])
    return(list("Scores" = Scores, "EigenT" = EigenT, Approximation = Scores%*%EigenT))
  }else{stop("'Dim' must be a least of dimension 1 and not exceed the dimension of the original data.")}
}
