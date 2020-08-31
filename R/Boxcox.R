#' Automatic Box Cox Transformation
#'
#' @param Data Numeric vector.
#' @param Method "MeanMedian" or "Shapiro". "MeanMedian" should be used if possible. "Shapiro" is an alternative if the "MeanMedian" result is not satisfactory.
#'
#' @return The numeric vector after the automatic Box Cox transformation.
#' @importFrom stats var
#' @export
#'
Boxcox=function(Data, Method = c("MeanMedian","Shapiro")){
  if(Method==c("MeanMedian","Shapiro") || Method=="MeanMedian"){
    TAB = MMLambda(Data)
    L = TAB$lambda
    A = TAB$Added.value
    OUTPUT = ((((Data+A)^L)-1)/L)
    if(sum(OUTPUT)==0 || var(OUTPUT)==0){stop("The chosen method is not suitable.")}
    print(noquote(paste("Lambda =", L)))
    print(noquote(paste("Added value =", A)))
    return(OUTPUT)
  }
  if(Method==c("Shapiro","MeanMedian") || Method=="Shapiro"){
    TAB = SHLambda(Data)
    L = TAB$lambda
    A = TAB$Added.value
    OUTPUT = ((((Data+A)^L)-1)/L)
    if(sum(OUTPUT)==0 || var(OUTPUT)==0){stop("The chosen method is not suitable.")}
    print(noquote(paste("Lambda =", L)))
    print(noquote(paste("Added value =", A)))
    return(OUTPUT)
  }
}
