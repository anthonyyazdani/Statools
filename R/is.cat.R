#' Categorical check
#'
#' @param x Vector
#' @param k Maximum unique elements such that is considered as categorical.
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples is.cat(round(runif(100)))
is.cat <- function(x, k = 2){
  if(length(unique(x)) <= k){return(TRUE)}else{return(FALSE)}
}
