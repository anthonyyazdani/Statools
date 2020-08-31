#' Make a function quiet
#'
#' @param x Function that should be quiet.
#'
#' @return Makes the function "x" quiet.
#' @export
#'
quiet <- function(x){
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
  }
