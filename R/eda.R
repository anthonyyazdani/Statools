#' Get a summary for a given data frame
#'
#' @param data Data frame or a single vector.
#' @param plot if TRUE histograms of numerical variables are plotted, TRUE by default.
#'
#' @return Metrics data frame
#' @importFrom funModeling df_status
#' @importFrom funModeling profiling_num
#' @importFrom funModeling plot_num
#' @export
#'
#' @examples eda(iris)
eda <- function(data, plot = TRUE){
  print(quiet(data.frame(df_status(data = data, print_results = TRUE))))
  print(data.frame(profiling_num(data)[,c(1:4,12,13)]))
  if(plot){
    plot_num(data)
  }
}
