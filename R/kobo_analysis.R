#' @name kobo_analysis
#' @rdname kobo_analysis
#' @title  Generate the analysis

#' @description  Automatically generate bar charts for each select_one and select_multiple for in the dataset. ggplot2 is used.
#'
#'
#' @author Elliott Messeiller
#'
#' @examples
#' kobo_analysis()
#'
#' @export kobo_analysis
#' @examples
#' \dontrun{
#' kobo_analysis()
#' }
#'
#'

kobo_analysis <- function() {
  source("code/0-config.R")

  # integrate variables from analysis plan
  if (analysis_plan=="Y"){
    kobo_analysis_plan()
  }
  # Weight data
  if (usedweight!="none"){
    kobo_weight()
  }

  ### Bar one graphs
  kobo_bar_one()

  ### Bar one disaggregation
  if(length(disagg)!=0){
    kobo_bar_one_facet()}


  ### Bar multiple graphs
  kobo_bar_multi()

  ### Bar one disaggregation
  if(length(disagg)!=0){
    kobo_bar_multi_facet()}


  ### Histograms
  kobo_histo()

  ### Word clouds
  #kobo_text_cloud()

  ### Correlations tables
  #kobo_correlation()

  cat(" #####################################################\n")
  cat(" #                                                   #\n")
  cat(" #     The analysis is completed!                    #\n")
  cat(" #                                                   #\n")
  cat(" # You will find all the outputs in the 'out' folder #\n")
  cat(" #                                                   #\n")
  cat(" #####################################################\n")

}
NULL
