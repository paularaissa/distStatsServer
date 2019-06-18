##'
#' @title Get Conditional Percentile
#' @description Computes the conditional percentile according to the sample list.
#' @details Get the percentile for given x value and cdf (sample list).
#' @param samplesList a vector,  the local cumDistFunc for each data node.
#' @return Combined percentile value.
#' @author Rui Camacho, Paula Raissa
#' @export
#'
getPercentile <- function(samplesList){
  targetSamples <- 0
  totalSamples <- 0
  for(val in samplesList) {
    targetSamples <- targetSamples + val$contVal
    totalSamples <- totalSamples + val$totalSamples
  }

  if (targetSamples == 0)
    stop("Please provide a valid x value", call. = FALSE)

  return(targetSamples/totalSamples)
}
