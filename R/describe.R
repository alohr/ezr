#' A one sentence description of what your function does
#'
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces.
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#
#' set.seed(0)
#' round(describe(rnorm(1000)), 2)

describe <- function(x, na.rm = TRUE, cols = NULL) {

  valid <- function(x) { sum(!is.na(x)) }

  #if (!na.rm) {
  #  x <- na.omit(x)
  #}

  percentiles <- quantile(x, c(0.001, 0.05, .5, .68, .95, .997, .999), na.rm = na.rm)

  # save column names for later
  names <- names(percentiles)
  attributes(percentiles) <- NULL

  dataframe <- data.frame(
    n = valid(x),
    mean = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    min = min(x, na.rm = na.rm),
    p1 = percentiles[1],
    p2 = percentiles[2],
    p3 = percentiles[3],
    p4 = percentiles[4],
    p5 = percentiles[5],
    p6 = percentiles[6],
    p7 = percentiles[7],
    max = max(x, na.rm = na.rm)
  )

  # fix the percentile column names
  p <- match("p1", names(dataframe))
  colnames(dataframe)[p:(p+6)] <- names

  dataframe
}
