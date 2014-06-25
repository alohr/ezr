percs <- function(x, na.rm = TRUE, cols = NULL) {
  
  valid <- function(x) { sum(!is.na(x)) }
  
  if (!na.rm) {
    x <- na.omit(x)
  }

  percentiles <- quantile(x, c(.68, .95, .997, .999), na.rm = na.rm)
  
  # save column names for later
  names <- names(percentiles)
  attributes(percentiles) <- NULL
  
  dataframe <- data.frame(
    n = valid(x),
    mean = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    min = min(x, na.rm = na.rm),
    median = median(x, na.rm = na.rm),
    p1 = percentiles[1],
    p2 = percentiles[2],
    p3 = percentiles[3],
    p4 = percentiles[4],
    max = max(x, na.rm = na.rm)
  )
  
  # fix the percentile column names
  p <- match("p1", names(dataframe))
  colnames(dataframe)[p:(p+3)] <- names
  
  dataframe
}
