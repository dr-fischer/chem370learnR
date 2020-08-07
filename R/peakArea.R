# Calculate Peak Area
#
# This function calculates the area under the curve for a chromatographic peak.
# It requires a 2-column data frame with time/X in column 1 and signal/Y in column 2.

peakArea <- function(C, x1, x2, p = TRUE, ...) {
  x <- C[C[,1] > x1 & C[,1] < x2, 1]
  y <- C[C[,1] > x1 & C[,1] < x2, 2]
  a = trapz(x, y)
  if (p == TRUE) {
    chemplot(C, type = 'l', xlab = 'Time', ylab = 'Signal (arbitrary units)', ...)
    areaplot::areaplot(x, y, col = '#C1A875',  add = TRUE, ...)
  }
  return(a)
}

