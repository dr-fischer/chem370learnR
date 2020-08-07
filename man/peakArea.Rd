\name{peakArea}
\alias{peakArea}
\title{Calculate Peak Area}
\usage{
peakArea(C, x1, x2, p = TRUE, ...)
}
\description{
This function calculates the area under the curve for a chromatographic peak.
It requires a 2-column data frame with time/X in column 1 and signal/Y in column 2.
}
\arguments{

  \item{C}{A 2-column data frame representing the chromatogram, with Time in column 1 and Signal in column 2}

  \item{x1}{The lower limit of integration}

  \item{y2}{The upper limitof integration}

  \item{p}{Whether or not to output a plot (Boolean)}

  \item{...}{Additional arguments to be passed to \code{plot}}
}
\value{
The area under the curve between points \code{x1} and \code{x2} and (optionally) a plot of the data with the integrated area shaded.
}
\examples{
data(chromatogram)
peakArea(chromatogram, 3.7, 5)
}
