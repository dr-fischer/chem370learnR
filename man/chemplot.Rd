\name{chemplot}
\alias{chemplot}
\title{Plot Data}
\usage{
chemplot(x, y, ...)
}

\description{
Plots data.  Identical to \code{magicaxis::magplot} with some defaults changed.
}
\arguments{

  \item{x}{The x values}

  \item{y}{The y values}

  \item{...}{Additional arguments to be passed to \code{magplot} or \code{plot}}
}

\examples{
data(chromatogram)
chemplot(chrom, type = 'l', xlab = 'Time (min)', ylab = 'Signal (mAU)')
}

\seealso{

\code{magicaxis::magplot()}

\code{plot()}

}
