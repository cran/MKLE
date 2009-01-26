\name{mkle}
\alias{mkle}
\title{ Maximum kernel likelihood estimation }
\description{
  Computes the maximum kernel likelihood estimator for a given dataset and bandwidth.
}
\usage{
mkle(data,bw=2*sd(data),kernel=c("gaussian", "epanechnikov", "rectangular", "triangular", 
     "biweight", "cosine", "optcosine"),gridsize=2^14)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the data for which the estimator should be found. }
  \item{bw}{ the smoothing bandwidth to be used. }
  \item{kernel}{  a character string giving the smoothing kernel to be used. This must be one of '"gaussian"', 
                  '"rectangular"', '"triangular"', '"epanechnikov"', '"biweight"', '"cosine"' or '"optcosine"', 
                  with default '"gaussian"'. May be abbreviated to a unique prefix (single letter).}
  \item{gridsize}{ the number of points at which the kernel density estimator is to be evaluated with \eqn{2^{14}} as the default. }
}
\details{
  The default for the bandwidth is \eqn{2s}, which is the near-optimal value if a Gaussian kernel is used. If the bandwidth is zero, the sample mean will be returned.

  Larger gridsize results in more acurate estimates but also longer computation times. The use of gridsizes between \eqn{2^{11}} and \eqn{2^{20}} is recommended.
}

\value{
  The maximum kernel likelihood estimator.
}
\references{ Jaki T., West R. W. (2008) Maximum kernel likelihood estimation. \emph{Journal of Computational and Graphical Statistics} Vol. 17(No 4), 976-993. }
\author{ Thomas Jaki }
\note{\code{\link{optimize}} is used for the optimization and \code{\link{density}} is used to estimate the kernel density.
}
\seealso{ \code{\link{klik}} }
\examples{
data(state)
plot(density(state$CRIME))
abline(v=mean(state$CRIME),col='red')
abline(v=mkle(state$CRIME),col='blue')
}
\keyword{ univar }
\keyword{ nonparametric }
