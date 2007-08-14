\name{mkle.ci}
\alias{mkle.ci}
\title{ Confidence intervals for the maximum kernel likelihood estimator }
\description{
  Computes different confidence intervals for the maximum kernel likelihood estimator for a given dataset and bandwidth.
}
\usage{
mkle.ci(data, bw=2*sd(data), alpha=0.1, kernel=c("gaussian", "epanechnikov", 
        "rectangular", "triangular", "biweight", "cosine", "optcosine"), 
        method=c("percentile", "wald","boott"), B=1000, gridsize=2^14)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the data for which the confidence interval should be found. }
  \item{bw}{ the smoothing bandwidth to be used. }
  \item{alpha}{ the significance level.}
  \item{kernel}{ a character string giving the smoothing kernel to be used. This must be one of '"gaussian"', '"rectangular"', '"triangular"', '"epanechnikov"', '"biweight"', '"cosine"' or '"optcosine"', with default '"gaussian"', and may be abbreviated to a unique prefix (single letter).}
  \item{method}{ a character string giving the type of interval to be used. This must be one of '"percentile"', '"wald"' or '"boott"'.}
  \item{B}{number of resamples used to estimate the mean squared error with 1000 as the default.}
  \item{gridsize}{ the number of points at which the kernel density estimator is to be evaluated with $2^14$ as the default. }
}
\details{
  The method can be a vector of strings containing the possible choices.

  The bootstrap-t-interval can be very slow for large datasets and a large number of resamples as a two layered resampling is necessary.
}

\value{
  A dataframe with the requested intervals.
}
\references{ 
Jaki T., West R. W. (2007) Maximum kernel likelihood estimation. Submitted to \textit{Journal of Computational and Graphical Statistics}. 

Davison, A. C. and Hinkley, D. V. (1997), Bootstrap Methods and their Applications, Cambridge Series in Statistical and Probabilistic Mathematics, Cambridge University Press.
}
\author{ Thomas Jaki }
}
\seealso{ \code{\link{mkle}} }
\examples{
data(state)
mkle.ci(state$CRIME,method=c('wald','percentile'),B=100,gridsize=2^11)
}
\keyword{ htest }
\keyword{ nonparametric }
