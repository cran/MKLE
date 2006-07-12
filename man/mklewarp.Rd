\name{mklewarp}
\alias{mklewarp}
\title{ Maximum kernel likelihood estimation }
\description{
  Computes the maximum kernel likelihood estimator for a given dataset and bandwidth using fast fourier transforms.
}
\usage{
mklewarp(data, bw = 1, gs = 2^11, K="gaussian")
}
\arguments{
  \item{data}{ the data for which the log likelihood will be computed. }
  \item{bw}{ the smoothing bandwidth to be used. }
  \item{gs}{ the number of gridpoints to be used for the fourier transform }
  \item{K}{a character string giving the kernel function to be
          used. This must be one of '"gaussian"', '"rectangular"',
          '"triangular"', '"epanechnikov"', '"biweight"', '"cosine"' or
          '"optcosine"', with default '"gaussian"'.}
}
\details{
  The underlying shifted kernel density estimator is defined as \deqn{\frac{1}{nh}\sum_{i=1}^nK(\frac{y-X_i-\bar X+\theta}{h})}.
  The default for the bandwidth is 2*sigma, which is the optimal value if a Gaussian kernel is used.
}
\value{
  the MKLE.
}
\references{ ~coming soon ~ }
\author{ Thomas Jaki }
\seealso{ \code{\link{mkle}} and \code{\link{klik}} }
\examples{
## compares the MKLE and the warped MKLE.
data(state)
attach(state)

mkle(crime)
mklewarp(crime)

}
}
\keyword{univar}

