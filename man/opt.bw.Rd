\name{opt.bw}
\alias{opt.bw}
\title{ Optimal bandwidth for the maximum kernel likelihood estimator }
\description{
 Estimates the optimal bandwidth for the maximum kernel likelihood estimator using a Gaussian kernel for a given dataset using the bootstrap.
}
\usage{
opt.bw(data, bws=c(sd(data),4*sd(data)), B=1000, gridsize=2^14)
}
\arguments{
  \item{data}{ the data for which the optimal bandwidth should be found. }
  \item{bws}{ a vector with the upper and lower bound for the bandwidth. }
  \item{B}{ number of resamples used to estimate the mean squared error with 1000 as the default.}
  \item{gridsize}{ the number of points at which the kernel density estimator is to be evaluated with $2^{14}$ as the default.}
}
\details{
  The bandwidth considered fall between one and 4 standard deviations. In addition the mse of the \code{\link{mkle}} for a bandwidth of zero will also be included. 

  The estimation of the optimal bandwidth might take several minutes depending on the number of bootstrap resamples and the gridsize used.
}

\value{
  The estimated optimal bandwidth.
}
\references{ 
Jaki T., West R. W. (2007) Maximum kernel likelihood estimation. Submitted to \textit{Journal of Computational and Graphical Statistics}. 

Davison, A. C. and Hinkley, D. V. (1997), Bootstrap Methods and their Applications, Cambridge Series in Statistical and Probabilistic Mathematics, Cambridge University Press.
}
\author{ Thomas Jaki }
\note{The \code{\link{optimize}} is used for the optimization.
}
\seealso{ \code{\link{mkle}} }
\examples{
data(state)
opt.bw(state$CRIME,B=100)
}
\keyword{ nonparametric }
