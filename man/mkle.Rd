\name{MKLE}
\alias{mkle}
\title{ Maximum kernel likelihood estimation }
\description{
  Computes the maximum kernel likelihood estimator for a given dataset and bandwidth.
}
\usage{
mkle(data, Kernel = dnorm, bw = 2*sd(data), small = TRUE)
}
\arguments{
  \item{data}{ the data for which the log likelihood will be computed. }
  \item{Kernel}{ a R function to be used as the kernel function. }
  \item{bw}{ the smoothing bandwidth to be used. }
  \item{small}{ logical; if 'TRUE', only the value of the estimator is returned. Otherwise the full optimization history will be included. }
}
\details{
  The underlying shifted kernel density estimator is defined as \deqn{\frac{1}{nh}\sum_{i=1}^nK(\frac{y-X_i-\bar X+\theta}{h}).}{1/(nh)sum K((y-X_i-\bar X+\theta)/h).}
  The default for the bandwidth is 2*sigma, which is the optimal value if a Gaussian kernel is used.
}
\value{
  The MKLE or a list with components: 

   \item{par}{The best set of parameters found.}
   \item{value}{The value of \code{\link{klik}} corresponding to \code{par}.}
   \item{counts}{A two-element integer vector giving the number of calls to \code{Klik}. This excludes those calls needed to compute the Hessian, if requested, and any calls to \code{fn} to compute a finite-difference approximation to the gradient.}
   \item{convergence}{An integer code. '0' indicates successful convergence.
          Error codes are

          '1' indicates that the iteration limit 'maxit' had been
               reached.

          '10' indicates degeneracy of the Nelder-Mead simplex.

          '51' indicates a warning from the '"L-BFGS-B"' method; see
               component 'message' for further details.

          '52' indicates an error from the '"L-BFGS-B"' method; see
               component 'message' for further details.}

   \item{message}{A character string giving any additional information returned by the optimizer, or 'NULL'.}
}
\references{ ~not yet~ }
\author{ Thomas Jaki }
\note{The \code{\link{optim}} with the method 'BFGS' is used for the optimization.
}
\seealso{ \code{\link{optim}} and \code{\link{klik}} }
\examples{

data(state)
attach(state)

mkle(crime)

}
\keyword{univar}
