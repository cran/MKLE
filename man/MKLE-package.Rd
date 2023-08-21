\name{MKLE-package}
\alias{MKLE-package}
\alias{MKLE}
\docType{package}
\title{
Maximum kernel likelihood estimation
}
\description{
Computes the maximum kernel likelihood estimator using fast fourier transforms.
}
\details{
\tabular{ll}{
Package: \tab MKLE\cr
Type: \tab Package\cr
Version: \tab 1.01\cr
Date: \tab 2023-08-21\cr
License: \tab GPL \cr
}
The maximum kernel likelihood estimator is defined to be the value \eqn{\hat \theta} that maximizes the estimated kernel likelihood based on the general location model,
\deqn{f(x|\theta) = f_{0}(x - \theta).} 

This model assumes that the mean associated with $f_0$ is zero which of course implies that the mean of
\eqn{X_i} is \eqn{\theta}. The kernel likelihood is the estimated likelihood based on the above model using a kernel density estimate, \eqn{\hat f(.|h,X_1,\dots,X_n)}, and is defined as
\deqn{\hat L(\theta|X_1,\dots,X_n) = \prod_{i=1}^n \hat f(X_{i}-(\bar{X}-\theta)|h,X_1,\dots,X_n).}

The resulting estimator therefore is an estimator of the mean of \eqn{X_i}.
}
\author{
Thomas Jaki

Maintainer: Thomas Jaki <jaki.thomas@gmail.com>
}
\references{
Jaki T., West R. W. (2008) Maximum kernel likelihood estimation. \emph{Journal of Computational
and Graphical Statistics} Vol. 17(No 4), 976-993.

Silverman, B. W. (1986), \emph{Density Estimation for Statistics and Data Analysis}, Chapman & Hall, 2nd ed.
}
\keyword{ package }

\examples{
data(state)
mkle(state$CRIME)
}
