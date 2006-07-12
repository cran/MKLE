\name{Kernels}
\alias{biweight}
\alias{triweight}
\alias{triangle}
\title{ Kernel functions }
\description{
  Evaluates the finite support kernels for a given x
}
\usage{
biweight(x)
triweight(x)
triangle(x)
}
\arguments{
  \item{x}{ Point at which the kernel is evaluated }
}
\details{
  The biweight kernel is defined as $15/16*(1-x^2)^2$ for |x|<1
  The triweight kernel is defined as $35/32*(1-x^2)^3$ for |x|<1		
  The triangle kernel is defined as $1-abs(x)$ for |x|<1	
}
\value{
Value of the kernel function.
}
\references{ Silverman, B. W. (1986) Density Estimation for Statistics and Data Analysis. \emph{Chapman & Hall} }
\author{ Thomas Jaki }
\seealso{ \code{\link{triweight}} and \code{\link{triangle}}  }

\keyword{ misc }
