\name{checkparms}
\alias{checkparms}
\title{ Checks if the kernel function is proper. }
\description{
Checks if the kernel function is nonnegative and integrates to 1.
}
\usage{
checkparms(Kernel)
}
\arguments{
\item{Kernel}{ a R function to be used as the kernel function. }
}
\value{
  will return a warning if the kernel is improper.
}
\author{ Thomas Jaki }
\seealso{ \code{\link{Kdensity}}}
\keyword{ misc }