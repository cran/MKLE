\name{klik}
\alias{klik}
\title{ Kernel log likelihood }
\description{
  The function computes the kernel log likelihood for a given \eqn{\hat \theta }.
}
\usage{
klik(delta , data, kde, grid, min)
}

\arguments{
  \item{delta}{ the difference of the parameter theta for which the kernel log likelihood will be computed and the sample mean. }
  \item{data}{ the data for which the kernel log likelihood will be computed. }
  \item{kde}{ an object of the class "density". }
  \item{grid}{ the stepsize between the x-values in kde. }
  \item{min}{ the smallest x-value in kde. }
}
\details{
  This function is intended to be called through the function \code{\link{mkle}} and is optimized for fast computation.
}
\value{
  The log likelihood based on the shifted kernel density estimator.
}
\references{ Jaki T., West R. W. (2008) Maximum kernel likelihood estimation. \emph{Journal of Computational
and Graphical Statistics} Vol. 17(No 4), 976-993. }
\author{ Thomas Jaki }
\seealso{ \code{\link{mkle}} }
\examples{
data(state)
attach(state)
bw<-2*sd(CRIME)
kdensity<-density(CRIME,bw=bw,kernel="biweight",
          from=min(CRIME)-2*bw,to=max(CRIME)+2*bw,n=2^12)
min<-kdensity$x[1]
grid<-kdensity$x[2]-min

# finds the kernel log likelihood at the sample mean
klik(0,CRIME, kdensity, grid, min)

}
\keyword{ distribution }
\keyword{ nonparametric }
