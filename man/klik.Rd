\name{Klik}
\alias{klik}
\title{ Kernel log likelihood }
\description{
  The function computes the kernel log likelihood for a given theta
}
\usage{
klik(theta = 0, data, Kernel = dnorm, bw = 2*sd(data))
}
\arguments{
  \item{theta}{ the parameter value for which the log likelihood will be computed. }
  \item{data}{ the data for which the log likelihood will be computed. }
  \item{Kernel}{ a R function to be used as the kernel function. }
  \item{bw}{ the smoothing bandwidth to be used. }
}
\details{
  The log likelihood of theta for a given bandwidth. 
}
\value{
  The log likelihood based on the shifted kernel density estimator.
}
\references{ ~in preperation~ }
\author{ Thomas Jaki }
\seealso{ \code{\link{Kdensity}} and \code{\link{mkle}}  }
\examples{

## plots the kernel log likelihood
data(state)
attach(state)
tv<-seq(min(crime),max(crime))

lik<-sapply(tv,klik,data=crime)	
plot(tv,lik,type='l',xlab='theta',ylab='Kernel Likelihood')
abline(v=mean(crime),col='red')
}
\keyword{univar}
