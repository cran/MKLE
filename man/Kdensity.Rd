\name{Kdensity}
\alias{Kdensity}
\title{ Kernel density estimator }
\description{
  Evaluates the shifted kernel density estimator 
}
\usage{
Kdensity(x, data, Kernel = dnorm, bw = 2*sd(data), theta = mean(data))
}
\arguments{
  \item{x}{ point at which the kernel density estimator is evaluated.  }
  \item{data}{ the data from which the estimate is to be computed. }
  \item{Kernel}{ a R function to be used as the kernel function. }
  \item{bw}{ the smoothing bandwidth to be used. }
  \item{theta}{ the location parameter used. }
}
\details{
  The location parameter theta shifts the kernel density estimator. Instead of centering the individual kernels on top of each datapoint, they will be shifted by theta-mean(data). Setting theta=mean(data) therefore gives the usual kernel density estimator.
  \deqn{\frac{1}{nh}\sum_{i=1}^nK(\frac{y-X_i-\bar X+\theta}{h}).}{1/(nh)sum K((y-X_i-\bar X+\theta)/h).}
}
\value{
  The value of the kernel density estimator
}
\references{ Silverman, B. W. (1986) Density Estimation for Statistics and Data Analysis. \emph{Chapman & Hall} }
\author{ Thomas Jaki }

\seealso{ \code{\link{density}} }
\examples{

## plots the kernel density estimator
data(state)
attach(state)
x<-seq(min(crime)-10,max(crime)+10,0.1)
plot(x,Kdensity(x,crime,theta=mean(crime)),type='l',ylab='Kernel Density',xlab='',lwd=2)


}
\keyword{distribution}
