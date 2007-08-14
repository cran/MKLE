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
Version: \tab 0.03\cr
Date: \tab 2007-08-14\cr
License: \tab GNU \cr
}
The maximum kernel likelihood estimator is defined to be the value $\hat\theta_h$ that maximizes the estimated kernel likelihood based on the general location model,
$$f(x|\theta) = f_{0}(x - \theta).$$ 

This model assumes that the mean associated with $f_0$ is zero which of course implies that the mean of
$X_i$ is $\theta$. The kernel likelihood is the estimated likelihood based on the above model using a kernel density estimate, $\hat f(.|h,X_1,\dots,X_n)$, and is defined as
$$\hat L(\theta|X_1,\dots,X_n) = \prod_{i=1}^n \hat f(X_{i}-(\bar{X}-\theta)|h,X_1,\dots,X_n).$$

The resulting estimator therefore is an estimator of the mean of $X_i$.
}
\author{
Thomas Jaki

Maintainer: Thomas Jaki <jaki.thomas@gmail.com>
}
\references{
Jaki T., West R. W. (2007) Maximum kernel likelihood estimation. Submitted to \textit{Journal of Computational
and Graphical Statistics}.

Silverman, B. W. (1986), \textit{Density Estimation for Statistics and Data Analysis}, Chapman & Hall, 2nd ed.
}
\keyword{ package }

\examples{
data(state)
mkle(state$CRIME)
}
