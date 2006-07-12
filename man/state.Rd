\name{state}
\alias{state}
\docType{data}
\title{ Violent crimes in the USA}
\description{
  The dataset gives the number of violent crimes per 100,000 population per state
}
\usage{data(state)}
\format{
  A data frame with 50 observations on the following 2 variables.
  \describe{
    \item{\code{state}}{a factor with state abbreviations as levels }
    \item{\code{crime}}{a numeric vector}
  }
}
\source{
  Shapiro, Robert J. (1998) Statistical Abstract of the United States. \emph{118 edn. U.S. Bureau of the Census.}
}

\examples{
data(state)
hist(state$crime)
}
\keyword{datasets}
