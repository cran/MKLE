\name{state}
\alias{state}
\docType{data}
\title{Violent death in the USA}
\description{
  The dataset gives the number of violent death per 100,000 population per state
}
\usage{data(state)}
\format{
  A data frame with 50 observations on the following 2 variables.
  \describe{
    \item{\code{STATE}}{a factor with levels \code{AK} \code{AL} \code{AR} \code{AZ} \code{CA} \code{CO} \code{CT} \code{DE} \code{FL} \code{GA} \code{HI} \code{IA} \code{ID} \code{IL} \code{IN} \code{KS} \code{KY} \code{LA} \code{MA} \code{MD} \code{ME} \code{MI} \code{MN} \code{MO} \code{MS} \code{MT} \code{NC} \code{ND} \code{NE} \code{NH} \code{NJ} \code{NM} \code{NV} \code{NY} \code{OH} \code{OK} \code{OR} \code{PA} \code{RI} \code{SC} \code{SD} \code{TN} \code{TX} \code{UT} \code{VA} \code{VT} \code{WA} \code{WI} \code{WV} \code{WY}}
    \item{\code{CRIME}}{a numeric vector}
  }
}
\source{
  Shapiro, Robert~J. 1998. Statistical Abstract of the United States. 118 edn. U.S. Bureau of the Census.}
\examples{
data(state)
hist(state$CRIME)
mkle(state$CRIME)
}
\keyword{datasets}
