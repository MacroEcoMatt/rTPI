\name{year_limits}
\alias{year_limits}
\docType{data}
\title{Yearly Average TPI Limits for Terrestrial Vertebrates}
\description{
  Contains the taxonomic information up to Class, for birds, mammals and
  reptiles, along with lower and upper thermal and aridity limits avaeraged
  over all months.
}
\usage{data("year_limits")}
\format{
  A data frame with 25982 observations on the following 11 variables.
  \describe{
    \item{\code{X}}{a numeric vector}
    \item{\code{Class}}{a character vector}
    \item{\code{Order}}{a character vector}
    \item{\code{Family}}{a character vector}
    \item{\code{Genus}}{a character vector}
    \item{\code{Species}}{a character vector}
    \item{\code{Binomial}}{a character vector}
    \item{\code{TMin}}{a numeric vector}
    \item{\code{TMax}}{a numeric vector}
    \item{\code{AMin}}{a numeric vector}
    \item{\code{AMax}}{a numeric vector}
  }
}

\source{}
\references{}
\examples{
data(year_limits)
## maybe head(year_limits, n=5)
}
\keyword{datasets}
