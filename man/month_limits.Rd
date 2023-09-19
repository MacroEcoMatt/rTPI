\name{month_limits}
\alias{month_limits}
\docType{data}
\title{Monthly TPI Limits for Terrestrial Vertebrates}
\description{
  Contains the taxonomic information up to Class, for birds, mammals and
  reptiles, along with lower and upper thermal and aridity limits for each
  month. Month date is provided in both number and character format.
}
\usage{data("month_limits")}
\format{
  A data frame with 311696 observations on the following 12 variables.
  \describe{
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
    \item{\code{MonthNumber}}{a numeric vector}
    \item{\code{MonthName}}{a character vector}
  }
}
\source{}
\references{}
\examples{
data(month_limits)
## maybe head(month_limits, n = 5)
}
\keyword{datasets}
