\name{Synonym_list}
\alias{Synonym_list}
\docType{data}
\title{List of Species Binomials and Synonyms}
\description{
  This Dataset contains a list of IUCN Red List binomial names for species and
  a corresponding list of known synonym names. IUCN_Binomial contains the used
  species Binomial in the TPI datsets. Synonym contains a list of species
  binomials that are synonyms with the IUCN Binomials
}
\usage{data("Synonym_list")}
\format{
  A data frame with 34046 observations on the following 2 variables.
  \describe{
    \item{\code{Synonym}}{a character vector}
    \item{\code{IUCN_Binomial}}{a character vector}
  }
}
\source{
  https://www.iucnredlist.org/search
}
\examples{
data(Synonym_list)
## maybe head(Synonym_list, n=5)
}
\keyword{datasets}
