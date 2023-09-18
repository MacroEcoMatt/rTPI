#' Checks for matching species Binomial name
#'
#' `binomial_check()` takes user submitted species binomial or list of binomials
#' and compares to the species names recorded in the TPI and API datafiles.
#' Returns True if there is a matching name and False if not. First letter of
#' Genus needs to be capitalized and species should be all lower case.
#'
#' @param sp_binomial Input string or vector. Must be a character vector
#'
#' @returns A a data frame.
#' @examples
#' #single species
#' binomial_check("Poecile atricapillus")
#' #multiple species
#' binomial_check(c("Poecile atricapillus","Melospiza melodia"))
#'
#' sp_list <- c("Poecile atricapillus","Melospiza melodia")
#' binomial_check(sp_list)
#' @export
binomial_check <- function(sp_binomial){
  tf <- (sp_binomial %in% year_limits$Binomial)
  Found <- as.vector(tf)
  Submitted_Binomial <- as.vector(sp_binomial)
  return(cbind(Submitted_Binomial,Found))
}
