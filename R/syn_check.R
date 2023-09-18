#' Converts submitted species names to match IUCN and BirdLife names
#'
#' `syn_check()` takes user submitted genus species binomial names and
#' creates a list that converts all submitted names to the appropriate matching
#' binomial utilized by the IUCN and BirdLife. Binomials are compared against
#' ITIS binomial synonyms. The list of binomial synonyms is not exhaustive and
#' any non matching names may need to be verified through other taxonomic
#' databases
#'
#' @param binomial_list Input string or vector of genus species binomials. Must
#' be a character vector. All submissions should have a capitalized first letter
#' of the genus name followed by a space and lower case species name: eg.
#' Melospiza melodia.
#'
#' @returns A dataframe containing the user submitted name and matching IUCN or
#' BirdLife genus species binomial
#'
#' @examples
#'
#' sp_list <- c("Melospiza melodia", "Zonotrichia albicollis")
#'
#' Compared_df <- syn_check(sp_list)
#'
#' sp_df <- data.frame(
#'                Binomial = c("Melospiza melodia", "Zonotrichia albicollis"),
#'                Month = c(1,2),
#'                TMin = c(0,1),
#'                TMax = c(15,13)
#'                )
#' df_Binomial_check <- syn_check(sp_df$Binomial)
#'
#' #or
#'
#' df_Binomial_check <- syn_check(sp_df[,"Binomial"])
#'
#' @export
syn_check <- function(binomial_list){

  User_Submitted <- unique(binomial_list)
  Matching_Binomial <- unique(binomial_list)

  for (i in 1:length(Matching_Binomial)){

    if (Matching_Binomial[i] %in% Synonym_list$IUCN_Binomial){

      next

    } else if (Matching_Binomial[i] %in% Synonym_list$Synonym){

      sub_name <- Synonym_list[Synonym_list$Synonym == Matching_Binomial[i],]

      Matching_Binomial <- replace(Matching_Binomial,i, sub_name$IUCN_Binomial)

    } else {

      Matching_Binomial <- replace(Matching_Binomial,i,NA)
    }
  }

  df <- data.frame(User_Submitted,Matching_Binomial)
  if(any(is.na(Matching_Binomial))){
    print("ATTENTION: some submitted species names returned NA values")
  }
  return(df)
}
