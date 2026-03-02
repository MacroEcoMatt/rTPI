#' Returns a list of species Binomial names from the niche limits datasets
#'
#' `species_list()` if left as NULL returns a data frame with columns for:
#' taxonomic Class, Order, Family, Genus, Species, and Binomial for all groups
#' of terrestrial vertebrates included in the niche limits data set. Users can
#' specify taxonomic class for sub sampling.
#'
#' @param class Input string. Options include: "mammal", "amphibian", "bird",
#' "reptile"
#'
#' @returns A a data frame.
#' @examples
#' #all species
#' all_list <- species_list()
#' #specific taxonomic class
#' mammal_list <- species_list("mammal")
#'
#' @export
species_list <- function(class = NULL){
  Class <- Order <- Family <- Genus <- Species <- Binomial <- MonthName <- MonthNumber <- TMax <- TMin <-
    AMax <- AMin <- NULL

  if(!is.null(class) && !class == "mammal" && !class == "amphibian" && !class == "bird" && !class == "reptile"){
    stop("class is misspecified")
  }

  if(is.null(Class)){
    sp_list_all <- yearly_limits[,c(1:6)]
  } else if (Class == "mammal"){
    sp_list_all <- yearly_limits[,c(1:6)]
    sp_list_all <- sp_list_all[sp_list_all$Class == "Mammalia",]
  } else if (Class == "amphibian"){
    sp_list_all <- yearly_limits[,c(1:6)]
    sp_list_all <- sp_list_all[sp_list_all$Class == "Amphibia",]
  } else if (Class == "bird"){
    sp_list_all <- yearly_limits[,c(1:6)]
    sp_list_all <- sp_list_all[sp_list_all$Class == "Aves",]
  } else if(Class == "reptile"){
    sp_list_all <- yearly_limits[,c(1:6)]
    sp_list_all <- sp_list_all[sp_list_all$Class == "Reptilia",]
  }
  return(sp_list_all)
}
