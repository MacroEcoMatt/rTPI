#' Extracts Thermal and Aridity Tolerance Limits for Taxonomic Groups
#'
#' `highertaxa_limits()` takes user submitted taxanomic group or list of groups
#' and extracts thermal and aridity position limits for each month. Optional
#' parameters can be specified to refine extracted limits to specific niche
#' limit, month, and yearly average limit values.
#'
#' @param taxa_code Taxonomic level. Enter: "class","order","family",or "genus".
#'
#' @param taxa_list Input string or vector of rank names. Must be a character
#' vector. All submissions should have a capitalized first letter.
#'
#' @param niche_limit Input string. Specify "tpi" for thermal limits, "api" for
#' aridity limits, or "both" for both limits. Default is "both".
#'
#' @param month_list Input number or character vector. Specify the corresponding
#' number for month of desired (1=Jan), or month code (Jan).
#' must be a number or character vector. Defaults to returning all months.
#'
#' @param yr_avg Boolean paramater. Defaults to FALSE. if specified as TRUE
#' an additional row is included in the dataframe containing average yearly
#' niche limits.
#'
#' @returns A dataframe containing the niche limit variables specified for the
#'  taxanomic groups submitted.
#'
#' @examples
#'
#' highertaxa_limits(taxa_code = "class", taxa_list="Aves")
#'
#' highertaxa_limits(taxa_code = "order",
#' taxa_list=c("Galliformes","Anseriformes"))
#'
#' #specify TPI only
#' highertaxa_limits(taxa_code = "class", taxa_list="Aves",
#' niche_limit = "tpi")
#'
#' #Specify January
#' highertaxa_limits(taxa_code = "class", taxa_list="Aves",
#' niche_limit = "tpi", month_list=1)
#'
#' highertaxa_limits(taxa_code = "class", taxa_list="Aves",
#' niche_limit = "tpi", month_list="Jan")
#'
#' Get yearly average TPI values
#' highertaxa_limits(taxa_code = "class", taxa_list="Aves",
#' niche_limit = "tpi", month_list="Jan", yr_avg=TRUE)
#' @export
highertaxa_limits <- function(taxa_code, taxa_list, niche_limit = NULL, month_list = NULL,
                              yr_avg = FALSE){
  if (taxa_code == "class"){
    month_df <- subset(month_limits, Class %in% taxa_list)
  } else if (taxa_code == "order"){
    month_df <- subset(month_limits, Order %in% taxa_list)
  } else if (taxa_code == "family"){
    month_df <- subset(month_limits, Family %in% taxa_list)
  } else if (taxa_code == "genus"){
    month_df <- subset(month_limits, Genus %in% taxa_list)
  } else {
    stop("Error: taxa_code out of range")
  }

  if(nrow(month_df)==0){
    stop("No Matches: check taxonomic names")
  }

  if (is.null(month_list)){
    month_df <- month_df
  } else if (is.numeric(month_list) & month_list %in% c(1:12)){
    month_df <- subset(month_df, MonthNumber %in% month_list)
  } else if (is.numeric(month_list) & !month_list %in% c(1:12)){
    stop("Error: month_list is out of range")
  } else if (is.character(month_list) & month_list %in% month.abb[1:12]){
    month_df <- subset(month_df, MonthName %in% month_list)
  } else if (is.character(month_list) & !month_list %in% month.abb[1:12]){
    stop("Error: month_list is out of range")
  }
  if (is.null(niche_limit) || niche_limit == "both"){
    month_df <- month_df
  } else if (niche_limit=="api"){
    month_df <- subset(month_df, select = -c(TMin,TMax))
  } else if (niche_limit=="tpi"){
    month_df <- subset(month_df, select = -c(AMin,AMax))
  } else{
    stop("Error: niche_limit out of range")
  }
  if (isFALSE(yr_avg)){
    return(month_df)
  } else if (isTRUE(yr_avg)){
    if (taxa_code == "class"){
      yr_df <- subset(year_limits, Class %in% taxa_list)
    } else if (taxa_code == "order"){
      yr_df <- subset(year_limits, Order %in% taxa_list)
    } else if (taxa_code == "family"){
      yr_df <- subset(year_limits, Family %in% taxa_list)
    } else if (taxa_code == "genus"){
      yr_df <- subset(year_limits, Genus %in% taxa_list)
    }
    if (is.null(niche_limit) || niche_limit == "both"){
      df_final <- dplyr::bind_rows(month_df, yr_df)
      df_final <- df_final[order(df_final$Binomial),]
      df_final$MonthNumber[is.na(df_final$MonthNumber)] <- "Average"
      df_final$MonthName[is.na(df_final$MonthName)] <- "Average"
      return(df_final)
    } else if (niche_limit=="api"){
      yr_df <- subset(yr_df, select = -c(TMin,TMax))
      df_final <- dplyr::bind_rows(month_df, yr_df)
      df_final <- df_final[order(df_final$Binomial),]
      df_final$MonthNumber[is.na(df_final$MonthNumber)] <- "Average"
      df_final$MonthName[is.na(df_final$MonthName)] <- "Average"
      return(df_final)
    } else if (niche_limit=="tpi"){
      yr_df <- subset(yr_df, select = -c(AMin,AMax))
      df_final <- dplyr::bind_rows(month_df, yr_df)
      df_final <- df_final[order(df_final$Binomial),]
      df_final$MonthNumber[is.na(df_final$MonthNumber)] <- "Average"
      df_final$MonthName[is.na(df_final$MonthName)] <- "Average"
      return(df_final)
    }
  } else {
    warning("Warning: yr_avg is non Boolean, yearly average niche limits were
            not extracted to output dataframe")
    return(month_df)
  }
}
