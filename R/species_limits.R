#' Extracts Thermal and Aridity Tolerance Limits
#'
#' `species_limits()` takes user submitted species binomial or list of binomials
#' and extracts thermal and aridity position limits for each month. Optional
#' parameters can be specified to refine extracted limits to specific niche
#' limit, month, and yearly average limit values.
#'
#' @param sp_binomial Input string or vector. Must be a character vector.
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
#'  species submitted.
#'
#' @examples
#'
#' species_limits("Poecile atricapillus")
#'
#' species_limits(c("Poecile atricapillus","Melospiza melodia"))
#'
#' sp_list <- c("Poecile atricapillus","Melospiza melodia")
#' species_limits(sp_list)
#'
#' #specify TPI only
#' species_limits("Poecile atricapillus", niche_limit = "tpi")
#'
#' #Specify January
#' species_limits("Poecile atricapillus", niche_limit = "tpi", month_list=1)
#' species_limits("Poecile atricapillus", niche_limit = "tpi", month_list="Jan")
#' #Get average values for year
#' species_limits("Poecile atricapillus", niche_limit = "tpi", month_list="Jan", yr_avg=TRUE)
#' @export
species_limits <- function(sp_binomial, niche_limit = NULL, month_list = NULL,
                           yr_avg = FALSE){
  MonthName <- MonthNumber <- TMax <- TMin <- AMax <- AMin <- Binomial <- NULL
  month_df <- subset(month_limits, Binomial %in% sp_binomial)
  if(nrow(month_df) == 0){
    stop ("No Binomial Matches: check species names")
  }
  if (is.null(month_list)){
    month_df <- month_df
  } else if (is.numeric(month_list) && max(month_list) < 13 && min(month_list) > 0){
    month_df <- subset(month_df, MonthNumber %in% month_list)
  } else if (is.numeric(month_list) && max(month_list) > 12 ||
             is.numeric(month_list) && min(month_list) < 0 ||
             is.numeric(month_list) && max(month_list) > 12 && min(month_list) < 0){
    stop("Error: month_list is out of range")
  } else if (is.character(month_list) && length(setdiff(month_list, month.abb[1:12])) == 0){
    month_df <- subset(month_df, MonthName %in% month_list)
  } else if (is.character(month_list) && length(setdiff(month_list, month.abb[1:12])) > 0){
    stop("Error: month_list is out of range")
  } else {
    stop("Error: month_list is not numeric or character")
  }
  if (is.null(niche_limit) || niche_limit == "both"){
    month_df <- month_df
  } else if (niche_limit == "api"){
    month_df <- subset(month_df, select = -c(TMin, TMax))
  } else if (niche_limit == "tpi"){
    month_df <- subset(month_df, select = -c(AMin, AMax))
  } else{
    stop("Error: niche_limit out of range")
  }
  if (isFALSE(yr_avg)){
    return(month_df)
  } else if (isTRUE(yr_avg)){
    yr_df <- subset(year_limits, Binomial %in% sp_binomial)
    if (is.null(niche_limit) || niche_limit == "both"){
      df_final <- dplyr::bind_rows(month_df, yr_df)
      df_final <- df_final[order(df_final$Binomial), ]
      df_final$MonthNumber[is.na(df_final$MonthNumber)] <- "Average"
      df_final$MonthName[is.na(df_final$MonthName)] <- "Average"
      return(df_final)
    } else if (niche_limit == "api"){
      yr_df <- subset(yr_df, select = -c(TMin, TMax))
      df_final <- dplyr::bind_rows(month_df, yr_df)
      df_final <- df_final[order(df_final$Binomial), ]
      df_final$MonthNumber[is.na(df_final$MonthNumber)] <- "Average"
      df_final$MonthName[is.na(df_final$MonthName)] <- "Average"
      return(df_final)
    } else if (niche_limit=="tpi"){
      yr_df <- subset(yr_df, select = -c(AMin, AMax))
      df_final <- dplyr::bind_rows(month_df, yr_df)
      df_final <- df_final[order(df_final$Binomial), ]
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
