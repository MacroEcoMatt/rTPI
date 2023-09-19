#' Calculte API for Know Temperature Data
#'
#' `api()` Takes dataframe submitted by user and calculates Aridity Position
#' Index. Uses species Binomial and Month to extract aridity niche limits then
#' takes user submitted aridity values and calculates API using the aridity
#' niche limits add a column to submitted dataframe with API values.Aridity
#' Index values cannot be calculated for snow cover, and due to some species
#' ranges being completely covered in snow for some seasons, certain species
#' will have values of NA returned for API calculations.These NA values indicate
#' that the period of calculation is during a period with no determined
#' aridity index measurement for the species range.
#'
#' @param sp_df Input data. Must be a dataframe containing a column titled
#' "Binomial" which contains genus species binomial ("Poecile atricapillus"),
#' a column title "Ar" which has temperature values in degrees Celsius, and a
#' column titled "Month" that contains a numeric value from 1-12 or month codes
#' (ex. "Jan"). If Month of observed aridity recording is unknown users may
#' choose to include "Year" in the Month column which will have the function
#' calculate API based on Yearly Average API limits. NAs will be ignored.
#'
#' @param ar_var Input string. Defaults to NULL which informs function to
#' calculate API for column titled "Ar". Options: "minmax", "all".
#' "minmax" indicates user has two columns with aridity data, one should be
#' titled "Amin", the other "Amax". "all" indicates user has three columns with
#' aridity data, should be titled "Amin", "Amax", and "Ar" ("Ar" indicating
#' mean aridity). Output columns for Amin = API_min, Amax = API_max,
#' Ar = API_mean when ar_var = "all".
#'
#' @param use_year Boolean parameter, defualt FALSE. set to TURE to use yearly
#' aridity tolerance limits instead of Monthly limits.
#'
#' @param flag_sp Boolean parameter, default FALSE. Set to true to add column
#' to final data frame that contains TRUE values for submitted species Binomials
#' that match API data set, and FALSE for rows that contain Binomials that do
#' not match those in the API data set. Used to Identify erroneous Binomials
#'
#' @param flag_month Boolean parameter, default FALSE. Set to true to add column
#' to final data frame that contains TRUE values for appropriately submitted
#' Month formats and FALSE for inappropriately formatted Month values. Used to
#' Identify erroneous month submissions.
#'
#' @param flag_ar Boolean parameter, default FALSE. Set to true to add
#' column(s) to final data frame that contains FALSE values if aridity
#' values are submitted as NA. TRUE is reported when no aridity errors are
#' submitted. Aids in identifying errors of aridity submission.
#'
#' @returns A Dataframe.
#' @examples
#' #example df:
#' species_data <- <- data.frame(
#'                Binomial = c("Poecile atricapillus", "Poecile atricapillus"),
#'                Month = c(1,2),
#'                AMax = c(5,5),
#'                AMin = c(2,1),
#'                Ar = c(0,3)
#'                )
#'
#' #would return API for column Tm
#' results <- api(species_data)
#'
#' #would return API for AMin and AMax
#' results <- api(species_data, ar_var = "minmax")
#'
#' #would return API for AMin, AMax, and Ar
#' results <- api(species_data, ar_var = "all", use_year = TRUE,
#'                 flag_sp = TRUE, flag_month = TRUE, flag_ar = TRUE)
#'
#' @export
api <- function(sp_df, ar_var = NULL, use_year = FALSE, flag_sp = FALSE,
                flag_month = FALSE, flag_ar = FALSE){
  ############NULL###############
  if(is.null(ar_var) && isTRUE(any(sp_df$Ar < 0))){
    stop("Error: Ar values must be 0 or greater; Some Ar values are negative")
  } else if(ar_var == "all" && isTRUE(any(sp_df$Ar < 0)) ||
            ar_var == "all" && isTRUE(any(sp_df$AMax < 0)) ||
            ar_var == "all" && isTRUE(any(sp_df$AMin < 0))){
    stop("Error: Ar,AMin,AMax values must be 0 or greater; Some values are negative")
  } else if (ar_var == "minmax" && isTRUE(any(sp_df$AMax < 0)) ||
             ar_var == "minmax" && isTRUE(any(sp_df$AMin < 0))){
    stop("Error: AMin,AMax values must be 0 or greater; Some values are negative")
  }
  if (is.null(ar_var)){
    API_OUTPUT <- sp_df
    API_OUTPUT$API <- NA

    if (isTRUE(flag_sp)){
      API_OUTPUT$BINOMIALS_MATCH <- (API_OUTPUT$Binomial %in% year_limits$Binomial)
    }
    if (isTRUE(flag_month)){
      for (nr in 1:nrow(API_OUTPUT)){
        if (is.na(API_OUTPUT[nr,"Month"])){
          API_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else if (!API_OUTPUT[nr,"Month"] == "Year" &&
                   !API_OUTPUT[nr,"Month"] %in% c(1:12) &&
                   !API_OUTPUT[nr,"Month"] %in% as.character(c(1:12)) &&
                   !API_OUTPUT[nr,"Month"] %in% month.abb[1:12]){
          API_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else {
          API_OUTPUT[nr,"Flag_Month"] <- TRUE
        }
      }
    }
    if (isTRUE(flag_ar)){
      API_OUTPUT$Flag_Ar <- (!is.na(API_OUTPUT$Ar))
    }

    for (row in 1:nrow(API_OUTPUT)){
      mnth <- API_OUTPUT$Month[[row]]

      if (isTRUE(use_year)){
        ar <- API_OUTPUT[row, "Ar"]
        AMinimum <- year_limits[year_limits$Binomial %in%
                                  API_OUTPUT[row,"Binomial"], "AMin"]
        AMaximum <- year_limits[year_limits$Binomial %in%
                                  API_OUTPUT[row,"Binomial"], "AMax"]
        if (length(AMinimum) > 0 && length(AMaximum) > 0){
          API_OUTPUT[row,"API"] <-
            as.numeric(signif((ar-AMinimum)/(AMaximum-AMinimum)),digits = 6)
        } else {
          API_OUTPUT[row,"API"] <- NA
        }
      } else {
        if (is.na(mnth)){
          API_OUTPUT[row,"API"] <- NA
        } else if (is.numeric(mnth) && mnth %in% c(1:12)){
          ar <- API_OUTPUT[row, "Ar"]
          AMinimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMin"]
          AMaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API"] <-
              as.numeric(signif((ar-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% month.abb[1:12]){
          ar <- API_OUTPUT[row, "Ar"]
          AMinimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "AMin"]
          AMaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API"] <-
              as.numeric(signif((ar-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% as.character(c(1:12))){
          ar <- API_OUTPUT[row, "Ar"]
          mnth <- as.numeric(mnth)
          AMinimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMin"]
          AMaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API"] <-
              as.numeric(signif((ar-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API"] <- NA
          }
        }else if (is.character(mnth) && mnth=="Year"){
          ar <- API_OUTPUT[row, "Ar"]
          AMinimum <- year_limits[year_limits$Binomial %in%
                                    API_OUTPUT[row,"Binomial"], "AMin"]
          AMaximum <- year_limits[year_limits$Binomial %in%
                                    API_OUTPUT[row,"Binomial"], "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API"] <-
              as.numeric(signif((ar-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API"] <- NA
          }
        } else {
          API_OUTPUT[row,"API"] <- NA
        }
      }
    }
    out_of_range <- API_OUTPUT[is.na(API_OUTPUT$API),"API"]
    if(length(out_of_range) > 0){
      warning("Some rows contained innaccurate information for species name,
            month or Ar. Rows with this type of error had API propogated
            with NA")
      return(API_OUTPUT)
    } else {
      return(API_OUTPUT)
    }
  }
  ############MINMAX###############
  else if (ar_var == "minmax"){
    API_OUTPUT <- sp_df
    API_OUTPUT$API_min <- NA
    API_OUTPUT$API_max <- NA

    if (isTRUE(flag_sp)){
      API_OUTPUT$BINOMIALS_MATCH <- (API_OUTPUT$Binomial %in% year_limits$Binomial)
    }
    if (isTRUE(flag_month)){
      for (nr in 1:nrow(API_OUTPUT)){
        if (is.na(API_OUTPUT[nr,"Month"])){
          API_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else if (!API_OUTPUT[nr,"Month"] == "Year" &&
                   !API_OUTPUT[nr,"Month"] %in% c(1:12) &&
                   !API_OUTPUT[nr,"Month"] %in% as.character(c(1:12)) &&
                   !API_OUTPUT[nr,"Month"] %in% month.abb[1:12]){
          API_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else {
          API_OUTPUT[nr,"Flag_Month"] <- TRUE
        }
      }
    }
    if (isTRUE(flag_ar)){
      API_OUTPUT$Flag_AMin <- (!is.na(API_OUTPUT$AMin))
      API_OUTPUT$Flag_AMax <- (!is.na(API_OUTPUT$AMax))
    }
    for (row in 1:nrow(API_OUTPUT)){
      mnth <- API_OUTPUT[row,"Month"]

      if (isTRUE(use_year)){
        amin <- API_OUTPUT[row, "AMin"]
        amax <- API_OUTPUT[row, "AMax"]

        AMinimum <- year_limits[year_limits$Binomial %in%
                                  API_OUTPUT[row,"Binomial"], "AMin"]
        AMaximum <- year_limits[year_limits$Binomial %in%
                                  API_OUTPUT[row,"Binomial"], "AMax"]

        if (length(AMinimum) > 0 && length(AMaximum) > 0){
          API_OUTPUT[row,"API_min"] <-
            as.numeric(signif((amin-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          API_OUTPUT[row,"API_max"] <-
            as.numeric(signif((amax-AMinimum)/(AMaximum-AMinimum)),digits = 6)
        } else {
          API_OUTPUT[row,"API_min"] <- NA
          API_OUTPUT[row,"API_max"] <- NA
        }
      } else {
        if (is.na(mnth)){
          API_OUTPUT[row,"API_min"] <- NA
          API_OUTPUT[row,"API_max"] <- NA
        } else if (is.numeric(mnth) && mnth %in% c(1:12)){
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          AMinimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMin"]
          AMaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-AMinimum)/(AMaximum-AMinimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% month.abb[1:12]){
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          AMinimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "AMin"]
          AMaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-AMinimum)/(AMaximum-AMinimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% as.character(c(1:12))){
          mnth <- as.numeric(mnth)
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          AMinimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMin"]
          AMaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-AMinimum)/(AMaximum-AMinimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
          }
        } else if (is.character(mnth) && mnth=="Year"){
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          AMinimum <- year_limits[year_limits$Binomial %in%
                                    API_OUTPUT[row,"Binomial"], "AMin"]
          AMaximum <- year_limits[year_limits$Binomial %in%
                                    API_OUTPUT[row,"Binomial"], "AMax"]
          if (length(AMinimum) > 0 && length(AMaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-AMinimum)/(AMaximum-AMinimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-AMinimum)/(AMaximum-AMinimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
          }
        } else {
          API_OUTPUT[row,"API_min"] <- NA
          API_OUTPUT[row, "API_max"] <- NA
        }
      }
    }
    out_of_rangemin <- API_OUTPUT[is.na(API_OUTPUT$API_min),1]
    out_of_rangemax <- API_OUTPUT[is.na(API_OUTPUT$API_max),1]
    if(length(out_of_rangemin)>0||length(out_of_rangemax)>0){
      warning("Some rows contained innaccurate information for species name,
            month, or Aridity Data. Rows with this type of error had the
            API propogated with NA")
      return(API_OUTPUT)
    } else {
      return(API_OUTPUT)
    }
  }
  ############ALL##################
  else if (ar_var == "all"){
    API_OUTPUT <- sp_df
    API_OUTPUT$API_min <- NA
    API_OUTPUT$API_max <- NA
    API_OUTPUT$API_mean <- NA
    if (isTRUE(flag_sp)){
      API_OUTPUT$BINOMIALS_MATCH <- (API_OUTPUT$Binomial %in% year_limits$Binomial)
    }
    if (isTRUE(flag_month)){
      for (nr in 1:nrow(API_OUTPUT)){
        if (is.na(API_OUTPUT[nr,"Month"])){
          API_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else if (!API_OUTPUT[nr,"Month"] == "Year" &&
                   !API_OUTPUT[nr,"Month"] %in% c(1:12) &&
                   !API_OUTPUT[nr,"Month"] %in% as.character(c(1:12)) &&
                   !API_OUTPUT[nr,"Month"] %in% month.abb[1:12]){
          API_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else {
          API_OUTPUT[nr,"Flag_Month"] <- TRUE
        }
      }
    }
    if (isTRUE(flag_ar)){
      API_OUTPUT$Flag_AMin <- (!is.na(API_OUTPUT$AMin))
      API_OUTPUT$Flag_AMax <- (!is.na(API_OUTPUT$AMax))
      API_OUTPUT$Flag_Ar<- (!is.na(API_OUTPUT$Ar))
    }
    for (row in 1:nrow(API_OUTPUT)){
      mnth <- API_OUTPUT[row,"Month"]

      if (isTRUE(use_year)){
        amin <- API_OUTPUT[row, "AMin"]
        amax <- API_OUTPUT[row, "AMax"]
        amean <- API_OUTPUT[row, "Ar"]
        Aminimum <- year_limits[year_limits$Binomial %in%
                                  API_OUTPUT[row,"Binomial"], "AMin"]
        Amaximum <- year_limits[year_limits$Binomial %in%
                                  API_OUTPUT[row,"Binomial"], "AMax"]
        if (length(Aminimum) > 0 && length(Amaximum) > 0){
          API_OUTPUT[row,"API_min"] <-
            as.numeric(signif((amin-Aminimum)/(Amaximum-Aminimum)),digits = 6)
          API_OUTPUT[row,"API_max"] <-
            as.numeric(signif((amax-Aminimum)/(Amaximum-Aminimum)),digits = 6)
          API_OUTPUT[row,"API_mean"] <-
            as.numeric(signif((amean-Aminimum)/(Amaximum-Aminimum)),digits = 6)
        } else {
          API_OUTPUT[row,"API_min"] <- NA
          API_OUTPUT[row,"API_max"] <- NA
          API_OUTPUT[row,"API_mean"] <- NA
        }
      } else {
        if (is.na(mnth)){
          API_OUTPUT[row,"API_min"] <- NA
          API_OUTPUT[row,"API_max"] <- NA
          API_OUTPUT[row,"API_mean"] <- NA
        } else if (is.numeric(mnth) && mnth %in% c(1:12)){
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          amean <- API_OUTPUT[row, "Ar"]
          Aminimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMin"]
          Amaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMax"]
          if (length(Aminimum) > 0 && length(Amaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_mean"] <-
              as.numeric(signif((amean-Aminimum)/(Amaximum-Aminimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
            API_OUTPUT[row,"API_mean"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% month.abb[1:12]){
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          amean <- API_OUTPUT[row, "Ar"]
          Aminimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "AMin"]
          Amaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "AMax"]
          if (length(Aminimum) > 0 && length(Amaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_mean"] <-
              as.numeric(signif((amean-Aminimum)/(Amaximum-Aminimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
            API_OUTPUT[row,"API_mean"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% as.character(c(1:12))){
          mnth <- as.numeric(mnth)
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          amean <- API_OUTPUT[row, "Ar"]
          Aminimum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMin"]
          Amaximum <- month_limits[month_limits$Binomial %in%
                                     API_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "AMax"]
          if (length(Aminimum) > 0 && length(Amaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_mean"] <-
              as.numeric(signif((amean-Aminimum)/(Amaximum-Aminimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
            API_OUTPUT[row,"API_mean"] <- NA
          }
        } else if (is.character(mnth) && mnth=="Year"){
          amin <- API_OUTPUT[row, "AMin"]
          amax <- API_OUTPUT[row, "AMax"]
          amean <- API_OUTPUT[row, "Ar"]
          Aminimum <- year_limits[year_limits$Binomial %in%
                                    API_OUTPUT[row,"Binomial"], "AMin"]
          Amaximum <- year_limits[year_limits$Binomial %in%
                                    API_OUTPUT[row,"Binomial"], "AMax"]
          if (length(Aminimum) > 0 && length(Amaximum) > 0){
            API_OUTPUT[row,"API_min"] <-
              as.numeric(signif((amin-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_max"] <-
              as.numeric(signif((amax-Aminimum)/(Amaximum-Aminimum)),digits = 6)
            API_OUTPUT[row,"API_mean"] <-
              as.numeric(signif((amean-Aminimum)/(Amaximum-Aminimum)),digits = 6)
          } else {
            API_OUTPUT[row,"API_min"] <- NA
            API_OUTPUT[row,"API_max"] <- NA
            API_OUTPUT[row,"API_mean"] <- NA
          }
        } else {
        API_OUTPUT[row,"API_min"] <- NA
        API_OUTPUT[row,"API_max"] <- NA
        API_OUTPUT[row,"API_mean"] <- NA
        }
      }
    }
    out_of_rangemin <- API_OUTPUT[is.na(API_OUTPUT$API_min),1]
    out_of_rangemax <- API_OUTPUT[is.na(API_OUTPUT$API_max),1]
    out_of_rangemean <- API_OUTPUT[is.na(API_OUTPUT$API_mean),1]
    if(length(out_of_rangemin)>0 || length(out_of_rangemax)>0 || length(out_of_rangemean)>0){
      warning("Some rows contained innaccurate information for species name,
            month, or Aridity Data. Rows with this type of error had the
            API propogated with NA")
      return(API_OUTPUT)
    } else {
      return(API_OUTPUT)
    }
  } else if (!is.null(ar_var) && !ar_var == "all" && !ar_var == "minmax"){
    stop("tmp_var is out of range")
  }
}
