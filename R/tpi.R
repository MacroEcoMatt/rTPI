#' Calculte TPI for Know Temperature Data
#'
#' `tpi()` Takes dataframe submitted by user and calculates Thermal Position
#' Index. Uses species Binomial and Date to extract thermal niche limits and
#' add a column to submitted dataframe with TPI values.
#'
#' @param sp_df Input data. Must be a dataframe containing a column titled
#' "Binomial" which contains genus species binomial ("Poecile atricapillus"),
#' a column title "Tm" which has temperature values in degrees Celcius, and a
#' column titled "Month" that contains a numeric value from 1-12 or month codes
#' (ex. "Jan"). If Month of observed temperature recording is unknown users may
#' choose to include "Year" in the Month column which will have the function
#' calculate TPI based on Yearly Average TPI limits. NAs will be ignored.
#'
#' @param tmp_var Input string. Defaults to NULL which informs function to
#' calculate TPI for column titled "Tm". Options: "minmax", "all".
#' "minmax" indicates user has two columns with temperature data, one should be
#' titled "Tmin", the other "Tmax". "all" indicates user has three columns with
#' temperature data, should be titled "Tmin", "Tmax", and "Tm" ("Tm" indicating
#' mean temperature). Output columns for Tmin = TPI_min, Tmax = TPI_max,
#' Tm = TPI_mean when tmp_var = "all".
#'
#' @param use_year Boolean parameter, defualt FALSE. set to TURE to use yearly
#' thermal tolerance limits when Month is NA
#'
#' @param flag_sp Boolean parameter, default FALSE. Set to true to add column
#' to final data frame that contains TRUE values for submitted species Binomials
#' that match TPI data set, and FALSE for rows that contain Binomials that do
#' not match those in the TPI data set. Used to Identify erroneous Binomials
#'
#' @param flag_month Boolean parameter, default FALSE. Set to true to add column
#' to final data frame that contains TRUE values for appropriately submitted
#' Month formats and FALSE for inappropriately formatted Month values. Used to
#' Identify erroneous month submissions.
#'
#' @param flag_tmp Boolean parameter, default FALSE. Set to true to add
#' column(s) to final data frame that contains FALSE values if temperature
#' values are submitted as NA. TRUE is reported when no temperature errors are
#' submitted. Aids in identifying errors of temperature submission.
#'
#' @returns A Dataframe.
#' @examples
#' #example df
#'
#' species_data <- data.frame(
#'                Binomial = c("Poecile atricapillus", "Poecile atricapillus"),
#'                Month = c(1,2),
#'                TMax = c(5,10),
#'                TMin = c(-2,-1),
#'                Tm = c(0,3)
#'                )
#'
#' #would return TPI for column Tm
#' results <- tpi(species_data)
#'
#' #would return TPI for TMin and TMax
#' results <- tpi(species_data, tmp_var = "minmax")
#'
#' #would return TPI for TMin, TMax, and Tm
#' results <- tpi(species_data, tmp_var = "all", use_year = TRUE,
#'                 flag_sp = TRUE, flag_month = TRUE, flag_tmp = TRUE)
#'
#' @export
tpi <- function(sp_df, tmp_var = NULL, use_year = FALSE, flag_sp = FALSE,
                flag_month = FALSE, flag_tmp = FALSE){
  ############NULL###############
  if (is.null(tmp_var)){
    TPI_OUTPUT <- sp_df
    TPI_OUTPUT$TPI <- NA

    if (isTRUE(flag_sp)){
      TPI_OUTPUT$BINOMIALS_MATCH <- (TPI_OUTPUT$Binomial %in% year_limits$Binomial)
    }
    if (isTRUE(flag_month)){
      for (nr in 1:nrow(TPI_OUTPUT)){
        if (is.na(TPI_OUTPUT[nr,"Month"])){
          TPI_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else if (!TPI_OUTPUT[nr,"Month"] == "Year" &&
                   !TPI_OUTPUT[nr,"Month"] %in% c(1:12) &&
                   !TPI_OUTPUT[nr,"Month"] %in% as.character(c(1:12)) &&
                   !TPI_OUTPUT[nr,"Month"] %in% month.abb[1:12]){
          TPI_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else {
          TPI_OUTPUT[nr,"Flag_Month"] <- TRUE
        }
      }
    }
    if (isTRUE(flag_tmp)){
      TPI_OUTPUT$Flag_Tm <- (!is.na(TPI_OUTPUT$Tm))
    }

    for (row in 1:nrow(TPI_OUTPUT)){
      mnth <- TPI_OUTPUT$Month[[row]]

      if(isTRUE(use_year)){
        t <- TPI_OUTPUT[row, "Tm"]
        TMinimum <- year_limits[year_limits$Binomial %in%
                                  TPI_OUTPUT[row,"Binomial"], "TMin"]
        TMaximum <- year_limits[year_limits$Binomial %in%
                                  TPI_OUTPUT[row,"Binomial"], "TMax"]
        if (length(TMinimum) > 0 && length(TMaximum) > 0){
          TPI_OUTPUT[row,"TPI"] <-
            as.numeric(signif((t-TMinimum)/(TMaximum-TMinimum)),digits = 6)
        } else {
          TPI_OUTPUT[row,"TPI"] <- NA
        }
      } else {
        if (is.na(mnth)){
          TPI_OUTPUT[row,"TPI"] <- NA
        } else if (is.numeric(mnth) && mnth %in% c(1:12)){
          t <- TPI_OUTPUT[row, "Tm"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI"] <-
              as.numeric(signif((t-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% month.abb[1:12]){
          t <- TPI_OUTPUT[row, "Tm"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI"] <-
              as.numeric(signif((t-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% as.character(c(1:12))){
          t <- TPI_OUTPUT[row, "Tm"]
          mnth <- as.numeric(mnth)
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI"] <-
              as.numeric(signif((t-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI"] <- NA
          }
        }else if (is.character(mnth) && mnth=="Year"){
          t <- TPI_OUTPUT[row, "Tm"]
          TMinimum <- year_limits[year_limits$Binomial %in%
                                    TPI_OUTPUT[row,"Binomial"], "TMin"]
          TMaximum <- year_limits[year_limits$Binomial %in%
                                    TPI_OUTPUT[row,"Binomial"], "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI"] <-
              as.numeric(signif((t-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI"] <- NA
          }
        } else {
          TPI_OUTPUT[row,"TPI"] <- NA
        }
      }
    }
    out_of_range <- TPI_OUTPUT[is.na(TPI_OUTPUT$TPI),"TPI"]
    if(length(out_of_range) > 0){
      warning("Some rows contained innaccurate information for species name,
            month or Tm. Rows with this type of error had the TPI propogated
            with NA")
      return(TPI_OUTPUT)
    } else {
      return(TPI_OUTPUT)
    }
  }
  ############MINMAX###############
  else if (tmp_var == "minmax"){
    TPI_OUTPUT <- sp_df
    TPI_OUTPUT$TPI_min <- NA
    TPI_OUTPUT$TPI_max <- NA

    if (isTRUE(flag_sp)){
      TPI_OUTPUT$BINOMIALS_MATCH <- (TPI_OUTPUT$Binomial %in% year_limits$Binomial)
    }
    if (isTRUE(flag_month)){
      for (nr in 1:nrow(TPI_OUTPUT)){
        if (is.na(TPI_OUTPUT[nr,"Month"])){
          TPI_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else if (!TPI_OUTPUT[nr,"Month"] == "Year"  &&
                   !TPI_OUTPUT[nr,"Month"] %in% c(1:12)  &&
                   !TPI_OUTPUT[nr,"Month"] %in% as.character(c(1:12)) &&
                   !TPI_OUTPUT[nr,"Month"] %in% month.abb[1:12]){
          TPI_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else {
          TPI_OUTPUT[nr,"Flag_Month"] <- TRUE
        }
      }
    }
    if (isTRUE(flag_tmp)){
      TPI_OUTPUT$Flag_TMin <- (!is.na(TPI_OUTPUT$TMin))
      TPI_OUTPUT$Flag_TMax <- (!is.na(TPI_OUTPUT$TMax))
    }
    for (row in 1:nrow(TPI_OUTPUT)){
      mnth <- TPI_OUTPUT[row,"Month"]

      if (isTRUE(use_year)){
        tmin <- TPI_OUTPUT[row, "TMin"]
        tmax <- TPI_OUTPUT[row, "TMax"]

        TMinimum <- year_limits[year_limits$Binomial %in%
                                  TPI_OUTPUT[row,"Binomial"], "TMin"]
        TMaximum <- year_limits[year_limits$Binomial %in%
                                  TPI_OUTPUT[row,"Binomial"], "TMax"]
        if (length(TMinimum) > 0 && length(TMaximum) > 0){
          TPI_OUTPUT[row,"TPI_min"] <-
            as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          TPI_OUTPUT[row,"TPI_max"] <-
            as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
        } else {
          TPI_OUTPUT[row,"TPI_min"] <- NA
          TPI_OUTPUT[row,"TPI_max"] <- NA
        }
      } else {
        if (is.na(mnth)){
          TPI_OUTPUT[row,"TPI_min"] <- NA
          TPI_OUTPUT[row,"TPI_max"] <- NA
        } else if (is.numeric(mnth) && mnth %in% c(1:12)){
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% month.abb[1:12]){
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% as.character(c(1:12))){
          mnth <- as.numeric(mnth)
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
          }
        } else if (is.character(mnth) && mnth=="Year"){
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          TMinimum <- year_limits[year_limits$Binomial %in%
                                    TPI_OUTPUT[row,"Binomial"], "TMin"]
          TMaximum <- year_limits[year_limits$Binomial %in%
                                    TPI_OUTPUT[row,"Binomial"], "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
          }
        } else {
          TPI_OUTPUT[row,"TPI_min"] <- NA
          TPI_OUTPUT[row, "TPI_max"] <- NA
        }
      }
    }
    out_of_rangemin <- TPI_OUTPUT[is.na(TPI_OUTPUT$TPI_min),1]
    out_of_rangemax <- TPI_OUTPUT[is.na(TPI_OUTPUT$TPI_max),1]
    if(length(out_of_rangemin)>0 || length(out_of_rangemax)>0){
      warning("Some rows contained innaccurate information for species name,
            month, or Temperature Data. Rows with this type of error had the
            TPI propogated with NA")
      return(TPI_OUTPUT)
    } else {
      return(TPI_OUTPUT)
    }
  }
  ############ALL##################
  else if (tmp_var == "all"){
    TPI_OUTPUT <- sp_df
    TPI_OUTPUT$TPI_min <- NA
    TPI_OUTPUT$TPI_max <- NA
    TPI_OUTPUT$TPI_mean <- NA
    if (isTRUE(flag_sp)){
      TPI_OUTPUT$BINOMIALS_MATCH <- (TPI_OUTPUT$Binomial %in% year_limits$Binomial)
    }
    if (isTRUE(flag_month)){
      for (nr in 1:nrow(TPI_OUTPUT)){
        if (is.na(TPI_OUTPUT[nr,"Month"])){
          TPI_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else if (!TPI_OUTPUT[nr,"Month"] == "Year" &&
                   !TPI_OUTPUT[nr,"Month"] %in% c(1:12) &&
                   !TPI_OUTPUT[nr,"Month"] %in% as.character(c(1:12)) &&
                   !TPI_OUTPUT[nr,"Month"] %in% month.abb[1:12]){
          TPI_OUTPUT[nr,"Flag_Month"] <- FALSE
        } else {
          TPI_OUTPUT[nr,"Flag_Month"] <- TRUE
        }
      }
    }
    if (isTRUE(flag_tmp)){
      TPI_OUTPUT$Flag_TMin <- (!is.na(TPI_OUTPUT$TMin))
      TPI_OUTPUT$Flag_TMax <- (!is.na(TPI_OUTPUT$TMax))
      TPI_OUTPUT$Flag_Tm <- (!is.na(TPI_OUTPUT$Tm))
    }
    for (row in 1:nrow(TPI_OUTPUT)){
      mnth <- TPI_OUTPUT[row,"Month"]

      if (isTRUE(use_year)){
        tmin <- TPI_OUTPUT[row, "TMin"]
        tmax <- TPI_OUTPUT[row, "TMax"]
        tmean <- TPI_OUTPUT[row, "Tm"]

        TMinimum <- year_limits[year_limits$Binomial %in%
                                  TPI_OUTPUT[row,"Binomial"], "TMin"]
        TMaximum <- year_limits[year_limits$Binomial %in%
                                  TPI_OUTPUT[row,"Binomial"], "TMax"]
        if (length(TMinimum) > 0 && length(TMaximum) > 0){
          TPI_OUTPUT[row,"TPI_min"] <-
            as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          TPI_OUTPUT[row,"TPI_max"] <-
            as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          TPI_OUTPUT[row,"TPI_mean"] <-
            as.numeric(signif((tmean-TMinimum)/(TMaximum-TMinimum)),digits = 6)
        } else {
          TPI_OUTPUT[row,"TPI_min"] <- NA
          TPI_OUTPUT[row,"TPI_max"] <- NA
          TPI_OUTPUT[row,"TPI_mean"] <- NA
        }
      } else {
        if (is.na(mnth)){
          TPI_OUTPUT[row,"TPI_min"] <- NA
          TPI_OUTPUT[row,"TPI_max"] <- NA
          TPI_OUTPUT[row,"TPI_mean"] <- NA
        } else if (is.numeric(mnth) && mnth %in% c(1:12)){
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          tmean <- TPI_OUTPUT[row, "Tm"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_mean"] <-
              as.numeric(signif((tmean-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
            TPI_OUTPUT[row,"TPI_mean"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% month.abb[1:12]){
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          tmean <- TPI_OUTPUT[row, "Tm"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthName == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_mean"] <-
              as.numeric(signif((tmean-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
            TPI_OUTPUT[row,"TPI_mean"] <- NA
          }
        } else if (is.character(mnth) && mnth %in% as.character(c(1:12))){
          mnth <- as.numeric(mnth)
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          tmean <- TPI_OUTPUT[row, "Tm"]
          TMinimum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMin"]
          TMaximum <- month_limits[month_limits$Binomial %in%
                                     TPI_OUTPUT[row,"Binomial"] &
                                     month_limits$MonthNumber == mnth, "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_mean"] <-
              as.numeric(signif((tmean-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
            TPI_OUTPUT[row,"TPI_mean"] <- NA
          }
        } else if (is.character(mnth) && mnth=="Year"){
          tmin <- TPI_OUTPUT[row, "TMin"]
          tmax <- TPI_OUTPUT[row, "TMax"]
          tmean <- TPI_OUTPUT[row, "Tm"]
          TMinimum <- year_limits[year_limits$Binomial %in%
                                    TPI_OUTPUT[row,"Binomial"], "TMin"]
          TMaximum <- year_limits[year_limits$Binomial %in%
                                    TPI_OUTPUT[row,"Binomial"], "TMax"]
          if (length(TMinimum) > 0 && length(TMaximum) > 0){
            TPI_OUTPUT[row,"TPI_min"] <-
              as.numeric(signif((tmin-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_max"] <-
              as.numeric(signif((tmax-TMinimum)/(TMaximum-TMinimum)),digits = 6)
            TPI_OUTPUT[row,"TPI_mean"] <-
              as.numeric(signif((tmean-TMinimum)/(TMaximum-TMinimum)),digits = 6)
          } else {
            TPI_OUTPUT[row,"TPI_min"] <- NA
            TPI_OUTPUT[row,"TPI_max"] <- NA
            TPI_OUTPUT[row,"TPI_mean"] <- NA
          }
        } else {
          TPI_OUTPUT[row,"TPI_min"] <- NA
          TPI_OUTPUT[row,"TPI_max"] <- NA
          TPI_OUTPUT[row,"TPI_mean"] <- NA
        }
      }
    }
    out_of_rangemin <- TPI_OUTPUT[is.na(TPI_OUTPUT$TPI_min),1]
    out_of_rangemax <- TPI_OUTPUT[is.na(TPI_OUTPUT$TPI_max),1]
    out_of_rangemean <- TPI_OUTPUT[is.na(TPI_OUTPUT$TPI_mean),1]
    if(length(out_of_rangemin)>0||length(out_of_rangemax)>0||length(out_of_rangemean)>0){
      warning("Some rows contained innaccurate information for species name,
            month, or Temperature Data. Rows with this type of error had the
            TPI propogated with NA")
      return(TPI_OUTPUT)
    } else {
      return(TPI_OUTPUT)
    }
  }else if (!is.null(tmp_var) && !tmp_var == "all" && !tmp_var == "minmax"){
    stop("tmp_var is out of range")
  }
}
