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
#' titled "amin", the other "amax". "all" indicates user has three columns with
#' aridity data, should be titled "amin", "amax", and "Ar" ("Ar" indicating
#' mean aridity). Output columns for amin = API_min, amax = API_max,
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
#' #example df
#' species_data <-data.frame(
#'                Binomial = c("Poecile atricapillus", "Poecile atricapillus"),
#'                Month = c(1,2),
#'                amax = c(5,5),
#'                amin = c(2,1),
#'                Ar = c(0,3)
#'                )
#'
#' #would return API for column Ar
#' api(species_data)
#'
#' #would return API for anin and anax
#' api(species_data, ar_var = "minmax")
#'
#' #would return API for anin, anax, and Ar
#' api(species_data, ar_var = "all", use_year = TRUE,
#'                 flag_sp = TRUE, flag_month = TRUE, flag_ar = TRUE)
#'
#' @export
api <- function(sp_df, ar_var = NULL, use_year = FALSE, flag_sp = FALSE,
                flag_month = FALSE, flag_ar = FALSE){
  Month<- AMax<- AMin<- API<- API_max<- API_mean<- API_min<- Ar<-amax <- amin <- NULL

  if(any(sp_df$Month %in% as.character(c(1:12))) &&
     any(sp_df$Month %in% month.abb[1:12])){
    stop("Month values are inconsistent: Months cannont contain 3 letter month codes and numeric values")
  }

  if (!is.null(ar_var) && !ar_var == "all" && !ar_var == "minmax"){
    stop("ar_var is out of range")
  }

  if(is.null(ar_var) && isTRUE(any(sp_df$Ar < 0))){
    stop("Error: Ar values must be 0 or greater; Some Ar values are negative")
  } else if(ar_var == "all" && isTRUE(any(sp_df$Ar < 0)) ||
            ar_var == "all" && isTRUE(any(sp_df$amax < 0)) ||
            ar_var == "all" && isTRUE(any(sp_df$amin < 0))){
    stop("Error: Ar, amin, amax values must be 0 or greater; Some values are negative")
  } else if (ar_var == "minmax" && isTRUE(any(sp_df$amax < 0)) ||
             ar_var == "minmax" && isTRUE(any(sp_df$amin < 0))){
    stop("Error: amin, amax values must be 0 or greater; Some values are negative")
  }

  ############NULL###############
  if (is.null(ar_var)){
    API_OUTPUT <- sp_df
    API_OUTPUT$API <- NA

    if (isTRUE(flag_sp)){
      API_OUTPUT$BINOMIALS_MATCH <- (API_OUTPUT$Binomial %in% yearly_limits$Binomial)
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
    if(isTRUE(use_year)){
      API_OUTPUT <- dplyr::left_join(API_OUTPUT,yearly_limits[,c(6,9:10)])

      API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                  API = (Ar-AMin)/(AMax-AMin))

      API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))
    }else{
      if(is.numeric(API_OUTPUT$Month)){
        mnth_lim <- month_limits[,c(6,9:11)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")

        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)
        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API = (Ar-AMin)/(AMax-AMin))
        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))

      }else if (is.character(API_OUTPUT$Month) && any(API_OUTPUT$Month %in% month.abb[1:12])){
        mnth_lim <- month_limits[,c(6,9:10,12)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)
        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API = (Ar-AMin)/(AMax-AMin))
        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))

      }else if (is.character(API_OUTPUT$Month) && any(API_OUTPUT$Month %in% as.character(c(1:12)))){
        mnth_lim <- month_limits[,c(6,9:11)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)
        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API = (Ar-AMin)/(AMax-AMin))
        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))
      }else{
        stop("Month values are incorrectly specified. Month should be reported numerically or as characters 1:12 or
           as three letter characters Jan:Dec")
      }
      if(any(na.omit(API_OUTPUT$Month =="Year"))){
        API_Year <- API_OUTPUT[API_OUTPUT$Month %in% "Year",]
        API_Year <- subset(API_Year,select = -c(API))

        API_Year <-  dplyr::left_join(API_Year,yearly_limits[,c(6,9:10)])
        API_Year <- dplyr::mutate(API_Year,
                                  API = (Ar-AMin)/(AMax-AMin))
        API_Year <- subset(API_Year,select = -c(AMin,AMax))

        API_Month <- API_OUTPUT[!API_OUTPUT$Month %in% "Year",]
        API_OUTPUT <- rbind(API_Month,API_Year)
      }

    }

    if(any(is.na(API_OUTPUT$API))){
      warning("Some rows contained innaccurate information for species name,
            month or Ar. Rows with this type of error had the API propogated
            with NA")}
    return(API_OUTPUT)

  }
  ############MINMAX###############
  else if (ar_var == "minmax"){
    API_OUTPUT <- sp_df
    API_OUTPUT$API_min <- NA
    API_OUTPUT$API_max <- NA

    if (isTRUE(flag_sp)){
      API_OUTPUT$BINOMIALS_MATCH <- (API_OUTPUT$Binomial %in% yearly_limits$Binomial)
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
      API_OUTPUT$Flag_AMin <- (!is.na(API_OUTPUT$amin))
      API_OUTPUT$Flag_AMax <- (!is.na(API_OUTPUT$amax))
    }
    if(isTRUE(use_year)){
      API_OUTPUT <- dplyr::left_join(API_OUTPUT,yearly_limits[,c(6,9,10)])

      API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                  API_min = (amin-AMin)/(AMax-AMin),
                                  API_max = (amax-AMin)/(AMax-AMin))

      API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))

    }else{
      if(is.numeric(API_OUTPUT$Month)){
        mnth_lim <- month_limits[,c(6,9,10,11)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")

        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)

        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API_min = (amin-AMin)/(AMax-AMin),
                                    API_max = (amax-AMin)/(AMax-AMin))

        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))

      }else if (is.character(API_OUTPUT$Month) && any(API_OUTPUT$Month%in% month.abb[1:12])){
        mnth_lim <- month_limits[,c(6,9,10,12)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")

        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)
        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API_min = (amin-AMin)/(AMax-AMin),
                                    API_max = (amax-AMin)/(AMax-AMin))

        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))

      }else if (is.character(API_OUTPUT$Month) && any(API_OUTPUT$Month %in% as.character(c(1:12)))){
        mnth_lim <- month_limits[,c(6,9,10,11)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        mnth_lim <- dplyr::mutate(mnth_lim, Month = as.character(Month))
        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)
        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API_min = (amin-AMin)/(AMax-AMin),
                                    API_max = (amax-AMin)/(AMax-AMin))

        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))
      }else{
        stop("Month values are incorrectly specified. Month should be reported numerically or as characters 1:12 or
           as three letter characters Jan:Dec")
      }

      if(any(na.omit(API_OUTPUT$Month =="Year"))){
        API_Year <- API_OUTPUT[API_OUTPUT$Month %in% "Year",]
        API_Year <- subset(API_Year,select = -c(API_min,API_max))

        API_Year <-  dplyr::left_join(API_Year,yearly_limits[,c(6,9,10)])
        API_Year <- dplyr::mutate(API_Year,
                                  API_min = (amin-AMin)/(AMax-AMin),
                                  API_max = (amax-AMin)/(AMax-AMin))

        API_Year <- subset(API_Year,select = -c(AMin,AMax))

        API_Month <- API_OUTPUT[!API_OUTPUT$Month %in% "Year",]
        API_OUTPUT <- rbind(API_Month,API_Year)
      }
    }
    if(any(is.na(API_OUTPUT$API_min)) ||any(is.na(API_OUTPUT$API_max))  ){
      warning("Some rows contained innaccurate information for species name,
            month or Ar. Rows with this type of error had the API propogated
            with NA")}
    return(API_OUTPUT)
  }

  ############ALL###############
  else if (ar_var == "all"){
    API_OUTPUT <- sp_df
    API_OUTPUT$API_min <- NA
    API_OUTPUT$API_max <- NA
    API_OUTPUT$API_mean <- NA
    if (isTRUE(flag_sp)){
      API_OUTPUT$BINOMIALS_MATCH <- (API_OUTPUT$Binomial %in% yearly_limits$Binomial)
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
      API_OUTPUT$Flag_AMin <- (!is.na(API_OUTPUT$amin))
      API_OUTPUT$Flag_AMax <- (!is.na(API_OUTPUT$amax))
      API_OUTPUT$Flag_Ar<- (!is.na(API_OUTPUT$Ar))
    }
    if(isTRUE(use_year)){
      API_OUTPUT <- dplyr::left_join(API_OUTPUT,yearly_limits[,c(6,9,10)])

      API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                  API_min = (amin-AMin)/(AMax-AMin),
                                  API_max = (amax-AMin)/(AMax-AMin),
                                  API_mean = (Ar - AMin)/(AMax-AMin))

      API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))


    }else{
      if(is.numeric(API_OUTPUT$Month)){
        mnth_lim <- month_limits[,c(6,9,10,11)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")

        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)

        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API_min = (amin-AMin)/(AMax-AMin),
                                    API_max = (amax-AMin)/(AMax-AMin),
                                    API_mean = (Ar - AMin)/(AMax-AMin))

        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))

      }else if (is.character(API_OUTPUT$Month) && any(API_OUTPUT$Month %in% month.abb[1:12])){
        mnth_lim <- month_limits[,c(6,9,10,12)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")

        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)
        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API_min = (amin-AMin)/(AMax-AMin),
                                    API_max = (amax-AMin)/(AMax-AMin),
                                    API_mean = (Ar - AMin)/(AMax-AMin))

        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))

      }else if (is.character(API_OUTPUT$Month) && any(API_OUTPUT$Month %in% as.character(c(1:12)))){
        mnth_lim <- month_limits[,c(6,9,10,11)]
        colnames(mnth_lim)<-c("Binomial","AMin","AMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        mnth_lim <- dplyr::mutate(mnth_lim, Month = as.character(Month))
        API_OUTPUT <- dplyr::left_join(API_OUTPUT,mnth_lim)
        API_OUTPUT <- dplyr::mutate(API_OUTPUT,
                                    API_min = (amin-AMin)/(AMax-AMin),
                                    API_max = (amax-AMin)/(AMax-AMin),
                                    API_mean = (Ar - AMin)/(AMax-AMin))

        API_OUTPUT <- subset(API_OUTPUT,select = -c(AMin,AMax))
      }else{
        stop("Month values are incorrectly specified. Month should be reported numerically or as characters 1:12 or
           as three letter characters Jan:Dec")
      }
      if(any(na.omit(API_OUTPUT$Month =="Year"))){
        API_Year <- API_OUTPUT[API_OUTPUT$Month %in% "Year",]
        API_Year <- subset(API_Year,select = -c(API_min,API_max,API_mean))

        API_Year <-  dplyr::left_join(API_Year,yearly_limits[,c(6,9,10)])
        API_Year <- dplyr::mutate(API_Year,
                                  API_min = (amin-AMin)/(AMax-AMin),
                                  API_max = (amax-AMin)/(AMax-AMin),
                                  API_mean = (Ar - AMin)/(AMax-AMin))

        API_Year <- subset(API_Year,select = -c(AMin,AMax))

        API_Month <- API_OUTPUT[!API_OUTPUT$Month %in% "Year",]
        API_OUTPUT <- rbind(API_Month,API_Year)
      }
    }
    if(any(is.na(API_OUTPUT$API_min)) ||any(is.na(API_OUTPUT$API_max))  ){
      warning("Some rows contained innaccurate information for species name,
            month or Ar. Rows with this type of error had the API propogated
            with NA")}
    return(API_OUTPUT)

  }
}
