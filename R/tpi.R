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
#' titled "tmin", the other "tmax". "all" indicates user has three columns with
#' temperature data, should be titled "tmin", "tmax", and "Tm" ("Tm" indicating
#' mean temperature). Output columns for tmin = TPI_min, tmax = TPI_max,
#' Tm = TPI_mean when tmp_var = "all".
#'
#' @param use_year Boolean parameter, defualt FALSE. set to TURE to use yearly
#' thermal tolerance limits when Month is NA
#'
#' @param tmp_unit Input string. Defaults to NULL which informs function to
#' calculate TPI using degrees Celsius. Options: "Fahrenheit", "Kelvin".
#' "Fahrenheit" indicates user submitted temperature data is in degrees
#' Fahrenheit."Kelvin" indicates user submitted temperature data is in degrees
#' Kelvin.
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
#'                tmax = c(5,10),
#'                tmin = c(-2,-1),
#'                Tm = c(0,3)
#'                )
#'
#' #would return TPI for column Tm
#' results <- tpi(species_data)
#'
#' #would return TPI for tmin and tmax
#' results <- tpi(species_data, tmp_var = "minmax")
#'
#' #would return TPI for tmin, tmax, and Tm
#' results <- tpi(species_data, tmp_var = "all", use_year = TRUE,
#'                 flag_sp = TRUE, flag_month = TRUE, flag_tmp = TRUE)
#' @importFrom stats na.omit
#' @export
tpi <- function(sp_df, tmp_var = NULL, use_year = FALSE, tmp_unit = NULL, flag_sp = FALSE,
                flag_month = FALSE, flag_tmp = FALSE){
  Month<- TMax<- TMin<- TPI<- TPI_max<- TPI_mean<- TPI_min<- Tm<-tmax <- tmin <- NULL
  if(any(sp_df$Month %in% as.character(c(1:12))) &&
     any(sp_df$Month %in% month.abb[1:12])){
    stop("Month values are inconsistent: Months cannont contain 3 letter month codes and numeric values")
  }

  if(!is.null(tmp_unit) && !tmp_unit == "Fahrenheit" && !tmp_unit == "Kelvin"){
    stop("tmp_unit is misspecified")
  }

  if (!is.null(tmp_var) && !tmp_var == "all" && !tmp_var == "minmax"){
    stop("tmp_var is out of range")
  }
  ############NULL###############
  if (is.null(tmp_var)){
    TPI_OUTPUT <- sp_df
    TPI_OUTPUT$TPI <- NA

    if (isTRUE(flag_sp)){
      TPI_OUTPUT$BINOMIALS_MATCH <- (TPI_OUTPUT$Binomial %in% yearly_limits$Binomial)
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
    if(isTRUE(use_year)){
      TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,yearly_limits[,c(6:8)])

      ##change units
      if(tmp_unit == "Fahrenheit"){
        TPI_OUTPUT <- TPI_OUTPUT %>% mutate(TMin = (TMin * (9/5))+32,
                                        TMax = (TMax * (9/5))+32)
      } else if(tmp_unit == "Kelvin"){
        TPI_OUTPUT <- TPI_OUTPUT %>% mutate(TMin = TMin +273.15,
                                        TMax = TMax +273.15)
      }else {TPI_OUTPUT <- TPI_OUTPUT}
      ##
      TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                  TPI = (Tm-TMin)/(TMax-TMin))

      TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))
    }else{
      if(is.numeric(TPI_OUTPUT$Month)){
        mnth_lim <- month_limits[,c(6:8,11)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##

        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)
        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI = (Tm-TMin)/(TMax-TMin))
        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))

      }else if (is.character(TPI_OUTPUT$Month) && any(TPI_OUTPUT$Month %in% month.abb[1:12])){
        mnth_lim <- month_limits[,c(6:8,12)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##
        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)
        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI = (Tm-TMin)/(TMax-TMin))
        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))

      }else if (is.character(TPI_OUTPUT$Month) && any(TPI_OUTPUT$Month %in% as.character(c(1:12)))){
        mnth_lim <- month_limits[,c(6:8,11)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        } else {mnth_lim <- mnth_lim}
        ##

        mnth_lim <- dplyr::mutate(mnth_lim, Month = as.character(Month))
        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)
        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI = (Tm-TMin)/(TMax-TMin))
        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))
      }else{
        stop("Month values are incorrectly specified. Month should be reported numerically or as characters 1:12 or
           as three letter characters Jan:Dec")
      }
      if(any(na.omit(TPI_OUTPUT$Month =="Year"))){
        TPI_Year <- TPI_OUTPUT[TPI_OUTPUT$Month %in% "Year",]
        TPI_Year <- subset(TPI_Year,select = -c(TPI))

        TPI_Year <-  dplyr::left_join(TPI_Year,yearly_limits[,c(6:8)])

        ##change units
        if(tmp_unit == "Fahrenheit"){
          TPI_Year <- TPI_Year %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          TPI_Year <- TPI_Year %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else{
          TPI_Year <- TPI_Year
        }
        ##
        TPI_Year <- dplyr::mutate(TPI_Year,
                                  TPI = (Tm-TMin)/(TMax-TMin))
        TPI_Year <- subset(TPI_Year,select = -c(TMin,TMax))

        TPI_Month <- TPI_OUTPUT[!TPI_OUTPUT$Month %in% "Year",]
        TPI_OUTPUT <- rbind(TPI_Month,TPI_Year)
      }

    }

    if(any(is.na(TPI_OUTPUT$TPI))){
      warning("Some rows contained innaccurate information for species name,
            month or Tm. Rows with this type of error had the TPI propogated
            with NA")}
    return(TPI_OUTPUT)

  }
  ############MINMAX###############
  else if (tmp_var == "minmax"){
    TPI_OUTPUT <- sp_df
    TPI_OUTPUT$TPI_min <- NA
    TPI_OUTPUT$TPI_max <- NA

    if (isTRUE(flag_sp)){
      TPI_OUTPUT$BINOMIALS_MATCH <- (TPI_OUTPUT$Binomial %in% yearly_limits$Binomial)
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
      TPI_OUTPUT$Flag_TMin <- (!is.na(TPI_OUTPUT$tmin))
      TPI_OUTPUT$Flag_TMax <- (!is.na(TPI_OUTPUT$tmax))
    }
    if(isTRUE(use_year)){
      TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,yearly_limits[,c(6:8)])

      ##change units
      if(tmp_unit == "Fahrenheit"){
        TPI_OUTPUT <- TPI_OUTPUT %>% mutate(TMin = (TMin * (9/5))+32,
                                        TMax = (TMax * (9/5))+32)
      } else if(tmp_unit == "Kelvin"){
        TPI_OUTPUT <- TPI_OUTPUT %>% mutate(TMin = TMin +273.15,
                                        TMax = TMax +273.15)
      }else {TPI_OUTPUT <- TPI_OUTPUT}
      ##


      TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                  TPI_min = (tmin-TMin)/(TMax-TMin),
                                  TPI_max = (tmax-TMin)/(TMax-TMin))

      TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))

    }else{
      if(is.numeric(TPI_OUTPUT$Month)){
        mnth_lim <- month_limits[,c(6:8,11)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##

        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)

        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI_min = (tmin-TMin)/(TMax-TMin),
                                    TPI_max = (tmax-TMin)/(TMax-TMin))

        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))

      }else if (is.character(TPI_OUTPUT$Month) && any(TPI_OUTPUT$Month%in% month.abb[1:12])){
        mnth_lim <- month_limits[,c(6:8,12)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##

        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)
        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI_min = (tmin-TMin)/(TMax-TMin),
                                    TPI_max = (tmax-TMin)/(TMax-TMin))

        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))

      }else if (is.character(TPI_OUTPUT$Month) && any(TPI_OUTPUT$Month %in% as.character(c(1:12)))){
        mnth_lim <- month_limits[,c(6:8,11)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##

        mnth_lim <- dplyr::mutate(mnth_lim, Month = as.character(Month))
        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)
        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI_min = (tmin-TMin)/(TMax-TMin),
                                    TPI_max = (tmax-TMin)/(TMax-TMin))

        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))
      }else{
        stop("Month values are incorrectly specified. Month should be reported numerically or as characters 1:12 or
           as three letter characters Jan:Dec")
      }

      if(any(na.omit(TPI_OUTPUT$Month =="Year"))){
        TPI_Year <- TPI_OUTPUT[TPI_OUTPUT$Month %in% "Year",]
        TPI_Year <- subset(TPI_Year,select = -c(TPI_min,TPI_max))

        TPI_Year <-  dplyr::left_join(TPI_Year,yearly_limits[,c(6:8)])

        ##change units
        if(tmp_unit == "Fahrenheit"){
          TPI_Year <- TPI_Year %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          TPI_Year <- TPI_Year %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {TPI_Year <- TPI_Year}
        ##

        TPI_Year <- dplyr::mutate(TPI_Year,
                                  TPI_min = (tmin-TMin)/(TMax-TMin),
                                  TPI_max = (tmax-TMin)/(TMax-TMin))

        TPI_Year <- subset(TPI_Year,select = -c(TMin,TMax))

        TPI_Month <- TPI_OUTPUT[!TPI_OUTPUT$Month %in% "Year",]
        TPI_OUTPUT <- rbind(TPI_Month,TPI_Year)
      }
    }
    if(any(is.na(TPI_OUTPUT$TPI_min)) ||any(is.na(TPI_OUTPUT$TPI_max))  ){
      warning("Some rows contained innaccurate information for species name,
            month or Tm. Rows with this type of error had the TPI propogated
            with NA")}
    return(TPI_OUTPUT)
  }

  ############ALL###############
  else if (tmp_var == "all"){
    TPI_OUTPUT <- sp_df
    TPI_OUTPUT$TPI_min <- NA
    TPI_OUTPUT$TPI_max <- NA
    TPI_OUTPUT$TPI_mean <- NA
    if (isTRUE(flag_sp)){
      TPI_OUTPUT$BINOMIALS_MATCH <- (TPI_OUTPUT$Binomial %in% yearly_limits$Binomial)
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
      TPI_OUTPUT$Flag_TMin <- (!is.na(TPI_OUTPUT$tmin))
      TPI_OUTPUT$Flag_TMax <- (!is.na(TPI_OUTPUT$tmax))
      TPI_OUTPUT$Flag_Tm <- (!is.na(TPI_OUTPUT$Tm))
    }
    if(isTRUE(use_year)){
      TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,yearly_limits[,c(6:8)])

      ##change units
      if(tmp_unit == "Fahrenheit"){
        TPI_OUTPUT <- TPI_OUTPUT %>% mutate(TMin = (TMin * (9/5))+32,
                                        TMax = (TMax * (9/5))+32)
      } else if(tmp_unit == "Kelvin"){
        TPI_OUTPUT <- TPI_OUTPUT %>% mutate(TMin = TMin +273.15,
                                        TMax = TMax +273.15)
      }else {TPI_OUTPUT <- TPI_OUTPUT}
      ##

      TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                  TPI_min = (tmin-TMin)/(TMax-TMin),
                                  TPI_max = (tmax-TMin)/(TMax-TMin),
                                  TPI_mean = (Tm - TMin)/(TMax-TMin))

      TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))


    }else{
      if(is.numeric(TPI_OUTPUT$Month)){
        mnth_lim <- month_limits[,c(6:8,11)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                              TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                              TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##

        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)

        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI_min = (tmin-TMin)/(TMax-TMin),
                                    TPI_max = (tmax-TMin)/(TMax-TMin),
                                    TPI_mean = (Tm - TMin)/(TMax-TMin))

        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))

      }else if (is.character(TPI_OUTPUT$Month) && any(TPI_OUTPUT$Month %in% month.abb[1:12])){
        mnth_lim <- month_limits[,c(6:8,12)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##

        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)
        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI_min = (tmin-TMin)/(TMax-TMin),
                                    TPI_max = (tmax-TMin)/(TMax-TMin),
                                    TPI_mean = (Tm - TMin)/(TMax-TMin))

        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))

      }else if (is.character(TPI_OUTPUT$Month) && any(TPI_OUTPUT$Month %in% as.character(c(1:12)))){
        mnth_lim <- month_limits[,c(6:8,11)]
        colnames(mnth_lim)<-c("Binomial","TMin","TMax","Month")
        mnth_lim$Month <- as.character(mnth_lim$Month)

        ##change units
        if(tmp_unit == "Fahrenheit"){
          mnth_lim <- mnth_lim %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          mnth_lim <- mnth_lim %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {mnth_lim <- mnth_lim}
        ##

        mnth_lim <- dplyr::mutate(mnth_lim, Month = as.character(Month))
        TPI_OUTPUT <- dplyr::left_join(TPI_OUTPUT,mnth_lim)
        TPI_OUTPUT <- dplyr::mutate(TPI_OUTPUT,
                                    TPI_min = (tmin-TMin)/(TMax-TMin),
                                    TPI_max = (tmax-TMin)/(TMax-TMin),
                                    TPI_mean = (Tm - TMin)/(TMax-TMin))

        TPI_OUTPUT <- subset(TPI_OUTPUT,select = -c(TMin,TMax))
      }else{
        stop("Month values are incorrectly specified. Month should be reported numerically or as characters 1:12 or
           as three letter characters Jan:Dec")
      }
      if(any(na.omit(TPI_OUTPUT$Month =="Year"))){
        TPI_Year <- TPI_OUTPUT[TPI_OUTPUT$Month %in% "Year",]
        TPI_Year <- subset(TPI_Year,select = -c(TPI_min,TPI_max,TPI_mean))

        TPI_Year <-  dplyr::left_join(TPI_Year,yearly_limits[,c(6:8)])
        ##change units
        if(tmp_unit == "Fahrenheit"){
          TPI_Year <- TPI_Year %>% mutate(TMin = (TMin * (9/5))+32,
                                          TMax = (TMax * (9/5))+32)
        } else if(tmp_unit == "Kelvin"){
          TPI_Year <- TPI_Year %>% mutate(TMin = TMin +273.15,
                                          TMax = TMax +273.15)
        }else {TPI_Year <- TPI_Year}
        ##
        TPI_Year <- dplyr::mutate(TPI_Year,
                                  TPI_min = (tmin-TMin)/(TMax-TMin),
                                  TPI_max = (tmax-TMin)/(TMax-TMin),
                                  TPI_mean = (Tm - TMin)/(TMax-TMin))

        TPI_Year <- subset(TPI_Year,select = -c(TMin,TMax))

        TPI_Month <- TPI_OUTPUT[!TPI_OUTPUT$Month %in% "Year",]
        TPI_OUTPUT <- rbind(TPI_Month,TPI_Year)
      }
    }
    if(any(is.na(TPI_OUTPUT$TPI_min)) ||any(is.na(TPI_OUTPUT$TPI_max))  ){
      warning("Some rows contained innaccurate information for species name,
            month or Tm. Rows with this type of error had the TPI propogated
            with NA")}
    return(TPI_OUTPUT)

  }
}

