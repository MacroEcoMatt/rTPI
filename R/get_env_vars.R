#' Extracts Environmental Variables from Coordinates and Dates
#'
#' `get_env_vars` Takes dataframe submitted by user and extracts envrionmental
#' variable from Terra Climate. Users can specify Temperature, Aridirty, or
#' both. Can extract data based on Year, Year/Month, Year/Month/Day or Season.
#' Requires climateR, raster, and sf packages.
#'
#' @param coord_data Input data. Must be a dataframe containing a column titled
#' "Lon" and "Lat", containing Longitude and Latitude in degrees. Depending on
#' date of observation users should include 3 separate columns for dates: "Year"
#' "Month" and "Day". All values for dates should be numeric. If interested in
#' getting seasonal average conditions data must include a column titled Season
#' and contain "spring", "summer", "fall", or "winter". Function accounts for
#' geographic location when considering season based on Hemisphere of
#' observation
#'
#' @param env_vars Character string or vector. Data will be extracted for each
#' specified parameter. Options: "both", "tmp", "aridity". Mean monthly
#' temperature is not available through Terra Climate. If users want to use
#' mean monthly temperatures they need to acquire their data separately.
#'
#' @param date_format Input character string to specify date resolution.
#' Defaults to "monthly" and uses Year and Month to extract data. Specify
#' "daily" if your date observations are contain Year,Month, and Day AND has
#' coordinates contained within the continental United States Years (1979-
#' present). Specify "seasonal" to get monthly average variable values for the
#' given season (generates 3 rows per input). Specify "yearly" to extract monthly
#' average variable values for each month (generates 12 rows per input).
#'
#' @returns A Dataframe.
#' @examples
#'
#'df <- data.frame(Year = c(2000,2000,2000,2000,2000,2000),
#' Month = c(6,6,6,6,6,6),
#' Day = c(3,5,7,9,11,13),
#' Lat = c(41.334287,41.334287,41.334287,41.334287,41.334287,41.334287),
#' Lon = c(-77.942020,-77.942020,-77.942020,-77.942020,-77.942020,-77.942020),
#' Season = c("spring","spring","spring","spring","spring","spring"))
#'
#'test <- get_env_vars(df, env_vars="aridity", date_format = "daily")
#'
#'test <- get_env_vars(df, env_vars="both", date_format="monthly")
#'
#'test <- get_env_vars(df, env_vars="tmp", date_format="yearly")
#'
#'test <- get_env_vars(df, env_vars="both", date_format="seasonal")
#'
#' @export
get_env_vars <- function(point_data, env_vars = "both", date_format = "monthly"){
  #CHECK PACKAGES
  try(if(isFALSE(require(climateR)))stop("No package: climateR"))
  try(if(isFALSE(require(AOI)))stop("No package: AOI"))
  try(if(isFALSE(require(sf)))stop("No package: sf"))
  try(if(isFALSE(require(raster)))stop("No package: raster"))
  try(if(isFALSE(require(terra)))stop("No package: terra"))
  try(if(isFALSE(require(AOI)))stop("No package: AOI"))
  try(if(isFALSE(require(dplyr)))stop("No package: dplyr"))
  #set up output sheet
  if(isTRUE(any(is.na(point_data$Lat))) || isTRUE(any(is.na(point_data$Lon))) ||
   isTRUE(any(point_data$Lat < -90)) || isTRUE(any(point_data$Lat > 90)) ||
   isTRUE(any(point_data$Lon < -180)) || isTRUE(any(point_data$Lon >180))){
    stop("Error in Latitude and Longitude: NA present or impossible values")
  }
  ifelse(env_vars == "tmp",
         point_data[,c("tmin","tmax")] <- NA,
         ifelse(env_vars=="aridity",
                point_data[,c("pet","ppt","aridity")] <- NA,
                point_data[,c("tmin","tmax","pet","ppt","aridity")] <- NA))

  #get environmental variables
  if(date_format == "monthly"){

    #create date column
    point_data[,"Date"] <- paste0(point_data$Year,"-",formatC(point_data$Month, width=2,flag=0),"-01")

    #set up spatial object
    df_coords <- point_data
    df_coords$CODE <- 1:nrow(df_coords)
    df_coords <- sf::st_as_sf(df_coords, coords = c("Lon","Lat"),crs=4326)

    if(env_vars=="both"){
      for(row in 1:nrow(point_data)){

        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("pet","ppt","tmax","tmin"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        pet <- climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")
        tmax <-climateR::extract_sites(r = tmpdata[3], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[4], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmin"] <- as.numeric(tmin[[1]][2])
        point_data[row, "tmax"] <- as.numeric(tmax[[1]][2])
        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                          ifelse(pet[[1]][2] == 0, NA,
                                                                 ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                        ppt[[1]][2]/pet[[1]][2]))))

      }
    } else if (env_vars=="tmp"){
      for(row in 1:nrow(point_data)){
        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("tmax","tmin"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        tmax <-climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmin"] <- as.numeric(tmin[[1]][2])
        point_data[row, "tmax"] <- as.numeric(tmax[[1]][2])
      }
    } else if (env_vars=="aridity"){
      for(row in 1:nrow(point_data)){
        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]

        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("pet","ppt"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        pet <- climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                        ifelse(pet[[1]][2] == 0, NA,
                                                               ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                      ppt[[1]][2]/pet[[1]][2]))))

      }
    } else {
      stop("env_vars out of range")
    }
  } else if (date_format == "yearly"){

    #create date column
    max_count <- (nrow(point_data)*12)

    point_data <- as.data.frame(point_data[rep(seq_len(nrow(point_data)), each = 12), ])
    point_data$Month_counter <- rep(as.character(1:12), times = (nrow(point_data)/12))
    point_data[,"Date"] <- paste0(point_data$Year,"-",formatC(point_data$Month_counter, width=2,flag=0),"-01")
    point_data <- point_data[,!names(point_data) %in% c("Month_counter")]

    #set up spatial object
    df_coords <- point_data
    df_coords$CODE <- 1:nrow(df_coords)
    df_coords <- sf::st_as_sf(df_coords, coords = c("Lon","Lat"),crs=4326)
    if(env_vars=="both"){
      for(row in 1:nrow(point_data)){
        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("pet","ppt","tmax","tmin"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        pet <- climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")
        tmax <-climateR::extract_sites(r = tmpdata[3], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[4], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmin"] <- as.numeric(tmin[[1]][2])
        point_data[row, "tmax"] <- as.numeric(tmax[[1]][2])
        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                        ifelse(pet[[1]][2] == 0, NA,
                                                               ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                      ppt[[1]][2]/pet[[1]][2]))))

      }
    } else if (env_vars=="tmp"){
      for(row in 1:nrow(point_data)){

        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("tmax","tmin"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        tmax <-climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmin"] <- as.numeric(tmin[[1]][2])
        point_data[row, "tmax"] <- as.numeric(tmax[[1]][2])
      }
    } else if (env_vars=="aridity"){
      for(row in 1:nrow(point_data)){

        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]

        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("pet","ppt"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        pet <- climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                        ifelse(pet[[1]][2] == 0, NA,
                                                               ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                      ppt[[1]][2]/pet[[1]][2]))))

      }
    } else {
      stop("env_vars out of range")
    }
  } else if (date_format == "seasonal"){
    #create Seasonal column
    point_data$Hemisphere <- ifelse(point_data$Lat < 0, "south", "north")
    point_data <- point_data[rep(seq_len(nrow(point_data)), each = 3), ]
    point_data$Season_count <- rep(1:3, times = (nrow(point_data)/3))
    seasondf <- data.frame(Season = c("spring","spring","spring","summer","summer","summer","fall","fall","fall","winter","winter","winter",
                                      "spring","spring","spring","summer","summer","summer","fall","fall","fall","winter","winter","winter"),
                           Hemisphere = c("north","north","north","north","north","north","north","north","north","north","north","north",
                                          "south","south","south","south","south","south","south","south","south","south","south","south"),
                           Season_count = c(1,2,3,1,2,3,1,2,3,1,2,3,
                                            1,2,3,1,2,3,1,2,3,1,2,3),
                           Month_season = c(3,4,5,6,7,8,9,10,11,1,2,12,
                                            9,10,11,12,1,2,3,4,5,6,7,8))
    point_data <- dplyr::left_join(point_data,seasondf)
    point_data <- point_data[,!names(point_data) %in% c("Season_count")]
    point_data[,"Date"] <- paste0(point_data$Year,"-",formatC(point_data$Month_season, width=2,flag=0),"-01")
    #create spatial object
    df_coords <- point_data
    df_coords$CODE <- 1:nrow(df_coords)
    df_coords <- sf::st_as_sf(df_coords, coords = c("Lon","Lat"),crs=4326)
    if(env_vars=="both"){
      for(row in 1:nrow(point_data)){

        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("pet","ppt","tmax","tmin"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        pet <- climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")
        tmax <-climateR::extract_sites(r = tmpdata[3], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[4], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmin"] <- as.numeric(tmin[[1]][2])
        point_data[row, "tmax"] <- as.numeric(tmax[[1]][2])
        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                        ifelse(pet[[1]][2] == 0, NA,
                                                               ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                      ppt[[1]][2]/pet[[1]][2]))))

      }
    } else if (env_vars=="tmp"){
      for(row in 1:nrow(point_data)){

        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("tmax","tmin"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        tmax <-climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmin"] <- as.numeric(tmin[[1]][2])
        point_data[row, "tmax"] <- as.numeric(tmax[[1]][2])
      }
    } else if (env_vars=="aridity"){
      for(row in 1:nrow(point_data)){

        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]

        tmpdata <- climateR::getTerraClim(AOI= aoi ,
                                          varname = c("pet","ppt"),
                                          startDate = date_c,
                                          endDate = NULL,
                                          verbose=NULL,
                                          dryrun = FALSE)

        pet <- climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                        ifelse(pet[[1]][2] == 0, NA,
                                                               ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                      ppt[[1]][2]/pet[[1]][2]))))

      }
    } else {
      stop("env_vars out of range")
    }
  } else if (date_format == "daily"){
    point_data[,"Date"] <- paste0(point_data$Year,"-",
                                  formatC(point_data$Month, width=2,flag=0),
                                  "-",formatC(point_data$Day, width=2,flag=0))
    point_data[,"Date_pet"] <- paste0(point_data$Year,"-",
                                      formatC(point_data$Month, width=2,flag=0),
                                      "-01")

    #set up spatial object
    df_coords <- point_data
    df_coords$CODE <- 1:nrow(df_coords)
    df_coords <- sf::st_as_sf(df_coords, coords = c("Lon","Lat"),crs=4326)
    if(env_vars=="both"){
      for(row in 1:nrow(point_data)){

        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        date_pet <<- point_data$Date_pet[[row]]
        tmpdata <- climateR::getGridMET(AOI= aoi ,
                                        varname = c("pr","tmmx","tmmn"),
                                        startDate = date_c,
                                        endDate = NULL,
                                        verbose=NULL,
                                        dryrun = FALSE)
        pet_mn <- climateR::getTerraClim(AOI= aoi ,
                                         startDate = date_pet,
                                         endDate = NULL,
                                         verbose=NULL,
                                         dryrun = FALSE)
        pet <- climateR::extract_sites(r = pet_mn[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")
        tmax <-climateR::extract_sites(r = tmpdata[3], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmin"] <- as.numeric(tmin[[1]][2] - 273.15)
        point_data[row, "tmax"] <- as.numeric(tmax[[1]][2] - 273.15)
        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                        ifelse(pet[[1]][2] == 0, NA,
                                                               ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                      ppt[[1]][2]/pet[[1]][2]))))

      }
    } else if (env_vars=="tmp"){
      for(row in 1:nrow(point_data)){


        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        date_pet <<- point_data$Date_pet[[row]]
        tmpdata <- climateR::getGridMET(AOI= aoi ,
                                        varname = c("tmmx","tmmn"),
                                        startDate = date_c,
                                        endDate = NULL,
                                        verbose=NULL,
                                        dryrun = FALSE)

        tmax <-climateR::extract_sites(r = tmpdata[2], pts = df_coords[as.numeric(row),], id = "CODE")
        tmin <- climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "tmmn"] <- as.numeric(tmin[[1]][2] - 273.15)
        point_data[row, "tmmx"] <- as.numeric(tmax[[1]][2] - 273.15)
      }
    } else if (env_vars=="aridity"){
      for(row in 1:nrow(point_data)){


        aoi <<- AOI::aoi_get(list(point_data$Lat[[row]],point_data$Lon[[row]],5,5))
        date_c <<- point_data$Date[[row]]
        date_pet <<- point_data$Date_pet[[row]]
        tmpdata <- climateR::getGridMET(AOI= aoi ,
                                        varname = c("pr"),
                                        startDate = date_c,
                                        endDate = NULL,
                                        verbose=NULL,
                                        dryrun = FALSE)
        pet_mn <- climateR::getTerraClim(AOI= aoi ,
                                         varname = c("pet"),
                                         startDate = date_pet,
                                         endDate = NULL,
                                         verbose=NULL,
                                         dryrun = FALSE)
        pet <- climateR::extract_sites(r = pet_mn[1], pts = df_coords[as.numeric(row),], id = "CODE")
        ppt <-climateR::extract_sites(r = tmpdata[1], pts = df_coords[as.numeric(row),], id = "CODE")

        point_data[row, "pet"] <- pet[[1]][2]
        point_data[row, "ppt"] <- ppt[[1]][2]

        point_data[row, "aridity"] <- as.numeric(ifelse(is.na(pet[[1]][2]), NA,
                                                        ifelse(pet[[1]][2] == 0, NA,
                                                               ifelse(ppt[[1]][2]/pet[[1]][2] >100, 100,
                                                                      ppt[[1]][2]/pet[[1]][2]))))

      }
    } else {
      stop("env_vars out of range")
    }
  } else {
    stop("date_format out of range")
  }
  return(point_data)
}
