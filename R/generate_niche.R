#' Creates niche limits from user specified inputs
#'
#' `generate_niche()` takes user submitted species distribution data and to
#' generate realized niche limits following the methods used in Watson & Kerr
#' (2025) Scientific Data (https://doi.org/10.1038/s41597-025-06130-1) When
#' all parameters are left default. Users can additionally specify time periods
#' for calculating niche limits. Since historical WorldClim monthly weather
#' data is not available to directly call in R we use historical monthly TMax
#' and TMin from TerraClimate. This will result in slightly different niche
#' limit estimates compared to those contained within this package.
#'
#' @param sp_dist Input SpatVector, List of SpatVectors or Dataframe.
#' Input SpatVector(s) will be assumed to be from a single species. Can be
#' points or polygons. Dataframe must have 3 columns with the headings: "Group",
#'"lon", "lat". The "Group" column can include any numeric or character string
#'used to group observations together. "lon" must have longitude coordinates
#'and "lat" must have latitude coordinates.
#'
#'
#'@param niche_vars Input character string. Default NULL generates Thermal and
#'aridity index niche limits. Can be specified as "temp" which generates only
#'thermal niche limits, or "arid" which generates only Aridity index niche
#'limits. Temperature, precipitation and potential evapotransipration data are
#'extracted from Terra Climate v1.1 at a resolution of 2.5 minutes.
#'
#'@param precip Boolean (TRUE/FALSE). When set to true precipitation niche
#'limits are generated instead of aridity index. Default FALSE. PMin, PMax
#'refer to lower and upper precipitation limits.
#'
#'@param time_period Numeric Vector. Users can specify the years over which
#'thermal and aridity niche limits are calculated. Default NULL calculates
#'niche limits over the years 1961-1975. Time periods are limited to the years
#'1950-2025
#'
#'@param monthly Boolean (TRUE/FALSE). Default TRUE calculates niche limits on
#'a monthly basis. FALSE calculates yearly average niche limits.
#'
#'@param tmp_units Character String. Default thermal niche limits are calculated
#'in degrees Celsius. Options: "Fahrenheit", "Kelvin", convert thermal niche
#'limits to the specified temperature unit.
#'
#' @returns A a data frame.
#' @examples
#' df <- data.frame(Group = c(1,1,1,1,1,1),
#' lat = c(41.334287,41.334287,41.334287,41.334287,41.334287,41.334287),
#' lon = c(-77.942020,-77.942020,-77.942020,-77.942020,-77.942020,-77.942020),
#'
#' niche_lim <- generate_niche(df)
#'
#' @export
generate_niche <- function(sp_dist,
                           niche_vars = NULL,
                           precip = FALSE,
                           time_period = NULL,
                           monthly = TRUE,
                           tmp_units = NULL){
  ##reliant functions####
  climater_dap = function(id, args, verbose, dryrun, print.arg = FALSE){
    args$id = id
    if(print.arg){print(args)}
    args$catalog = do.call(climater_filter, args[names(args) %in% formalArgs(climater_filter)])
    args$verbose = verbose
    if(print.arg){print(args)}

    if(dryrun){
      args$verbose = TRUE
      args$varname = NULL
      if(print.arg){print(args)}
      x = do.call(dap_crop, args[names(args) %in% formalArgs(dap_crop)])
    } else {
      args$varname = NULL
      if(print.arg){print(args)}
      x = do.call(dap, args[names(args) %in% formalArgs(dap)])
    }

    x
  }
  climater_filter <- function(id = NULL,asset = NULL, AOI = NULL, startDate = NULL,endDate = NULL,
                              varname = NULL, model = NULL, scenario= NULL, ensemble = NULL) {

    variable <- description <- duration <- e <- s <- URL <- NULL

    if(is.null(id)){
      catalog = climateR::catalog
    } else {
      catalog = dplyr::filter(climateR::catalog, id == !!id)
    }

    if (nrow(catalog) == 0) {
      stop("no data to filter.", call. = FALSE)
    }

    if(!is.null(asset)){
      catalog = dplyr::filter(catalog, asset == !!asset)
    }

    if (nrow(catalog) == 0) {
      stop("no data to filter.", call. = FALSE)
    }

    if(inherits(AOI, "list")){
      AOI = AOI[[1]]
    }

    ### 1  ---- varname filter
    if(!is.null(varname)){
      u <- unique(catalog$variable)

      if (all(varname %in% catalog$variable)) {
        catalog <- dplyr::filter(catalog, varname %in% !!varname | variable %in% !!varname)
      } else {
        bad <- varname[!varname %in% u]

        m <- dplyr::distinct(dplyr::select(catalog, variable, description, units))

        stop("'", bad, "' not availiable varname for '", catalog$id[1], "'. Try: \n\t",
             paste(">", paste0(m$variable, " [", m$units, "] (", m$description, ")"),
                   collapse = "\n\t"
             ),
             call. = FALSE
        )
      }
    }

    if(!is.null(scenario)){
      if ("historical" %in% catalog$scenario) {
        scenario <- c("historical", scenario)
      }

      if(!is.null(scenario)){
        catalog <- dplyr::filter(catalog, scenario %in% !!scenario)
      }
    }


    ### 2 ---- model filter
    if (!is.null(model)) {
      u <- unique(catalog$model)

      if (is.numeric(model)) {
        if (length(u) >= model) {
          model <- sample(unique(catalog$model), model, replace = FALSE)
        } else {
          stop("There are only ", length(u), " unique models.", call. = FALSE)
        }
      }

      if (all(model %in% catalog$model)) {
        catalog <- dplyr::filter(catalog, model %in% !!model)
      } else {
        bad <- model[!model %in% u]

        m <- dplyr::distinct(dplyr::select(catalog, model, ensemble))

        stop("'", bad, "' not availiable model for '", catalog$id[1], "'. Try: \n\t",
             paste(">", paste0(m$model, " [", m$ensemble, "]"),
                   collapse = "\n\t"
             ),
             call. = FALSE
        )
      }
    }


    ### ---- AOI filter
    if(!is.null(AOI)){

      if(inherits(AOI, "sfc")){
        AOI = terra::vect(AOI)
      }

      gid = sapply(1:nrow(catalog), function(x) {
        suppressWarnings({
          tryCatch({
            sum(terra::is.related(terra::project(terra::ext(AOI), terra::crs(AOI), catalog$crs[x]),
                                  make_vect(catalog[x,]),
                                  "intersects")) > 0
          }, error = function(e) {
            FALSE
          })

        })
      })

      catalog = catalog[gid, ]

      if(nrow(catalog) == 0){
        stop("No data found in provided AOI.", call. = FALSE)
      }

    }

    ### 3 ---- date & scenario filter

    if(!is.null(startDate)){

      endDate = ifelse(is.null(endDate), as.character(startDate), as.character(endDate))

      tmp <- catalog %>%
        dplyr::mutate(
          s = do.call("rbind", strsplit(duration, "/"))[, 1],
          e = do.call("rbind", strsplit(duration, "/"))[, 2]
        ) %>%
        dplyr::mutate(e = ifelse(e == "..", as.character(Sys.Date()), e)) %>%
        dplyr::filter(as.Date(e) >= as.Date(startDate) & as.Date(s) <= as.Date(endDate))

      if(nrow(tmp) == 0){
        stop("Valid Date Range(s) includes: ",
             paste("\n\t>", unique(paste0( catalog$duration, " [", catalog$scenario, "]")),
                   collapse = ""
             ), call. = FALSE)
      } else {
        catalog = tmp
      }
    }


    ### 2 ---- ensemble filter
    # If ensemble is NULL set to 1
    if(is.null(ensemble)){ ensemble = 1 }
    # If data has ensembles set to TRUE
    eflag = any(!is.na(catalog$ensemble))

    if(eflag) {
      if (all(ensemble %in% catalog$ensemble) | !is.numeric(ensemble)) {
        catalog <- dplyr::filter(catalog, ensemble %in% !!ensemble)
      } else if (is.numeric(ensemble)) {

        cond = any(table(catalog$model, catalog$ensemble) > ensemble)

        catalog =  dplyr::slice_sample(catalog,
                                by = c('id', 'variable', 'model', "scenario"),
                                n = ensemble)

        if (ensemble == 1 & cond) {
          message(
            "Multiple ensembles available per model. Since `ensemble = NULL`, we default to:\n\t> ",
            paste0(catalog$model, " [", catalog$scenario, "] [", catalog$ensemble, "]",
                   collapse = "\n\t> ")
          )

        }
      } else {
        bad <- ensemble[!ensemble %in% catalog$ensemble]

        m <- dplyr::distinct(dplyr::select(catalog, model, ensemble))

        stop(
          "'",
          bad,
          "' not availiable ensemble for '",
          catalog$id[1],
          "'. Try: \n\t",
          paste(">",  m$ensemble, collapse = "\n\t"),
          call. = FALSE
        )
      }

      if(nrow(catalog) == 0 ){
        stop("Configuration not found.")
      }
    }




    catalog[!duplicated(dplyr::select(catalog, -URL)), ]
  }
  getTerraClim = function(AOI, varname = NULL,
                          startDate = NULL, endDate = NULL,
                          verbose = FALSE, ID = NULL, dryrun = FALSE){
    climater_dap("terraclim",
                 as.list(environment()),
                 verbose, dryrun)
  }
  ####required lists####

  months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

  ##checks of paramaters####
  #check sp_dist supplied correct
  if(isFALSE(is.data.frame(sp_dist)) &&
     isFALSE(class(sp_dist) == "SpatVector") &&
     isFALSE(is.list(sp_dist)) &&
     isFALSE(class(sp_dist[[1]]) =="SpatVector")){
    stop("Supplied sp_dist incompatible type")
  }
  #check sp_dist supplied correct
  if(isTRUE(is.data.frame(sp_dist)) &&
     isFALSE("Group" %in% colnames(sp_dist)) ||
     isTRUE(is.data.frame(sp_dist)) &&
     isFALSE("lon" %in% colnames(sp_dist)) ||
     isTRUE(is.data.frame(sp_dist)) &&
     isFALSE("lat" %in% colnames(sp_dist))){
    stop("Supplied sp_dist column names are misspecified")}
  #check sp_dist supplied correct
  if(isTRUE(is.data.frame(sp_dist)) &&
     min(sp_dist$lon, na.rm=T) < -180 ||
     isTRUE(is.data.frame(sp_dist)) &&
     max(sp_dist$lon, na.rm=T) > 180 ||
     isTRUE(is.data.frame(sp_dist)) &&
     min(sp_dist$lat, na.rm=T) < -90 ||
     isTRUE(is.data.frame(sp_dist)) &&
     max(sp_dist$lat, na.rm=T) > 90){
    stop("Some lon/lat out of possible range")}
  #check sp_dist list is spatvectors correct
  if(isTRUE(is.list(sp_dist)) &&
     isFALSE(is.data.frame(sp_dist)) &&
     isFALSE(class(sp_dist[[1]]) == "SpatVector")){
    stop("Supplied sp_dist list does not contain SpatVectors")
  }
  #check niche_vars supplied correct
  if(isFALSE(is.null(niche_vars)) &&
     isFALSE(niche_vars == "temp") &&
     isFALSE(niche_vars == "arid")){
    stop("Supplied niche_vars misspecified")
  }
  #check precip supplied correct
  if(!isFALSE(precip) &&
     !isTRUE(precip)){
    stop("precip not of type Boolean")
  }
  #check time_period supplied correct
  if(isFALSE(is.null(time_period)) &&
     isFALSE(is.vector(time_period, mode="numeric"))){
    stop("time period non numeric")
  }
  #check time_period supplied correct
  if(min(time_period < 1950) || max(time_period) > 2025){
    stop("time period out of range")}
  #check monthly supplied correct
  if(!isTRUE(monthly) &&
     !isFALSE(monthly)){
    stop("monthly is non Boolean")
  }
  #check tmp_units supplied correct
  if(isFALSE(is.null(tmp_units)) &&
     isFALSE(tmp_units == "Fahrenheit") &&
     isFALSE(tmp_units == "Kelvin")){
    stop("temp units misspecified")
  }

  ##main function####
  if(is.null(time_period)){
    time_period <- c(1961:1975)
  }

  ##Niche limit methods for a Dataframe
  if(isTRUE(is.data.frame(sp_dist))){

    grp_list <- unique(sp_dist$Group)
    grp_all_df <- data.frame(NULL)

    if (is.null(niche_vars) && isFALSE(precip)){

      for(g in grp_list){

        cur_grp <- sp_dist %>% dplyr::filter(Group == g)

        cur_grp_xy <- terra::vect(cur_grp, geom=c("lon", "lat"),
                           crs="+proj=longlat +datum=WGS84")

        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(cur_grp_xy, varname = c("tmax","tmin","ppt","pet"),
                                  startDate = startdate[[1]])

            df <- data.frame(PPT = suppressWarnings(terra::extract(tclim[[2]],cur_grp_xy)),
                             PET = suppressWarnings(terra::extract(tclim[[1]],cur_grp_xy)))%>%
              dplyr::select(2,4)

            colnames(df) <- c("PPT","PET")

            df <- df %>% dplyr::mutate(AI = PPT/PET)%>%
              dplyr::mutate(AI = ifelse(is.infinite(AI), NA, AI))%>%
              dplyr::filter(!is.na(AI))

            if(nrow(df) == 0){
              amax <- NA
              amin <- NA
            } else {
              lq <- quantile(df$AI,probs = c(0.25))[[1]]
              uq <- quantile(df$AI,probs = c(0.75))[[1]]
              upper_limt <- uq + 1.5*(uq-lq)

              if(max(df$AI) > upper_limt){
                amax <- upper_limt
              } else {
                amax <- max(df$AI)
              }
              amin <- min(df$AI)
            }

            tmax <- max(suppressWarnings(terra::extract(tclim[[3]], cur_grp_xy))[,2], na.rm=T)
            tmin <- min(suppressWarnings(terra::extract(tclim[[4]], cur_grp_xy))[,2], na.rm=T)


            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 Group = c(g),
                                 TMin = c(tmin),
                                 TMax = c(tmax),
                                 AMin = c(amin),
                                 AMax = c(amax))

            grp_all_df <- rbind(grp_all_df, cur_ar[1,])
          }
        }
      }
      grp_all_df <- grp_all_df %>%
        dplyr::select(-Year) %>% dplyr::group_by(Month, Group) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

    else if (is.null(niche_vars) && isTRUE(precip)){

      for(g in grp_list){

        cur_grp <- sp_dist %>% dplyr::filter(Group == g)

        cur_grp_xy <- terra::vect(cur_grp, geom=c("lon", "lat"),
                           crs="+proj=longlat +datum=WGS84")

        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(cur_grp_xy, varname = c("tmax","tmin","ppt"),
                                  startDate = startdate[[1]])

            df <- data.frame(PPT = suppressWarnings(terra::extract(tclim[[1]],cur_grp_xy)))%>%dplyr::select(2)

            colnames(df) <- c("PPT")

            pmin <- min(df$PPT, na.rm=T)
            pmax <- max(df$PPT, na.rm=T)

            tmax <- max(suppressWarnings(terra::extract(tclim[[2]], cur_grp_xy))[,2], na.rm=T)
            tmin <- min(suppressWarnings(terra::extract(tclim[[3]], cur_grp_xy))[,2], na.rm=T)


            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 Group = c(g),
                                 TMin = c(tmin),
                                 TMax = c(tmax),
                                 PMin = c(pmin),
                                 PMax = c(pmax))

            grp_all_df <- rbind(grp_all_df, cur_ar[1,])
          }
        }
      }
      grp_all_df <- grp_all_df %>%
        dplyr::select(-Year) %>% dplyr::group_by(Month, Group) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

    else if(niche_vars == "temp"){

      for(g in grp_list){

        cur_grp <- sp_dist %>% dplyr::filter(Group == g)

        cur_grp_xy <- terra::vect(cur_grp, geom=c("lon", "lat"),
                           crs="+proj=longlat +datum=WGS84")

        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(cur_grp_xy, varname = c("tmax","tmin"),
                                  startDate = startdate[[1]])

            tmax <- max(suppressWarnings(terra::extract(tclim[[1]], cur_grp_xy))[,2], na.rm=T)
            tmin <- min(suppressWarnings(terra::extract(tclim[[2]], cur_grp_xy))[,2], na.rm=T)


            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 Group = c(g),
                                 TMin = c(tmin),
                                 TMax = c(tmax))

            grp_all_df <- rbind(grp_all_df, cur_ar[1,])
          }
        }
      }
      grp_all_df <- grp_all_df %>%
        dplyr::select(-Year) %>% dplyr::group_by(Month, Group) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

    else if (niche_vars == "ar" && isFALSE(precip)){

      for(g in grp_list){

        cur_grp <- sp_dist %>% dplyr::filter(Group == g)

        cur_grp_xy <- terra::vect(cur_grp, geom=c("lon", "lat"),
                           crs="+proj=longlat +datum=WGS84")

        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(cur_grp_xy, varname = c("ppt","pet"),
                                  startDate = startdate[[1]])

            df <- data.frame(PPT = suppressWarnings(terra::extract(tclim[[2]],cur_grp_xy)),
                             PET = suppressWarnings(terra::extract(tclim[[1]],cur_grp_xy)))%>%
              dplyr::select(2,4)

            colnames(df) <- c("PPT","PET")

            df <- df %>% dplyr::mutate(AI = PPT/PET)%>%
              dplyr::mutate(AI = ifelse(is.infinite(AI), NA, AI))%>%
              dplyr::filter(!is.na(AI))

            if(nrow(df) == 0){
              amax <- NA
              amin <- NA
            } else {
              lq <- quantile(df$AI,probs = c(0.25))[[1]]
              uq <- quantile(df$AI,probs = c(0.75))[[1]]
              upper_limt <- uq + 1.5*(uq-lq)

              if(max(df$AI) > upper_limt){
                amax <- upper_limt
              } else {
                amax <- max(df$AI)
              }
              amin <- min(df$AI)
            }

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 Group = c(g),
                                 AMin = c(amin),
                                 AMax = c(amax))

            grp_all_df <- rbind(grp_all_df, cur_ar[1,])
          }
        }
      }
      grp_all_df <- grp_all_df %>%
        dplyr::select(-Year) %>% dplyr::group_by(Month, Group) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

    else if (niche_vars == "ar" && isTRUE(precip)){
      for(g in grp_list){

        cur_grp <- sp_dist %>% dplyr::filter(Group == g)

        cur_grp_xy <- terra::vect(cur_grp, geom=c("lon", "lat"),
                           crs="+proj=longlat +datum=WGS84")

        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(cur_grp_xy, varname = c("ppt"),
                                  startDate = startdate[[1]])

            df <- data.frame(PPT = suppressWarnings(terra::extract(tclim[[1]],cur_grp_xy)))%>%dplyr::select(2)

            colnames(df) <- c("PPT","PET")

            pmin <- min(df$PPT, na.rm=T)
            pmax <- max(df$PPT, na.rm=T)

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 Group = c(g),
                                 PMin = c(pmin),
                                 PMax = c(pmax))

            grp_all_df <- rbind(grp_all_df, cur_ar[1,])
          }
        }
      }
      grp_all_df <- grp_all_df %>%
        dplyr::select(-Year) %>% dplyr::group_by(Month, Group) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

  }

  ##Niche Limit methods for a list of SpatVectors
  else if (isTRUE(is.list(sp_dist)) &&
           isFALSE(is.data.frame(sp_dist)) &&
           isTRUE(class(sp_dist[[1]]) =="SpatVector")){

    if(isTRUE(is.null(niche_vars)) && isFALSE(precip)){

      all_df <- data.frame(NULL)
      for(sp in 1:length(sp_dist)){
        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(sp_dist[[sp]], varname = c("tmax","tmin","ppt","pet"),
                                  startDate = startdate[[1]])
            pet <- tclim[[1]]
            ppt <- tclim[[2]]
            tmax <- tclim[[3]]
            tmin <- tclim[[4]]

            terra::crs(pet) <- terra::crs(ppt) <- terra::crs(tmax) <- terra::crs(tmin) <- terra::crs(sp_dist[[sp]])

            pet <- terra::crop(terra::mask(pet,sp_dist[[sp]]),sp_dist[[sp]])
            ppt <- terra::crop(terra::mask(ppt,sp_dist[[sp]]),sp_dist[[sp]])
            tmax <- terra::crop(terra::mask(tmax,sp_dist[[sp]]),sp_dist[[sp]])
            tmin <- terra::crop(terra::mask(tmin,sp_dist[[sp]]),sp_dist[[sp]])

            ai <- ppt/pet
            ai <- terra::subst(ai,  Inf, NA)

            lq <- terra::global(ai, quantile, na.rm=T)[1,2]
            uq <- terra::global(ai, quantile, na.rm=T)[1,4]
            upper_limt <- uq[1] + 1.5*(uq[1]-lq[1])

            if(terra::global(ai, "max", na.rm=T)[1,1] > upper_limt[1]){
              AMax <- upper_limt[1]
            } else {
              AMax <- terra::global(ai, "max", na.rm=T)[1,1]
            }
            AMin <- terra::global(ai, "min", na.rm=T)[1,1]
            TMax <- terra::global(tmax, "max", na.rm=T)[1,1]
            TMin <- terra::global(tmin, "min", na.rm=T)[1,1]

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 TMin = c(TMin[1]),
                                 TMax = c(TMax[1]),
                                 AMin = c(AMin[1]),
                                 AMax = c(AMax[1]))

            all_df <- rbind(all_df, cur_ar[1,])
          }
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month, List_Position) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

    else if(is.null(niche_vars) && isTRUE(precip)){
      all_df <- data.frame(NULL)
      for(sp in 1:length(sp_dist)){
        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(sp_dist[[sp]], varname = c("tmax","tmin","ppt"),
                                  startDate = startdate[[1]])
            ppt <- tclim[[1]]
            tmax <- tclim[[2]]
            tmin <- tclim[[3]]

            terra::crs(ppt) <- terra::crs(tmax) <- terra::crs(tmin) <- terra::crs(sp_dist[[sp]])

            ppt <- terra::crop(terra::mask(ppt,sp_dist[[sp]]),sp_dist[[sp]])
            tmax <- terra::crop(terra::mask(tmax,sp_dist[[sp]]),sp_dist[[sp]])
            tmin <- terra::crop(terra::mask(tmin,sp_dist[[sp]]),sp_dist[[sp]])

            PMin <- terra::global(ppt, "min", na.rm=T)[1,1]
            PMax <- terra::global(ppt, "max", na.rm=T)[1,1]

            TMax <- terra::global(tmax, "max", na.rm=T)[1,1]
            TMin <- terra::global(tmin, "min", na.rm=T)[1,1]

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 TMin = c(TMin[1]),
                                 TMax = c(TMax[1]),
                                 PMin = c(PMin[1]),
                                 PMax = c(PMax[1]))

            all_df <- rbind(all_df, cur_ar[1,])
          }
        }

      }

      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month, List_Position) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

    else if(niche_vars == "temp"){

      all_df <- data.frame(NULL)
      for(sp in 1:length(sp_dist)){
        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(sp_dist[[sp]], varname = c("tmax","tmin"),
                                  startDate = startdate[[1]])

            tmax <- tclim[[1]]
            tmin <- tclim[[2]]

            terra::crs(tmax) <- terra::crs(tmin) <- terra::crs(sp_dist[[sp]])


            tmax <- terra::crop(terra::mask(tmax,sp_dist[[sp]]),sp_dist[[sp]])
            tmin <- terra::crop(terra::mask(tmin,sp_dist[[sp]]),sp_dist[[sp]])

            TMax <- terra::global(tmax, "max", na.rm=T)[1,1]
            TMin <- terra::global(tmin, "min", na.rm=T)[1,1]

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 TMin = c(TMin[1]),
                                 TMax = c(TMax[1]))

            all_df <- rbind(all_df, cur_ar[1,])
          }
        }
      }

      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month, List_Position) %>%
        dplyr::summarise_all(mean, na.rm=T)

    }

    else if (niche_vars == "arid" && isFALSE(precip)){
      all_df <- data.frame(NULL)
      for(sp in 1:length(sp_dist)){
        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(sp_dist[[sp]], varname = c("ppt","pet"),
                                  startDate = startdate[[1]])
            pet <- tclim[[1]]
            ppt <- tclim[[2]]


            terra::crs(pet) <- terra::crs(ppt) <- terra::crs(sp_dist[[sp]])

            pet <- terra::crop(terra::mask(pet,sp_dist[[sp]]),sp_dist[[sp]])
            ppt <- terra::crop(terra::mask(ppt,sp_dist[[sp]]),sp_dist[[sp]])
            ai <- ppt/pet
            ai <- terra::subst(ai,  Inf, NA)

            lq <- terra::global(ai, quantile, na.rm=T)[1,2]
            uq <- terra::global(ai, quantile, na.rm=T)[1,4]
            upper_limt <- uq[1] + 1.5*(uq[1]-lq[1])

            if(terra::global(ai, "max", na.rm=T)[1,1] > upper_limt[1]){
              AMax <- upper_limt[1]
            } else {
              AMax <- terra::global(ai, "max", na.rm=T)[1,1]
            }
            AMin <- terra::global(ai, "min", na.rm=T)[1,1]

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 AMin = c(AMin[1]),
                                 AMax = c(AMax[1]))

            all_df <- rbind(all_df, cur_ar[1,])
          }
        }
      }

      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month,List_Position) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

    else if (niche_vars == "arid" && isTRUE(precip)){
      all_df <- data.frame(NULL)
      for(sp in 1:length(sp_dist)){
        for(yr in time_period){
          for(m in months){

            startdate <- list(paste0(yr,"-",m,"-01"))

            tclim <- getTerraClim(sp_dist[[sp]], varname = c("ppt"),
                                  startDate = startdate[[1]])
            ppt <- tclim[[1]]


            terra::crs(ppt) <- terra::crs(sp_dist[[sp]])

            pet <- terra::crop(terra::mask(pet,sp_dist[[sp]]),sp_dist[[sp]])

            PMin <- terra::global(ppt, "min", na.rm=T)[1,1]
            PMax <- terra::global(ppt, "max", na.rm=T)[1,1]

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 PMin = c(PMin[1]),
                                 PMax = c(PMax[1]))

            all_df <- rbind(all_df, cur_ar[1,])
          }
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month,List_Position) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }

  }

  ##Niche Limit methods for a signle of SpatVector
  else if(isTRUE(class(sp_dist)[1] =="SpatVector")){

    if(isTRUE(is.null(niche_vars)) && isFALSE(precip)){

      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- list(paste0(yr,"-",m,"-01"))

          tclim <- getTerraClim(sp_dist, varname = c("tmax","tmin","ppt","pet"),
                                startDate = startdate[[1]])
          pet <- tclim[[1]]
          ppt <- tclim[[2]]
          tmax <- tclim[[3]]
          tmin <- tclim[[4]]

          terra::crs(pet) <- terra::crs(ppt) <- terra::crs(tmax) <- terra::crs(tmin) <- terra::crs(sp_dist)

          pet <- terra::crop(terra::mask(pet,sp_dist),sp_dist)
          ppt <- terra::crop(terra::mask(ppt,sp_dist),sp_dist)
          tmax <- terra::crop(terra::mask(tmax,sp_dist),sp_dist)
          tmin <- terra::crop(terra::mask(tmin,sp_dist),sp_dist)
          ai <- ppt/pet
          ai <- terra::subst(ai,  Inf, NA)

          lq <- terra::global(ai, quantile, na.rm=T)[1,2]
          uq <- terra::global(ai, quantile, na.rm=T)[1,4]
          upper_limt <- uq[1] + 1.5*(uq[1]-lq[1])

          if(terra::global(ai, "max", na.rm=T)[1,1] > upper_limt[1]){
            AMax <- upper_limt[1]
          } else {
            AMax <- terra::global(ai, "max", na.rm=T)[1,1]
          }
          AMin <- terra::global(ai, "min", na.rm=T)[1,1]
          TMax <- terra::global(tmax, "max", na.rm=T)[1,1]
          TMin <- terra::global(tmin, "min", na.rm=T)[1,1]

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               TMin = c(TMin[1]),
                               TMax = c(TMax[1]),
                               AMin = c(AMin[1]),
                               AMax = c(AMax[1]))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)
    }

    else if(is.null(niche_vars) && isTRUE(precip)){
      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- list(paste0(yr,"-",m,"-01"))

          tclim <- getTerraClim(sp_dist, varname = c("tmax","tmin","ppt"),
                                startDate = startdate[[1]])
          ppt <- tclim[[1]]
          tmax <- tclim[[2]]
          tmin <- tclim[[3]]

          terra::crs(ppt) <- terra::crs(tmax) <- terra::crs(tmin) <- terra::crs(sp_dist)

          ppt <- terra::crop(terra::mask(ppt,sp_dist),sp_dist)
          tmax <- terra::crop(terra::mask(tmax,sp_dist),sp_dist)
          tmin <- terra::crop(terra::mask(tmin,sp_dist),sp_dist)

          PMin <- terra::global(ppt, "min", na.rm=T)[1,1]
          PMax <- terra::global(ppt, "max", na.rm=T)[1,1]

          TMax <- terra::global(tmax, "max", na.rm=T)[1,1]
          TMin <- terra::global(tmin, "min", na.rm=T)[1,1]

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               TMin = c(TMin[1]),
                               TMax = c(TMax[1]),
                               PMin = c(PMin[1]),
                               PMax = c(PMax[1]))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)
    }

    else if(niche_vars == "temp"){

      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- list(paste0(yr,"-",m,"-01"))

          tclim <- getTerraClim(sp_dist, varname = c("tmax","tmin"),
                                startDate = startdate[[1]])

          tmax <- tclim[[1]]
          tmin <- tclim[[2]]

          terra::crs(tmax) <- terra::crs(tmin) <- terra::crs(sp_dist)


          tmax <- terra::crop(terra::mask(tmax,sp_dist),sp_dist)
          tmin <- terra::crop(terra::mask(tmin,sp_dist),sp_dist)

          TMax <- terra::global(tmax, "max", na.rm=T)[1,1]
          TMin <- terra::global(tmin, "min", na.rm=T)[1,1]

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               TMin = c(TMin[1]),
                               TMax = c(TMax[1]))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)

    }

    else if (niche_vars == "arid" && isFALSE(precip)){
      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- list(paste0(yr,"-",m,"-01"))

          tclim <- getTerraClim(sp_dist, varname = c("ppt","pet"),
                                startDate = startdate[[1]])
          pet <- tclim[[1]]
          ppt <- tclim[[2]]


          terra::crs(pet) <- terra::crs(ppt) <- terra::crs(sp_dist)

          pet <- terra::crop(terra::mask(pet,sp_dist),sp_dist)
          ppt <- terra::crop(terra::mask(ppt,sp_dist),sp_dist)
          ai <- ppt/pet
          ai <- terra::subst(ai,  Inf, NA)

          lq <- terra::global(ai, quantile, na.rm=T)[1,2]
          uq <- terra::global(ai, quantile, na.rm=T)[1,4]
          upper_limt <- uq[1] + 1.5*(uq[1]-lq[1])

          if(terra::global(ai, "max", na.rm=T)[1,1] > upper_limt[1]){
            AMax <- upper_limt[1]
          } else {
            AMax <- terra::global(ai, "max", na.rm=T)[1,1]
          }
          AMin <- terra::global(ai, "min", na.rm=T)[1,1]

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               AMin = c(AMin[1]),
                               AMax = c(AMax[1]))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)
    }

    else if (niche_vars == "arid" && isTRUE(precip)){
      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- list(paste0(yr,"-",m,"-01"))

          tclim <- getTerraClim(sp_dist, varname = c("ppt"),
                                startDate = startdate[[1]])
          ppt <- tclim[[1]]


          terra::crs(ppt) <- terra::crs(sp_dist)

          ppt <- terra::crop(mask(ppt,sp_dist),sp_dist)

          PMin <- terra::global(ppt, "min", na.rm=T)[1,1]
          PMax <- terra::global(ppt, "max", na.rm=T)[1,1]

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               PMin = c(PMin[1]),
                               PMax = c(PMax[1]))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)
    }
  }

  #sumarise if monthly is false, otherwise return data frame##
  if(isTRUE(class(sp_dist)=="SpatVector")){
    if(isFALSE(monthly)){
      all_df <- all_df %>%
        dplyr::select(-Month) %>%
        dplyr::summarise_all(mean, na.rm=T)
    }
    if(isTRUE(is.null(tmp_units))){
      all_df <- all_df
    }else if (isTRUE(tmp_units == "Fahrenheit")){
      all_df <- all_df %>% mutate(TMin = (TMin*(9/5))+32,
                                  TMax = (TMax*(9/5))+32)
    } else if (isTRUE(tmp_units == "Kelvin")){
      all_df <- all_df %>% mutate(TMin = TMin + 273.15,
                                  TMax = TMax + 273.15)
    }
    return(all_df)
  }

  else if (isTRUE(is.data.frame(sp_dist))){
    if(isFALSE(monthly)){
      grp_all_df <- grp_all_df[,-1]
      grp_all_df <- grp_all_df%>%
        dplyr::group_by(Group)%>%
        dplyr::summarise_all(mean, na.rm=T)
    }
    if(isTRUE(is.null(tmp_units))){
      grp_all_df <- grp_all_df
    }else if (isTRUE(tmp_units == "Fahrenheit")){
      grp_all_df <- grp_all_df %>% mutate(TMin = (TMin*(9/5))+32,
                                  TMax = (TMax*(9/5))+32)
    } else if (isTRUE(tmp_units == "Kelvin")){
      grp_all_df <- grp_all_df %>% mutate(TMin = TMin + 273.15,
                                  TMax = TMax + 273.15)
    }
    return(grp_all_df)
  }

  else if (isTRUE(is.list(sp_dist)) &&
           isFALSE(is.data.frame(sp_dist)) &&
           isTRUE(class(sp_dist[[1]]) =="SpatVector")){
    if(isFALSE(monthly)){
      all_df <- all_df[,-1]
      all_df <- all_df %>%
        dplyr::group_by(List_Position)%>%
        dplyr::summarise_all(mean,na.rm=T)
    }
    if(isTRUE(is.null(tmp_units))){
      all_df <- all_df
    }else if (isTRUE(tmp_units == "Fahrenheit")){
      all_df <- all_df %>% mutate(TMin = (TMin*(9/5))+32,
                                  TMax = (TMax*(9/5))+32)
    } else if (isTRUE(tmp_units == "Kelvin")){
      all_df <- all_df %>% mutate(TMin = TMin + 273.15,
                                  TMax = TMax + 273.15)
    }
    return(all_df)
  }
}

