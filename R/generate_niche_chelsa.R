#' Creates niche limits from user specified inputs
#'
#' `generate_niche_chelsa()` takes user submitted species distribution data and to
#' generate realized niche limits following the methods used in Watson & Kerr
#' (2025) Scientific Data (https://doi.org/10.1038/s41597-025-06130-1) When
#' all parameters are left default. One important caveat is that the CHELSA
#' dataset only goes back as far as 1980. Therefore users must specify time
#' periods of interst for calculating niche limits. This will result in
#' different niche limit estimates compared to those contained within this
#' package since the underlying climate dataset is different. Since the spatial
#' resolution is 1km the extracted raster files can be large, making this niche
#' limit generation process slower than the generate_niche function.
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
#'extracted from CHELSA-monthly v2.1 at a resolution of 1 square km.
#'
#'@param precip Boolean (TRUE/FALSE). When set to true precipitation niche
#'limits are generated instead of aridity index. Default FALSE. PMin, PMax
#'refer to lower and upper precipitation limits.
#'
#'@param time_period Numeric Vector. Users can specify the years over which
#'thermal and aridity niche limits are calculated. Default NULL calculates
#'niche limits over the years 1980-1990. Time periods are limited to the years
#'1980-2021
#'
#'@param monthly Boolean (TRUE/FALSE). Default TRUE calculates niche limits on
#'a monthly basis. FALSE calculates yearly average niche limits.
#'
#'@param tmp_units Character String. Default thermal niche limits are calculated
#'in degrees Celsius. Options: "Fahrenheit", "Celsius", convert thermal niche
#'limits to the specified temperature unit.
#'
#' @returns A a data frame.
#' @examples
#' df <- data.frame(Group = c(1,1,1,1,1,1),
#' lat = c(41.334287,41.334287,41.334287,41.334287,41.334287,41.334287),
#' lon = c(-77.942020,-77.942020,-77.942020,-77.942020,-77.942020,-77.942020),
#'
#' niche_lim <- generate_niche_chelsa(df)
#'
#' @export
generate_niche_chelsa <- function(sp_dist,
                           niche_vars = NULL,
                           precip = FALSE,
                           time_period = NULL,
                           monthly = TRUE,
                           tmp_units = NULL){
  ##reliant functions####
  library(Rchelsa)
  getChelsa <- function(var, date = NULL, coords = NULL, extent = NULL,
                        startdate = NULL, enddate = NULL,
                        protocol = "vsicurl", verbose = FALSE,
                        dataset,
                        ssp = NULL, forcing = NULL) {

    if (missing(dataset)) stop("Please supply `dataset` (e.g., 'CHELSA-climatologies').")

    ds <- tolower(dataset)
    ds <- gsub("[[:space:]_]+", "-", ds)

    if (!is.null(startdate) && is.null(enddate)) stop("If startdate is provided, enddate is required.")
    if (is.null(startdate) && !is.null(enddate)) stop("If enddate is provided, startdate is required.")
    if (!is.null(coords) && !is.null(extent))   stop("Using extent and coords at the same time is not supported.")

    seq_mode <- switch(ds,
                       "chelsa-daily"                   = "daily",
                       "chelsa-monthly"                 = "monthly",
                       "chelsa-climatologies"           = "clim_single",
                       "chelsa-bioclim"                 = "period_single",
                       "chelsa-trace21k-centennial"     = "trace_bc",
                       "chelsa-trace21k-bioclim"        = "trace_years",
                       "chelsacanaryclim-climatologies" = "period_single",
                       stop("Unsupported dataset: ", dataset)
    )

    # single timestep
    if (!is.null(date) && is.null(startdate) && is.null(enddate)) {
      fileurl <- createURL(var, date, dataset = dataset, ssp = ssp, forcing = forcing)
      rurl <- paste0("/", protocol, "/", fileurl)
      r <- terra::rast(rurl)
      dseq <- list(date)
      if (isTRUE(verbose)) cat(printMetadata(fileurl))
    }

    # range mode
    if (!is.null(startdate) && !is.null(enddate)) {
      if (seq_mode %in% c("clim_single","period_single"))
        stop("This dataset expects a single timestep/period, not a range.")

      if (seq_mode == "daily") {
        if (!inherits(startdate, "Date") || !inherits(enddate, "Date"))
          stop("For CHELSA-daily, start/end must be Date.")
        dseq <- as.list(seq.Date(startdate, enddate, by = "day"))
      } else if (seq_mode == "monthly") {
        if (!inherits(startdate, "Date") || !inherits(enddate, "Date"))
          stop("For CHELSA-monthly, start/end must be Date.")
        dseq <- as.list(seq.Date(startdate, enddate, by = "month"))
      } else if (seq_mode == "trace_bc") {
        dseq <- getBCdateSeq(startdate, enddate)  # list of c(month, year)
      } else if (seq_mode == "trace_years") {
        get_year <- function(x){
          if (inherits(x, "Date")) as.integer(format(x, "%Y"))
          else if (is.numeric(x))  as.integer(x[length(x)])
          else if (is.character(x) && grepl("^-?\\d+$", x)) as.integer(x)
          else stop("Provide integer years for TraCE21k-bioclim.")
        }
        y0 <- get_year(startdate); y1 <- get_year(enddate)
        step <- if (y0 <= y1) 1L else -1L
        dseq <- as.list(seq(y0, y1, by = step))
      }

      rl <- character(length(dseq))
      for (i in seq_along(dseq)) {
        this_date <- if (seq_mode == "trace_bc") unname(dseq[[i]]) else dseq[[i]]
        fileurl <- createURL(var, this_date, dataset = dataset, ssp = ssp, forcing = forcing)
        rl[i] <- paste0("/", protocol, "/", fileurl)
        if (isTRUE(verbose) && i == 1) cat(printMetadata(fileurl))
      }
      r <- terra::rast(rl)
    }

    # extraction / cropping
    if (!is.null(coords)) {
      label_time <- function(dseq){
        if (ds == "chelsa-trace21k-bioclim") {
          sapply(dseq, function(y) sprintf("%+05d-01-01", as.integer(y)))
        } else if (ds == "chelsa-trace21k-centennial") {
          sapply(dseq, function(x) sprintf("%+05d-%02d-01", as.integer(x[2]), as.integer(x[1])))
        } else {
          unlist(dseq)
        }
      }
      if (nrow(coords) == 1) {
        r1 <- terra::extract(r, coords)
        out <- data.frame(time = label_time(dseq),
                          value = as.vector(unlist(r1))[-1])
        names(out)[2] <- paste0(var, "_lon:", coords[1,1], "_lat:", coords[1,2])
        r <- out
      } else {
        r1 <- terra::extract(r, coords)
        rx <- data.frame(time = label_time(dseq))
        for (i in seq_len(nrow(r1))) {
          rx <- cbind(rx, as.vector(unlist(r1[i,]))[-1])
          names(rx)[i+1] <- paste0(var, "_lon:", coords[i,1], "_lat:", coords[i,2])
        }
        r <- rx
      }
    }

    if (!is.null(extent)) {
      extcoords <- data.frame(lon = c(extent[1], extent[2]),
                              lat = c(extent[3], extent[4]))
      r <- terra::crop(r, extcoords)
    }

    r
  }

  # dataset-first; supports SSP/forcing subfolders for CHELSA-bioclim & -climatologies
  createURL <- function(var, date, dataset, extent = "global",
                        ssp = NULL, forcing = NULL,
                        forcing_variant = 'r1i1p1f1_w5e5') {
    canon_ds <- function(x){ x <- tolower(trimws(x)); gsub("[^a-z0-9]+","-",x) }
    ds     <- canon_ds(dataset)
    var    <- as.character(var)
    extent <- tolower(extent)

    # normalize inputs
    ssp    <- if (!is.null(ssp) && nzchar(ssp)) tolower(ssp) else NULL
    forcing_in <- if (!is.null(forcing) && nzchar(forcing)) forcing else NULL
    forcing_file <- if (!is.null(forcing_in)) tolower(forcing_in) else NULL

    # keep underscores/hyphens in variant; lower-case
    variant_file <- if (!is.null(forcing_variant) && nzchar(forcing_variant)) {
      tolower(gsub("[^a-z0-9_\\-]+", "", forcing_variant))
    } else NULL

    # folder wants title-cased codes; UKESM gets "UKESM1-0-LL"
    forcing_folder <- if (!is.null(forcing_in)) {
      ff <- toupper(forcing_in)
      ff <- sub("^UKESM", "UKESM", ff)
      ff
    } else NULL

    fmt_year <- function(y){ y <- as.integer(y); if (y < 0) sprintf("-%03d", abs(y)) else sprintf("%04d", y) }
    parse_month_year <- function(x){
      if (is.numeric(x) && length(x)==2) list(mm=sprintf("%02d",x[1]), yy=fmt_year(x[2]))
      else if (inherits(x,"Date"))       list(mm=format(x,"%m"), yy=format(x,"%Y"))
      else stop("date must be a Date or numeric c(month, year)")
    }
    parse_period <- function(x){
      if (is.character(x) && grepl("^\\d{4}-\\d{4}$", x)) x
      else if (is.numeric(x) && length(x)==2) sprintf("%04d-%04d", as.integer(x[1]), as.integer(x[2]))
      else stop("Use period 'YYYY-YYYY' or numeric c(start,end).")
    }
    period_from_year <- function(y){
      y <- as.integer(y)
      if (y>=1981 && y<=2010) return("1981-2010")
      if (y>=2011 && y<=2040) return("2011-2040")
      if (y>=2041 && y<=2070) return("2041-2070")
      if (y>=2071 && y<=2100) return("2071-2100")
      stop("Year ", y, " is outside supported climatology periods.")
    }

    # ---------- CHELSA v2.x (chelsa02) ----------
    if (ds %in% c("chelsa-daily","chelsa-monthly","chelsa-climatologies","chelsa-bioclim")) {
      host  <- "https://os.unil.cloud.switch.ch/chelsa02/"
      model <- "chelsa"

      if (ds == "chelsa-daily") {
        if (!inherits(date,"Date")) stop("For CHELSA-daily, 'date' must be a Date.")
        base <- paste0(host, model, "/", extent, "/daily/", var, "/", format(date,"%Y"), "/")
        file <- sprintf("CHELSA_%s_%s_%s_%s_V.2.1.tif", var, format(date,"%d"), format(date,"%m"), format(date,"%Y"))
        return(paste0(base, file))
      }

      if (ds == "chelsa-monthly") {
        my <- parse_month_year(date)
        base <- paste0(host, model, "/", extent, "/monthly/", var, "/", my$yy, "/")
        file <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif", var, my$mm, my$yy)
        return(paste0(base, file))
      }

      if (ds == "chelsa-climatologies") {
        # need month + period; supports projections via ssp/forcing
        mm <- NULL; period <- NULL
        if (inherits(date,"Date"))                         { mm <- format(date,"%m"); period <- period_from_year(format(date,"%Y")) }
        else if (is.numeric(date) && length(date)==2)     { mm <- sprintf("%02d",date[1]); period <- period_from_year(date[2]) }
        else if (is.numeric(date) && length(date)==3)     { mm <- sprintf("%02d",date[1]); period <- sprintf("%04d-%04d",date[2],date[3]) }
        else stop("For CHELSA-climatologies, use Date or numeric c(month,year) or c(month,start,end).")

        base <- paste0(host, model, "/", extent, "/climatologies/", var, "/", period, "/")
        if (!is.null(ssp) && !is.null(forcing_folder)) base <- paste0(base, forcing_folder, "/", ssp, "/")

        if (!is.null(ssp) && !is.null(forcing_file)) {
          prefix <- forcing_file
          if (!is.null(variant_file)) prefix <- paste0(prefix, "_", variant_file)  # insert here
          file <- sprintf("CHELSA_%s_%s_%s_%s_%s_V.2.1.tif",
                          prefix, ssp, var, mm, period)
        } else {
          file <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif", var, mm, period)
        }
        return(paste0(base, file))
      }

      if (ds == "chelsa-bioclim") {
        # period; projections supported via ssp/forcing (no month in filename)
        period <- parse_period(date)
        base   <- paste0(host, model, "/", extent, "/bioclim/", var, "/", period, "/")
        if (!is.null(ssp) && !is.null(forcing_folder)) base <- paste0(base, forcing_folder, "/", ssp, "/")

        if (!is.null(ssp) && !is.null(forcing_file)) {
          file <- sprintf("CHELSA_%s_%s_%s_%s_V.2.1.tif",
                          forcing_file, ssp, var, period)
        } else {
          file <- sprintf("CHELSA_%s_%s_V.2.1.tif", var, period)
        }
        return(paste0(base, file))
      }
    }

    # ---------- TraCE21k (unchanged) ----------
    if (ds %in% c("chelsa-trace21k-centennial","chelsa-trace21k-bioclim")) {
      host  <- "https://os.zhdk.cloud.switch.ch/chelsa01/"
      model <- "chelsa_trace21k"
      group <- if (ds == "chelsa-trace21k-bioclim") "bioclim" else "centennial"
      base  <- paste0(host, model, "/", extent, "/", group, "/", var, "/")

      if (group == "bioclim") {
        yy <- if (inherits(date,"Date")) format(date,"%Y")
        else if (is.numeric(date))  fmt_year(date[[length(date)]])
        else if (is.character(date) && grepl("^-?\\d+$", date)) fmt_year(as.integer(date))
        else stop("TraCE21k-bioclim needs a single year (Date, numeric, or character).")
        file <- sprintf("CHELSA_TraCE21k_%s_%s_V.1.0.tif", var, yy)
        return(paste0(base, file))
      } else {
        if (var %in% c("orog","glz","scd","swe")) {
          yy <- if (inherits(date,"Date")) format(date,"%Y")
          else if (is.numeric(date))  fmt_year(date[[length(date)]])
          else if (is.character(date) && grepl("^-?\\d+$", date)) fmt_year(as.integer(date))
          file <- sprintf("CHELSA_TraCE21k_%s_%s_V.1.0.tif", var, yy)
        } else {
          my <- parse_month_year(date)
          file <- sprintf("CHELSA_TraCE21k_%s_%s_%s_V.1.0.tif", var, my$mm, my$yy)
        }
        return(paste0(base, file))
      }
    }

    # ---------- CanaryClim (fixed: now returns file URL) ----------
    if (ds == "chelsacanaryclim-climatologies") {
      host <- "https://os.zhdk.cloud.switch.ch/chelsa01/"

      # date parsing:
      # - monthly vars (tas, tasmin, tasmax, pr): use c(mm,start,end) or a Date
      # - bioclim vars (bio1..bio19): use "YYYY-YYYY" or c(start,end)
      is_bioclim <- grepl("^bio\\d{1,2}$", tolower(var))

      mm <- NULL
      period <- NULL

      if (is_bioclim) {
        if (is.null(date)) {
          period <- "1979-2013"
        } else if (is.character(date) && grepl("^\\d{4}-\\d{4}$", date)) {
          period <- date
        } else if (is.numeric(date) && length(date)==2) {
          period <- sprintf("%04d-%04d", as.integer(date[1]), as.integer(date[2]))
        } else if (inherits(date,"Date")) {
          period <- "1979-2013"
        } else stop("For CanaryClim bioclim, use period 'YYYY-YYYY' or numeric c(start,end).")
      } else {
        # monthly variable
        if (inherits(date,"Date")) {
          mm <- format(date, "%m")
          yy <- as.integer(format(date, "%Y"))
          period <- if (yy <= 2013) "1979-2013" else "2071-2100"
        } else if (is.numeric(date) && length(date)==3) {
          mm <- sprintf("%02d", as.integer(date[1]))
          period <- sprintf("%04d-%04d", as.integer(date[2]), as.integer(date[3]))
        } else {
          stop("For CanaryClim monthly vars (tas/tasmin/tasmax/pr) use numeric c(month,start,end) or a Date.")
        }
      }

      base <- paste0(host, "chelsa_canaryclim/canaries/climatologies/", var, "/", period, "/")

      # Filenames used on the server (common patterns). We pick one.
      # If your mirror uses a slightly different stem, add it here.
      filename <- if (is_bioclim) {
        # Bioclim has no month in filename
        # Example pattern: CanaryClim_bio1_1979-2013_V1.0.tif
        sprintf("CHELSACanaryClim_%s_%s_V.1.0.tif", var, period)
      } else {
        # Monthly has month in filename
        # Example pattern: CanaryClim_tas_01_1979-2013_V1.0.tif
        sprintf("CHELSACanaryClim_%s_%s_%s_V.1.0.tif", var, mm, period)
      }

      return(paste0(base, filename))
    }

    stop("Unsupported dataset: ", dataset)
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
  if(min(time_period < 1980) || max(time_period) > 2021){
    stop("time period out of range")}
  #check monthly supplied correct
  if(!isTRUE(monthly) &&
     !isFALSE(monthly)){
    stop("monthly is non Boolean")
  }
  #check tmp_units supplied correct
  if(isFALSE(is.null(tmp_units)) &&
     isFALSE(tmp_units == "Fahrenheit") &&
     isFALSE(tmp_units == "Celsius")){
    stop("temp units misspecified")
  }

  ##main function####
  if(is.null(time_period)){
    time_period <- c(1980:1990)
  }

  ##Niche limit methods for a Dataframe
  if(isTRUE(is.data.frame(sp_dist))){

    grp_list <- unique(sp_dist$Group)
    grp_all_df <- data.frame(NULL)

    if (is.null(niche_vars) && isFALSE(precip)){

      for(g in grp_list){

        cur_grp <- sp_dist %>% dplyr::filter(Group == g)

        cur_grp_xy <- cur_grp[,c("lon","lat")]

        for(yr in time_period){
          for(m in months){

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #TMAX
            tasmax <- (t(getChelsa("tasmax", coords = cur_grp_xy,
                               startdate = startdate, enddate = startdate,
                               dataset = "chelsa-monthly")))[-1,]

            tmax <- max(tasmax, na.rm=T)
            rm(tasmax)
            gc()
            #TMIN
            tasmin <- (t(getChelsa("tasmin", coords = cur_grp_xy,
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly")))[-1,]

            tmin <- min(tasmin, na.rm=T)
            rm(tasmin)
            gc()
            #AI
            pet <- t(getChelsa("pet", coords = cur_grp_xy,
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly"))
            pr <- t(getChelsa("pr", coords = cur_grp_xy,
                            startdate = startdate, enddate = startdate,
                            dataset = "chelsa-monthly"))


            ai <- data.frame(AI = pr/pet)
            ai <- ai[-1,]
            ai[is.infinite(ai)] <- NA

            if(length(na.omit(ai))==0){
              amax <- NA
              amin <- NA
            }else {
              lq <- quantile(ai, probs = c(0.25),na.rm=T)[[1]]
              uq <- quantile(ai, probs = c(0.75),na.rm=T)[[1]]
              upper_limt <- uq + 1.5*(uq-lq)

              if(max(ai, na.rm=T) > upper_limt){
                amax <- upper_limt
              } else {
                amax <- max(ai, na.rm=T)
              }
              amin <- min(ai, na.rm=T)
            }

            rm(pet, pr, ai)
            gc()

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

        cur_grp_xy <- cur_grp[,c("lon","lat")]

        for(yr in time_period){
          for(m in months){

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #TMAX
            tasmax <- (t(getChelsa("tasmax", coords = cur_grp_xy,
                                   startdate = startdate, enddate = startdate,
                                   dataset = "chelsa-monthly")))[-1,]

            tmax <- max(tasmax, na.rm=T)
            rm(tasmax)
            gc()
            #TMIN
            tasmin <- (t(getChelsa("tasmin", coords = cur_grp_xy,
                                   startdate = startdate, enddate = startdate,
                                   dataset = "chelsa-monthly")))[-1,]

            tmin <- min(tasmin, na.rm=T)
            rm(tasmin)
            gc()
            #AI

            pr <- (t(getChelsa("pr", coords =cur_grp_xy,
                            startdate = startdate, enddate = startdate,
                            dataset = "chelsa-monthly")))[-1,]

            pmin <- min(pr, na.rm=T)
            pmax <- max(pr, na.rm=T)

            rm(pr)
            gc()

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

        cur_grp_xy <- cur_grp[,c("lon","lat")]

        for(yr in time_period){
          for(m in months){

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #TMAX
            tasmax <- (t(getChelsa("tasmax", coords = cur_grp_xy,
                                   startdate = startdate, enddate = startdate,
                                   dataset = "chelsa-monthly")))[-1,]

            tmax <- max(tasmax, na.rm=T)
            rm(tasmax)
            gc()
            #TMIN
            tasmin <- (t(getChelsa("tasmin", coords = cur_grp_xy,
                                   startdate = startdate, enddate = startdate,
                                   dataset = "chelsa-monthly")))[-1,]

            tmin <- min(tasmin, na.rm=T)
            rm(tasmin)
            gc()

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

        cur_grp_xy <- cur_grp[,c("lon","lat")]

        for(yr in time_period){
          for(m in months){

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #AI
            pet <- t(getChelsa("pet", coords = cur_grp_xy,
                               startdate = startdate, enddate = startdate,
                               dataset = "chelsa-monthly"))
            pr <- t(getChelsa("pr", coords = cur_grp_xy,
                              startdate = startdate, enddate = startdate,
                              dataset = "chelsa-monthly"))


            ai <- data.frame(AI = pr/pet)
            ai <- ai[-1,]
            ai[is.infinite(ai)] <- NA

            if(length(na.omit(ai))==0){
              amax <- NA
              amin <- NA
            }else {
              lq <- quantile(ai, probs = c(0.25),na.rm=T)[[1]]
              uq <- quantile(ai, probs = c(0.75),na.rm=T)[[1]]
              upper_limt <- uq + 1.5*(uq-lq)

              if(max(ai, na.rm=T) > upper_limt){
                amax <- upper_limt
              } else {
                amax <- max(ai, na.rm=T)
              }
              amin <- min(ai, na.rm=T)
            }
            rm(pet, pr, ai)
            gc()

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

        cur_grp_xy <- cur_grp[,c("lon","lat")]

        for(yr in time_period){
          for(m in months){

            startdate <- as.Date(paste0(yr,"-",m,"-15"))


            pr <- (t(getChelsa("pr", coords = cur_grp_xy,
                            startdate = startdate, enddate = startdate,
                            dataset = "chelsa-monthly")))[-1,]

            pmin <- min(pr, na.rm=T)
            pmax <- max(pr, na.rm=T)

            rm(pr)
            gc()

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

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #TMAX
            tasmax <- getChelsa("tasmax", extent = terra::ext(sp_dist[[sp]]),
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly")

            tmax <- max(suppressWarnings(terra::extract(tasmax, sp_dist[[sp]]))[,2],na.rm=T)
            rm(tasmax)
            gc()
            #TMIN
            tasmin <- getChelsa("tasmin", extent = terra::ext(sp_dist[[sp]]),
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly")

            tmin <- min(suppressWarnings(terra::extract(tasmin, sp_dist[[sp]]))[,2],na.rm=T)
            rm(tasmin)
            gc()
            #AI
            pet <- getChelsa("pet", extent = terra::ext(sp_dist[[sp]]),
                             startdate = startdate, enddate = startdate,
                             dataset = "chelsa-monthly")
            pr <- getChelsa("pr", extent = terra::ext(sp_dist[[sp]]),
                            startdate = startdate, enddate = startdate,
                            dataset = "chelsa-monthly")
            ai <- pr/pet
            ai <- terra::subst(ai, Inf, NA)

            ai_ext <- suppressWarnings(terra::extract(ai, sp_dist[[sp]]))[,2]

            ai_ext <- na.omit(ai_ext)

            if(length(ai_ext) == 0){
              amax <- NA
              amin <- NA

            } else {
              lq <- quantile(ai_ext, probs = c(0.25),na.rm=T)[[1]]
              uq <- quantile(ai_ext, probs = c(0.75),na.rm=T)[[1]]
              upper_limt <- uq + 1.5*(uq-lq)

              if(max(ai_ext, na.rm=T) > upper_limt){
                amax <- upper_limt
              } else {
                amax <- max(ai_ext, na.rm=T)
              }
              amin <- min(ai_ext, na.rm=T)

            }

            rm(pet, pr, ai)
            gc()

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 TMin = c(tmin),
                                 TMax = c(tmax),
                                 AMin = c(amin),
                                 AMax = c(amax))

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

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #TMAX
            tasmax <- getChelsa("tasmax", extent = terra::ext(sp_dist[[sp]]),
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly")

            tmax <- max(suppressWarnings(terra::extract(tasmax, sp_dist[[sp]]))[,2],na.rm=T)
            rm(tasmax)
            gc()
            #TMIN
            tasmin <- getChelsa("tasmin", extent = terra::ext(sp_dist[[sp]]),
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly")

            tmin <- min(suppressWarnings(terra::extract(tasmin, sp_dist[[sp]]))[,2],na.rm=T)
            rm(tasmin)
            gc()
            #AI
            pr <- getChelsa("pr", extent = terra::ext(sp_dist[[sp]]),
                            startdate = startdate, enddate = startdate,
                            dataset = "chelsa-monthly")

            pmin <- min(suppressWarnings(terra::extract(pr, sp_dist[[sp]]))[,2],na.rm=T)
            pmax <- max(suppressWarnings(terra::extract(pr, sp_dist[[sp]]))[,2],na.rm=T)

            rm(pr)
            gc()

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 TMin = c(tmin),
                                 TMax = c(tmax),
                                 PMin = c(pmin),
                                 PMax = c(pmax))

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

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #TMAX
            tasmax <- getChelsa("tasmax", extent = terra::ext(sp_dist[[sp]]),
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly")

            tmax <- max(suppressWarnings(terra::extract(tasmax, sp_dist[[sp]]))[,2],na.rm=T)
            rm(tasmax)
            gc()
            #TMIN
            tasmin <- getChelsa("tasmin", extent = terra::ext(sp_dist[[sp]]),
                                startdate = startdate, enddate = startdate,
                                dataset = "chelsa-monthly")

            tmin <- min(suppressWarnings(terra::extract(tasmin, sp_dist[[sp]]))[,2],na.rm=T)
            rm(tasmin)
            gc()

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 TMin = c(tmin),
                                 TMax = c(tmax))

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

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            #AI
            pet <- getChelsa("pet", extent = terra::ext(sp_dist[[sp]]),
                             startdate = startdate, enddate = startdate,
                             dataset = "chelsa-monthly")
            pr <- getChelsa("pr", extent = terra::ext(sp_dist[[sp]]),
                            startdate = startdate, enddate = startdate,
                            dataset = "chelsa-monthly")
            ai <- pr/pet
            ai <- terra::subst(ai, Inf, NA)

            ai_ext <- suppressWarnings(terra::extract(ai, sp_dist[[sp]]))[,2]
            ai_ext <- na.omit(ai_ext)
            if(length(ai_ext) == 0){
              amax <- NA
              amin <- NA

            } else {
              lq <- quantile(ai_ext, probs = c(0.25),na.rm=T)[[1]]
              uq <- quantile(ai_ext, probs = c(0.75),na.rm=T)[[1]]
              upper_limt <- uq + 1.5*(uq-lq)

              if(max(ai_ext, na.rm=T) > upper_limt){
                amax <- upper_limt
              } else {
                amax <- max(ai_ext, na.rm=T)
              }
              amin <- min(ai_ext, na.rm=T)

            }
            rm(pet, pr, ai)
            gc()

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 AMin = c(amin),
                                 AMax = c(amax))

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

            startdate <- as.Date(paste0(yr,"-",m,"-15"))

            pr <- getChelsa("pr", extent = terra::ext(sp_dist[[sp]]),
                            startdate = startdate, enddate = startdate,
                            dataset = "chelsa-monthly")

            pmin <- min(suppressWarnings(terra::extract(pr, sp_dist[[sp]]))[,2],na.rm=T)
            pmax <- max(suppressWarnings(terra::extract(pr, sp_dist[[sp]]))[,2],na.rm=T)

            rm(pr)
            gc()

            cur_ar <- data.frame(Year = c(yr),
                                 Month = c(m),
                                 List_Position = c(sp),
                                 PMin = c(pmin),
                                 PMax = c(pmax))

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

          startdate <- as.Date(paste0(yr,"-",m,"-15"))

          #TMAX
          tasmax <- getChelsa("tasmax", extent = terra::ext(sp_dist),
                              startdate = startdate, enddate = startdate,
                              dataset = "chelsa-monthly")

          tmax <- max(suppressWarnings(terra::extract(tasmax, sp_dist))[,2],na.rm=T)
          rm(tasmax)
          gc()
          #TMIN
          tasmin <- getChelsa("tasmin", extent = terra::ext(sp_dist),
                              startdate = startdate, enddate = startdate,
                              dataset = "chelsa-monthly")

          tmin <- min(suppressWarnings(terra::extract(tasmin, sp_dist))[,2],na.rm=T)
          rm(tasmin)
          gc()
          #AI
          pet <- getChelsa("pet", extent = terra::ext(sp_dist),
                           startdate = startdate, enddate = startdate,
                           dataset = "chelsa-monthly")
          pr <- getChelsa("pr", extent = terra::ext(sp_dist),
                          startdate = startdate, enddate = startdate,
                          dataset = "chelsa-monthly")
          ai <- pr/pet
          ai <- terra::subst(ai, Inf, NA)

          ai_ext <- suppressWarnings(terra::extract(ai, sp_dist))[,2]

          ai_ext <- na.omit(ai_ext)
          if(length(ai_ext) == 0){
            amax <- NA
            amin <- NA

          } else {
            lq <- quantile(ai_ext, probs = c(0.25),na.rm=T)[[1]]
            uq <- quantile(ai_ext, probs = c(0.75),na.rm=T)[[1]]
            upper_limt <- uq + 1.5*(uq-lq)

            if(max(ai_ext, na.rm=T) > upper_limt){
              amax <- upper_limt
            } else {
              amax <- max(ai_ext, na.rm=T)
            }
            amin <- min(ai_ext, na.rm=T)

          }

          rm(pet, pr, ai)
          gc()

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               TMin = c(tmin),
                               TMax = c(tmax),
                               AMin = c(amin),
                               AMax = c(amax))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)
    }

    else if(is.null(niche_vars) && isTRUE(precip)){
      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- as.Date(paste0(yr,"-",m,"-15"))

          #TMAX
          tasmax <- getChelsa("tasmax", extent = terra::ext(sp_dist),
                              startdate = startdate, enddate = startdate,
                              dataset = "chelsa-monthly")

          tmax <- max(suppressWarnings(terra::extract(tasmax, sp_dist))[,2],na.rm=T)
          rm(tasmax)
          gc()
          #TMIN
          tasmin <- getChelsa("tasmin", extent = terra::ext(sp_dist),
                              startdate = startdate, enddate = startdate,
                              dataset = "chelsa-monthly")

          tmin <- min(suppressWarnings(terra::extract(tasmin, sp_dist))[,2],na.rm=T)
          rm(tasmin)
          gc()
          #AI

          pr <- getChelsa("pr", extent = terra::ext(sp_dist),
                          startdate = startdate, enddate = startdate,
                          dataset = "chelsa-monthly")

          pmin <- min(suppressWarnings(terra::extract(pr, sp_dist))[,2],na.rm=T)
          pmax <- max(suppressWarnings(terra::extract(pr, sp_dist))[,2],na.rm=T)

          rm(pr)
          gc()

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               TMin = c(tmin),
                               TMax = c(tmax),
                               PMin = c(pmin),
                               PMax = c(pmax))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)
    }

    else if(niche_vars == "temp"){

      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- as.Date(paste0(yr,"-",m,"-15"))

          #TMAX
          tasmax <- getChelsa("tasmax", extent = terra::ext(sp_dist),
                              startdate = startdate, enddate = startdate,
                              dataset = "chelsa-monthly")

          tmax <- max(suppressWarnings(terra::extract(tasmax, sp_dist))[,2],na.rm=T)
          rm(tasmax)
          gc()
          #TMIN
          tasmin <- getChelsa("tasmin", extent = terra::ext(sp_dist),
                              startdate = startdate, enddate = startdate,
                              dataset = "chelsa-monthly")

          tmin <- min(suppressWarnings(terra::extract(tasmin, sp_dist))[,2],na.rm=T)
          rm(tasmin)
          gc()

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               TMin = c(tmin),
                               TMax = c(tmax))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)

    }

    else if (niche_vars == "arid" && isFALSE(precip)){
      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- as.Date(paste0(yr,"-",m,"-15"))

          #AI
          pet <- getChelsa("pet", extent = terra::ext(sp_dist),
                           startdate = startdate, enddate = startdate,
                           dataset = "chelsa-monthly")
          pr <- getChelsa("pr", extent = terra::ext(sp_dist),
                          startdate = startdate, enddate = startdate,
                          dataset = "chelsa-monthly")
          ai <- pr/pet
          ai <- terra::subst(ai, Inf, NA)

          ai_ext <- suppressWarnings(terra::extract(ai, sp_dist))[,2]
          ai_ext <- na.omit(ai_ext)
          if(length(ai_ext) == 0){
            amax <- NA
            amin <- NA
          } else {
            lq <- quantile(ai_ext, probs = c(0.25),na.rm=T)[[1]]
            uq <- quantile(ai_ext, probs = c(0.75),na.rm=T)[[1]]
            upper_limt <- uq + 1.5*(uq-lq)

            if(max(ai_ext, na.rm=T) > upper_limt){
              amax <- upper_limt
            } else {
              amax <- max(ai_ext, na.rm=T)
            }
            amin <- min(ai_ext, na.rm=T)

          }

          rm(pet, pr, ai)
          gc()

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               AMin = c(amin),
                               AMax = c(amax))

          all_df <- rbind(all_df, cur_ar[1,])
        }
      }
      all_df <- all_df %>% dplyr::select(-Year) %>% dplyr::group_by(Month) %>% dplyr::summarise_all(mean, na.rm=T)
    }

    else if (niche_vars == "arid" && isTRUE(precip)){
      all_df <- data.frame(NULL)

      for(yr in time_period){
        for(m in months){

          startdate <- as.Date(paste0(yr,"-",m,"-15"))

          pr <- getChelsa("pr", extent = terra::ext(sp_dist),
                          startdate = startdate, enddate = startdate,
                          dataset = "chelsa-monthly")

          pmin <- min(suppressWarnings(terra::extract(pr, sp_dist))[,2],na.rm=T)
          pmax <- max(suppressWarnings(terra::extract(pr, sp_dist))[,2],na.rm=T)

          rm(pr)
          gc()

          cur_ar <- data.frame(Year = c(yr),
                               Month = c(m),
                               PMin = c(pmin),
                               PMax = c(pmax))

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
      all_df <- all_df %>% mutate(TMin = ((TMin - 32)*(5/9))+273.15,
                                  TMax = ((TMax - 32)*(5/9))+273.15)
    } else if (isTRUE(tmp_units == "Celsius")){
      all_df <- all_df %>% mutate(TMin = TMin - 273.15,
                                  TMax = TMax - 273.15)
    }
    return(all_df)
  }

  else if (isTRUE(is.data.frame(sp_dist))){
    if(isFALSE(monthly)){
      grp_all_df <- grp_all_df[,-1]
        grp_all_df <- grp_all_df %>%
          dplyr::group_by(Group)%>%
        dplyr::summarise_all(mean, na.rm=T)
    }
    if(isTRUE(is.null(tmp_units))){
      grp_all_df <- grp_all_df
    }else if (isTRUE(tmp_units == "Fahrenheit")){
      grp_all_df <- grp_all_df %>% mutate(TMin = ((TMin - 32)*(5/9))+273.15,
                                          TMax = ((TMax - 32)*(5/9))+273.15)
    } else if (isTRUE(tmp_units == "Celsius")){
      grp_all_df <- grp_all_df %>% mutate(TMin = TMin - 273.15,
                                          TMax = TMax - 273.15)
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
      all_df <- all_df %>% mutate(TMin = ((TMin - 32)*(5/9))+273.15,
                                  TMax = ((TMax - 32)*(5/9))+273.15)
    } else if (isTRUE(tmp_units == "Celsius")){
      all_df <- all_df %>% mutate(TMin = TMin - 273.15,
                                  TMax = TMax - 273.15)
    }
    return(all_df)
  }
}

