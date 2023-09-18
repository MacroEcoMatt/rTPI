
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rTPI

<!-- badges: start -->
<!-- badges: end -->

The goal of rTPI is to provide a set of tools for calculating Thermal
Position and Aridity Position Indices (TPI and API). These indices are
useful for evaluating the relative impact of temperature and aridity
values given species specific thermal tolerance or aridity tolerance
limits.

You provide environmental, or location data with genus species binomial
names, and are returned a dataframe with TPI or API values. These values
are scaled temperature and aridity values that are specific to each
species niche tolerance. Typically, values will fall between 0 (lower
niche limits) and 1 (upper niche limit). However, values can exceed
species tolerance limits and fall outside of 0 and 1, representing
extreme environmental conditions given species specific tolerances.

## Installation

You can install the development version of rTPI by downloading the
package directly and installing from the .zip file or like so:

``` r
# devtools::install_github("MacroEcoMatt/rTPI")
```

\##Functions include: binomial_check - cross check submitted species
binomial (eg. Melospiza melodia) against species binomials in the
TPI/API dataset

syn_check - if submitted binomial does not match package binomial
records, this function will check user submitted binomial against known
species synonyms and return matching TPI/API binomials

species_limits - used to extract a dataframe containint the TPI/API
information for submitted list of species

highertaxa_limits - used to extract a dataframe containint the TPI/API
information for submitted list of higher taxa groups (e.g taxa_code =
“clas”, taxa_list = “Aves)

get_env_vars - used to collect historical temperature and aridity data
based on user submitted data. This function utilizes the climateR
package. This is particularly useful for extracting Aridity Index values
which is what API is calculated from.

tpi - calculates TPI and prodcues a dataframe of values based on user
submitted data. Requires species binomial, and temperature data in
degrees celcius

api - calculates API and prodcues a dataframe of values based on user
submitted data. Requires species binomial, and Aridity Index values. SEE
get_env_vars.

``` r
library(rTPI)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: sf
#> Linking to GEOS 3.11.2, GDAL 3.6.2, PROJ 9.2.0; sf_use_s2() is TRUE
#> Loading required package: climateR
#> Loading required package: terra
#> terra 1.7.39
sp_list <- c("Poecile atricapillus","Melospiza melodia")

##binomial_check
sp_check_df <- binomial_check(sp_list)

##syn_check
Compared_df <- syn_check(sp_list)

##species_limits
sp_limits_data <- species_limits(sp_list, niche_limit = NULL, month_list = NULL,
                           yr_avg = FALSE)

##highertaxa_limits
Aves_TPI <- highertaxa_limits(taxa_code = "class", taxa_list="Aves", 
                              niche_limit = "tpi", month_list="Jan", 
                              yr_avg=TRUE)

##get_env_vars
df <- data.frame(Year = c(2000,2000),
  Month = c(6,7),
  Day = c(3,5),
  Lat = c(41.334287,41.334287),
  Lon = c(-77.942020,-77.942020),
  Season = c("summer","summer"))

Summer_env_vars <- get_env_vars(df, env_vars= "both", date_format = "monthly")
#> Loading required package: raster
#> Loading required package: sp
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
#> 
#> Attaching package: 'raster'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> Loading required package: AOI
#> source:   http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg... 
#> varname(s):
#>    > pet [mm] (water_potential_evaporation_amount)
#>    > ppt [mm] (precipitation_amount)
#>    > tmax [degC] (air_temperature)
#>    > tmin [degC] (air_temperature)
#> ==================================================
#> diminsions:  3, 2, 1 (names: lon,lat,time)
#> resolution:  0.042, 0.042, 1 months
#> extent:      -78, -77.88, 41.29, 41.38 (xmin, xmax, ymin, ymax)
#> crs:         +proj=longlat +a=6378137 +f=0.00335281066474748 +p...
#> time:        2000-06-01 to 2000-06-01
#> ==================================================
#> values: 24 (vars*X*Y*T)source:    http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg... 
#> varname(s):
#>    > pet [mm] (water_potential_evaporation_amount)
#>    > ppt [mm] (precipitation_amount)
#>    > tmax [degC] (air_temperature)
#>    > tmin [degC] (air_temperature)
#> ==================================================
#> diminsions:  3, 2, 1 (names: lon,lat,time)
#> resolution:  0.042, 0.042, 1 months
#> extent:      -78, -77.88, 41.29, 41.38 (xmin, xmax, ymin, ymax)
#> crs:         +proj=longlat +a=6378137 +f=0.00335281066474748 +p...
#> time:        2000-07-01 to 2000-07-01
#> ==================================================
#> values: 24 (vars*X*Y*T)

##TPI AND API
species_data <- data.frame(
                Binomial = c("Poecile atricapillus", "Poecile atricapillus"),
                Month = c(1,2),
                TMax = c(5,10),
                TMin = c(-2,-1),
                Tm = c(0,3),
                AMin = c(0,0.5),
                AMax = c(2,3),
                Ar = c(1,1.1)
                )

tpi_Poec_atr <- tpi(species_data, tmp_var = NULL, use_year = FALSE, flag_sp = FALSE,
                flag_month = FALSE, flag_tmp = FALSE)

api_Poec_atr <- api(species_data, ar_var = NULL, use_year = FALSE, flag_sp = FALSE,
                flag_month = FALSE, flag_ar = FALSE)
```

\#Formatting: This is important for the get_env_vars, tpi, and api
functions as users need to submit dataframes with specific column names.

get_env_vars: needs a dataframe with columns titled: “Lat” and “Lon” -
for finding geographic locations, and the data in these columns must be
in decimal degrees.

Depending on the environmental information required different dates can
be included, but submitted dataframes must have at least a column titled
“Year”. Other date columns can include “Month” and “Day”. “Year” and
“Day” must be numeric, but “Month” can be numeric or character (e.g: 1,
“Jan”, “1”).

tpi: Must contain columns titled: “Tm” and “Binomial”. “Tm” contains a
temperature value, “Binomial” contains species binomial. “TMax” and
“TMin” can also be used as column names containing maximum and minimum
temperature values.

IF no month is submitted then the function defualts to using yearly
average upper and lower thermal tolerance limits for TPI calculation.
“Month” can be specified in the as numeric (1-12) or character
(“Jan”-“Dec”).

api: Must contain columns titled: “Ar” and “Binomial”. “Ar” contains an
Aridity Index value, “Binomial” contains species binomial. “AMax” and
“AMin” can also be used as column names containing maximum and minimum
Aridity Index values.

IF no month is submitted then the function defaults to using yearly
average upper and lower Aridity tolerance limits for API calculation.
“Month” can be specified in the as numeric (1-12) or character
(“Jan”-“Dec”).

\#Common Issues: - misspelled binomials: must be capitalized first
letter on genus name followed by a space then all lowercase species name

- Incorrectly entered data: Aridity Index Values must be positive, and
  negative numbers for Ar, AMin, AMax in api function will throw and
  error. Lat and Lon must be in decimal degrees.
