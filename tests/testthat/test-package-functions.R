library(testthat)
#TEST FUNCTION binomial_check
test_that("single string entry reutrns data", {
  expect_length(binomial_check("Poecile atricapillus"),2)
  expect_length(binomial_check(c("Poecile atricapillus", "Melospiza melodia")),4)
  expect_length(binomial_check(c("Poecile atricapillus", "Melospiza me")),4)
  expect_length(binomial_check(c("Poecile atripillus", "Melospiza dia")),4)
})

#TEST FUNCTION species_list
test_that("single string entry reutrns data", {
  expect_error(species_list(class="m"))
  expect_no_error(species_list(class="mammal"))
  expect_no_error(species_list(class="bird"))
  expect_no_error(species_list(class="amphibian"))
  expect_no_error(species_list(class="reptile"))
})

#TEST FUNCTION highertaxa_limits
test_that("Check that error codes work properly and returns proper results",{
  expect_error(highertaxa_limits(taxa_code ="c"))
  expect_no_error(highertaxa_limits(taxa_code ="class", yr_avg = F, taxa_list = "Aves"))
  expect_no_error(highertaxa_limits(taxa_code ="class", yr_avg = T, taxa_list = "Aves"))
  expect_no_error(highertaxa_limits(taxa_code ="order", yr_avg = T, taxa_list = "Squamata"))
  expect_no_error(highertaxa_limits(taxa_code ="family", yr_avg = T, taxa_list = "Muridae"))
  expect_no_error(highertaxa_limits(taxa_code ="genus", yr_avg = T, taxa_list = "Abeillia"))
  expect_error(highertaxa_limits(taxa_code ="cls", yr_avg = T, taxa_list = "Aves"))
  expect_error(highertaxa_limits(taxa_code ="class", niche_limit ="a", yr_avg = T, taxa_list = "Aves"))

  expect_error(highertaxa_limits(taxa_code ="class", taxa_list = "av"))
  expect_error(highertaxa_limits(taxa_code ="class", taxa_list = "av", yr_avg=T))

  expect_no_error(highertaxa_limits(taxa_code ="class", niche_limit="api", taxa_list = "Aves"))
  expect_no_error(highertaxa_limits(taxa_code ="class", niche_limit="tpi", taxa_list = "Aves"))
  expect_error(highertaxa_limits(taxa_code ="class", niche_limit="i", taxa_list = "Aves"))

  expect_error(highertaxa_limits(taxa_code ="class", yr_avg = T, taxa_list = "Aves", niche_limit="b"))
  expect_no_error(highertaxa_limits(taxa_code ="class", yr_avg = T, taxa_list = "Aves", niche_limit="api"))
  expect_no_error(highertaxa_limits(taxa_code ="class", yr_avg = T, taxa_list = "Aves", niche_limit="tpi"))
  expect_warning(highertaxa_limits(taxa_code ="class", yr_avg = "d", taxa_list = "Aves", niche_limit="tpi"))

  expect_error(highertaxa_limits(taxa_code ="class", taxa_list = "Aves", month_list = 13))
  expect_no_error(highertaxa_limits(taxa_code ="class", taxa_list = "Aves", niche_limit = "tpi", month_list = 12))
  expect_no_error(highertaxa_limits(taxa_code ="class", taxa_list = "Aves", niche_limit = "tpi", month_list = "Jan"))
  expect_error(highertaxa_limits(taxa_code ="class", taxa_list = "Aves", niche_limit = "tpi", month_list = "jn"))
})

#TEST FUNCTION species_limits
test_that("Check that error codes work properly and returns proper results",{
  expect_no_error(species_limits("Poecile atricapillus"))
  expect_error(species_limits("Poecile atricapillus", niche_limit = "t"))
  expect_error(species_limits("x"))
  expect_error(species_limits("Poecile atricapillus", month_list = c(-1,13)))
  expect_error(species_limits("Poecile atricapillus", month_list = c("jjan")))
  expect_error(species_limits("Poecile atricapillus", month_list = T))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "both"))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "tpi"))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "api"))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "both", month_list = c(1,2,3)))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "tpi", month_list = c(1,2,3)))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "api", month_list = c(1,2,3)))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "both", month_list = c("Jan","Feb")))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "tpi", month_list = c("Jan","Feb")))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "api", month_list = c("Jan","Feb")))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "both", month_list = c("Jan","Feb"),yr_avg = T))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "tpi", month_list = c("Jan","Feb"),yr_avg = T))
  expect_no_error(species_limits("Poecile atricapillus", niche_limit = "api", month_list = c("Jan","Feb"),yr_avg = T))
  expect_error(species_limits("Poecile atricapillus", niche_limit = "i", month_list = c("Jan","Feb"),yr_avg = T))
  expect_warning(species_limits("Poecile atricapillus", niche_limit = "tpi", month_list = c("Jan","Feb"),yr_avg = "dd"))
})

species_data <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atrisdfpillus","Poecile atricapillus"),
  Month = c(1,2,3,13),
  tmax = c(5,10,12,NA),
  tmin = c(-2,-1,0,NA),
  Tm = c(0,3,-1,NA)
)

species_noerrors <- data.frame(
  Binomial = c("Poecile atricapillus","Poecile atricapillus","x","x","x"),
  Month = c("Feb",NA,"Jan",NA,"Year"),
  tmax = c(5,13,2,3,4),
  tmin = c(-2,1,2,4,6),
  Tm = c(0,2,3,5,6)
)
clean_data <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c(1),
  tmax = c(5),
  tmin = c(-2),
  Tm = c(0)
)
clean_datab <- data.frame(
  Binomial = c("Poecile atricapillus","Poecile atricapillus"),
  Month = c("1","Year"),
  tmax = c(5,5),
  tmin = c(-2,1),
  Tm = c(0,1)
)
clean_datac <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("Jan"),
  tmax = c(5),
  tmin = c(-2),
  Tm = c(0)
)
species_noerrors2 <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atricapillus","x","x","x"),
  Month = c("Feb","Year","2","Mar","1","Year"),
  tmax = c(5,10,12,12,2,2),
  tmin = c(-2,-1,0,1,1,3),
  Tm = c(0,3,4,3,3,4)
)
species_char_month <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("Ja"),
  tmax = c(5),
  tmin = c(-2),
  Tm = c(0)
)
#TEST FUNCTION tpi
test_that("Check that error codes work properly and returns proper results",{
  expect_no_error(tpi(clean_data))
  expect_no_error(tpi(clean_data, tmp_var = "minmax"))
  expect_no_error(tpi(clean_data, tmp_var = "all"))
  expect_no_error(tpi(clean_datab))
  expect_no_error(tpi(clean_datab, tmp_var = "minmax"))
  expect_no_error(tpi(clean_datab, tmp_var = "all"))
  expect_no_error(tpi(clean_datac))
  expect_no_error(tpi(clean_datac, tmp_var = "minmax"))
  expect_no_error(tpi(clean_datac, tmp_var = "all"))

  expect_no_error(tpi(clean_data, tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_data, tmp_var = "minmax", tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_data, tmp_var = "all", tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_datab, tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_datab, tmp_var = "minmax", tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_datab, tmp_var = "all", tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_datac, tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_datac, tmp_var = "minmax", tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_datac, tmp_var = "all", tmp_unit = "Fahrenheit"))

  expect_no_error(tpi(clean_data, tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_data, tmp_var = "minmax", tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_data, tmp_var = "all", tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_datab, tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_datab, tmp_var = "minmax", tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_datab, tmp_var = "all", tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_datac, tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_datac, tmp_var = "minmax", tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_datac, tmp_var = "all", tmp_unit = "Kelvin"))

  expect_no_error(tpi(clean_data, use_year = T))
  expect_no_error(tpi(clean_data, tmp_var = "minmax", use_year = T))
  expect_no_error(tpi(clean_data, tmp_var = "all", use_year = T))

  expect_no_error(tpi(clean_data, use_year = T, tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_data, tmp_var = "minmax", use_year = T, tmp_unit = "Fahrenheit"))
  expect_no_error(tpi(clean_data, tmp_var = "all", use_year = T, tmp_unit = "Fahrenheit"))

  expect_no_error(tpi(clean_data, use_year = T, tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_data, tmp_var = "minmax", use_year = T, tmp_unit = "Kelvin"))
  expect_no_error(tpi(clean_data, tmp_var = "all", use_year = T, tmp_unit = "Kelvin"))

  expect_no_error(tpi(clean_data, flag_month = T, flag_tmp = T,flag_sp=T))
  expect_no_error(tpi(clean_data, tmp_var = "minmax",flag_sp = T, flag_month = T, flag_tmp = T))
  expect_no_error(tpi(clean_data, tmp_var = "all",flag_sp = T, flag_month = T, flag_tmp = T))

  expect_warning(tpi(species_data, flag_month = T, flag_tmp = T,flag_sp=T))
  expect_warning(tpi(species_data, tmp_var = "minmax",flag_month = T, flag_tmp = T,flag_sp=T))
  expect_warning(tpi(species_data, tmp_var = "all",flag_month = T, flag_tmp = T,flag_sp=T))

  expect_warning(tpi(species_noerrors, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T))
  expect_warning(tpi(species_noerrors,tmp_var = "minmax", flag_sp = T, flag_month = T, flag_tmp = T, use_year = T))
  expect_warning(tpi(species_noerrors, tmp_var = "all", flag_sp = T, flag_month = T, flag_tmp = T, use_year = T))

  expect_error(tpi(species_noerrors2))
  expect_error(tpi(clean_data, tmp_unit = "a"))
  expect_error(tpi(clean_data,tmp_var = "a"))
  expect_error(tpi(species_char_month,tmp_var = "minmax"))
  expect_error(tpi(species_char_month,tmp_var = "all"))
  expect_error(tpi(species_char_month))
})

clean_data2 <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c(1),
  amax = c(5),
  amin = c(2),
  Ar = c(4)
)

clean_data3 <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("1","Year"),
  amax = c(5,2),
  amin = c(2,2),
  Ar = c(4,2)
)
clean_data4 <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("Jan"),
  amax = c(5),
  amin = c(2),
  Ar = c(4)
)
species_data2 <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atrisdfpillus","Poecile atricapillus"),
  Month = c(1,2,3,13),
  amax = c(0,1,1,NA),
  amin = c(0,0,0,NA),
  Ar = c(0,3,1,NA)
)
species_noerrors22 <- data.frame(
  Binomial = c("Poecile atricapillus","Poecile atricapillus","x","x","x"),
  Month = c("Feb",NA,"Jan",NA,"Year"),
  amax = c(5,13,2,3,4),
  amin = c(2,1,2,4,6),
  Ar = c(0,2,3,5,6)
)

species_noerrors2b <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atricapillus","x","x","x"),
  Month = c("Feb","Year","2","Mar","1","Year"),
  amax = c(5,10,12,12,2,2),
  amin = c(2,1,0,1,1,3),
  Ar = c(0,3,4,3,3,4)
)
species_char_month <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("Ja"),
  tmax = c(5),
  tmin = c(2),
  Tm = c(0)
)

mon_neg <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("Jan"),
  amax = c(5),
  amin = c(2),
  Ar = c(-1)
)
mon_neg2 <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("Jan"),
  amax = c(-5),
  amin = c(-2),
  Ar = c(-1)
)
mon_neg3 <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c("Jan"),
  amax = c(-5),
  amin = c(-2),
  Ar = c(1)
)
#TEST FUNCTION api
test_that("Check that error codes work properly and returns proper results",{
  expect_no_error(api(clean_data2))
  expect_no_error(api(clean_data2, ar_var = "minmax"))
  expect_no_error(api(clean_data2, ar_var = "all"))
  expect_no_error(api(clean_data3))
  expect_no_error(api(clean_data3, ar_var = "minmax"))
  expect_no_error(api(clean_data3, ar_var = "all"))
  expect_no_error(api(clean_data4))
  expect_no_error(api(clean_data4, ar_var = "minmax"))
  expect_no_error(api(clean_data4, ar_var = "all"))

  expect_no_error(api(clean_data2, use_year = T))
  expect_no_error(api(clean_data2, ar_var = "minmax", use_year = T))
  expect_no_error(api(clean_data2, ar_var = "all", use_year = T))

  expect_no_error(api(clean_data2, flag_month = T, flag_ar = T,flag_sp=T))
  expect_no_error(api(clean_data2, ar_var = "minmax",flag_sp = T, flag_month = T, flag_ar = T))
  expect_no_error(api(clean_data2, ar_var = "all", flag_sp = T, flag_month = T, flag_ar = T))

  expect_warning(api(species_data2, flag_month = T, flag_ar = T,flag_sp=T))
  expect_warning(api(species_data2, ar_var = "minmax",flag_month = T, flag_ar = T,flag_sp=T))
  expect_warning(api(species_data2, ar_var = "all",flag_month = T, flag_ar = T,flag_sp=T))

  expect_warning(api(species_noerrors22, flag_sp = T, flag_month = T, flag_ar = T, use_year = T))
  expect_warning(api(species_noerrors22, ar_var = "minmax", flag_sp = T, flag_month = T, flag_ar = T, use_year = T))
  expect_warning(api(species_noerrors22, ar_var = "all", flag_sp = T, flag_month = T, flag_ar = T, use_year = T))

  expect_error(api(species_noerrors2b))
  expect_error(api(clean_data2, ar_var = "a"))
  expect_error(api(species_char_month, ar_var = "minmax"))
  expect_error(api(species_char_month, ar_var = "all"))
  expect_error(api(species_char_month))

  expect_error(api(mon_neg3,ar_var = "minmax"))
  expect_error(api(mon_neg2,ar_var = "all"))
  expect_error(api(mon_neg))
})

splist <- c("Poecile atricapillus", "Parus atricapillus","ile atricapillus")
#TEST FUNCTION syn_check
test_that("Check to ensure that variables are extracted appropriately",{
  expect_no_error(syn_check(splist))
})

##generate niche
niche_noerrors <- data.frame(
  Group = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(0,0,0,0),
  lon = c(15,16,17,18)
)
wrong_group <- data.frame(
  Grou = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(0,0,0,0),
  lon = c(15,16,17,18)
)
wrong_lat <- data.frame(
  Group = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(200,0,0,0),
  lon = c(15,16,17,18)
)
wrong_type <- TRUE
wrong_type2 <- list("t","t")

spatvect_obj <- terra::vect(data.frame(
  Group = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(1,0,-1,0),
  lon = c(15,16,17,18)),geom=c("lon", "lat"),
  crs="+proj=longlat +datum=WGS84")

spatvect_obj2 <- list(spatvect_obj,spatvect_obj)

#TEST FUNCTION generate niche
test_that("Check that error codes work properly and returns proper results",{
  expect_no_error(generate_niche(niche_noerrors, monthly=F, time_period=c(1980)))
  expect_no_error(generate_niche(niche_noerrors, precip = T, time_period=c(1980)))
  expect_no_error(generate_niche(niche_noerrors, tmp_units = "Kelvin", time_period=c(1980)))
  expect_no_error(generate_niche(niche_noerrors, tmp_units = "Fahrenheit", time_period=c(1980)))
  expect_no_error(generate_niche(niche_noerrors, niche_vars = "temp", time_period=c(1980)))
  expect_no_error(generate_niche(niche_noerrors, niche_vars = "arid", time_period=c(1980)))
  expect_no_error(generate_niche(niche_noerrors, niche_vars = "arid", precip = F, time_period=c(1980)))

  expect_no_error(generate_niche(spatvect_obj, monthly = F, time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj, precip = T, time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj, niche_vars = "temp", time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj, niche_vars = "arid", time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj, niche_vars = "arid", precip = F, time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj, tmp_units = "Kelvin", time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj, tmp_units = "Fahrenheit", time_period=c(1980)))

  expect_no_error(generate_niche(spatvect_obj2, monthly = F, time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj2, precip = T, time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj2, niche_vars = "temp", time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj2, niche_vars = "arid", time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj2, niche_vars = "arid", precip = F, time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj2, tmp_units = "Kelvin", time_period=c(1980)))
  expect_no_error(generate_niche(spatvect_obj2, tmp_units = "Fahrenheit", time_period=c(1980)))

  expect_error(generate_niche(wrong_group))
  expect_error(generate_niche(wrong_lat))
  expect_error(generate_niche(wrong_type))
  expect_error(generate_niche(niche_noerrors, monthly="tt"))
  expect_error(generate_niche(niche_noerrors, precip="tt"))
  expect_error(generate_niche(niche_noerrors, time_period = c(1900)))
  expect_error(generate_niche(niche_noerrors, time_period = c("1900")))
  expect_error(generate_niche(niche_noerrors, niche_vars = "tt"))
  expect_error(generate_niche(wrong_type2))
  expect_error(generate_niche(niche_noerrors, temp_units = "tt"))
})

##generate niche
niche_noerrors <- data.frame(
  Group = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(1,0,-1,0),
  lon = c(15,16,17,18)
)
wrong_group <- data.frame(
  Grou = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(1,0,-1,0),
  lon = c(15,16,17,18)
)
wrong_lat <- data.frame(
  Group = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(290,0,0,0),
  lon = c(15,16,17,18)
)
wrong_type <- TRUE
wrong_type2 <- list("t","t")
spatvect_obj <- terra::vect(data.frame(
  Group = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  lat = c(1,0,-1,0),
  lon = c(15,16,17,18)),geom=c("lon", "lat"),
  crs="+proj=longlat +datum=WGS84")

spatvect_obj2 <- list(spatvect_obj,spatvect_obj)

#TEST FUNCTION generate niche chelsa
test_that("Check that error codes work properly and returns proper results",{
  expect_no_error(generate_niche_chelsa(niche_noerrors, monthly=F, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(niche_noerrors, precip = T, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(niche_noerrors, tmp_units = "Celsius", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(niche_noerrors, tmp_units = "Fahrenheit", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(niche_noerrors, niche_vars = "temp", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(niche_noerrors, niche_vars = "arid", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(niche_noerrors, niche_vars = "arid", precip = F, time_period=c(1980)))

  expect_no_error(generate_niche_chelsa(spatvect_obj, monthly = F, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj, precip = T, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj, niche_vars = "temp", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj, niche_vars = "arid", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj, niche_vars = "arid", precip = F, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj, tmp_units = "Celsius", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj, tmp_units = "Fahrenheit", time_period=c(1980)))

  expect_no_error(generate_niche_chelsa(spatvect_obj2, monthly = F, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj2, precip = T, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj2, niche_vars = "temp", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj2, niche_vars = "arid", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj2, niche_vars = "arid", precip = F, time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj2, tmp_units = "Celsius", time_period=c(1980)))
  expect_no_error(generate_niche_chelsa(spatvect_obj2, tmp_units = "Fahrenheit", time_period=c(1980)))

  expect_error(generate_niche_chelsa(wrong_group))
  expect_error(generate_niche_chelsa(wrong_lat))
  expect_error(generate_niche_chelsa(wrong_type))
  expect_error(generate_niche_chelsa(niche_noerrors, precip="tt"))
  expect_error(generate_niche_chelsa(niche_noerrors, time_period = c(1900)))
  expect_error(generate_niche_chelsa(niche_noerrors, time_period = c("1900")))
  expect_error(generate_niche_chelsa(niche_noerrors, niche_vars = "tt"))
  expect_error(generate_niche_chelsa(wrong_type2))
  expect_error(generate_niche_chelsa(niche_noerrors, temp_units = "tt"))
})
