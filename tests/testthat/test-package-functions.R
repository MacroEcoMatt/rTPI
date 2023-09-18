library(testthat)
#TEST FUNCTION binomial_check
test_that("single string entry reutrns data", {
  expect_length(binomial_check("Poecile atricapillus"),2)
  expect_length(binomial_check(c("Poecile atricapillus", "Melospiza melodia")),4)
  expect_length(binomial_check(c("Poecile atricapillus", "Melospiza me")),4)
  expect_length(binomial_check(c("Poecile atripillus", "Melospiza dia")),4)
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
  Month = c(1,2,3,14),
  TMax = c(5,10,12,13),
  TMin = c(-2,-1,0,2),
  Tm = c(0,3,-1,1)
)
species_noerrors <- data.frame(
  Binomial = c("Poecile atricapillus","Poecile atricapillus","x","x"),
  Month = c(1,NA,2,NA),
  TMax = c(5,13,2,3),
  TMin = c(-2,1,2,4),
  Tm = c(0,2,3,5)
)
clean_data <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c(1),
  TMax = c(5),
  TMin = c(-2),
  Tm = c(0)
)
species_noerrors2 <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atricapillus","x","x","x"),
  Month = c("Feb","Year","2","Mar","1","Year"),
  TMax = c(5,10,12,12,2,2),
  TMin = c(-2,-1,0,1,1,3),
  Tm = c(0,3,4,3,3,4)
)
species_char_month <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  Month = c("Ja","Feb","Year","2","Jan","Jan","Mar"),
  TMax = c(5,10,12,13,13,13,NA),
  TMin = c(-2,-1,0,1,1,NA,2),
  Tm = c(0,3,4,2,NA,1,2)
)
#TEST FUNCTION tpi
test_that("Check that error codes work properly and returns proper results",{
  expect_no_error(tpi(clean_data))
  expect_no_error(tpi(clean_data, tmp_var = "minmax"))
  expect_no_error(tpi(clean_data, tmp_var = "all"))

  expect_warning(tpi(species_data, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T))
  expect_warning(tpi(species_char_month, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T))

  expect_warning(tpi(species_data, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "minmax"))
  expect_warning(tpi(species_char_month, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "minmax"))

  expect_warning(tpi(species_data, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "all"))
  expect_warning(tpi(species_char_month, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "all"))

  expect_warning(tpi(species_noerrors, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "all"))
  expect_warning(tpi(species_noerrors, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "minmax"))
  expect_warning(tpi(species_noerrors, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T))
  expect_warning(tpi(species_noerrors2, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "all"))
  expect_warning(tpi(species_noerrors2, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T, tmp_var = "minmax"))
  expect_warning(tpi(species_noerrors2, flag_sp = T, flag_month = T, flag_tmp = T, use_year = T))

  expect_warning(tpi(species_data, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F))
  expect_warning(tpi(species_char_month, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F))

  expect_warning(tpi(species_data, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "minmax"))
  expect_warning(tpi(species_char_month, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "minmax"))

  expect_warning(tpi(species_data, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "all"))
  expect_warning(tpi(species_char_month, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "all"))

  expect_warning(tpi(species_noerrors, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "all"))
  expect_warning(tpi(species_noerrors, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "minmax"))
  expect_warning(tpi(species_noerrors, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F))
  expect_warning(tpi(species_noerrors2, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "all"))
  expect_warning(tpi(species_noerrors2, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F, tmp_var = "minmax"))
  expect_warning(tpi(species_noerrors2, flag_sp = T, flag_month = T, flag_tmp = T, use_year = F))

  expect_error(tpi(species_char_month, tmp_var = "l"))

  expect_warning(tpi(species_noerrors, tmp_var = NULL))
  expect_warning(tpi(species_noerrors, tmp_var = "all"))
  expect_warning(tpi(species_noerrors, tmp_var = "minmax"))
})
clean_data2 <- data.frame(
  Binomial = c("Poecile atricapillus"),
  Month = c(1),
  AMax = c(5),
  AMin = c(2),
  Ar = c(4)
)
a_data <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atrisdfpillus","Poecile atricapillus"),
  Month = c(1,2,3,14),
  AMax = c(5,3,2,2),
  AMin = c(2,1,0,2),
  Ar = c(0,3,1,1)
)
a_noerrors <- data.frame(
  Binomial = c("Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","x","x"),
  Month = c(1,2,3,12,NA,2,NA),
  AMax = c(5,3,2,2,4,4,5),
  AMin = c(2,1,0,2,1,2,1),
  Ar = c(0,3,1,1,1,1,1)
)

a_ar_errors <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atricapillus"),
  Month = c("Feb","Year","2"),
  AMax = c(5,-10,12),
  AMin = c(-2,1,0),
  Ar = c(0,3,-4)
)

a_noerrors2 <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atricapillus","x","x","x"),
  Month = c("Feb","Year","2","Mar","1","Year"),
  AMax = c(5,10,12,5,6,7),
  AMin = c(2,1,0,1,2,1),
  Ar = c(0,3,4,3,3,2)
)
a_char_month <- data.frame(
  Binomial = c("Poecile atricapillus", "Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus","Poecile atricapillus"),
  Month = c("Ja","Feb","Year","2","Jan","Jan","Mar"),
  AMax = c(5,10,12,13,13,13,NA),
  AMin = c(2,1,0,1,1,NA,2),
  Ar = c(0,3,4,2,NA,1,2)
)
#TEST FUNCTION api
test_that("Check that error codes work properly and returns proper results",{
  expect_no_error(api(clean_data2))
  expect_no_error(api(clean_data2, ar_var = "minmax"))
  expect_no_error(api(clean_data2, ar_var = "all"))

  expect_warning(api(a_data, flag_sp = T, flag_month = T, flag_ar = T, use_year = T))
  expect_warning(api(a_char_month, flag_sp = T, flag_month = T, flag_ar = T, use_year = T))

  expect_warning(api(a_data, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "minmax"))
  expect_warning(api(a_char_month, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "minmax"))

  expect_warning(api(a_data, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "all"))
  expect_warning(api(a_char_month, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "all"))

  expect_warning(api(a_noerrors, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "all"))
  expect_warning(api(a_noerrors, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "minmax"))
  expect_warning(api(a_noerrors, flag_sp = T, flag_month = T, flag_ar = T, use_year = T))
  expect_warning(api(a_noerrors2, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "all"))
  expect_warning(api(a_noerrors2, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "minmax"))
  expect_warning(api(a_noerrors2, flag_sp = T, flag_month = T, flag_ar = T, use_year = T))

  expect_warning(api(a_data, flag_sp = T, flag_month = T, flag_ar = T, use_year = F))
  expect_warning(api(a_char_month, flag_sp = T, flag_month = T, flag_ar = T, use_year = F))

  expect_warning(api(a_data, flag_sp = T, flag_month = T, flag_ar = T, use_year = F, ar_var = "minmax"))
  expect_warning(api(a_char_month, flag_sp = T, flag_month = T, flag_ar = T, use_year = F, ar_var = "minmax"))

  expect_warning(api(a_data, flag_sp = T, flag_month = T, flag_ar = T, use_year = T, ar_var = "all"))
  expect_warning(api(a_char_month, flag_sp = T, flag_month = T, flag_ar = T, use_year = F, ar_var = "all"))

  expect_warning(api(a_noerrors, flag_sp = T, flag_month = T, flag_ar = T, use_year = F, ar_var = "all"))
  expect_warning(api(a_noerrors, flag_sp = T, flag_month = T, flag_ar = T, use_year = F, ar_var = "minmax"))
  expect_warning(api(a_noerrors, flag_sp = T, flag_month = T, flag_ar = T, use_year = F))
  expect_warning(api(a_noerrors2, flag_sp = T, flag_month = T, flag_ar = T, use_year = F, ar_var = "all"))
  expect_warning(api(a_noerrors2, flag_sp = T, flag_month = T, flag_ar = T, use_year = F, ar_var = "minmax"))
  expect_warning(api(a_noerrors2, flag_sp = T, flag_month = T, flag_ar = T, use_year = F))

  expect_error(api(a_char_month, ar_var = "l"))
  expect_error(api(a_ar_errors))
  expect_error(api(a_ar_errors, ar_var = "all"))
  expect_error(api(a_ar_errors, ar_var = "minmax"))
})

df <- data.frame(Year = c(2000,2001,2002,2003,2005),
                 Month = c(6,7,8,9,1),
                 Day = c(3,5,7,9,2),
                 Lat = c(41.334287,41.334287,41.334287,41.334287,71.312061),
                 Lon = c(-77.942020,-77.942020,-77.942020,-77.942020,-109.624424),
                 Season = c("spring","summer","fall","winter","winter"))

df2 <- data.frame(Year = c(2000),
                 Month = c(6),
                 Day = c(3),
                 Lat = c(NA),
                 Lon = c(NA),
                 Season = c("spring"))

df3 <- data.frame(Year = c(2000,2001,2002,2003),
                 Month = c(6,7,8,9),
                 Day = c(3,5,7,9),
                 Lat = c(41.334287,41.334287,41.334287,41.334287),
                 Lon = c(-77.942020,-77.942020,-77.942020,-77.942020),
                 Season = c("spring","summer","fall","winter"))
#TEST FUNCTION get_env_vars
test_that("Check to ensure that variables are extracted appropriately",{
  expect_no_error(get_env_vars(df3, env_vars="aridity", date_format = "daily"))
  expect_no_error(get_env_vars(df, env_vars="aridity", date_format = "monthly"))
  expect_no_error(get_env_vars(df, env_vars="aridity", date_format = "yearly"))
  expect_no_error(get_env_vars(df, env_vars="aridity", date_format = "seasonal"))
  expect_no_error(get_env_vars(df3, env_vars="tmp", date_format = "daily"))
  expect_no_error(get_env_vars(df, env_vars="tmp", date_format = "monthly"))
  expect_no_error(get_env_vars(df, env_vars="tmp", date_format = "yearly"))
  expect_no_error(get_env_vars(df, env_vars="tmp", date_format = "seasonal"))
  expect_no_error(get_env_vars(df3, env_vars="both", date_format = "daily"))
  expect_no_error(get_env_vars(df, env_vars="both", date_format = "monthly"))
  expect_no_error(get_env_vars(df, env_vars="both", date_format = "yearly"))
  expect_no_error(get_env_vars(df, env_vars="both", date_format = "seasonal"))
  expect_error(get_env_vars(df, env_vars="bh", date_format = "monthly"))
  expect_error(get_env_vars(df3, env_vars="bh", date_format = "daily"))
  expect_error(get_env_vars(df, env_vars="bh", date_format = "yearly"))
  expect_error(get_env_vars(df, env_vars="bh", date_format = "seasonal"))
  expect_error(get_env_vars(df, env_vars="both", date_format = "sl"))
  expect_error(get_env_vars(df2, env_vars="both", date_format = "seasonal"))

})

splist <- c("Poecile atricapillus", "Parus atricapillus","ile atricapillus")
#TEST FUNCTION syn_check
test_that("Check to ensure that variables are extracted appropriately",{
  expect_no_error(syn_check(splist))
})
