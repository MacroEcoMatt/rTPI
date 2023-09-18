.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Using Function: get_env_vars requires installing the climateR package from gitHub",
                        'use the following code: devtools::install_github("mikejohnson51/climateR")')
}
