if (interactive() && stringr::str_detect(getwd(), "Packages")) {
  suppressMessages(require(devtools))
}

.pkg_name <- "zibas"
reinstall <- function() {
  if (.pkg_name %in% installed.packages()) {
    remove.packages(.pkg_name, lib = "/Users/josh/.rlibs/common/4.1")
  }
  devtools::load_all()
  devtools::document()
  devtools::install()
}

devtools::load_all()