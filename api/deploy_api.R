## setting things up
##--------------------------------------------------------------------------
require(analogsea)
require(plumber)

do_oauth(reauth = TRUE)

covidnews_do <- plumber::do_provision(name = "covidnews",
                                      example = FALSE)

droplet_wait()

install_r_package(covidnews_do, "dplyr")
install_r_package(covidnews_do, "purrr")

do_deploy_api(covidnews_do, "covidnews", "api", 8000)