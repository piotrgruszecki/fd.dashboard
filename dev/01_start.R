# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "fd.dashboard", # The Name of the package containing the App
  pkg_title = "Shiny Dashboard", # The Title of the package containing the App
  pkg_description = "Dashboard presenting Brightspot data, using Shiny.", # The Description of the package containing the App
  author_first_name = "Piotr", # Your First Name
  author_last_name = "Gruszecki", # Your Last Name
  author_email = "piotr.gruszecki@altamedia.pl", # Your Email
  repo_url = "https://github.com/piotrgruszecki/fd_dashboard.git" # The URL of the GitHub Repo (optional)
)

## Set {golem} options ----
golem::set_golem_options()
golem::amend_golem_config(key = "dbname",   value = "fd_reports")
golem::amend_golem_config(key = "username", value = "fd_reports_user")
golem::amend_golem_config(key = "password", value = "6Pb27%n_jH6J^@r&gJR-KbSxp--+8TBK")
golem::amend_golem_config(key = "host",     value = "rdsamazon.cbief3wnjkp9.eu-west-1.rds.amazonaws.com")
golem::amend_golem_config(key = "encoding", value = "latin1")

golem::amend_golem_config(key = "table_leads_clean", value = "bs_leads_clean")
golem::amend_golem_config(key = "table_leads_log", value = "bs_leads_log")

golem::amend_golem_config(key = "table_profiles_clean", value = "bs_profiles_clean_tmp")
golem::amend_golem_config(key = "table_profiles_log", value = "bs_profiles_log")

golem::amend_golem_config(key = "table_geo_timezones", value = "geo_tz")

golem::amend_golem_config(key = "plot.color",       value = "#CA0813")
golem::amend_golem_config(key = "plot.color.light", value = "#FC9272")
golem::amend_golem_config(key = "line_thickness",   value = 1)
golem::amend_golem_config(key = "spinner.color",    value = "#0dc5c1")


## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license(copyright_holder = "altamedia OÜ" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()
usethis::use_github()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
golem::remove_favicon()

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

