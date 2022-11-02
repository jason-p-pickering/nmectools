
.libPaths("/var/lib/dhis2/dhis/scripts/nmectools/renv/library/R-4.1/x86_64-pc-linux-gnu")
setwd("/var/lib/dhis2/dhis/scripts/nmectools")

source("renv/activate.R")
source(".Rprofile")

#Get this path from an environment variable


require(magrittr)
require(dplyr)
require(purrr)
require(nmectools)

loginToDHIS2("/var/lib/dhis2/dhis/dish.json")

getMIDPUserAgents <- function(path) {

  parsed_log <- extractLogFile(path)

  parsed_log %>%
    dplyr::select(event_date = V4,
                  username = V3,
                  useragent = V10) %>%
    dplyr::filter(grepl("MIDP",useragent)) %>%
    dplyr::mutate(event_date = gsub("^\\[","",event_date)) %>%
    dplyr::mutate(event_date = strptime(event_date, format ="%d/%b/%Y:%T ")) %>%
    dplyr::group_by(username,useragent) %>%
    dplyr::arrange(desc(event_date)) %>%
    dplyr::top_n(n = 1, wt = event_date) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

}

extractLogFile <- function(path) {
  is_zipped <- grepl("gz$",path)
  if (is_zipped) {
    parsed_log <- read.table(zz <- gzfile(path,'rt'), fill = TRUE)
    close(zz)
  } else {
    parsed_log <- read.table(path, fill = TRUE)
  }

  parsed_log
}


ougs <- getOrgUnitStructure(d2_session = d2_default_session)

mobile_users <- datimutils:::api_get(paste0("api/users?",
                                     "fields=phoneNumber,organisationUnits[id,name],",
                                     "userCredentials[lastLogin,username,disabled]"), d2_session = d2_default_session) %>%
  purrr::pluck("users") %>%
  tidyr::unnest("organisationUnits") %>%
  dplyr::select(phoneNumber,
    orgunit_uid = id,
    name,
    username = userCredentials.username) %>%
  dplyr::distinct() %>%
  dplyr::group_by(phoneNumber,username) %>%
  dplyr::top_n(n = 1, wt = name) %>%
  dplyr::inner_join(ougs,by="orgunit_uid") %>%
  dplyr::select(phoneNumber,
                user_orgunit = name,
                username,
                province,
                district,
                facility)


#Get a list of all files
log_files <- list.files(path = "/var/log/nginx", pattern = "^access", full.names = TRUE)

user_agents <- purrr::map_dfr(log_files,getMIDPUserAgents) %>%
  dplyr::group_by(username,useragent) %>%
  dplyr::arrange(desc(event_date)) %>%
  dplyr::top_n(n = 1, wt = event_date) %>%
  dplyr::ungroup() %>%
  dplyr::inner_join(mobile_users, by = "username") %>%
  dplyr::distinct()
