#' Title
#'
#' @param d2_session
#'
#' @return
#' @export
#'

getDataCHWList <- function(d2_session) {


  url <- paste0(d2_session$base_url, "api/users?fields=firstName,surname,",
  "phoneNumber,userCredentials[username,disabled],organisationUnits[path]&paging=false")

    httr::GET(url, handle = d2_session$handle) %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("users") %>%
    tidyr::unnest("userCredentials") %>%
    dplyr::filter(disabled == FALSE) %>%
    tidyr::unnest("organisationUnits") %>%
    dplyr::mutate(data_chw_name = paste(firstName, surname),
                  level = stringr::str_count(path, "/")) %>%
    dplyr::select(data_chw_phone = phoneNumber,
                  data_chw_username = username,
                  data_chw_name,
                  level) %>%
    dplyr::filter(level >= 4) %>%
    dplyr::group_by(data_chw_phone, data_chw_username, data_chw_name) %>%
    dplyr::filter(level == max(level)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  }

#' Title
#'
#' @param d2_session
#'
#' @return
#' @export
#'

getStep3Orgunits <- function(d2_session) {

  url <- paste0(d2_session$base_url, "api/dataSets/Gso59nSpsrG?fields=",
  "organisationUnits[id,name,contactPerson,phoneNumber]")

  httr::GET(url, handle = d2_session$handle) %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("organisationUnits") %>%
    dplyr::rename(orgunit_name = name,
                  orgunit_uid = id,
                  contact_phone = phoneNumber,
                  contact_person = contactPerson)
}
