
#' Title
#'
#' @param d2_session
#'
#' @return
#' @export
#'
getRapidReportUsers <- function(d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {

  #Get any users which are part of the exclusion list
  excluded_users <- datimutils::getUserGroups("KiTZhKdn5VL",
                                              fields = "users[userCredentials[username]]",
                                              d2_session = d2_session) %>%
    purrr::pluck("users") %>%
    unlist(.)

   #Get all users of the _DATASET_Mobile user group

  users <- datimutils:::api_get(paste0("api/userGroups/D0WkbASa55p?",
  "fields=users[phoneNumber,organisationUnits[id,name],",
  "userCredentials[lastLogin,username,disabled]"), d2_session = d2_session) %>%
    purrr::pluck("users") %>%
    tidyr::unnest("organisationUnits") %>%
    dplyr::mutate(phoneNumber = gsub("[[:space:]]", "", phoneNumber)) %>%
    dplyr::mutate(phoneNumber = stringr::str_replace(phoneNumber, "^[\\+]?26", "")) %>%
    dplyr::mutate(service_provider = dplyr::case_when(
      stringr::str_detect(phoneNumber, "^(076|096|073)") ~ "MTN",
      stringr::str_detect(phoneNumber, "^(097|077)") ~ "Airtel",
      stringr::str_detect(phoneNumber, "^095") ~ "Zamtel",
      TRUE ~ "Unknown")) %>%
    dplyr::rename(orgunit_uid = id) %>%
    dplyr::mutate(is_excluded = userCredentials.username %in% excluded_users)


  users

}
