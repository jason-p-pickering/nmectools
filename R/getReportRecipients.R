


#' Title
#'
#' @param usergroup_uid
#' @param d2_session
#'
#' @return
#' @export
#'

getReportRecipients <- function(usergroup_uid, d2_session = dynGet("d2_default_session",
                                                                     inherits = TRUE)) {
  url <- paste0("users?fields=email&filter=userGroups.id:in:[",usergroup_uid,"]")
  datimutils:::api_get(url, d2_session = d2_session) %>%
    purrr::pluck("users") %>%
      dplyr::filter(grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", email)) %>%
      dplyr::distinct() %>%
      dplyr::pull(email)

}
