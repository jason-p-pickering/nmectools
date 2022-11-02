


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
    datimutils::getUserGroups(usergroup_uid, fields = "users[email]", d2_session = d2_session) %>%
      dplyr::filter(grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", email)) %>%
      dplyr::distinct() %>%
      dplyr::pull(email)

}
