


#' Title
#'
#' @param usergroup_uid
#' @param d2_session
#'
#' @return
#' @export
#'

getReportRecipients <-function(usergroup_uid,d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {
  user_emails <- datimutils::getUserGroups(usergroup_uid,
                                           fields = "users[email]") %>%
    dplyr::pull(email)

  is_valid_email <- grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$",user_emails)

  user_emails[is_valid_email]
}
