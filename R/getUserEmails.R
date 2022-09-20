


report_users <- function(d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {
  user_emails <- datimutils::getUserGroups("WiQAkLe2M2q",
                                           fields = "users[email]") %>%
    dplyr::pull(email)

  is_valid_email <- grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", user_emails)

  user_emails[is_valid_email]
}
