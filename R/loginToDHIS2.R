#' Title
#' @name loginToDHIS2
#' @description Essentially a re-export
#' of loginToDATIM from the datimutils package
#' @param config_path Path to a configuration file
#' @param config_path_level Config path level
#' @param username Username if not using a config
#' @param password Password if not using a config
#' @param base_url Base URL if not using a config
#' @param d2_session_name Name of the session
#' @param d2_session_envir Environment of the session
#'
#' @return
#' @export
#'

loginToDHIS2 <- function(
  config_path = NULL,
  config_path_level = "dhis",
  username = NULL,
  password = NULL,
  base_url = NULL,
  d2_session_name = "d2_default_session",
  d2_session_envir = parent.frame()
) {
  datimutils::loginToDATIM(
    config_path = config_path,
    config_path_level = config_path_level,
    username = username,
    password = password,
    base_url = base_url,
    d2_session_name = d2_session_name,
    d2_session_envir = d2_session_envir)
}
