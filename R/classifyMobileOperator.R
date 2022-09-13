#' Title
#' @import dplyr
#' @import magrittr
#' @import stringr
#'
#' @param df
#' @param phone_var Name of the variable which contains the phone number
#' @param output_var Name of the output variable which should describe the donor
#'
#' @return
#' @export
#'
classifyMobileOperator <- function(df, phone_var, output_var) {

  phone_var <- dplyr::enquo(phone_var)
  output_var <- dplyr::enquo(output_var)

  df %>%
  dplyr::mutate(!!phone_var := gsub("[[:space:]]", "", !!phone_var)) %>%
  dplyr::mutate(!!phone_var := stringr::str_replace(!!phone_var, "^[\\+]?26", "")) %>%
  dplyr::mutate(!!output_var := dplyr::case_when(
    stringr::str_detect(!!phone_var, "^(076|096|073)[0-9]{7}$") ~ "MTN",
    stringr::str_detect(!!phone_var, "^(097|077)[0-9]{7}$") ~ "Airtel",
    stringr::str_detect(!!phone_var, "^095[0-9]{7}") ~ "Zamtel",
    TRUE ~ "Unknown"))
}
