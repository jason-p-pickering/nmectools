#' Title
#'
#' @param report_date
#' @param d2_session
#'
#' @return
#' @export
#'
getWeeklyTopupData <- function(report_date,d2_session) {


  url <- paste0(
    d2_session$base_url,
    "api/sqlViews/KWG6cuaCNAU/data.csv?var=reportExDate:",
    format(report_date, format = "%Y-%m-%d")
  )

 topup_data <-  url %>%
    httr::GET(httr::timeout(180),
              handle = d2_session$handle) %>%
    httr::content(., "text") %>%
    readr::read_csv(file = ., show_col_types = FALSE) %>%
    #Calculate the amount based on the number of elapsed days
    #Note the report is set to execute on a Tuesday, so add a day
   #Anything less than 8 days indicates that the period which
   #has been reported on is the current period. Therefore, it receives
   # no topup. The first past period (8-15 days) receives 5 and so forth.

    dplyr::mutate(amount = dplyr::case_when(period_age < 8 ~ 0,
                                            period_age >= 8 & period_age < 15 ~ 5,
                                            period_age >= 15 & period_age < 22 ~ 3,
                                            period_age >=22 & period_age < 29 ~ 2,
                                            period_age >= 29 ~ 1))


}
