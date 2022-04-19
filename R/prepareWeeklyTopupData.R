#' Title
#' @importFrom magrittr %>%
#' @param report_date Date as a character in format YYYY-MM-DD
#' @param d2_session d2_session object
#'
#' @return
#' @export
#'


getWeeklyRawData <- function(report_date,d2_session) {
  url <-
    paste0(
      d2_session$base_url,
      "api/sqlViews/PaIP00gzoyN/data.csv?var=reportExDate:",
      report_date
    )

topup_data <-  url %>%
  httr::GET(httr::timeout(180),
            handle = d2_session$handle) %>%
  httr::content(., "text") %>%
  readr::read_csv(file = ., show_col_types = FALSE) %>%
  dplyr::rename(orgunit_uid = uid,
                orgunit_name = name,
                submitted_by = username) %>%
  dplyr::group_by(orgunit_uid,orgunit_name,iso,startdate,submitted_by) %>%
  #Take the last submission if there have been several
  dplyr::mutate(is_last_submission = submission_date == max(submission_date)) %>%
  dplyr::filter(is_last_submission) %>%
  #Calculate the age of the report
  dplyr::ungroup() %>%
  dplyr::mutate(period_age =  as.Date(report_date) - startdate) %>%
  dplyr::mutate(amount = dplyr::case_when(period_age < 8 ~ 0,
                                          period_age >= 8 & period_age < 15 ~ 5,
                                          period_age >= 15 & period_age < 22 ~ 3,
                                          period_age >=22 & period_age < 29 ~ 2,
                                          period_age >= 29 ~ 1))

topup_data
}


#' Title
#' @param report_date Date as a character in format YYYY-MM-DD
#' @param d2_session d2_session object
#'
#' @return
#' @export
#'


prepareWeeklyTopupData <- function(report_date,d2_session) {

  d<-createReportInfo()
  d$report_date <- report_date

  d$topup_users <- getRapidReportUsers(d2_session) %>%
    dplyr::select(-name) %>%
    dplyr::rename(facility_username = userCredentials.username)

  topup_data <- getWeeklyRawData(report_date,d2_session)
  n_raw <- NROW(topup_data)

  d$airtime_donor <- getTopupOrgunitGroups(d2_session) %>%
    dplyr::select(orgunit_uid ,airtime_donor) %>%
    dplyr::distinct()

  #Join by the orgunit
  topup_data <- dplyr::left_join(topup_data,d$topup_users, by = "orgunit_uid") %>%
    dplyr::left_join(d$airtime_donor, by = "orgunit_uid") %>%
    dplyr::mutate(airtime_donor = dplyr::case_when(is.na(airtime_donor) ~ "Unknown",
                                                   TRUE ~ airtime_donor)) %>%
  #Include any Unknown service provider (bad or missing number), inactive users
  # or any zero rated topups.
  dplyr::mutate( status_good =  service_provider != "Unknown" &
                                   !is_excluded &
                                   amount != 0) %>%
  dplyr::mutate(submitted_by_proxy = facility_username != submitted_by)

  #This is the raw data consisting of all eligible topups
  d$topup_data <- topup_data %>% dplyr::filter(status_good)
  d$bad_records <- topup_data %>% dplyr::filter(!status_good)
  d$bad_records <- d$bad_records %>%
    dplyr::mutate(error_condition = dplyr::case_when(is_excluded == TRUE  ~ "User has been excluded",
                                                     service_provider == "Unknown" ~ "Unknown service provider/Bad number",
                                                     amount == 0 ~ "Report in the future"))

  d$topup_summary <- d$topup_data  %>%
    dplyr::group_by(phoneNumber, service_provider,airtime_donor) %>%
    #Remove duplicates if any
    dplyr::summarise(amount = sum(amount)) %>%
    dplyr::mutate(VoucherType = "Direct-Topup") %>%
    dplyr::select(ServiceProvider = service_provider,
                  Recipient = phoneNumber,
                  Amount = amount,
                  airtime_donor)

  message("Creating summary tables")
  d <- createTopupSummaryTables(d)
  message("Creating attachment files")
  d <- writeAttachmentFiles(d)

  d

}
