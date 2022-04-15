#' Title
#' @importFrom magrittr %>%
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
    dplyr::select(-name)

  topup_data <- getWeeklyTopupData(report_date,d2_session)
  n_raw <- NROW(topup_data)

  d$airtime_donor <- getTopupOrgunitGroups(d2_session) %>%
    dplyr::select(orgunit_uid ,airtime_donor) %>%
    dplyr::distinct()

  topup_data <- dplyr::left_join(topup_data,d$topup_users, by = "orgunit_uid") %>%
    dplyr::left_join(d$airtime_donor, by = "orgunit_uid") %>%
    dplyr::mutate(airtime_donor = dplyr::case_when(is.na(airtime_donor) ~ "Unknown",
                                                   TRUE ~ airtime_donor))

  #Include any Unknown service provider (bad or missing number), inactive users
  # or any zero rated topups.
    topup_data <- topup_data  %>%
    dplyr::mutate( status_good =  service_provider != "Unknown" &
                                   airtime_donor != "Unknown" &
                                   !is_excluded &
                                   amount != 0)

  #This is the raw data consisting of all eligible topups
  d$topup_data <- topup_data %>% dplyr::filter(status_good)
  d$bad_records <- topup_data %>% dplyr::filter(!status_good)
  d$bad_records <- d$bad_records %>%
    dplyr::mutate(error_condition = dplyr::case_when(is_excluded == TRUE  ~ "User has been excluded",
                                                     service_provider == "Unknown" ~ "Unknown service provider/Bad number",
                                                     airtime_donor == "Unknown" ~ "Unknown airtime donor",
                                                     amount == 0 ~ "Report in the future"))

  d$topup_summary <- d$topup_data  %>%
    dplyr::group_by(phoneNumber, service_provider,airtime_donor) %>%
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
