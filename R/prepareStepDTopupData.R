#' Title
#'
#' @param report_date
#' @param d2_session
#'
#' @return
#' @export
#'
prepareStepDTopupReport <- function(report_date,d2_session) {

  d <- list()
  d <- createReportInfo()
  d$raw_data <- getStepDTopUpData(report_date,d2_session)

  #Get the raw data


  d$facility_report <- d$raw_data %>%
    dplyr::filter(qualifies & contact_phone_operator != "Unknown") %>%
    dplyr::select(contact_phone_operator,
                  airtime_donor,
                  contact_phone,
                  period_age) %>%
    dplyr::mutate(
      amount = dplyr::case_when(
        period_age <= 10 ~ 20,
        period_age > 10 &
          period_age <= 30 ~ 15,
        period_age > 30 &
          period_age <= 60 ~ 10,
        period_age > 60 ~ 5
      )
    ) %>%
    dplyr::group_by(contact_phone_operator,
                    airtime_donor,
                    contact_phone) %>%
    dplyr::summarise(amount = sum(amount), .groups = "drop") %>%
    dplyr::arrange(desc(amount)) %>%
    dplyr::mutate(VoucherType = "Direct-Topup") %>%
    dplyr::select(ServiceProvider = contact_phone_operator,
                  VoucherType,
                  Recipient = contact_phone,
                  Amount = amount,
                  airtime_donor)

  d$facility_bad_numbers <- d$raw_data %>%
    dplyr::filter(bad_facility_number) %>%
    dplyr::select(district,orgunit_name,contact_phone) %>%
    dplyr::distinct() %>%
    dplyr::arrange(district,orgunit_name)

  d$data_chw_report <- d$raw_data %>%
    dplyr::filter(qualifies & data_chw_phone_operator != "Unknown") %>%
    dplyr::select(airtime_donor,data_chw_phone, data_chw_phone_operator) %>%
    dplyr::group_by(airtime_donor,data_chw_phone, data_chw_phone_operator) %>%
    dplyr::summarise(amount = dplyr::n() * 4, .groups = "drop") %>%
    dplyr::mutate(VoucherType = "Direct-Topup") %>%
    dplyr::select(ServiceProvider = data_chw_phone_operator,
                  VoucherType,
                  Recipient = data_chw_phone,
                  Amount = amount,
                  airtime_donor)

  d$data_chw_bad_numbers <- d$raw_data %>%
    dplyr::filter(bad_chw_number) %>%
    dplyr::select(district,orgunit_name,data_chw_name,
                  data_chw_phone, storedby) %>%
    dplyr::distinct()


  d


}
