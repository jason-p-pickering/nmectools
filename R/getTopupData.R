#' Title
#'
#' @param report_date
#' @param d2_session
#'
#' @return
#' @export
#'
getWeeklyTopupData <- function(report_date, d2_session) {


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
                                            period_age >= 22 & period_age < 29 ~ 2,
                                            period_age >= 29 ~ 1))


}



#' Title
#' @import assertthat
#' @param report_date
#' @param d2_session
#'
#' @return
#' @export
#'
getStepDTopUpData <- function(report_date, d2_session) {


   ougs <- getOrgUnitStructure(d2_session) %>%
     dplyr::select(orgunit_uid, uidlevel3, district) %>%
     tidyr::drop_na()

   #Get the DataCHWSs list
   #Note that the orgunits are nested
   data_chws <- getDataCHWList(d2_session)
   #Get the organisation unit details
   orgunits <- getStep3Orgunits(d2_session)
   #Get airtime donors
   orgunit_groups <- getTopupOrgunitGroups(d2_session)

  url  <- paste0(
    d2_session$base_url,
    "api/sqlViews/ZvPV5OXLp3x/data.csv?var=reportExDate:",
    format(report_date, format = "%Y-%m-%d")
  )
  response <- httr::GET(url, httr::timeout(600),
              handle = d2_session$handle)

    if (response$status_code == 200L) {
      raw_data <-
        response %>%
        httr::content(., "text") %>%
        readr::read_csv(file = ., col_types = readr::cols(.default = "c"))  %>%
        dplyr::mutate(orgunit_uid = uid,
                      startdate = as.Date(startdate),
                      enddate = as.Date(enddate),
                      submission_date = as.Date(submission_date),
                      first_date = as.Date(first_date),
                      report_age = as.numeric(report_age),
                      period_age = as.numeric(period_age)) %>%
        dplyr::filter(record_count > 0)
    } else {
      stop("Could not retreive StepD raw data from the server!")
    }


 start_rows <- NROW(raw_data)

  raw_data <- raw_data %>%  dplyr::left_join(ougs, by = "orgunit_uid")
  assertthat::are_equal(NROW(raw_data), start_rows)

  raw_data <- raw_data %>% dplyr::left_join(orgunits, by = "orgunit_uid")
  assertthat::are_equal(NROW(raw_data), start_rows)

  #Join the data chws
  raw_data <- raw_data %>%
    dplyr::left_join((
      data_chws %>% dplyr::select(data_chw_phone, data_chw_username, data_chw_name)
    ),
    by = c("storedby" = "data_chw_username"))
  assertthat::are_equal(NROW(raw_data), start_rows)

  #Join the trained by group
  raw_data <- raw_data %>%
      dplyr::left_join(orgunit_groups, by = "orgunit_uid") %>%
      dplyr::mutate(trained_by = dplyr::case_when(is.na(trained_by) ~ "MACEPA Trained",
                                                TRUE ~ trained_by),
                  airtime_donor = dplyr::case_when(is.na(airtime_donor) ~ "MACEPA",
                                                   TRUE ~ airtime_donor))
  assertthat::are_equal(NROW(raw_data), start_rows)


  #Classify the facility operator

    raw_data <- raw_data %>% classifyMobileOperator(contact_phone, "contact_phone_operator")
    raw_data <- raw_data %>% classifyMobileOperator(data_chw_phone, "data_chw_phone_operator")

    raw_data <- raw_data %>%
      dplyr::mutate(missing_mandatory_des = record_count != 9,
                  report_in_future = period_age < 0,
                  not_monthly = !stringr::str_detect(iso, "^20[12][0-9]{3}"),
                  bad_facility_number = contact_phone_operator == "Unknown",
                  bad_chw_number = data_chw_phone_operator == "Unknown",
                  unknown_donor = airtime_donor == "Unknown",
                  qualifies = !(missing_mandatory_des | report_in_future | not_monthly),
                  elapsed_months = (lubridate::interval(enddate, as.Date(report_date)) %/% months(1)))

    raw_data
  }
