#' Title
#'
#' @param d Consolidate data object
#' @param lucr_api_key Lucr API key to convert currencies
#'
#' @return
#' @export
#'

createStepDSummaryTables <- function(d) {

  d$facility_summary_table <- d$facility_report %>%
    dplyr::group_by(airtime_donor) %>%
    dplyr::summarise(Amount = sum(Amount)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(USD = lucr::currency_convert(Amount,from="ZMW",to="USD",key=
                                                 d$info$lucr_api_key)) %>%
    dplyr::mutate(airtime_donor = as.character(airtime_donor)) %>%
    dplyr::bind_rows(data.frame(airtime_donor = "Total",
                                Amount = sum(.$Amount),
                                USD = sum(.$USD),
                                stringsAsFactors = FALSE)) %>%
    dplyr::mutate(USD = format(round(USD,2),big.mark = ",",nsmall=2),
                  Amount = format(Amount,big.mark = ",", nsmall=0)) %>%
    dplyr::rename("Donor" = airtime_donor,
                  "Amount (ZMW)" = Amount,
                  "Amount (USD)" = USD)


  d$facility_summary_table_html <- tableHTML::tableHTML(d$facility_summary_table
                                                        ,caption = "CHW Summary")

  #Export the image as a PNG
  table_image <- paste0(tempfile(),".png")
  d$facility_summary_table_html_png <- table_image
  Sys.setenv(OPENSSL_CONF="/etc/ssl phantomjs --version")
  tableHTML::tableHTML_to_image(tableHTML::tableHTML(d$facility_summary_table),
                                file = table_image,type = "png")


  #Create the DataCHW summary table

  d$data_chw_summary_table <- d$data_chw_report %>%
    dplyr::group_by(airtime_donor) %>%
    dplyr::summarise(Amount = sum(Amount)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(USD = lucr::currency_convert(Amount,from="ZMW",to="USD",key=
                                                 d$info$lucr_api_key)) %>%
    dplyr::mutate(airtime_donor = as.character(airtime_donor)) %>%
    dplyr::bind_rows(data.frame(airtime_donor = "Total",
                                Amount = sum(.$Amount),
                                USD = sum(.$USD),
                                stringsAsFactors = FALSE)) %>%
    dplyr::mutate(USD = format(round(USD,2),big.mark = ",",nsmall=2),
                  Amount = format(Amount,big.mark = ",", nsmall=0)) %>%
    dplyr::rename("Donor" = airtime_donor,
                  "Amount (ZMW)" = Amount,
                  "Amount (USD)" = USD)


  d$data_chw_summary_table_html <- tableHTML::tableHTML(d$data_chw_summary_table
                                                        ,caption = "Data CHW Summary")

  #Export the image as a PNG
  table_image <- paste0(tempfile(),".png")
  d$data_chw_summary_table_html_png <- table_image
  Sys.setenv(OPENSSL_CONF="/etc/ssl phantomjs --version")
  tableHTML::tableHTML_to_image(tableHTML::tableHTML(d$data_chw_summary_table),
                                file = table_image,type = "png")


  d

}
