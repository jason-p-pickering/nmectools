#' Title
#'
#' @param d Consolidate data object
#' @param lucr_api_key Lucr API key to convert currencies
#'
#' @return
#' @export
#'

createTopupSummaryTables <- function(d) {



  ex_rate <- getUSDExchangeRate(d$info$lucr_api_key)

  topup_summary_table <- d$topup_summary %>%
    dplyr::group_by(airtime_donor) %>%
    dplyr::summarise(Amount = sum(Amount)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(USD =  Amount * ex_rate) %>%
    dplyr::mutate(airtime_donor = as.character(airtime_donor)) %>%
    dplyr::bind_rows(data.frame(airtime_donor = "Total",
                                Amount = sum(.$Amount),
                                USD = sum(.$USD),
                                stringsAsFactors = FALSE)) %>%
    dplyr::mutate(USD = sprintf("$%.2f", USD)) %>%
    dplyr::rename("Donor" = airtime_donor,
                  "Amount (ZMW)" = Amount,
                  "Amount (USD)" = USD)


  d$topup_summary_table <- topup_summary_table

  d$topup_summary_table_html <- tableHTML::tableHTML(topup_summary_table)

  #Export the image as a PNG
  table_image <- paste0(tempfile(), ".png")
  d$topup_summary_table_html_png <- table_image
  Sys.setenv(OPENSSL_CONF = "/etc/ssl phantomjs --version")
  tableHTML::tableHTML_to_image(tableHTML::tableHTML(d$topup_summary_table),
                                file = table_image, type = "png")

  #Create a markdown version of the table
  d$topup_summary_table_md <-
    knitr::kable(topup_summary_table, "markdown",
                 align = rep("r", length(topup_summary_table[, 1])))

  d

}
