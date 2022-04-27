#' Title
#'
#' @param d Consolidate data object
#' @param lucr_api_key Lucr API key to convert currencies
#'
#' @return
#' @export
#'

createTopupSummaryTables <- function(d) {

  topup_summary_table <- d$topup_summary %>%
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
    dplyr::mutate(USD = sprintf("$%.2f",USD)) %>%
    dplyr::rename("Donor" = airtime_donor,
                  "Amount (ZMW)" = Amount,
                  "Amount (USD)" = USD)


  d$topup_summary_table <- topup_summary_table

  d$topup_summary_table_html <- knitr::kable(topup_summary_table,"html",
                                             align = rep("r",length(topup_summary_table[,1]))) %>%
  kableExtra::kable_styling(bootstrap_options = "bordered", full_width = FALSE)

  d$topup_summary_table_md <- knitr::kable(topup_summary_table, "markdown")

  d

}
