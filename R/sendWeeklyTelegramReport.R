#' Title
#'
#' @import kableExtra
#' @import telegram.bot
#' @param d Data object
#'
#' @return
#' @export
#'

sendWeeklyTelegramReport <- function(d) {

  bot <- telegram.bot::Bot(token = d$info$telegram_token)
  bot$sendPhoto(d$info$telegram_chat_id, d$topup_summary_table_html_png)
  # Send document
  bot$sendDocument(d$info$telegram_chat_id,
                   document = d$zip_file_name
  )

}


#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
sendStepDTelegramReport <- function(d) {

  bot <- telegram.bot::Bot(token = d$info$telegram_token)
  message <- paste0("*StepD Topup Report Summary* \r\n",
                    "Date: ", d$info$report_date, "\r\n",
                    "Qualifying reports: ", sum(d$raw_data$qualifies), "\r\n",
                    "Disqualified reports: ", sum(!d$raw_data$qualifies), "\r\n",
                    "Unknown Facility numbers:", NROW(d$facility_bad_numbers), "\r\n",
                    "Unknown Data CHW numbers: ", length(unique(d$data_chw_bad_numbers$storedby)),"\r\n",
                    "Facility total: ",
                   d$facility_summary_table %>% dplyr::filter(Donor == "Total") %>% dplyr::pull(2),
                   " ZMW (",
                   d$facility_summary_table %>% dplyr::filter(Donor == "Total") %>% dplyr::pull(3),
                   "USD)",
                   "\r\n Data CHW total: ",
                   d$data_chw_summary_table %>% dplyr::filter(Donor == "Total") %>% dplyr::pull(2),
                   " ZMW (",
                   d$data_chw_summary_table %>% dplyr::filter(Donor == "Total") %>% dplyr::pull(3),
                   "USD)")
  bot$send_message(d$info$telegram_chat_id,message,parse_mode = "Markdown")
  bot$sendDocument(d$info$telegram_chat_id,
                   document = d$zip_file_name)
  }
