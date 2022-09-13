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
