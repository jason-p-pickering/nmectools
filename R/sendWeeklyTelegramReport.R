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

  bot <- Bot(token = d$info$telegram_token)
  #Save the table as an image
  table_image <- paste0(tempfile(),".png")
  kable(d$topup_summary_table, "html") %>%
    kable_styling("striped") %>%
    kableExtra::save_kable(table_image)
  bot$sendPhoto(d$info$telegram_chat_id, table_image)


  # Send document
  bot$sendDocument(d$info$telegram_chat_id,
                   document = d$zip_file_name
  )

}
