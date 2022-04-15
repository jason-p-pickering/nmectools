#' Title
#'
#' @return
#' @export
#'

createReportInfo <- function() {

   d <- list()

  info = list(
    lucr_api_key = Sys.getenv("lucr_api_key"),
    telegram_token = Sys.getenv("telegram_token"),
    telegram_chat_id = Sys.getenv("telegram_chat_id")
  )

  d$info <- info

  d
}
