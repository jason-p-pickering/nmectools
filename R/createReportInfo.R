#' Title
#'
#' @return
#' @export
#'

createReportInfo <- function(report_date) {

   d <- list()

   d$report_date  <- tryCatch({as.Date(report_date)},
            error = function(e) {
              message("The report date does not appear to be valid")
              stop()
            })

  d$info = list(
    lucr_api_key = Sys.getenv("lucr_api_key"),
    telegram_token = Sys.getenv("telegram_token"),
    telegram_chat_id = Sys.getenv("telegram_chat_id")
  )

  d
}
