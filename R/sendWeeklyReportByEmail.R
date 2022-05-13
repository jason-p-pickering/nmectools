#' Title
#'
#' @param d Data object
#' @param d2_session datimutils session
#'
#' @return
#' @export
#'

sendWeeklyReportByEmail<-function(d,d2_session) {
  #Format the mail
  from<-"noreply@dhis.co.zm"
  subject<-paste("Weekly Topup Report for ", as.Date(d$report_date)- lubridate::days(7) , "-",d$report_date)

  msg <- sendmailR::mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  <title>Topup Summary</title>
</head>
<body>',d$topup_summary_table_html,
                                    '</body>
</html>'))

  msg[["headers"]][["Content-Type"]] <- "text/html"
  to<-getReportRecipients(d2_session)
  body<-list(body_text = msg, sendmailR::mime_part(d$zip_file_name))
  sendmailR::sendmail(from,to,subject,body)
}



#' Title
#'
#' @param d
#' @param d2_session
#'
#' @return
#' @export
#'
sendStepDReportByEmail<-function(d,d2_session) {

  #Format the mail
  from<-"noreply@dhis.co.zm"
  subject<-paste("Step D Monthly Report for ", d$report_date)

  msg <- sendmailR::mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  <title>Topup Summary</title>
</head>
<body>',as.character(d$facility_summary_table_html), "</>",
                    as.character(d$data_chw_summary_table_html),
                                    '</body>
</html>'))

  msg[["headers"]][["Content-Type"]] <- "text/html"
  to<-getReportRecipients("WiQAkLe2M2q",d2_session)
  body<-list(body_text = msg, sendmailR::mime_part(d$zip_file_name))
  sendmailR::sendmail(from,to,subject,body)
}
