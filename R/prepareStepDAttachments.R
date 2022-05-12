#' Title
#'
#' @param d
#'
#' @return
#' @export
#'

prepareStepDAttacments <- function(d) {
  donors<-unique(d$facility_report$airtime_donor)
  working_dir<-tempdir()
  attachments<-list()

  for (i in seq_along(donors)) {
    foo<-dplyr::filter(d$facility_report, airtime_donor == donors[i]) %>%
      dplyr::select(-airtime_donor)
    attachment_name.new<-paste0(working_dir,"/stepD_facility_report_",format(as.Date(d$report_date),"%Y%m%d"),donors[i],".xlsx",sep="")
    attachments <- append(attachments,attachment_name.new)
    openxlsx::write.xlsx(foo,attachment_name.new)
  }

  #Write the failed topups to a file as well.
  bad_records_attachment <- paste0(working_dir,"/bad_facility_records_",format(as.Date(d$report_date),"%Y%m%d"),".xlsx")
  openxlsx::write.xlsx(d$facility_bad_numbers, bad_records_attachment)
  attachments <-append(attachments,bad_records_attachment)

  #Supply the raw data
  topup_data_attachment <- paste0(working_dir,"/stepD_raw_data_",format(as.Date(report_date),"%Y%m%d"),".xlsx")
  openxlsx::write.xlsx(d$raw_data,topup_data_attachment)
  attachments <-append(attachments,topup_data_attachment)

  #Create the data chw report

  donors <- unique(d$data_chw_report$airtime_donor)

  for (i in seq_along(donors)) {
    foo <- dplyr::filter(d$data_chw_report,airtime_donor == donors[i]) %>%
      dplyr::select(-airtime_donor)
  attachment_name.new <- paste0(working_dir,"/stepD_data_chw_report_",format(as.Date(d$report_date),"%Y%m%d"), donors[i],".xlsx")
  attachments <- append(attachments,attachment_name.new)
  openxlsx::write.xlsx(foo,attachment_name.new)
  }

  d$attachments <- attachments

  #Create a zip archive of all of the files

  zip_file_name<-paste(working_dir,"/stepD_report_",format(as.Date(d$report_date),"%Y%m%d"),".zip",sep="")
  zip(zipfile = zip_file_name,
      files = unlist(d$attachments),
      flags = '-r9XjD')

  #Return the list of attachments and zip file
  d$zip_file_name <- zip_file_name


  d
}
