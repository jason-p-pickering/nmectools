writeAttachmentFiles <- function(d) {
  donors<-unique(d$topup_summary$airtime_donor)
  working_dir<-tempdir()
  attachments<-list()

  for (i in seq_along(donors)) {
    foo<-dplyr::filter(d$topup_summary, airtime_donor == donors[i]) %>%
      dplyr::select(-airtime_donor)
    attachment_name.new<-paste(working_dir,"/topup_report_",format(as.Date(d$report_date),"%Y%m%d"),"_",donors[i],".xlsx",sep="")
    attachments[i]<-attachment_name.new
    openxlsx::write.xlsx(foo,attachment_name.new)
  }

  #Write the failed topups to a file as well.
  bad_records_attachment <- paste0(working_dir,"/bad_records_",format(as.Date(d$report_date),"%Y%m%d"),".xlsx")
  openxlsx::write.xlsx(d$bad_records,bad_records_attachment)
  attachments <-append(attachments,bad_records_attachment)
  #Supply the raw data
  topup_data_attachment <- paste0(working_dir,"/topup_data_",format(as.Date(report_date),"%Y%m%d"),".xlsx")
  openxlsx::write.xlsx(d$topup_data,topup_data_attachment)
  attachments <-append(attachments,topup_data_attachment)

  d$attachments <- attachments

  #Create a zip archive of all of the files
  wd<-tempdir()
  zip_file_name<-paste(wd,"/topup_report_",format(as.Date(d$report_date),"%Y%m%d"),".zip",sep="")
  zip(zipfile = zip_file_name,
      files = unlist(d$attachments),
      flags = '-r9XjD')

  d$zip_file_name <- zip_file_name

  d
}
