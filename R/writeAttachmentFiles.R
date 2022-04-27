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

  #Create a list of users with bad phone numbers

  d$users_bad_phonenumbers <-d$topup_users %>%
    dplyr::filter(service_provider == 'Unknown' &
                    !is_excluded) %>%
    dplyr::left_join(d$orgunit_structure) %>%
    dplyr::select(province,district,facility,health_post,
      facility_username,
                  phoneNumber,
                  last_login = userCredentials.lastLogin,
                  disabled = userCredentials.disabled) %>%
    dplyr::distinct() %>%
    dplyr::mutate(last_login = strptime(last_login, "%Y-%m-%dT%H:%M:%S")) %>%
    dplyr::arrange(dplyr::desc(last_login))

    topup_data_attachment <- paste0(working_dir,"/users_bad_phonenumbers_",format(as.Date(report_date),"%Y%m%d"),".xlsx")
    openxlsx::write.xlsx(d$users_bad_phonenumbers,topup_data_attachment)
    attachments <-append(attachments,topup_data_attachment)


  d$attachments <- attachments

  #Create a zip archive of all of the files
  wd<-tempdir()
  zip_file_name<-paste(wd,"/topup_report_",format(as.Date(d$report_date),"%Y%m%d"),".zip",sep="")
  zip(zipfile = zip_file_name,
      files = unlist(d$attachments),
      flags = '-r9XjD')

  #Return the list of attachments and zip file
  d$zip_file_name <- zip_file_name


  d
}
