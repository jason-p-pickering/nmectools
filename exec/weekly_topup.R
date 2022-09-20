# Scripts for local testing
# require(nmectools)
# #Get this path from an environment variable
# loginToDHIS2("/home/jason/.secrets/nmec.json")
# #Get this from the command line
# report_date <- "2022-09-13"
#
# d <- prepareWeeklyTopupData(report_date,d2_session = d2_default_session)
#
#
# sendWeeklyReportByEmail(d)
# sendWeeklyTelegramReport(d)
#
# #Be sure we are in the right directory


.libPaths("/var/lib/dhis2/dhis/scripts/nmectools/renv/library/R-4.1/x86_64-pc-linux-gnu")
setwd("/var/lib/dhis2/dhis/scripts/nmectools")

source("renv/activate.R")
source(".Rprofile")

require(nmectools)
loginToDHIS2("/var/lib/dhis2/dhis/dish.json")
#Start to calculate the topup report
cmd_args<-commandArgs();
#TODO make this more robust
report_date<-as.Date(cmd_args[6])
if (is.na(report_date)) { reportExDate<-Sys.Date()}
d <- prepareWeeklyTopupData(report_date, d2_session = d2_default_session)
sendWeeklyReportByEmail(d)
sendWeeklyTelegramReport(d)
