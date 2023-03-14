
.libPaths("/var/lib/dhis2/dhis/scripts/nmectools/renv/library/R-4.2/x86_64-pc-linux-gnu")
setwd("/var/lib/dhis2/dhis/scripts/nmectools")

source("renv/activate.R")
source(".Rprofile")


require(nmectools)
require(magrittr)
#Get this path from an environment variable
loginToDHIS2("/var/lib/dhis2/dhis/dish.json")

#Get this from the command line
cmd_args<-commandArgs();
#TODO make this more robust
report_date<-as.Date(cmd_args[6])
if (is.na(report_date)) { report_date <- Sys.Date()}

d <- prepareStepDTopupReport(report_date, d2_session = d2_default_session)
d <- createStepDSummaryTables(d)
d <- prepareStepDAttacments(d)


sendStepDTelegramReport(d)
sendStepDReportByEmail(d, d2_session = d2_default_session)
