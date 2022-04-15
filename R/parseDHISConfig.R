
#' Title
#'
#' @param filepath
#'
#' @return A dataframe of key value pairs parsed
#' from a DHIS2 configuration file
#' @export
#'

parseDHISConf<-function(filepath) {

  conn <- file(filepath,open="r")
  linn <-readLines(conn)
  close(conn)
  foo<-data.frame(do.call('rbind', strsplit(as.character(linn),'=',fixed=TRUE)),stringsAsFactors=FALSE)

  transform(foo,key=str_trim(X1),value=str_trim(X2))


}
