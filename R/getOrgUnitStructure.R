#' Title
#'
#' @param d2_session
#'
#' @return
#' @export
#'
getOrgUnitStructure <- function(d2_session)  {

    response <- paste0(d2_session$base_url,
                "api/sqlViews/CvYLXS2pyQv/data.csv") %>%
    httr::GET(httr::timeout(180),
              handle = d2_session$handle)

    if (response$status_code == 200L) {
      response %>%
      httr::content(., "text") %>%
        readr::read_csv(file = ., show_col_types = FALSE)
    } else
    {
      stop("Could not retreive orgunit structure view.")
    }


}
