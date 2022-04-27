#' Title
#'
#' @param d2_session
#'
#' @return
#' @export
#'
getOrgUnitStructure <- function(d2_session)  {

    paste0(d2_session$base_url,
                "api/sqlViews/CvYLXS2pyQv/data.csv") %>%
    httr::GET(httr::timeout(180),
              handle = d2_session$handle) %>%
    httr::content(., "text") %>%
    readr::read_csv(file = ., show_col_types = FALSE)

}

