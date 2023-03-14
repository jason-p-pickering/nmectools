#' Title
#' @import tidyr
#' @import datimutils
#' @param d2_session
#'
#' @return
#' @export
#'
#' @examples
getAirtimeDonorGroup <- function(d2_session = dynGet("d2_default_session",
                               inherits = TRUE)) {
    datimutils::getOrgUnitGroupSets("qiglQewaM4q",
                                    fields = "organisationUnitGroups[id,name,organisationUnits[id]]",
                                    d2_session = d2_session) %>%
      tidyr::unnest(organisationUnitGroups, names_sep = ".") %>%
      tidyr::unnest(organisationUnitGroups.organisationUnits, names_sep = ".") %>%
      dplyr::select(
        orgunit_uid = organisationUnitGroups.organisationUnits.id,
        airtime_donor = organisationUnitGroups.name,
        airtime_donor_id = organisationUnitGroups.id
      )
  }

#' Title
#'
#' @param d2_session
#'
#' @return
#' @export
#'
getTrainedByGroup <- function(d2_session = dynGet("d2_default_session",
                               inherits = TRUE)) {

    datimutils::getOrgUnitGroupSets("l8Pc8wpIVw1",
                                    fields = "organisationUnitGroups[id,name,organisationUnits[id]]",
                                    d2_session = d2_session) %>%
      tidyr::unnest(organisationUnitGroups, names_sep = ".") %>%
      tidyr::unnest(organisationUnitGroups.organisationUnits, names_sep = ".") %>%
      dplyr::select(
        orgunit_uid = organisationUnitGroups.organisationUnits.id,
        trained_by = organisationUnitGroups.name,
        trained_by_id = organisationUnitGroups.id
      )
  }

#' Title
#' @import httr
#' @import readr
#' @param d2_session
#'
#' @return
#' @export
#'

getTopupOrgunitGroups <-
  function(d2_session = dynGet("d2_default_session",
                               inherits = TRUE)) {
    # airtime_donor_groups <- getAirtimeDonorGroup(d2_session)
    # trained_by_groups <- getTrainedByGroup(d2_session)
    #
    # dplyr::full_join(airtime_donor_groups, trained_by_groups, by = "orgunit_id")

    #We will use the old way, until the duplicative orgunit group members
    #problem is solved

    url <- paste0(d2_session$base_url, "api/sqlViews/ff28isLPBSk/data.csv")

    response <- httr::GET(url, httr::timeout(180),
                handle = d2_session$handle)

      if (response$status_code == 200L) {
         response %>%
          httr::content(., "text") %>%
          readr::read_csv(file = ., show_col_types = FALSE)
      } else {
        stop("Could not retreive topup orgunit groups!")
      }


  }
