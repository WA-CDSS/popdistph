#' Convert Washington counties to local health jurisdictions
#'
#' Maps Washington State county names to the name of the local health
#' jurisdiction (LHJ) that serves them. Most counties have their own health
#' department, so their names are returned unchanged. Counties served by a
#' multi-county health district are mapped to that district's name:
#'
#' * Ferry, Pend Oreille, and Stevens: `"Tri-County"`
#' * Benton and Franklin: `"Benton-Franklin"`
#' * Chelan and Douglas: `"Chelan-Douglas"`
#'
#' @param county A character vector of Washington county names, without the
#'   "County" suffix (e.g. `"Benton"`, not `"Benton County"`). Matching is
#'   case-sensitive.
#'
#' @returns A character vector the same length as `county`, giving the health
#'   jurisdiction name for each element. Names that are not part of a
#'   multi-county district (including unrecognized names) are returned
#'   unchanged, and `NA` stays `NA`.
#'
#' @export
#'
#' @examples
#' county_to_hd(c("King", "Benton", "Ferry", "Douglas"))
#'
#' # Use inside a data pipeline
#' df <- data.frame(county = c("Franklin", "Spokane"), pop = c(100, 200))
#' df$health_dept <- county_to_hd(df$county)
#' df
county_to_hd <- function(county) {
  health_dept <- dplyr::case_when(
    county %in% c("Ferry", "Pend Oreille", "Stevens") ~ "Tri-County",
    county %in% c("Benton", "Franklin") ~ "Benton-Franklin",
    county %in% c("Chelan", "Douglas") ~ "Chelan-Douglas",
    TRUE ~ county
  )
  return(health_dept)
}
