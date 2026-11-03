# race_ethn_dist.R
#
# Population counts by race and ethnicity for Washington State, built from
# American Community Survey (ACS) tables B03001 ("Hispanic or Latino Origin
# by Specific Origin") and B03002 ("Hispanic or Latino Origin by Race") via
# tidycensus.
#
# Contents:
#   race_ethn_6_distr()  Exported. Downloads B03001/B03002 estimates and
#                        returns counts for six mutually exclusive
#                        race/ethnicity groups for counties, the whole
#                        state, or local health jurisdictions.

#' Six-category race and ethnicity distribution in Washington
#'
#' Downloads American Community Survey (ACS) estimates with
#' [tidycensus::get_acs()] and returns population counts for six mutually
#' exclusive race/ethnicity groups. Results can be returned for each
#' Washington county, for the state as a whole, or for each local health
#' jurisdiction (see [county_to_hd()]).
#'
#' Counts cover people of all ages (unlike [joint_age_sex_dist()], which only
#' covers adults). Only the point estimates are used; margins of error are
#' dropped.
#'
#' The six groups add up to the total population and are built as follows:
#'
#' | `race_eth`             | ACS source                                         |
#' |------------------------|----------------------------------------------------|
#' | `"Mexican"`            | B03001_004 (Hispanic or Latino: Mexican)           |
#' | `"Other Hispanic"`     | B03001_003 (Hispanic or Latino) minus B03001_004   |
#' | `"Non-Hispanic White"` | B03002_003 (Not Hispanic: White alone)             |
#' | `"Non-Hispanic Black"` | B03002_004 (Not Hispanic: Black or African American alone) |
#' | `"Non-Hispanic Asian"` | B03002_006 (Not Hispanic: Asian alone)             |
#' | `"Non-Hispanic Other"` | B03002_002 (Not Hispanic total) minus the three groups above |
#'
#' `"Non-Hispanic Other"` therefore includes non-Hispanic people who are
#' American Indian and Alaska Native alone, Native Hawaiian and Other Pacific
#' Islander alone, some other race alone, or two or more races.
#'
#' A Census API key is required. See [`tidycensus::census_api_key()`].
#'
#' @param year Integer. ACS data year. For `acs_type = "acs5"`, this is the
#'   final year of the 5-year period (e.g. `2023` for 2019-2023).
#' @param acs_type Either `"acs1"` (1-year estimates) or `"acs5"` (5-year
#'   estimates). 1-year estimates are only published for areas with 65,000
#'   or more people, so smaller counties are missing from `"acs1"` results.
#'   Because the `"state"` and `"hd"` totals are sums of county estimates,
#'   they will undercount with `"acs1"`.
#' @param geography The level to summarize to:
#'   * `"county"` (default): one row per county and race/ethnicity group.
#'   * `"state"`: Washington State totals by race/ethnicity group.
#'   * `"hd"`: totals by local health jurisdiction and race/ethnicity group.
#'
#' @returns An ungrouped tibble with one row per combination of geography and
#'   race/ethnicity group, and these columns:
#'   * `race_eth`: the race/ethnicity group (see the table above).
#'   * `count`: the estimated number of people.
#'   * `county` (when `geography = "county"`): county name without the
#'     "County" suffix.
#'   * `hd` (when `geography = "hd"`): the health jurisdiction name.
#'
#' @seealso [county_to_hd()] for the county-to-health-jurisdiction mapping,
#'   and [joint_age_sex_dist()] for counts by age and sex.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires a Census API key
#' county_re <- race_ethn_6_distr(2023, acs_type = "acs5", geography = "county")
#' state_re <- race_ethn_6_distr(2023, acs_type = "acs5", geography = "state")
#' hd_re <- race_ethn_6_distr(2023, acs_type = "acs5", geography = "hd")
#' }
race_ethn_6_distr <- function(year, acs_type = "acs1", geography = "county") {
  if (!(acs_type %in% c("acs1", "acs5"))) {
    stop("acs_type must be either acs1 or acs5")
  }
  if (!(geography %in% c("county", "state", "hd"))) {
    stop("geography must be one of county, state, or hd")
  }
  WA_race_ethn <- tidycensus::get_acs(
    geography = "county",
    state = "WA",
    variables = c(
      total = "B03001_001",
      hispanic_mexican = "B03001_004",
      hispanic_other = "B03001_003",
      nonhisp_white = "B03002_003",
      nonhisp_black = "B03002_004",
      nonhisp_asian = "B03002_006",
      nonhisp_total = "B03002_002"
    ),
    year = year,
    output = "tidy",
    survey = acs_type
  )
  # Remove MOE and pivot wider
  WA_race_ethn <- WA_race_ethn |>
    dplyr::select(-moe) |>
    tidyr::pivot_wider(
      names_from = variable,
      values_from = estimate
    )
  # Create Population counts
  WA_race_ethn <- WA_race_ethn |>
    dplyr::mutate(
      Mexican = hispanic_mexican,
      `Other Hispanic` = hispanic_other - hispanic_mexican,
      `Non-Hispanic White` = nonhisp_white,
      `Non-Hispanic Black` = nonhisp_black,
      `Non-Hispanic Asian` = nonhisp_asian,
      `Non-Hispanic Other` = nonhisp_total -
        (nonhisp_white + nonhisp_black + nonhisp_asian),
      total_check = Mexican +
        `Other Hispanic` +
        `Non-Hispanic White` +
        `Non-Hispanic Black` +
        `Non-Hispanic Asian` +
        `Non-Hispanic Other`,
      check_dif = total - total_check
    ) |>
    dplyr::relocate(total_check, check_dif, .after = total)
  # Check that the 6 sub-categories add up to the total population
  if (any(WA_race_ethn$check_dif != 0, na.rm = TRUE)) {
    warning(
      "population counts of the 6 sub-categories do not add up to the total"
    )
  }
  # Select the desired columns and pivot longer
  WA_county_counts <- WA_race_ethn |>
    dplyr::select(
      NAME,
      Mexican,
      `Other Hispanic`,
      `Non-Hispanic White`,
      `Non-Hispanic Black`,
      `Non-Hispanic Asian`,
      `Non-Hispanic Other`
    ) |>
    tidyr::pivot_longer(
      cols = -NAME,
      values_to = "count",
      names_to = "race_eth"
    )

  WA_county_counts <- WA_county_counts |>
    dplyr::mutate(
      county = stringr::str_replace(NAME, "\\ County, Washington", "")
    ) |>
    dplyr::select(-NAME)
  if (geography == "county") {
    return(WA_county_counts)
  } else if (geography == "state") {
    WA_state_counts <- WA_county_counts |>
      dplyr::group_by(race_eth) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
    return(WA_state_counts)
  } else if (geography == "hd") {
    hd_race_eth_counts <- WA_county_counts |>
      dplyr::mutate(hd = popdistph::county_to_hd(county)) |>
      dplyr::group_by(race_eth, hd) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
    return(hd_race_eth_counts)
  }
}
