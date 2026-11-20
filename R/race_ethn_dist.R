# race_ethn_dist.R
#
# Population counts by race and ethnicity for Washington State, built from
# American Community Survey (ACS) tables B03001 ("Hispanic or Latino Origin
# by Specific Origin") and B03002 ("Hispanic or Latino Origin by Race"), or,
# for census blocks, decennial census table P5 ("Hispanic or Latino Origin by
# Race"), via tidycensus.
#
# Contents:
#   race_ethn_6_distr()      Exported. Validates arguments, handles caching,
#                            and returns counts for mutually exclusive
#                            race/ethnicity groups for counties, the whole
#                            state, local health jurisdictions, block groups,
#                            blocks, or ZCTAs.
#   .race_ethn_6_build()     Internal. Downloads the data and builds the
#                            result.
#   .race_ethn_variables()   Internal. Census variable IDs for the source
#                            (ACS or decennial year).

#' Six-category race and ethnicity distribution in Washington
#'
#' Downloads American Community Survey (ACS) estimates with
#' [tidycensus::get_acs()] (or, for census blocks, decennial census counts
#' with [tidycensus::get_decennial()]) and returns population counts for six
#' mutually exclusive race/ethnicity groups. Results can be returned for each
#' Washington county, block group, census block, or ZIP Code Tabulation Area
#' (ZCTA), for the state as a whole, or for each local health jurisdiction
#' (see [county_to_health_department()]).
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
#' **Census blocks** (`geography = "block"`, `table_type = "dec"`) use
#' decennial census table P5 (2020 DHC `P5_*N` or 2010 SF1 `P005*`). Mexican
#' origin is not published at the block level, so block results have five
#' groups: `"Hispanic"` (all Hispanic or Latino people) replaces `"Mexican"`
#' and `"Other Hispanic"`. The non-Hispanic groups are defined as above. 2020
#' block counts include the Census Bureau's disclosure-avoidance noise and
#' are less reliable for small blocks.
#'
#' A Census API key is required to download data (not to read a cached
#' result). See [`tidycensus::census_api_key()`].
#'
#' @param year Integer. Data year. For `table_type = "acs5"`, this is the
#'   final year of the 5-year period (e.g. `2023` for 2019-2023). For
#'   `table_type = "dec"`, this must be `2010` or `2020`.
#' @param table_type The census product to use:
#'   * `"acs1"` (default): ACS 1-year estimates. Only published for areas
#'     with 65,000 or more people, so smaller counties are missing. Because
#'     the `"state"` and `"health_department"` totals are sums of county
#'     estimates, they will undercount with `"acs1"`.
#'   * `"acs5"`: ACS 5-year estimates. Required for `"block group"` and
#'     `"zcta"`.
#'   * `"dec"`: decennial census. Required for, and only supported with,
#'     `geography = "block"`.
#' @param geography The level to summarize to:
#'   * `"county"` (default): one row per county and race/ethnicity group.
#'   * `"state"`: Washington State totals by race/ethnicity group.
#'   * `"health_department"`: totals by local health jurisdiction and race/ethnicity group.
#'   * `"block group"`: one row per census block group and race/ethnicity
#'     group. Requires `table_type = "acs5"`.
#'   * `"block"`: one row per census block and race/ethnicity group. Requires
#'     `table_type = "dec"` and `year = 2010` or `2020`.
#'   * `"zcta"`: one row per ZIP Code Tabulation Area and race/ethnicity
#'     group. Requires `table_type = "acs5"`. ZCTAs are not nested within
#'     states, so Washington ZCTAs are identified by their ZIP prefix
#'     (980-994). A few ZCTAs cross the state line; their counts include the
#'     out-of-state part.
#' @param cache Logical. If `TRUE`, the result is saved to disk and an
#'   identical later call (same `year`, `table_type`, `geography`, and
#'   popdistph version) reads the saved result instead of downloading again.
#'   Defaults to `FALSE`. See [popdistph_clear_cache()].
#'
#' @returns An ungrouped tibble with one row per combination of geography and
#'   race/ethnicity group, and these columns:
#'   * `race_eth`: the race/ethnicity group (see the table above).
#'   * `count`: the estimated number of people.
#'   * `county` (when `geography` is `"county"`, `"block group"`, or
#'     `"block"`): county name without the "County" suffix.
#'   * `block_group` (when `geography = "block group"`): the 12-digit block
#'     group GEOID.
#'   * `block` (when `geography = "block"`): the 15-digit block GEOID.
#'   * `zcta` (when `geography = "zcta"`): the 5-digit ZCTA.
#'   * `health_department` (when `geography = "health_department"`): the health jurisdiction name.
#'
#' @seealso [county_to_health_department()] for the county-to-health-jurisdiction mapping,
#'   [joint_age_sex_dist()] for counts by age and sex, and
#'   [popdistph_clear_cache()] for managing cached results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires a Census API key
#' county_re <- race_ethn_6_distr(2023, table_type = "acs5", geography = "county")
#' state_re <- race_ethn_6_distr(2023, table_type = "acs5", geography = "state")
#' health_department <- race_ethn_6_distr(2023, table_type = "acs5",
#'  geography = "health_department")
#' block_group_re <- race_ethn_6_distr(2023, table_type = "acs5",
#'  geography = "block group")
#' zcta_re <- race_ethn_6_distr(2023, table_type = "acs5", geography = "zcta")
#' block_re <- race_ethn_6_distr(2020, table_type = "dec", geography = "block")
#'
#' # Save the result; an identical later call reads it from disk
#' block_re <- race_ethn_6_distr(2020, table_type = "dec", geography = "block",
#'  cache = TRUE)
#' }
race_ethn_6_distr <- function(
  year,
  table_type = "acs1",
  geography = "county",
  cache = FALSE
) {
  .check_args(table_type, geography, year)
  .with_cache(
    cache,
    fun = "race_ethn_6_distr",
    year = year,
    table_type = table_type,
    geography = geography,
    build = function() .race_ethn_6_build(year, table_type, geography)
  )
}


#' Download and build the race/ethnicity counts
#'
#' Internal worker for [race_ethn_6_distr()]; arguments are assumed to have
#' been validated with `.check_args()`.
#'
#' @keywords internal
#' @noRd
.race_ethn_6_build <- function(year, table_type, geography) {
  WA_race_ethn <- .get_wa_counts(
    variables = .race_ethn_variables(table_type, year),
    year = year,
    table_type = table_type,
    geography = geography
  ) |>
    tidyr::pivot_wider(
      names_from = variable,
      values_from = estimate
    )
  # Hispanic groups: Mexican is not published for census blocks
  if (table_type == "dec") {
    message(
      "Mexican origin is not published for census blocks; ",
      "Hispanic people are returned as a single \"Hispanic\" group."
    )
    WA_race_ethn <- WA_race_ethn |>
      dplyr::mutate(Hispanic = hispanic)
    groups <- "Hispanic"
  } else {
    WA_race_ethn <- WA_race_ethn |>
      dplyr::mutate(
        Mexican = hispanic_mexican,
        `Other Hispanic` = hispanic_other - hispanic_mexican
      )
    groups <- c("Mexican", "Other Hispanic")
  }
  # Non-Hispanic groups
  WA_race_ethn <- WA_race_ethn |>
    dplyr::mutate(
      `Non-Hispanic White` = nonhisp_white,
      `Non-Hispanic Black` = nonhisp_black,
      `Non-Hispanic Asian` = nonhisp_asian,
      `Non-Hispanic Other` = nonhisp_total -
        (nonhisp_white + nonhisp_black + nonhisp_asian)
    )
  groups <- c(
    groups,
    "Non-Hispanic White",
    "Non-Hispanic Black",
    "Non-Hispanic Asian",
    "Non-Hispanic Other"
  )
  # Check that the sub-categories add up to the total population
  total_check <- rowSums(as.matrix(WA_race_ethn[groups]))
  if (any(WA_race_ethn$total != total_check, na.rm = TRUE)) {
    warning(
      "population counts of the race/ethnicity sub-categories do not add ",
      "up to the total"
    )
  }
  # Select the desired columns and pivot longer
  WA_counts <- WA_race_ethn |>
    dplyr::select(GEOID, dplyr::all_of(groups)) |>
    tidyr::pivot_longer(
      cols = -GEOID,
      values_to = "count",
      names_to = "race_eth"
    )
  .aggregate_wa_counts(WA_counts, geography, group_vars = "race_eth")
}


#' Census variable IDs for the race/ethnicity groups
#'
#' @param table_type `"acs1"`, `"acs5"`, or `"dec"`.
#' @param year Data year (used to pick the decennial table naming).
#' @returns A named character vector of variable IDs.
#' @keywords internal
#' @noRd
.race_ethn_variables <- function(table_type, year) {
  if (table_type != "dec") {
    return(c(
      total = "B03001_001",
      hispanic_mexican = "B03001_004",
      hispanic_other = "B03001_003",
      nonhisp_white = "B03002_003",
      nonhisp_black = "B03002_004",
      nonhisp_asian = "B03002_006",
      nonhisp_total = "B03002_002"
    ))
  }
  if (year == 2020) {
    # 2020 DHC table P5: Hispanic or Latino Origin by Race
    return(c(
      total = "P5_001N",
      nonhisp_total = "P5_002N",
      nonhisp_white = "P5_003N",
      nonhisp_black = "P5_004N",
      nonhisp_asian = "P5_006N",
      hispanic = "P5_010N"
    ))
  }
  # 2010 SF1 table P5: Hispanic or Latino Origin by Race
  c(
    total = "P005001",
    nonhisp_total = "P005002",
    nonhisp_white = "P005003",
    nonhisp_black = "P005004",
    nonhisp_asian = "P005006",
    hispanic = "P005010"
  )
}
