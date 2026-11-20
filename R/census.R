# census.R
#
# Internal helpers shared by race_ethn_6_distr() and joint_age_sex_dist() for
# downloading Washington ACS / decennial census counts and attaching
# geography columns.
#
# Contents:
#   .check_args()          Validate table_type / geography / year
#                          combinations.
#   .get_wa_counts()       Download counts for Washington at the level needed
#                          for the requested geography (ACS or decennial).
#   .get_wa_acs()          ACS download (county, block group, or ZCTA).
#   .get_wa_dec_blocks()   Decennial census block download, county by county.
#   .dec_sumfile()         Decennial summary file for a given year.
#   .is_wa_zcta()          Flag ZCTAs whose 3-digit prefix is a Washington
#                          ZIP prefix (980-994).
#   .wa_county_codes()     3-digit FIPS codes of Washington counties.
#   .wa_county_name()      Map 5-digit county FIPS codes to WA county names.
#   .aggregate_wa_counts() Add geography columns and, for "state" and
#                          "health_department", sum counts across counties.

.wa_geographies <- c(
  "county",
  "state",
  "health_department",
  "block group",
  "block",
  "zcta"
)

# Decennial census years with block-level data supported by this package.
.dec_years <- c(2010, 2020)

#' @keywords internal
#' @noRd
.check_args <- function(table_type, geography, year) {
  if (length(geography) != 1 || !(geography %in% .wa_geographies)) {
    stop(
      "geography must be one of county, state, health_department, ",
      "block group, block, or zcta"
    )
  }
  if (geography == "block" && !identical(table_type, "dec")) {
    stop(
      "Census blocks are only published in the decennial census, not the ",
      "ACS. Use table_type = \"dec\" with year = ",
      paste(.dec_years, collapse = " or "),
      "."
    )
  }
  if (length(table_type) != 1 || !(table_type %in% c("acs1", "acs5", "dec"))) {
    stop("table_type must be one of acs1, acs5, or dec")
  }
  if (table_type == "dec") {
    if (geography != "block") {
      stop(
        "table_type = \"dec\" is only supported with geography = \"block\". ",
        "Use table_type = \"acs1\" or \"acs5\" for other geographies."
      )
    }
    if (length(year) != 1 || !(year %in% .dec_years)) {
      stop(
        "Decennial census block data requires year = ",
        paste(.dec_years, collapse = " or "),
        ", not ",
        paste(year, collapse = ", "),
        "."
      )
    }
  }
  if (geography %in% c("block group", "zcta") && table_type != "acs5") {
    stop(
      "geography = \"",
      geography,
      "\" is only available with ",
      "table_type = \"acs5\""
    )
  }
  invisible(TRUE)
}

#' Download Washington counts for a geography
#'
#' @returns A tibble with columns `GEOID`, `variable`, and `estimate`.
#' @keywords internal
#' @noRd
.get_wa_counts <- function(variables, year, table_type, geography) {
  if (table_type == "dec") {
    counts <- .get_wa_dec_blocks(variables, year)
  } else {
    counts <- .get_wa_acs(variables, year, table_type, geography)
  }
  dplyr::select(counts, GEOID, variable, estimate)
}

#' Download Washington ACS estimates
#'
#' County, state, and health department results are all built from
#' county-level estimates. Block groups are requested for Washington
#' directly. ZCTAs cannot be requested by state (for 2020 and later), so all
#' ZCTAs are downloaded and then filtered with `.is_wa_zcta()`.
#'
#' @param table_type `"acs1"` or `"acs5"`, passed to `survey`.
#' @returns The tidy output of [tidycensus::get_acs()]: `GEOID`, `NAME`,
#'   `variable`, `estimate`, `moe`.
#' @keywords internal
#' @noRd
.get_wa_acs <- function(variables, year, table_type, geography) {
  if (geography == "zcta") {
    acs <- tidycensus::get_acs(
      geography = "zcta",
      variables = variables,
      year = year,
      output = "tidy",
      survey = table_type
    )
    return(acs[.is_wa_zcta(acs$GEOID), ])
  }
  api_geography <- if (geography == "block group") "block group" else "county"
  tidycensus::get_acs(
    geography = api_geography,
    state = "WA",
    variables = variables,
    year = year,
    output = "tidy",
    survey = table_type
  )
}

#' Download Washington decennial census block counts
#'
#' The Census API requires a county for block-level requests, so this
#' requests each Washington county in turn and combines the results.
#'
#' @returns The tidy output of [tidycensus::get_decennial()] with `value`
#'   renamed to `estimate`.
#' @keywords internal
#' @noRd
.get_wa_dec_blocks <- function(variables, year) {
  sumfile <- .dec_sumfile(year)
  .wa_county_codes() |>
    purrr::map(function(county_code) {
      tidycensus::get_decennial(
        geography = "block",
        state = "WA",
        county = county_code,
        variables = variables,
        year = year,
        sumfile = sumfile,
        output = "tidy"
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::rename(estimate = value)
}

#' @keywords internal
#' @noRd
.dec_sumfile <- function(year) {
  if (year == 2020) "dhc" else "sf1"
}

#' @keywords internal
#' @noRd
.is_wa_zcta <- function(zcta) {
  prefix <- suppressWarnings(as.integer(substr(zcta, 1, 3)))
  !is.na(prefix) & prefix >= 980 & prefix <= 994
}

#' @keywords internal
#' @noRd
.wa_county_codes <- function() {
  fips <- tidycensus::fips_codes
  unique(fips$county_code[fips$state == "WA"])
}

#' @keywords internal
#' @noRd
.wa_county_name <- function(county_fips) {
  fips <- tidycensus::fips_codes
  wa <- fips[fips$state == "WA", ]
  lookup <- rlang::set_names(
    sub(" County$", "", wa$county),
    paste0(wa$state_code, wa$county_code)
  )
  unname(lookup[county_fips])
}

#' Add geography columns and aggregate to the requested level
#'
#' @param counts A data frame with a `GEOID` column, a `count` column, and
#'   the columns named in `group_vars`.
#' @param geography One of `.wa_geographies`.
#' @param group_vars Character vector of non-geographic grouping columns
#'   (e.g. `"race_eth"`, or `c("sex", "age_cat_str")`).
#'
#' @returns A tibble without `GEOID`. Columns added by geography:
#'   * `"county"`: `county`.
#'   * `"block group"`: `block_group` (12-digit GEOID) and `county`.
#'   * `"block"`: `block` (15-digit GEOID) and `county`.
#'   * `"zcta"`: `zcta` (5-digit ZCTA).
#'   * `"health_department"`: `health_department` (summed over counties).
#'   * `"state"`: none (summed over counties).
#' @keywords internal
#' @noRd
.aggregate_wa_counts <- function(counts, geography, group_vars) {
  if (geography == "zcta") {
    counts$zcta <- counts$GEOID
  } else {
    if (geography == "block group") {
      counts$block_group <- counts$GEOID
    }
    if (geography == "block") {
      counts$block <- counts$GEOID
    }
    counts$county <- .wa_county_name(substr(counts$GEOID, 1, 5))
  }
  counts <- dplyr::select(counts, -GEOID)

  if (geography == "state") {
    by <- group_vars
  } else if (geography == "health_department") {
    counts$health_department <- county_to_health_department(counts$county)
    by <- c(group_vars, "health_department")
  } else {
    return(counts)
  }
  counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
}
