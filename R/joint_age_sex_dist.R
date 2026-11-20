# joint_age_sex_dist.R
#
# Adult (18+) population counts by sex and age group for Washington State,
# built from American Community Survey (ACS) table B01001 ("Sex by Age"), or,
# for census blocks, decennial census table P12 ("Sex by Age"), via
# tidycensus.
#
# Contents:
#   joint_age_sex_dist()     Exported. Validates arguments, handles caching,
#                            and returns counts by sex x age group for
#                            counties, the whole state, local health
#                            jurisdictions, block groups, blocks, or ZCTAs.
#   .joint_age_sex_build()   Internal. Downloads the data and builds the
#                            result.
#   .age_sex_crosswalk_5yr() Internal. Builds a lookup table that maps each
#                            B01001 variable to its sex and to a 5-year age
#                            group.
#   .age_sex_crosswalk_dec() Internal. Same, for decennial table P12.
#   .add_age_cats()          Internal. Shared age-bin -> age-group logic.

#' Joint age and sex distribution of Washington adults
#'
#' Downloads American Community Survey (ACS) estimates from table B01001
#' ("Sex by Age") with [tidycensus::get_acs()] (or, for census blocks,
#' decennial census counts with [tidycensus::get_decennial()]) and returns
#' adult (18+) population counts by sex and age group. Results can be
#' returned for each Washington county, block group, census block, or ZIP
#' Code Tabulation Area (ZCTA), for the state as a whole, or for each local
#' health jurisdiction (see [county_to_health_department()]).
#'
#' The detailed age bins are combined into these groups: `"18-19"`,
#' `"20-24"`, `"25-29"`, ..., `"75-79"`, and `"80+"`. Only the point
#' estimates are summed; margins of error are dropped.
#'
#' **Census blocks** (`geography = "block"`, `table_type = "dec"`) use
#' decennial census table P12 (2020 DHC `P12_*N` or 2010 SF1 `P012*`).
#' P12 uses the same age bins as B01001, so the age groups are identical.
#' 2020 block counts include the Census Bureau's disclosure-avoidance noise
#' and are less reliable for small blocks.
#'
#' A Census API key is required to download data (not to read a cached
#' result). See [tidycensus::census_api_key()].
#'
#' @param year Integer. Data year. For `table_type = "acs5"`, this is the
#'   final year of the 5-year period (e.g. `2023` for 2019-2023). For
#'   `table_type = "dec"`, this must be `2010` or `2020`.
#' @param table_type The census product to use:
#'   * `"acs1"` (default): ACS 1-year estimates. Only published for areas
#'     with 65,000 or more people, so some smaller counties will be missing.
#'   * `"acs5"`: ACS 5-year estimates. Required for `"block group"` and
#'     `"zcta"`.
#'   * `"dec"`: decennial census. Required for, and only supported with,
#'     `geography = "block"`.
#' @param geography The level to summarize to:
#'   * `"county"` (default): one row per county, sex, and age group.
#'   * `"state"`: Washington State totals by sex and age group.
#'   * `"health_department"`: totals by local health jurisdiction, sex, and age group.
#'   * `"block group"`: one row per census block group, sex, and age group.
#'     Requires `table_type = "acs5"`.
#'   * `"block"`: one row per census block, sex, and age group. Requires
#'     `table_type = "dec"` and `year = 2010` or `2020`.
#'   * `"zcta"`: one row per ZIP Code Tabulation Area, sex, and age group.
#'     Requires `table_type = "acs5"`. ZCTAs are not nested within states,
#'     so Washington ZCTAs are identified by their ZIP prefix (980-994). A
#'     few ZCTAs cross the state line; their counts include the out-of-state
#'     part.
#' @param cache Logical. If `TRUE`, the result is saved to disk and an
#'   identical later call (same `year`, `table_type`, `geography`, and
#'   popdistph version) reads the saved result instead of downloading again.
#'   Defaults to `FALSE`. See [popdistph_clear_cache()].
#'
#' @returns An ungrouped tibble with one row per combination of geography,
#'   sex, and age group, and these columns:
#'   * `sex`: `"Male"` or `"Female"`.
#'   * `age_cat_str`: the age group, e.g. `"25-29"` or `"80+"`.
#'   * `count`: the estimated number of people.
#'   * `county` (when `geography` is `"county"`, `"block group"`, or
#'     `"block"`): county name without the "County" suffix.
#'   * `block_group` (when `geography = "block group"`): the 12-digit block
#'     group GEOID.
#'   * `block` (when `geography = "block"`): the 15-digit block GEOID.
#'   * `zcta` (when `geography = "zcta"`): the 5-digit ZCTA.
#'   * `health_department` (when `geography = "health_department"`):
#'     the health jurisdiction name.
#'
#' @seealso [county_to_health_department()] for the county-to-health-jurisdiction mapping,
#'   [race_ethn_6_distr()] for counts by race and ethnicity, and
#'   [popdistph_clear_cache()] for managing cached results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires a Census API key
#' county_dist <- joint_age_sex_dist(2023, table_type = "acs5")
#' state_dist <- joint_age_sex_dist(2023, table_type = "acs5", geography = "state")
#' health_department_dist <- joint_age_sex_dist(2023, table_type = "acs5",
#'  geography = "health_department")
#' block_group_dist <- joint_age_sex_dist(2023, table_type = "acs5",
#'  geography = "block group")
#' zcta_dist <- joint_age_sex_dist(2023, table_type = "acs5", geography = "zcta")
#' block_dist <- joint_age_sex_dist(2020, table_type = "dec", geography = "block")
#'
#' # Save the result; an identical later call reads it from disk
#' block_dist <- joint_age_sex_dist(2020, table_type = "dec", geography = "block",
#'  cache = TRUE)
#' }
joint_age_sex_dist <- function(
  year,
  table_type = "acs1",
  geography = "county",
  cache = FALSE
) {
  .check_args(table_type, geography, year)
  .with_cache(
    cache,
    fun = "joint_age_sex_dist",
    year = year,
    table_type = table_type,
    geography = geography,
    build = function() .joint_age_sex_build(year, table_type, geography)
  )
}


#' Download and build the age/sex counts
#'
#' Internal worker for [joint_age_sex_dist()]; arguments are assumed to have
#' been validated with `.check_args()`.
#'
#' @keywords internal
#' @noRd
.joint_age_sex_build <- function(year, table_type, geography) {
  if (table_type == "dec") {
    crosswalk_table <- .age_sex_crosswalk_dec(year)
  } else {
    crosswalk_table <- .age_sex_crosswalk_5yr(year, table_type)
  }
  age_counts <- .get_wa_counts(
    variables = crosswalk_table$NAME,
    year = year,
    table_type = table_type,
    geography = geography
  ) |>
    dplyr::select(GEOID, NAME = variable, estimate)
  age_counts_w_cat <- dplyr::left_join(age_counts, crosswalk_table, by = "NAME")
  age_sex_counts <- age_counts_w_cat |>
    dplyr::filter(!is.na(age_min)) |>
    dplyr::group_by(sex, age_cat_str, GEOID) |>
    dplyr::summarise(count = sum(estimate), .groups = "drop")
  .aggregate_wa_counts(
    age_sex_counts,
    geography,
    group_vars = c("sex", "age_cat_str")
  )
}


#' Build the B01001 variable-to-age-group crosswalk
#'
#' Internal helper for [joint_age_sex_dist()]. Uses
#' [tidycensus::load_variables()] to get every variable in ACS table B01001
#' and parses each variable's label (e.g.
#' `"Estimate!!Total:!!Male:!!22 to 24 years"`) into a sex and an age range.
#' The age bins are then assigned to age groups with `.add_age_cats()`.
#'
#' Total and sex-total rows (which have no age) are dropped.
#'
#' @param year Integer. ACS year passed to [tidycensus::load_variables()].
#' @param table_type Either `"acs1"` or `"acs5"`.
#'
#' @returns See `.add_age_cats()`.
#'
#' @keywords internal
#' @noRd
.age_sex_crosswalk_5yr <- function(year, table_type) {
  if (!(table_type %in% c("acs1", "acs5"))) {
    stop("table_type must be either acs1 or acs5")
  }
  var_df <- tidycensus::load_variables(year, table_type)
  var_df <- var_df |> dplyr::filter(grepl("B01001_", name))
  split_label <- var_df |>
    dplyr::pull(label) |>
    stringr::str_split("!!") |>
    purrr::map(.f = function(x) {
      if (length(x) == 4) {
        return(x)
      } else {
        return(c(x, rep(NA, 4 - length(x))))
      }
    }) |>
    purrr::list_transpose() |>
    rlang::set_names(c("level1", "level2", "sex", "age_str")) |>
    dplyr::bind_cols()
  age_info <- var_df |>
    dplyr::select(NAME = name, label) |>
    dplyr::bind_cols(split_label[, c("sex", "age_str")]) |>
    dplyr::mutate(sex = gsub(":", "", sex))
  .add_age_cats(age_info)
}


#' Build the decennial P12 variable-to-age-group crosswalk
#'
#' Internal helper for [joint_age_sex_dist()]. Decennial table P12 has the
#' same 23 age bins per sex as ACS B01001, in the same order: variables
#' 003-025 are male and 027-049 are female. The variable IDs differ by year:
#' `P12_003N` (2020 DHC) vs `P012003` (2010 SF1). The crosswalk is
#' hard-coded because the decennial label formats differ between years.
#'
#' @param year `2010` or `2020`.
#'
#' @returns See `.add_age_cats()`.
#'
#' @keywords internal
#' @noRd
.age_sex_crosswalk_dec <- function(year) {
  age_bins <- c(
    "Under 5 years",
    "5 to 9 years",
    "10 to 14 years",
    "15 to 17 years",
    "18 and 19 years",
    "20 years",
    "21 years",
    "22 to 24 years",
    "25 to 29 years",
    "30 to 34 years",
    "35 to 39 years",
    "40 to 44 years",
    "45 to 49 years",
    "50 to 54 years",
    "55 to 59 years",
    "60 and 61 years",
    "62 to 64 years",
    "65 and 66 years",
    "67 to 69 years",
    "70 to 74 years",
    "75 to 79 years",
    "80 to 84 years",
    "85 years and over"
  )
  idx <- c(3:25, 27:49)
  ids <- if (year == 2020) {
    sprintf("P12_%03dN", idx)
  } else {
    sprintf("P012%03d", idx)
  }
  sex <- rep(c("Male", "Female"), each = length(age_bins))
  age_str <- rep(age_bins, 2)
  age_info <- dplyr::tibble(
    NAME = ids,
    label = paste0("Total!!", sex, "!!", age_str),
    sex = sex,
    age_str = age_str
  )
  .add_age_cats(age_info)
}


#' Assign detailed age bins to adult age groups
#'
#' Parses each age bin's text into an age range, assigns it to a 5-year age
#' group, and drops bins entirely under 18.
#'
#' Special cases:
#' * "Under 5 years" gets `age_min = 0`.
#' * "85 years and over" becomes `"85+"` and gets `age_max = 100`.
#' * The 15-19 group becomes `"18-19"` (only the 18-19 bin is kept).
#' * Ages 80 and over are combined into `"80+"`, with `age_cat_lower = 80`
#'   and `age_cat_upper = 89` for every bin in that group. The 89 is a
#'   placeholder, not a real age limit.
#'
#' @param age_info A tibble with columns `NAME`, `label`, `sex`, and
#'   `age_str`.
#'
#' @returns A tibble with one row per adult age-by-sex variable, and these
#'   columns:
#'   * `NAME`: the census variable ID (e.g. `"B01001_010"`). This matches
#'     the `variable` column from tidycensus.
#'   * `label`: the full label.
#'   * `sex`: `"Male"` or `"Female"`.
#'   * `age_str`: the original age bin text (`"85+"` for 85 and over).
#'   * `age_min`, `age_max`: numeric bounds of the age bin.
#'   * `age_cat_lower`, `age_cat_upper`: numeric bounds of the age group,
#'     shared by every row with the same `age_cat_str`. These are 18 and 19
#'     for the 18-19 group, and 80 and 89 for the 80+ group.
#'   * `age_cat_str`: the age group label used in the output of
#'     [joint_age_sex_dist()].
#'
#' @keywords internal
#' @noRd
.add_age_cats <- function(age_info) {
  age_info <- age_info |>
    dplyr::mutate(
      age_str = ifelse(age_str == "85 years and over", "85+", age_str)
    )
  age_lims <- age_info$age_str |>
    stringr::str_split("to|and") |>
    purrr::map(
      .f = function(x) {
        nums <- stringr::str_extract(x, pattern = "[0-9]+")
        return(as.numeric(c(min(nums), max(nums))))
      }
    ) |>
    purrr::list_transpose()
  age_info$age_min <- age_lims[[1]]
  age_info$age_max <- age_lims[[2]]
  age_info[age_info$age_max %in% 5, "age_min"] <- 0
  age_info[age_info$age_min %in% 85, "age_max"] <- 100
  age_info$age_cat_lower <- floor(age_info$age_min / 5) * 5
  age_info$age_cat_upper <- age_info$age_cat_lower + 4
  age_info$age_cat_str <- paste0(
    age_info$age_cat_lower,
    "-",
    age_info$age_cat_upper
  )
  age_info <- age_info |>
    dplyr::filter(age_max >= 18) |>
    dplyr::mutate(
      age_cat_lower = pmax(age_cat_lower, 18),
      age_cat_str = gsub("15-19", "18-19", age_cat_str),
      age_cat_str = gsub("80-84", "80+", age_cat_str),
      age_cat_str = ifelse(age_min == 85, "80+", age_cat_str),
    )
  return(age_info)
}
