# joint_age_sex_dist.R
#
# Adult (18+) population counts by sex and age group for Washington State,
# built from American Community Survey (ACS) table B01001 ("Sex by Age")
# via tidycensus.
#
# Contents:
#   joint_age_sex_dist()     Exported. Downloads B01001 estimates and returns
#                            counts by sex x age group for counties, the
#                            whole state, or local health jurisdictions.
#   .age_sex_crosswalk_5yr() Internal. Builds a lookup table that maps each
#                            B01001 variable to its sex and to a 5-year age
#                            group.

#' Joint age and sex distribution of Washington adults
#'
#' Downloads American Community Survey (ACS) estimates from table B01001
#' ("Sex by Age") with [tidycensus::get_acs()] and returns adult (18+)
#' population counts by sex and age group. Results can be returned for each
#' Washington county, for the state as a whole, or for each local health
#' jurisdiction (see [county_to_health_department()]).
#'
#' The detailed B01001 age bins are combined into these groups: `"18-19"`,
#' `"20-24"`, `"25-29"`, ..., `"75-79"`, and `"80+"`. Only the point
#' estimates are summed; margins of error are dropped.
#'
#' A Census API key is required. See [tidycensus::census_api_key()].
#'
#' @param year Integer. ACS data year. For `acs_type = "acs5"`, this is the
#'   final year of the 5-year period (e.g. `2023` for 2019-2023).
#' @param acs_type Either `"acs1"` (1-year estimates) or `"acs5"` (5-year
#'   estimates). 1-year estimates are only published for areas with 65,000
#'   or more people, so some smaller counties will be missing.
#' @param geography The level to summarize to:
#'   * `"county"` (default): one row per county, sex, and age group.
#'   * `"state"`: Washington State totals by sex and age group.
#'   * `"health_department"`: totals by local health jurisdiction, sex, and age group.
#'
#' @returns A tibble with one row per combination of geography, sex, and age
#'   group, and these columns:
#'   * `sex`: `"Male"` or `"Female"`.
#'   * `age_cat_str`: the age group, e.g. `"25-29"` or `"80+"`.
#'   * `count`: the estimated number of people.
#'   * `county` (when `geography = "county"`): county name without the
#'     "County" suffix.
#'   * `health_department` (when `geography = "health_department"`):
#'     the health jurisdiction name.
#'
#'   The `"state"` and `"health_department"` results are still grouped. Call
#'   [dplyr::ungroup()] if needed.
#'
#' @seealso [county_to_health_department()] for the county-to-health-jurisdiction mapping.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires a Census API key
#' county_dist <- joint_age_sex_dist(2023, acs_type = "acs5")
#' state_dist <- joint_age_sex_dist(2023, acs_type = "acs5", geography = "state")
#' health_department_dist <- joint_age_sex_dist(2023, acs_type = "acs5",
#'  geography = "health_department")
#' }
joint_age_sex_dist <- function(year, acs_type = "acs1", geography = "county") {
  if (!(acs_type %in% c("acs1", "acs5"))) {
    stop("acs_type must be either acs1 or acs5")
  }
  if (!(geography %in% c("county", "state", "health_department"))) {
    stop("geography must be one of county, state, or health_department")
  }
  crosswalk_table <- .age_sex_crosswalk_5yr(year, acs_type)
  age_counts <- tidycensus::get_acs(
    "county",
    variables = crosswalk_table$NAME,
    state = "WA",
    year = year,
    survey = acs_type
  ) |>
    dplyr::rename(location = NAME, NAME = variable)
  age_counts_w_cat <- dplyr::left_join(age_counts, crosswalk_table, by = "NAME")
  age_sex_county_counts <- age_counts_w_cat |>
    dplyr::filter(!is.na(age_min)) |>
    dplyr::group_by(sex, age_cat_str, location) |>
    dplyr::summarise(count = sum(estimate), .groups = "keep") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      county = stringr::str_replace(location, "\\ County, Washington", "")
    ) |>
    dplyr::select(-location)
  if (geography == "county") {
    return(age_sex_county_counts)
  }
  if (geography == "state") {
    age_sex_washington <- age_sex_county_counts |>
      dplyr::group_by(sex, age_cat_str) |>
      dplyr::summarise(count = sum(count), .groups = "drop")
    return(age_sex_washington)
  }
  if (geography == "health_department") {
    age_sex_washington <- age_sex_county_counts |>
      dplyr::mutate(
        health_department = popdistph::county_to_health_department(county)
      ) |>
      dplyr::group_by(sex, age_cat_str, health_department) |>
      dplyr::summarise(count = sum(count), .groups = "drop")
    return(age_sex_washington)
  }
}


#' Build the B01001 variable-to-age-group crosswalk
#'
#' Internal helper for [joint_age_sex_dist()]. Uses
#' [tidycensus::load_variables()] to get every variable in ACS table B01001
#' and parses each variable's label (e.g.
#' `"Estimate!!Total:!!Male:!!22 to 24 years"`) into a sex and an age range.
#' Each detailed age bin is then assigned to a 5-year age group, and bins
#' entirely under 18 are dropped.
#'
#' Special cases:
#' * "Under 5 years" gets `age_min = 0`.
#' * "85 years and over" gets `age_max = 100`.
#' * The 15-19 group becomes `"18-19"` (only the 18-19 bin is kept).
#' * Ages 80 and over are combined into `"80+"`, with `age_cat_lower = 80`
#'   and `age_cat_upper = 89` for every bin in that group. The 89 is a
#'   placeholder, not a real age limit.
#'
#' Total and sex-total rows (which have no age) are dropped.
#'
#' @param year Integer. ACS year passed to [tidycensus::load_variables()].
#' @param acs_type Either `"acs1"` or `"acs5"`.
#'
#' @returns A tibble with one row per adult B01001 age-by-sex variable, and
#'   these columns:
#'   * `NAME`: the ACS variable ID (e.g. `"B01001_010"`). This matches the
#'     `variable` column from [tidycensus::get_acs()].
#'   * `label`: the full ACS label.
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
.age_sex_crosswalk_5yr <- function(year, acs_type) {
  if (!(acs_type %in% c("acs1", "acs5"))) {
    stop("acs_type must be either acs1 or acs5")
  }
  var_df <- tidycensus::load_variables(year, acs_type)
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
    dplyr::bind_cols(split_label[, c("sex", "age_str")])
  age_info <- age_info |>
    dplyr::mutate(
      sex = gsub(":", "", sex),
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
