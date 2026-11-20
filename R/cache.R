# cache.R
#
# On-disk caching of results from race_ethn_6_distr() and
# joint_age_sex_dist().
#
# A cached result is keyed on the function name, year, table_type,
# geography, and the installed popdistph version, so installing a new
# version of the package automatically rebuilds the data instead of reusing
# results from an older version.
#
# Contents:
#   popdistph_clear_cache() Exported. Deletes all cached results.
#   popdistph_cache_dir()   Exported. Returns the cache directory.
#   .cache_file()           Internal. Path of the cache file for a request.
#   .with_cache()           Internal. Return a cached result or build and
#                           save one.

#' Manage the popdistph cache
#'
#' [race_ethn_6_distr()] and [joint_age_sex_dist()] can save their results to
#' disk with `cache = TRUE`, so that an identical call later reads the saved
#' result instead of downloading from the Census API again.
#'
#' Cached results are stored as `.rds` files in a user cache directory (see
#' [tools::R_user_dir()]). Each file is keyed on the function, `year`,
#' `table_type`, `geography`, and the installed popdistph version. Updating
#' popdistph therefore rebuilds the data on the next call; files from older
#' versions are left in place until you call `popdistph_clear_cache()`.
#'
#' The Census Bureau occasionally revises published data. Call
#' `popdistph_clear_cache()` to force a fresh download.
#'
#' @returns `popdistph_cache_dir()` returns the path of the cache directory
#'   (which may not exist yet). `popdistph_clear_cache()` deletes every
#'   cached file and invisibly returns the paths of the deleted files.
#'
#' @export
#'
#' @examples
#' popdistph_cache_dir()
#' \dontrun{
#' popdistph_clear_cache()
#' }
popdistph_cache_dir <- function() {
  tools::R_user_dir("popdistph", which = "cache")
}

#' @rdname popdistph_cache_dir
#' @export
popdistph_clear_cache <- function() {
  dir <- popdistph_cache_dir()
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  unlink(files)
  message("Removed ", length(files), " cached file(s) from ", dir)
  invisible(files)
}

#' Path of the cache file for a request
#'
#' @param fun Name of the exported function (e.g. `"joint_age_sex_dist"`).
#' @param year,table_type,geography The arguments of the request.
#' @returns A file path ending in `.rds`.
#' @keywords internal
#' @noRd
.cache_file <- function(fun, year, table_type, geography) {
  version <- as.character(utils::packageVersion("popdistph"))
  key <- paste(
    fun,
    year,
    table_type,
    gsub("[^A-Za-z0-9]+", "-", geography),
    paste0("v", version),
    sep = "_"
  )
  file.path(popdistph_cache_dir(), paste0(key, ".rds"))
}

#' Return a cached result, or build and cache it
#'
#' @param cache Logical. If not `TRUE`, `build()` is called and nothing is
#'   read or written.
#' @param fun,year,table_type,geography Passed to `.cache_file()`.
#' @param build A function with no arguments that builds the result.
#' @returns The result of `build()`, or the cached copy of it.
#' @keywords internal
#' @noRd
.with_cache <- function(cache, fun, year, table_type, geography, build) {
  if (!isTRUE(cache)) {
    return(build())
  }
  path <- .cache_file(fun, year, table_type, geography)
  if (file.exists(path)) {
    message("Using cached result: ", path)
    return(readRDS(path))
  }
  result <- build()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, path)
  result
}
