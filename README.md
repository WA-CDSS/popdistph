---
output:
  html_document:
    keep_md: true
---

# Population Distributions for Public Health

This package is created to quickly create useful population distribution counts for public health.

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Functions

* `joint_age_sex_dist()`: adult (18+) counts by sex and age group.
* `race_ethn_6_distr()`: counts for six mutually exclusive race/ethnicity groups.
* `county_to_health_department()`: maps Washington counties to local health jurisdictions.
* `popdistph_cache_dir()` / `popdistph_clear_cache()`: locate or clear cached results.

A Census API key is required to download data. See `tidycensus::census_api_key()`.

## Geographies and table types

| `geography`           | `table_type`       | `year`                 |
|-----------------------|--------------------|------------------------|
| `"county"` (default)  | `"acs1"` or `"acs5"` | any ACS year         |
| `"state"`             | `"acs1"` or `"acs5"` | any ACS year         |
| `"health_department"` | `"acs1"` or `"acs5"` | any ACS year         |
| `"block group"`       | `"acs5"`           | any ACS 5-year year    |
| `"zcta"`              | `"acs5"`           | any ACS 5-year year    |
| `"block"`             | `"dec"`            | `2010` or `2020`       |

Census blocks are only published in the decennial census. At the block
level, `race_ethn_6_distr()` returns a single `"Hispanic"` group in place of
`"Mexican"` and `"Other Hispanic"`, because Mexican origin is not published
for blocks.

## Examples


``` r
library(popdistph)

joint_age_sex_dist(2023, table_type = "acs5", geography = "county")
race_ethn_6_distr(2023, table_type = "acs5", geography = "zcta")
joint_age_sex_dist(2020, table_type = "dec", geography = "block")
```

## Caching

Set `cache = TRUE` to save a result to disk. An identical later call (same
`year`, `table_type`, `geography`, and installed popdistph version) reads the
saved result instead of downloading again. Updating popdistph rebuilds the
data on the next call.


``` r
blocks <- race_ethn_6_distr(2020, table_type = "dec", geography = "block",
  cache = TRUE)

popdistph_cache_dir()   # where cached results are stored
popdistph_clear_cache() # delete all cached results
```
