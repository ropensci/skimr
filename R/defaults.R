#' @include stats.R
NULL


# Default summarizing functions for each type -----------------------------

numeric_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  mean = mean_num,
  sd = sd_num,
  min = purrr::partial(min, na.rm = TRUE),
  quantile = quantile_num,
  max = purrr::partial(max, na.rm = TRUE),
  hist = inline_hist
)

factor_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  count = purrr::partial(table, useNA = "always"),
  n_unique = n_unique
)

character_funs <- list (
  missing  = n_missing,
  complete = n_complete,
  n = length,
  min = min_char,
  max = max_char,
  empty = n_empty,
  n_unique = n_unique
)

logical_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  count = purrr::partial(table, useNA = "always"),
  mean = purrr::partial(mean, na.rm = TRUE)
)

integer_funs <- numeric_funs


complex_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length
)

date_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  n_unique = n_unique
)

ts_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  start = ts_start,
  end = ts_end,
  frequency = stats::frequency,
  deltat = stats::deltat,
  mean = mean_num,
  sd = sd_num,
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = median_num,
  line_graph  = inline_linegraph
)

posixct_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  min = purrr::partial(min, na.rm = TRUE),
  max = purrr::partial(max, na.rm = TRUE),
  median = purrr::partial(median, na.rm = TRUE),
  n_unique = n_unique 
)

asis_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = n_unique,
  min_length= list_min_length,
  max_length = list_max_length
)

list_funs <- list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = n_unique,
  min_length = list_lengths_min,
  median_length = list_lengths_median,
  max_length = list_lengths_max
)

.default <- list(
  numeric = numeric_funs,
  integer = integer_funs,
  factor = factor_funs,
  character = character_funs,
  logical = logical_funs,
  complex = complex_funs,
  date = date_funs,
  Date = date_funs,
  ts = ts_funs,
  POSIXct = posixct_funs,
  list = list_funs,
  AsIs = asis_funs
)
