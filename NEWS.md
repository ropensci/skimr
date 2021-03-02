# skimr 2.1.3

### MINOR IMPROVEMENTS

*   Add support for data tables when dtplyr is used.
*   Improve tests.

# skimr 2.1.2

### MINOR IMPROVEMENTS

*   Add support for lubridate Timespan objects.
*   Improvements to Supporting Additional Objects vignette.

### BUG FIXES

*   Update package to work with new version of `knitr`.

# skimr 2.1.1 (2020-04-15)

### MINOR IMPROVEMENTS

*   Prepare for release of dplyr 1.0 and related packages.
*   0-length sfls are now permitted.

# skimr 2.1.0 (2020-01-10)

### NEW FEATURES

We've made `to_long()` generic, supporting a more intuitive interface.

*   Called on a `skim_df`, it reshapes the output into the V1 long style.
*   Called on other tibble-like objects, it first skims then produces the long
    output. You can pass a custom skim function, like `skim_tee()`

Thanks @sethlatimer for suggesting this feature.

### BUG FIXES

*   Update package to work with new version of `tibble`.
*   Adds more flexibility in the rule width for `skimr::summarize()`.
*   More README badges and documentation crosslinks

# skimr 2.0.1 (2019-11-23)

### BUG FIXES

Address failed build in CRAN due to lack of UTF-8 support in some platforms.

# skimr 2.0.0 (2019-11-12)

### Welcome to skimr V2

V2 is a complete rewrite of `skimr`, incorporating all of the great feedback the
developers have received over the last year. A big thank you goes to @GShotwell,
@akraemer007, @puterleat, @tonyfischetti, @Nowosad, @rgayler, @jrosen48,
@randomgambit, @elben10, @koliii, @AndreaPi, @rubenarslan, @GegznaV, @svraka,
@dpprdan and to our ROpenSci reviewers @jenniferthompson and @jimhester for all
of the great support and feedback over the last year. We couldn't have done this
without you.

For most users using `skimr` will not change in terms of visual outputs. However
for users who use `skimr` outputs as part of a larger workflow the differences
are substantial.

### Breaking changes

#### The `skim_df`

We've changed the way data is represented within `skimr` to closer match
expectations. It is now wide by default. This makes piping statistics much
simpler

```
skim(iris) %>%
  dplyr::filter(numeric.sd > 1)
```

This means that the old reshaping functions `skim_to_wide()` and
`skim_to_list()` are deprecated. The latter is replaced with a reshaping
function called `partition()` that breaks a `skim_df` into a list by data type.
Similarly, `yank()` gets a specific data type from the `skim_df`. `to_long()`
gets you data that is closest to the format in the old API.

As the above example suggests, columns of summary statistics are prefixed by
`skim_type`. That is, statistics from numeric columns all begin `numeric.`,
those for factors all begin `factor.`, and so on.

#### Rendering

We've deprecated support for `pander()` and our `kable()` method. Instead, we
now support `knitr` through the `knit_print()` API. This is much more seamless
than before. Having a `skim_df` as the final object in a code chunk should
produce nice results in the majority of RMarkdown formats.

#### Customizing and extending

We've deprecated the previous approach customization. We no longer use
`skim_format()` and `skim_with()` no longer depends on a global state. Instead
`skim_with()` is now a function factory. Customization creates a new skimming
function.

```
my_skim <- skim_with(numeric = sfl(mad = mad))
```

The fundamental tool for customization is the `sfl` object, a skimmer function
list. It is used within `skim_with()` and also within our new API for adding
default functions for new data types, the generic `get_skimmers()`.

Most of the options set in `skim_format` are now either in function arguments or
print arguments. The former can be updated using `skim_with`, the latter in a
call to `print()`. In RMarkdown documents, you can change the number of
displayed digits by adding the `skimr_digits` option to your code chunk.

### OTHER NEW FEATURES

*   Substantial improvements to `summary()`, and it is now incorporated into
    `print()` methods.
*   `focus()` is like `dplyr::select()`, but it keeps around the columns
    `skim_type` and `skim_variable`.
*   We are also evaluating the behavior of different `dplyr` verbs to make sure
    that they place nice with `skimr` objects.
*   While `skimr` has never really focused on performance, it should do a better
    job on big data sets with lots of different columns.
*   New statistic for character variables counting the number of rows that are
    completely made up of white space.
*   We now export `skim_without_charts()` as a fallback for when unicode support
    is not possible.
*   By default, `skimr` removes the tibble metadata when generating output. On
    some platforms, this can lead to all output getting removed. To disable that
    behavior, set either `strip_metadata = FALSE` when calling print or use
    `options(skimr_strip_metadata = FALSE)`.

### BUG FIXES

*   Adjust code for several tidyverse soft deprecations.
*   Fix issue where multibyte characters were causing an error.

### MINOR IMPROVEMENTS

*   Change top_counts to use useNA = "no".

# skimr 1.0.6 (2019-05-27)

### BUG FIXES

*   Fix issue where skim_tee() was not respecting ... options.
*   Fix issue where all NA character vectors were not returning NA for max() and
    min()

# skimr 1.0.5 (2019-01-05)

This is likely to be the last release of skimr version 1. Version 2 has major
changes to the API. Users should review and prepare for those changes now.

### BUG FIXES

*   Fix issue where multibyte characters were causing an error.
*   Fix problem in which purrr cannot find mean.default.

# skimr 1.0.4 (2018-01-12)

This is likely to be the last release of skimr version 1. Version 2 has major
changes to the API. Users should review and prepare for those changes now.

### BUG FIXES

*   Fix failures in handling dplyr verbs related to upcoming release of dplyr
    0.8.0.

# skimr 1.0.3 (2018-06-06)

### NEW FEATURES

*   You can use skim_with() with a nest list of functions: `skim_with(.list =
    mylist)` or `skim_with(!!!mylist)`
*   More polished display of subtables in default printing.

### BUG FIXES

*   Fix issue with conflict between knitr and skimr versions of kable() that
    occurred intermittently.
*   Do not skim a class when the skimmer list is empty for that class.
*   Fix a mistake in a test of skim_print for top counts.

# skimr 1.0.2 (2018-04-04)

### NEW FEATURES

*   You can create skimmers with the formula syntax from `rlang`:
    `skim_with(iqr = ~IQR(.x, na.rm = TRUE))`.

### MAJOR CHANGES

*   The median label has been changed to p50 for consistency with the previous
    changes to p0 and p100.

### MINOR IMPROVEMENTS

*   Improvements and corrections to to README and other documentation.
*   New vignette showing defaults for skimmers and formats.
*   Vector output match data frame output more closely.
*   Add minimum required version for testhat.
*   Add minimum required version for knitr.

### BUG FIXES

*   You can use `skim_with()` to add and remove skimmers at the same time, i.e.
    `skim_with(iqr = IQR, hist = NULL)` works as expected.
*   Histograms work when Inf or -Inf are present.
*   Change seq( ) parameter to length.out to avoid problems with name matching.
*   Summary should not display a data frame name of "." (which occurs when
    piping begins with the data frame).

# skimr 1.0.1 (2018-01-09)

### NEW FEATURES

*   Add support for spark plots on Windows

### MAJOR CHANGES

*   `spark_line()` and `spark_bar()` are no longer exported
*   Default statistics for numeric changed from `min(x)` and `max(x)` to
    `quantile(x, probs = 0)` and `quantile(x, probs = 1)`. These changes lead to
    more predictable behaviors when a column is all NA values.

#### MINOR IMPROVEMENTS

*   Add minimimum required version for stringr
*   Improve documentation in general, especially those related to fonts

### BUG FIXES

*   Fix issue where a histogram for data with all `NA`s threw an error
*   Suppress progress bars from `dplyr::do()`

# skimr 0.92 (2017-12-19)

### MAJOR CHANGES

*   `skim_v()` is no longer exported. Vectors are now directly supported via
    `skim.default()`.
*   Change license to GPL 3

### NEW FEATURES

*   Add support for `kable()` and `pander()` for `skim_df` objects.
*   Add summary method for `skim_df` objects.
*   Add support for tidy select to skim specific columns of a data frame.
*   Add support for skimming individual vectors via `skim.default()`.

# skimr 0.91 (2017-10-14)

### NEW FEATURES

*   Handling of grouped data (generated by `dplyr::group_by()`)
*   Printing for all column classes
*   Add indicator of if a factor is ordered to skim object for factor
*   Introduction of flexible formatting
*   Easy dropping of individual functions
*   Vignettes for basic use and use with specialized object types
*   Updated README and added CONTRIBUTING.md and CONDUCT.md
*   New public get_skimmers function to access skim functions
*   Support for difftime class

### MINOR IMPROVEMENTS

*   Add header to print providing summary information about data.

### BUG FIXES

*   Change from Colformat to Pillar.

# skimr 0.900 (2017-07-16)

### BUG FIXES

*   Fix documentation for get_fun_names()
*   Fix test and build errors and notes
