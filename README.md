<!-- README.md is generated from README.Rmd. Please edit that file -->
skimr <img src="man/figures/logo.png" align="right" height="139" />
===================================================================

[![Build
Status](https://travis-ci.org/ropensci/skimr.svg?branch=master)](https://travis-ci.org/ropensci/skimr)
[![codecov](https://codecov.io/gh/ropensci/skimr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/skimr)
[![](https://badges.ropensci.org/175_status.svg)](https://github.com/ropensci/onboarding/issues/175)

`skimr` provides a frictionless approach to summary statistics which
conforms to the [principle of least
surprise](https://en.wikipedia.org/wiki/Principle_of_least_astonishment),
displaying summary statistics the user can skim quickly to understand
their data. It handles different data types and returns a `skim_df`
object which can be included in a pipeline or displayed nicely for the
human reader.

Installation
------------

The current released version of `skimr` can be installed from CRAN. If
you wish to install the current build of the next release you can do so
using the following:

    # install.packages("devtools")
    devtools::install_github("ropenscilabs/skimr")

The APIs for this branch should be considered reasonably stable but
still subject to change if an issue is discovered.

To install the version with the most recent changes that have not yet
been incorporated in the master branch (and may not be):

    devtools::install_github("ropenscilabs/skimr", ref = "develop")

Do not rely on APIs from the develop branch, as they are likely to
change.

Skim statistics in the console
------------------------------

`skimr`:

-   Provides a larger set of statistics than `summary()`, including
    missing, complete, n, and sd.
-   reports each data types separately
-   handles dates, logicals, and a variety of other types
-   supports spark-bar and spark-line based on [Hadley Wickham's pillar
    package](https://github.com/hadley/pillar).

### Separates variables by class:

    skim(chickwts)

    ## Warning: Unquoting language objects with `!!!` is soft-deprecated as of rlang 0.3.0.
    ## Please use `!!` instead.
    ## 
    ##   # Bad:
    ##   dplyr::select(data, !!!enquo(x))
    ## 
    ##   # Good:
    ##   dplyr::select(data, !!enquo(x))    # Unquote single quosure
    ##   dplyr::select(data, !!!enquos(x))  # Splice list of quosures
    ## 
    ## This warning is displayed once per session.

    ## Skim summary statistics
    ##  n obs: 71 
    ##  n variables: 2

    ## Warning: `set_attrs()` is soft-deprecated as of rlang 0.3.0
    ## This warning is displayed once per session.

    ## ── Variable type: factor ──────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable missing complete     n ordered n_unique top_counts                        
    ## 1 feed           0       71    71 FALSE          6 soy: 14, cas: 12, lin: 12, sun: 12
    ## ── Variable type: numeric ─────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable missing complete     n  mean    sd    p0   p25   p50   p75  p100 hist 
    ## 1 weight         0       71    71  261.  78.1  108.  204.  258.  324.  423. ▆▆▇▇▃

### Presentation is in a compact horizontal format:

    skim(iris)

    ## Skim summary statistics
    ##  n obs: 150 
    ##  n variables: 5 
    ## ── Variable type: factor ──────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable missing complete     n ordered n_unique top_counts                      
    ## 1 Species        0      150   150 FALSE          3 set: 50, ver: 50, vir: 50, NA: 0
    ## ── Variable type: numeric ─────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable     missing complete     n  mean    sd    p0   p25   p50   p75  p100 hist 
    ## 1 Sepal.Length       0      150   150  5.84 0.828 4.30  5.10   5.80  6.40  7.90 ▆▇▇▅▂
    ## 2 Sepal.Width        0      150   150  3.06 0.436 2.00  2.80   3.00  3.30  4.40 ▁▆▇▂▁
    ## 3 Petal.Length       0      150   150  3.76 1.77  1.00  1.60   4.35  5.10  6.90 ▇▁▆▇▂
    ## 4 Petal.Width        0      150   150  1.20 0.762 0.100 0.300  1.30  1.80  2.50 ▇▁▇▅▃

### Built in support for strings, lists and other column classes

    skim(dplyr::starwars)

    ## Skim summary statistics
    ##  n obs: 87 
    ##  n variables: 13 
    ## ── Variable type: character ───────────────────────────────────────────────────────────────────────────────────────────
    ##   variable   missing complete     n   min   max empty n_unique whitespace
    ## 1 name             0       87    87     3    21     0       87          0
    ## 2 hair_color       5       82    87     4    13     0       12          0
    ## 3 skin_color       0       87    87     3    19     0       31          0
    ## 4 eye_color        0       87    87     3    13     0       15          0
    ## 5 gender           3       84    87     4    13     0        4          0
    ## 6 homeworld       10       77    87     4    14     0       48          0
    ## 7 species          5       82    87     3    14     0       37          0
    ## ── Variable type: list ────────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable  missing complete     n n_unique min_length max_length
    ## 1 films           0       87    87       24          1          7
    ## 2 vehicles        0       87    87       11          0          2
    ## 3 starships       0       87    87       17          0          5
    ## ── Variable type: numeric ─────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable   missing complete     n  mean    sd    p0   p25   p50   p75  p100 hist 
    ## 1 height           6       81    87 174.   34.8   66. 167.   180. 191.   264. ▁▁▇▅▁
    ## 2 mass            28       59    87  97.3 169.    15.  55.6   79.  84.5 1358. ▇▁▁▁▁
    ## 3 birth_year      44       43    87  87.6 155.     8.  35.0   52.  72.0  896. ▇▁▁▁▁

### Has a useful summary function

    skim(iris) %>%
      summary()

    ## A skim object    
    ## 
    ## Name: `iris`   
    ## Number of Rows: 150   
    ## Number of Columns: 5    
    ##     
    ## Column type frequency    
    ## factor: 1   
    ## numeric: 4

### Individual columns can be selected using tidyverse-style selectors

    skim(iris, Sepal.Length, Petal.Length)

    ## Skim summary statistics
    ##  n obs: 150 
    ##  n variables: 5 
    ## ── Variable type: numeric ─────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable     missing complete     n  mean    sd    p0   p25   p50   p75  p100 hist 
    ## 1 Sepal.Length       0      150   150  5.84 0.828  4.30  5.10  5.80  6.40  7.90 ▆▇▇▅▂
    ## 2 Petal.Length       0      150   150  3.76 1.77   1.00  1.60  4.35  5.10  6.90 ▇▁▆▇▂

### Handles grouped data

`skim()` can handle data that has been grouped using
`dplyr::group_by()`.

    iris %>%
      dplyr::group_by(Species) %>%
      skim()

    ## Skim summary statistics
    ##  n obs: 150 
    ##  n variables: 5 
    ##  group variables: Species 
    ## ── Variable type: numeric ─────────────────────────────────────────────────────────────────────────────────────────────
    ##    variable     Species    missing complete     n  mean    sd    p0   p25   p50   p75  p100 hist 
    ##  1 Sepal.Length setosa           0       50    50 5.01  0.352 4.30  4.80  5.00  5.20  5.80  ▃▃▇▅▁
    ##  2 Sepal.Length versicolor       0       50    50 5.94  0.516 4.90  5.60  5.90  6.30  7.00  ▂▇▆▃▃
    ##  3 Sepal.Length virginica        0       50    50 6.59  0.636 4.90  6.22  6.50  6.90  7.90  ▁▃▇▃▂
    ##  4 Sepal.Width  setosa           0       50    50 3.43  0.379 2.30  3.20  3.40  3.68  4.40  ▁▃▇▅▂
    ##  5 Sepal.Width  versicolor       0       50    50 2.77  0.314 2.00  2.52  2.80  3.00  3.40  ▁▅▆▇▂
    ##  6 Sepal.Width  virginica        0       50    50 2.97  0.322 2.20  2.80  3.00  3.18  3.80  ▂▆▇▅▁
    ##  7 Petal.Length setosa           0       50    50 1.46  0.174 1.00  1.40  1.50  1.58  1.90  ▁▃▇▃▁
    ##  8 Petal.Length versicolor       0       50    50 4.26  0.470 3.00  4.00  4.35  4.60  5.10  ▂▂▇▇▆
    ##  9 Petal.Length virginica        0       50    50 5.55  0.552 4.50  5.10  5.55  5.88  6.90  ▃▇▇▃▂
    ## 10 Petal.Width  setosa           0       50    50 0.246 0.105 0.100 0.200 0.200 0.300 0.600 ▇▂▂▁▁
    ## 11 Petal.Width  versicolor       0       50    50 1.33  0.198 1.00  1.20  1.30  1.50  1.80  ▅▇▃▆▁
    ## 12 Petal.Width  virginica        0       50    50 2.03  0.275 1.40  1.80  2.00  2.30  2.50  ▂▇▆▅▇

Knitted results
---------------

Simply skimming a data frame will produce the horizontal print layout
shown above. We provide a `knit_print` method for the types of objects
in this package so that similar results are produced in documents.

Customizing skimr
-----------------

Although skimr provides opinionated defaults, it is highly customizable.
Users can specify their own statistics, change the formatting of
results, create statistics for new classes and develop skimmers for data
structures that are not data frames.

### Specify your own statistics and classes

Users can specify their own statistics using a list combined with the
`skim_with()` function factory. `skim_with()` returns a new `skim`
function that can be called on your data. You can use this factory to
produce summaries for any type of column within your data.

Assignment within a call to `skim_with()` relies on a helper function,
`sfl` or `skimr` function list. This is a light wrapper around
`dplyr::funs()`. It will automatically generate names from the provided
values.

By default, functions in the `sfl` call are appended to the default
skimmers.

    my_skim <- skim_with(numeric = sfl(mad))
    my_skim(iris, Sepal.Length)

**Skim summary statistics**

<table style="width: auto;" class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
n\_obs
</th>
<th style="text-align:right;">
n\_cols
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
5
</td>
</tr>
</tbody>
</table>
**Variable type: numeric**

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">missing</th>
<th align="right">complete</th>
<th align="right">n</th>
<th align="right">mean</th>
<th align="right">sd</th>
<th align="right">p0</th>
<th align="right">p25</th>
<th align="right">p50</th>
<th align="right">p75</th>
<th align="right">p100</th>
<th align="left">hist</th>
<th align="right">mad</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="right">0</td>
<td align="right">150</td>
<td align="right">150</td>
<td align="right">5.84</td>
<td align="right">0.83</td>
<td align="right">4.3</td>
<td align="right">5.1</td>
<td align="right">5.8</td>
<td align="right">6.4</td>
<td align="right">7.9</td>
<td align="left">▆▇▇▅▂</td>
<td align="right">1.04</td>
</tr>
</tbody>
</table>

But you can also use the dummy argument pattern from `dplyr::funs` to
set particular function arguments. Setting the `append = FALSE` argument
uses only those functions that you've provided.

    my_skim <- skim_with(numeric = sfl(iqr = IQR, p99 = quantile(., probs = .99)),
                         append = FALSE)
    my_skim(iris, Sepal.Length)

**Skim summary statistics**

<table style="width: auto;" class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
n\_obs
</th>
<th style="text-align:right;">
n\_cols
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
5
</td>
</tr>
</tbody>
</table>
**Variable type: numeric**

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">iqr</th>
<th align="right">p99</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="right">1.3</td>
<td align="right">7.7</td>
</tr>
</tbody>
</table>

And you can default skimmers by setting them to `NULL`.

    my_skim <- skim_with(numeric = sfl(hist = NULL))
    my_skim(iris, Sepal.Length)

**Skim summary statistics**

<table style="width: auto;" class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
n\_obs
</th>
<th style="text-align:right;">
n\_cols
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
5
</td>
</tr>
</tbody>
</table>
**Variable type: numeric**

<table>
<thead>
<tr class="header">
<th align="left">variable</th>
<th align="right">missing</th>
<th align="right">complete</th>
<th align="right">n</th>
<th align="right">mean</th>
<th align="right">sd</th>
<th align="right">p0</th>
<th align="right">p25</th>
<th align="right">p50</th>
<th align="right">p75</th>
<th align="right">p100</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Sepal.Length</td>
<td align="right">0</td>
<td align="right">150</td>
<td align="right">150</td>
<td align="right">5.84</td>
<td align="right">0.83</td>
<td align="right">4.3</td>
<td align="right">5.1</td>
<td align="right">5.8</td>
<td align="right">6.4</td>
<td align="right">7.9</td>
</tr>
</tbody>
</table>

### Skimming other objects

`skimr` has summary functions for the following types of data by
default:

-   `numeric`
-   `integer`
-   `character`
-   `factor`
-   `logical`
-   `complex`
-   `Date`
-   `POSIXct`
-   `ts`
-   `AsIs`

`skimr` also provides a small API for writing packages that provide
their own default summary functions for data types not covered above. It
relies on R S3 methods for the `get_skimmers` function. This function
should return a `sfl`, similar to customization within `skim_with()`,
but you should also provide a value for the `class` argument. Here's an
example.

    get_skimmers.my_data_type <- function(column) {
      sfl(
        .class = "my_data_type",
        missing = n_missing,
        complete = n_complete,
        p99 = quantile(., probs = .99))
    }

Limitations of current version
------------------------------

We are aware that there are issues with rendering the inline histograms
and line charts in various contexts, some of which are described below.

### Support for spark histograms

There are known issues with printing the spark-histogram characters when
printing a data frame. For example, `"▂▅▇"` is printed as
`"<U+2582><U+2585><U+2587>"`. This longstanding problem [originates in
the low-level
code](http://r.789695.n4.nabble.com/Unicode-display-problem-with-data-frames-under-Windows-td4707639.html)
for printing dataframes. While some cases have been addressed, there
are, for example, reports of this issue in Emacs ESS.

This means that while `skimr` can render the histograms to the console
and in `kable()`, it cannot in other circumstances. This includes:

-   rendering a `skimr` data frame within `pander()`
-   converting a `skimr` data frame to a vanilla R data frame, but
    tibbles render correctly

One workaround for showing these characters in Windows is to set the
CTYPE part of your locale to Chinese/Japanese/Korean with
`Sys.setlocale("LC_CTYPE", "Chinese")`. These values do show up by
default when printing a data-frame created by `skim()` as a list
(`as.list()`) or as a matrix (`as.matrix()`).

### Printing spark histograms and line graphs in knitted documents

Spark-bar and spark-line work in the console, but may not work when you
knit them to a specific document format. The same session that produces
a correctly rendered HTML document may produce an incorrectly rendered
PDF, for example. This issue can generally be addressed by changing
fonts to one with good building block (for histograms) and Braille
support (for line graphs). For example, the open font "DejaVu Sans" from
the `extrafont` package supports these. You may also want to try
wrapping your results in `knitr::kable()`. Please see the vignette on
using fonts for details.

Displays in documents of different types will vary. For example, one
user found that the font "Yu Gothic UI Semilight" produced consistent
results for Microsoft Word and Libre Office Write.

Contributing
------------

We welcome issue reports and pull requests, including potentially adding
support for commonly used variable classes. However, in general, we
encourage users to take advantage of skimr's flexibility to add their
own customized classes. Please see the [contributing](CONTRIBUTING.md)
and [conduct](CONDUCT.md) documents.

[![ropenci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
