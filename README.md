
skimr
=====

[![Build Status](https://travis-ci.org/ropenscilabs/skimr.svg?branch=master)](https://travis-ci.org/ropenscilabs/skimr) [![codecov](https://codecov.io/gh/ropenscilabs/skimr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/skimr) [![](https://badges.ropensci.org/175_status.svg)](https://github.com/ropensci/onboarding/issues/175)

The goal of skimr is to provide a frictionless approach to dealing with summary statistics iteratively and interactively as part of a pipeline, and that conforms to the principle of least surprise.

`Skimr` provides summary statistics that you can skim quickly to understand your data and see what may be missing. It handles different data types (numerics, factors, etc), and returns a skimr object that can be piped or displayed nicely for the human reader.

Installation
------------

The current released version of Skimr can be install from CRAN. If you wish to install the current build of the next release you can do so using the following:

``` r
# install.packages("devtools")
devtools::install_github("ropenscilabs/skimr")
```

The APIs for this branch should be considered reasonably stable but still subject to change if an issue is discovered.

To install the version with the most recent changes that have not yet been incorporated in the master branch (and may not be):

``` r
devtools::install_github("ropenscilabs/skimr", ref = "develop")
```

Do not rely on APIs from the develop branch.

Skim statistics in the console
------------------------------

-   Provides a larger set of statistics than `summary()` including missing, complete, n, sd
-   reports numeric/int/double separately from factor/chr
-   handles dates, logicals
-   supports spark-bar and spark-line based on [Hadley Wickham's pillar package](https://github.com/hadley/pillar).

**Nicely separates variables by class:**

``` r
skim(chickwts)
```

    ## Skim summary statistics
    ##  n obs: 71 
    ##  n variables: 2 
    ## 
    ## Variable type: factor 
    ##  variable missing complete  n n_unique                         top_counts ordered
    ##      feed       0       71 71        6 soy: 14, cas: 12, lin: 12, sun: 12   FALSE
    ## 
    ## Variable type: numeric 
    ##  variable missing complete  n   mean    sd  p0   p25 median   p75 p100     hist
    ##    weight       0       71 71 261.31 78.07 108 204.5    258 323.5  423 ▃▅▅▇▃▇▂▂

**Presentation is in a compact horizontal format:**

``` r
skim(iris)
```

    ## Skim summary statistics
    ##  n obs: 150 
    ##  n variables: 5 
    ## 
    ## Variable type: factor 
    ##  variable missing complete   n n_unique                       top_counts ordered
    ##   Species       0      150 150        3 set: 50, ver: 50, vir: 50, NA: 0   FALSE
    ## 
    ## Variable type: numeric 
    ##      variable missing complete   n mean   sd  p0 p25 median p75 p100     hist
    ##  Petal.Length       0      150 150 3.76 1.77 1   1.6   4.35 5.1  6.9 ▇▁▁▂▅▅▃▁
    ##   Petal.Width       0      150 150 1.2  0.76 0.1 0.3   1.3  1.8  2.5 ▇▁▁▅▃▃▂▂
    ##  Sepal.Length       0      150 150 5.84 0.83 4.3 5.1   5.8  6.4  7.9 ▂▇▅▇▆▅▂▂
    ##   Sepal.Width       0      150 150 3.06 0.44 2   2.8   3    3.3  4.4 ▁▂▅▇▃▂▁▁

**Individual columns of a data frame can be selected using tidyverse-style selectors:**

``` r
skim(iris, Sepal.Length, Petal.Length)
```

    ## Skim summary statistics
    ##  n obs: 150 
    ##  n variables: 5 
    ## 
    ## Variable type: numeric 
    ##      variable missing complete   n mean   sd  p0 p25 median p75 p100     hist
    ##  Petal.Length       0      150 150 3.76 1.77 1   1.6   4.35 5.1  6.9 ▇▁▁▂▅▅▃▁
    ##  Sepal.Length       0      150 150 5.84 0.83 4.3 5.1   5.8  6.4  7.9 ▂▇▅▇▆▅▂▂

\*\* Handles grouped data: \*\*

Skim() can handle data that has been grouped using `dplyr::group_by`.

``` r
iris %>% dplyr::group_by(Species) %>% skim()
```

    ## Skim summary statistics
    ##  n obs: 150 
    ##  n variables: 5 
    ##  group variables: Species 
    ## 
    ## Variable type: numeric 
    ##     Species     variable missing complete  n mean   sd  p0  p25 median  p75 p100     hist
    ##      setosa Petal.Length       0       50 50 1.46 0.17 1   1.4    1.5  1.58  1.9 ▁▁▅▇▇▅▂▁
    ##      setosa  Petal.Width       0       50 50 0.25 0.11 0.1 0.2    0.2  0.3   0.6 ▂▇▁▂▂▁▁▁
    ##      setosa Sepal.Length       0       50 50 5.01 0.35 4.3 4.8    5    5.2   5.8 ▂▃▅▇▇▃▁▂
    ##      setosa  Sepal.Width       0       50 50 3.43 0.38 2.3 3.2    3.4  3.68  4.4 ▁▁▃▅▇▃▂▁
    ##  versicolor Petal.Length       0       50 50 4.26 0.47 3   4      4.35 4.6   5.1 ▁▃▂▆▆▇▇▃
    ##  versicolor  Petal.Width       0       50 50 1.33 0.2  1   1.2    1.3  1.5   1.8 ▆▃▇▅▆▂▁▁
    ##  versicolor Sepal.Length       0       50 50 5.94 0.52 4.9 5.6    5.9  6.3   7   ▃▂▇▇▇▃▅▂
    ##  versicolor  Sepal.Width       0       50 50 2.77 0.31 2   2.52   2.8  3     3.4 ▁▂▃▅▃▇▃▁
    ##   virginica Petal.Length       0       50 50 5.55 0.55 4.5 5.1    5.55 5.88  6.9 ▂▇▃▇▅▂▁▂
    ##   virginica  Petal.Width       0       50 50 2.03 0.27 1.4 1.8    2    2.3   2.5 ▂▁▇▃▃▆▅▃
    ##   virginica Sepal.Length       0       50 50 6.59 0.64 4.9 6.23   6.5  6.9   7.9 ▁▁▃▇▅▃▂▃
    ##   virginica  Sepal.Width       0       50 50 2.97 0.32 2.2 2.8    3    3.18  3.8 ▁▃▇▇▅▃▁▂

\*\* Options for kable and pander: \*\*

Enhanced print options are available by piping to kable() or pander().

skim\_df object (long format)
-----------------------------

By default `skim` prints beautifully in the console, but it also produces a long, tidy-format skim\_df object that can be computed on.

``` r
a <-  skim(chickwts)
dim(a)
```

    ## [1] 23  6

``` r
print.data.frame(skim(chickwts))
```

    ##    variable    type       stat     level    value formatted
    ## 1    weight numeric    missing      .all   0.0000         0
    ## 2    weight numeric   complete      .all  71.0000        71
    ## 3    weight numeric          n      .all  71.0000        71
    ## 4    weight numeric       mean      .all 261.3099    261.31
    ## 5    weight numeric         sd      .all  78.0737     78.07
    ## 6    weight numeric         p0      .all 108.0000       108
    ## 7    weight numeric        p25      .all 204.5000     204.5
    ## 8    weight numeric     median      .all 258.0000       258
    ## 9    weight numeric        p75      .all 323.5000     323.5
    ## 10   weight numeric       p100      .all 423.0000       423
    ## 11   weight numeric       hist      .all       NA  ▃▅▅▇▃▇▂▂
    ## 12     feed  factor    missing      .all   0.0000         0
    ## 13     feed  factor   complete      .all  71.0000        71
    ## 14     feed  factor          n      .all  71.0000        71
    ## 15     feed  factor   n_unique      .all   6.0000         6
    ## 16     feed  factor top_counts   soybean  14.0000   soy: 14
    ## 17     feed  factor top_counts    casein  12.0000   cas: 12
    ## 18     feed  factor top_counts   linseed  12.0000   lin: 12
    ## 19     feed  factor top_counts sunflower  12.0000   sun: 12
    ## 20     feed  factor top_counts  meatmeal  11.0000   mea: 11
    ## 21     feed  factor top_counts horsebean  10.0000   hor: 10
    ## 22     feed  factor top_counts      <NA>   0.0000     NA: 0
    ## 23     feed  factor    ordered      .all   0.0000     FALSE

Compute on the full skim\_df object
-----------------------------------

``` r
skim(mtcars) %>% dplyr::filter(stat=="hist")
```

    ## # A tibble: 11 x 6
    ##    variable type    stat  level value formatted
    ##    <chr>    <chr>   <chr> <chr> <dbl> <chr>    
    ##  1 mpg      numeric hist  .all     NA ▃▇▇▇▃▂▂▂ 
    ##  2 cyl      numeric hist  .all     NA ▆▁▁▃▁▁▁▇ 
    ##  3 disp     numeric hist  .all     NA ▇▆▁▂▅▃▁▂ 
    ##  4 hp       numeric hist  .all     NA ▃▇▃▅▂▃▁▁ 
    ##  5 drat     numeric hist  .all     NA ▃▇▁▅▇▂▁▁ 
    ##  6 wt       numeric hist  .all     NA ▃▃▃▇▆▁▁▂ 
    ##  7 qsec     numeric hist  .all     NA ▃▂▇▆▃▃▁▁ 
    ##  8 vs       numeric hist  .all     NA ▇▁▁▁▁▁▁▆ 
    ##  9 am       numeric hist  .all     NA ▇▁▁▁▁▁▁▆ 
    ## 10 gear     numeric hist  .all     NA ▇▁▁▆▁▁▁▂ 
    ## 11 carb     numeric hist  .all     NA ▆▇▂▇▁▁▁▁

Works with strings, lists and other column classes.
---------------------------------------------------

``` r
skim(dplyr::starwars)
```

    ## Skim summary statistics
    ##  n obs: 87 
    ##  n variables: 13 
    ## 
    ## Variable type: character 
    ##    variable missing complete  n min max empty n_unique
    ##   eye_color       0       87 87   3  13     0       15
    ##      gender       3       84 87   4  13     0        4
    ##  hair_color       5       82 87   4  13     0       12
    ##   homeworld      10       77 87   4  14     0       48
    ##        name       0       87 87   3  21     0       87
    ##  skin_color       0       87 87   3  19     0       31
    ##     species       5       82 87   3  14     0       37
    ## 
    ## Variable type: integer 
    ##  variable missing complete  n   mean    sd p0 p25 median p75 p100     hist
    ##    height       6       81 87 174.36 34.77 66 167    180 191  264 ▁▁▁▂▇▃▁▁
    ## 
    ## Variable type: list 
    ##   variable missing complete  n n_unique min_length median_length max_length
    ##      films       0       87 87       24          1             1          7
    ##  starships       0       87 87       17          0             0          5
    ##   vehicles       0       87 87       11          0             0          2
    ## 
    ## Variable type: numeric 
    ##    variable missing complete  n  mean     sd p0  p25 median  p75 p100     hist
    ##  birth_year      44       43 87 87.57 154.69  8 35       52 72    896 ▇▁▁▁▁▁▁▁
    ##        mass      28       59 87 97.31 169.46 15 55.6     79 84.5 1358 ▇▁▁▁▁▁▁▁

Users can add new classes.

Specify your own statistics
---------------------------

``` r
funs <- list(iqr = IQR,
    quantile = purrr::partial(quantile, probs = .99))
  skim_with(numeric = funs, append = FALSE)
  skim(iris, Sepal.Length)
```

    ## Skim summary statistics
    ##  n obs: 150 
    ##  n variables: 5 
    ## 
    ## Variable type: numeric 
    ##      variable iqr quantile
    ##  Sepal.Length 1.3      7.7

``` r
# Restore defaults
  skim_with_defaults()
```

Limitations of current version
------------------------------

We are aware that there are issues with rendering the inline histograms and line charts in various contexts, some of which are described below.

### Support for spark histograms

There are known issues with printing the spark-histogram characters when printing a data-frame. For example, `"▂▅▇"` is printed as `"<U+2582><U+2585><U+2587>"`. Th is longstanding problem [originates in the low-level code](http://r.789695.n4.nabble.com/Unicode-display-problem-with-data-frames-under-Windows-td4707639.html) for printing dataframes. For example there are reports of this issue in Emacs ESS.

This means that while `skimr` can render the histograms to the console and in kable, it cannot in other circumstances. This includes:

-   rendering a `skimr` data frame within pander
-   converting a `skimr` data frame to a vanilla R data frame, but tibbles render correctly

In previous versions of One workaround for showing these characters in Windows is to set the CTYPE part of your locale to Chinese/Japanese/Korean with `Sys.setlocale("LC_CTYPE", "Chinese")`. These values do show up by default when printing a data-frame created by `skim()` as a list (`as.list()`) or as a matrix (`as.matrix()`).

### Printing spark histograms and line graphs in knitted documents

Spark-bar and spark-line work in the console but may not work when you knit them to a specific document format. The same session that produces a correctly rendered HTML document may produce an incorrectly rendered PDF, for example. This issue can generally be addressed by changing fonts to one with good building block (for histograms) and braille support (for line graphs). For example, the open font "DejaVu Sans" from the `extra font` package supports these.
You may also want to try wrapping your results in `knitr::kable()`. Please see the vignette on using fonts for details on this.

Displays in documents of different types will vary. For example, one user found that the font "Yu Gothic UI Semilight" produced consistent results for Microsoft Word and Libre Office Write.

Contributing
------------

We welcome issue reports and pull requests including potentially adding support for commonly used variable classes. However in general we encourage users to take advantage of skimr's flexibility to add their own customized classes. Please see the [contributing](contributing.md) and [conduct](conduct.md) documents.
