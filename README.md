skimr
=====

The goal of skimr is to provide a frictionless approach to dealing with summary statistics iteratively and interactively as part of a pipeline, and that conforms to the principle of least surprise. 

See our blog post [here](https://rawgit.com/ropenscilabs/skimr/master/blog.html).

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("ropenscilabs/skimr")
```

## Features


`library(skimr)` provides summary statistics that you can skim quickly to understand and your data and see what may be missing. It handles different data types (numerics, factors, etc), and returns a skimr object that can be piped or displayed nicely for the human reader. 

### printed skim object
For quick viewing in the console:

![](man/figures/skim_chickwts.png)
![](man/figures/skim_mtcars.png)
![](man/figures/skim_iris.png)

### skim_df object (long format)

``` r
library(skimr)
   
## full data frame   
skim(chickwts)   
# A tibble: 22 × 5
#       var    type     stat level    value
# *   <chr>   <chr>    <chr> <chr>    <dbl>
# 1  weight numeric  missing  .all   0.0000
# 2  weight numeric complete  .all  71.0000
# 3  weight numeric        n  .all  71.0000
# 4  weight numeric     mean  .all 261.3099
# 5  weight numeric       sd  .all  78.0737
# 6  weight numeric      min  .all 108.0000
# 7  weight numeric   median  .all 258.0000
# 8  weight numeric quantile   25% 204.5000
# 9  weight numeric quantile   75% 323.5000
# 10 weight numeric      max  .all 423.0000
# ... with 12 more rows

## factor vector
chickwts %>% 
  dplyr::select(feed) %>%
  skim()
   
# A tibble: 11 × 5
#      var   type     stat     level value
# *  <chr>  <chr>    <chr>     <chr> <dbl>
# 1   feed factor  missing      .all     0
# 2   feed factor complete      .all    71
# 3   feed factor        n      .all    71
# 4   feed factor    count    casein    12
# 5   feed factor    count horsebean    10
# 6   feed factor    count   linseed    12
# 7   feed factor    count  meatmeal    11
# 8   feed factor    count   soybean    14
# 9   feed factor    count sunflower    12
# 10  feed factor    count      <NA>     0
# 11  feed factor n_unique      .all     6 
   
```


### specify your own statistics
