## ------------------------------------------------------------------------
summary(iris)

## ------------------------------------------------------------------------
summary(iris$Sepal.Length)

## ------------------------------------------------------------------------
fivenum(iris$Sepal.Length)

## ------------------------------------------------------------------------
summary(iris$Species)

## ---- render = knitr::normal_print---------------------------------------
library(skimr)
skim(iris)

## ---- render = knitr::normal_print---------------------------------------
skim(iris) %>%
  dplyr::select(skim_type)

## ---- render = knitr::normal_print---------------------------------------
skim(iris) %>%
  tibble::as_tibble()

## ---- render = knitr::normal_print---------------------------------------
skim(iris) %>%
  dplyr::filter(skim_variable == "Petal.Length")

## ---- render = knitr::normal_print---------------------------------------
skim(iris) %>%
  dplyr::select(skim_type, skim_variable, n_missing)

## ---- render = knitr::normal_print---------------------------------------
skim(iris) %>%
  dplyr::select(skim_type, skim_variable, numeric.mean)

## ---- render = knitr::normal_print---------------------------------------
iris %>%
  dplyr::group_by(Species) %>%
  skim()

## ---- render = knitr::normal_print---------------------------------------
skim(iris, Sepal.Length, Species)

## ---- render = knitr::normal_print---------------------------------------
skim(iris, starts_with("Sepal"))

## ---- render = knitr::normal_print---------------------------------------
skim(lynx)

## ------------------------------------------------------------------------
all.equal(skim(lynx), skim(as.data.frame(lynx)))

## ---- render = knitr::normal_print---------------------------------------
m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 4, ncol = 3)
m

## ---- render = knitr::normal_print---------------------------------------
colMeans(m)
skim(m)    # Similar to summary.matrix and colMeans()

## ---- render = knitr::normal_print---------------------------------------
rowMeans(m)
skim(t(m))

## ---- render = knitr::normal_print---------------------------------------
skim(c(m))
mean(m)

## ---- render = knitr::normal_print---------------------------------------
iris_setosa <- iris %>%
  skim_tee() %>%
  dplyr::filter(Species == "setosa")

## ---- render = knitr::normal_print---------------------------------------
iris %>%
  skim() %>%
  partition()

## ---- render = knitr::normal_print---------------------------------------
iris %>%
  skim() %>%
  yank("numeric")

## ---- render = knitr::normal_print---------------------------------------
iris %>%
  skim() %>%
  to_long()

## ---- render = knitr::normal_print---------------------------------------
iris %>%
  skim() %>%
  focus(n_missing, numeric.mean)

## ------------------------------------------------------------------------
skim(Orange)

## ------------------------------------------------------------------------
skim(Orange) %>%
  yank("numeric")

## ------------------------------------------------------------------------
my_skim <- skim_with(numeric = sfl(new_mad = mad))
my_skim(faithful)

## ------------------------------------------------------------------------
my_skim <- skim_with(numeric = sfl(new_mad = mad), append = FALSE)
my_skim(faithful)

## ------------------------------------------------------------------------
no_hist <- skim_with(ts = sfl(line_graph = NULL))
no_hist(Nile)

## ------------------------------------------------------------------------
my_skim <- skim_with(
  numeric = sfl(total = ~ sum(., na.rm = TRUE)),
  factor = sfl(missing = ~ sum(is.na(.))),
  append = FALSE
)

my_skim(iris)

## ------------------------------------------------------------------------
my_skim <- skim_with(base = sfl(length = length))
my_skim(faithful)

## ------------------------------------------------------------------------
#' @export
my_package_skim <- skim_with()

## ------------------------------------------------------------------------
get_skimmers.my_type <- function(column) {
  sfl(
    skim_type = "my_type",
    total = sum
  )
}

my_data <- data.frame(
  my_type = structure(1:3, class = c("my_type", "integer"))
)
skim(my_data)

