context("Skim a vector within a data frame")

# Expected response for mtcars  ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat, ~level,     ~value,   ~formatted_value,
  "numeric","missing",".all",0,"0",
  "numeric","complete",".all",32,"32",
  "numeric","n",".all",32,"32",
  "numeric","mean",".all",mean(mtcars$mpg)," 20.1",
  "numeric","sd",".all",sd(mtcars$mpg),"  6.0",
  "numeric","min",".all",10.4,"10.4",
  "numeric","median",".all",19.2," 19.2",
  "numeric","quantile","25%",15.425," 15.4",
  "numeric","quantile","75%",22.8," 22.8",
  "numeric","max",".all",33.9,"33.9",
  "numeric","hist",".all",0,"▂▅▇▇▇▃▁▁▂▂"
  )

skim_with_defaults()
test_that("skim_v returns expected response for numeric vectors", {
  input <- skim_v(mtcars$mpg)
  expect_equal(input, correct)
})


## Expected response for iris Species ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,    ~value,   ~formatted_value,
  "factor","missing",".all",0,"0",
  "factor","complete",".all",150,"150",
  "factor","n",".all",150,"150",
  "factor","count","setosa",50,"50",
  "factor","count","versicolor",50,"50",
  "factor","count","virginica",50,"50",
  "factor","count",NA,0,"0",
  "factor","n_unique",".all",3,"3"
  )

test_that("skim_v returns expected response for factor vectors", {
  input <- skim_v(iris$Species)
  expect_identical(input, correct)
})

## Expected response for iris Species ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,    ~value,   ~formatted_value,
  "factor","missing",".all",4,"4",
  "factor","complete",".all",146,"146",
  "factor","n",".all",150,"150",
  "factor","count","setosa",46,"46",
  "factor","count","versicolor",50,"50",
  "factor","count","virginica",50,"50",
  "factor","count",NA,4,"4",
  "factor","n_unique",".all",3,"3"
  )

test_that("skim_v handles factors when NAs are present", {
  iris$Species[15:18] <- NA 
  input <- skim_v(iris$Species)
  expect_identical(input, correct)
})

skim_with_defaults()
pathological <- c((2 ^ .Machine$double.digits), NA, 
    -(2 ^ .Machine$double.digits))
path_quantiles <- quantile(pathological, probs = c(.25, .75), na.rm = TRUE,
    names = FALSE)

correct_pathological_numeric <- tibble::tribble(
  ~type,          ~stat, ~level,    ~value,   ~formatted_value,
    "numeric","missing",".all",1,"1",
  "numeric","complete",".all",2,"2",
  "numeric","n",".all",3,"3",
  "numeric","mean",".all",0,"  0.0",
  "numeric","sd",".all",sd(pathological, na.rm = TRUE),"12738103345051546.0",
  "numeric","min",".all", -(2^.Machine$double.digits),"-9007199254740992",
  "numeric","median",".all",0,"  0.0",
  "numeric","quantile","25%",path_quantiles[1],"-4503599627370496.0",
  "numeric","quantile","75%",path_quantiles[2],"4503599627370496.0",
  "numeric","max",".all",+(2^.Machine$double.digits), as.character(+(2^.Machine$double.digits)),
  "numeric","hist",".all",0,"▇▁▁▁▁▁▁▁▁▇"
  )

test_that("skim_v handles numeric vectors with NAs and extreme numbers", {
  input <- skim_v(pathological)
  expect_identical(input, correct_pathological_numeric)
})

## Expected response for chr input ----------------------------------------

correct <- tibble::tribble(
  ~type,          ~stat,     ~level,    ~value,   ~formatted_value,
  "character","missing",".all",1,"1",
  "character","complete",".all",4,"4",
  "character","n",".all",5,"5",
  "character","min",".all",0,"0",
  "character","max",".all",4,"4",
  "character","empty",".all",1,"1",
  "character","n_unique",".all",4,"4"
  )

test_that("skim_v returns expected response for chr vectors", {
  dat <- c("AAAB","ABc","acb",NA,"")
  input <- skim_v(dat)
  expect_identical(input, correct)
})


# Expected response for chickwt logical ---------------------------

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,       ~value,   ~formatted_value,
  "logical","missing",".all",0,"0",
  "logical","complete",".all",71,"71",
  "logical","n",".all",71,"71",
  "logical","count","FALSE",36,"36",
  "logical","count","TRUE",35,"35",
  "logical","count",NA,0,"0",
  "logical","mean",".all",35/71,as.character(35/71)
  
  )

test_that("skim_v returns expected response for logical vectors", {
  data("chickwts")
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea'))
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})


# Expected response for chickwt logical with NA ---------------------------

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,       ~value,   ~formatted_value,
  "logical","missing",".all",4,"4",
  "logical","complete",".all",67,"67",
  "logical","n",".all",71,"71",
  "logical","count","FALSE",32,"32",
  "logical","count","TRUE",35,"35",
  "logical","count",NA,4,"4",
  "logical","mean",".all",35/67,as.character(35/67)
)

test_that("skim_v returns expected response for logical vectors", {
  dat <-  chickwts %>% dplyr::mutate(log_col = stringr::str_detect(feed, 'ea')) 
  dat$log_col[15:18] <- NA 
  input <- skim_v(dat$log_col)
  expect_identical(input, correct)
})


# Expected response forcomplex with NA ------------------------------

correct <- tibble::tribble(
  ~type,       ~stat,    ~level,       ~value,   ~formatted_value,
  "complex","missing",".all",4,"4",
  "complex","complete",".all",67,"67",
  "complex","n",".all",71,"71"
)

test_that("skim_v returns expected response for complex vectors", {
  data("chickwts")
  dat <-  chickwts %>% dplyr::mutate(test_complex = weight) 
  dat$test_complex[1:2] <- dat$test_complex[1:2] + 2i
  dat$test_complex[15:18] <- NA 
  input <- skim_v(dat$test_complex)
  expect_identical(input, correct)
})

# Expected response for Date  ----------------------------------------

correct <- tibble::tribble(
  ~type,       ~stat,       ~level,          ~value,   ~formatted_value,
  "Date","missing",".all",1,"1",
  "Date","complete",".all",9,"9",
  "Date","n",".all",10,"10",
  "Date","min",".all",15156,"2011-07-01",
  "Date","max",".all",15165,"2011-07-10",
  "Date","median",".all",15161,"2011-07-06",
  "Date","n_unique",".all",9,"9"
)

test_that("skim_v returns expected response for Date vectors", {
  dat <- seq(as.Date("2011-07-01"), by=1, len=10)
  dat[2] <- NA
  input <- skim_v(dat)
  expect_identical(input, correct)
})

# Not this test relies on the immediately prior correct definition
test_that("skim_v handles objects with multiple classes", {
  dat <- seq(as.Date("2011-07-01"), by=1, len=10)
  dat[2] <- NA
  class(dat) <- c("strange_type", "Date")
  input <- skim_v(dat)
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~type,       ~stat,       ~level,       ~value,   ~formatted_value,
  "character","missing",".all",0,"0",
  "character","complete",".all",71,"71",
  "character","n",".all",71,"71",
  "character","min",".all",1,"1",
  "character","max",".all",1,"1",
  "character","empty",".all",0,"0",
  "character","n_unique",".all",6,"6"
)

test_that("skim_v handles objects with two unknown classes when variable is like a factor", {
  data("chickwts")
  class(chickwts$feed) <- c("strange", "stranger")
  input<-skim_v(chickwts$feed)
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~type,        ~stat,       ~level,       ~value,   ~formatted_value,
  "character","missing",".all",0,"0",
  "character","complete",".all",71,"71",
  "character","n",".all",71,"71",
  "character","min",".all",3,"3",
  "character","max",".all",3,"3",
  "character","empty",".all",0,"0",
  "character","n_unique",".all",66,"66"
)

test_that("skim_v handles objects with two unknown classes when variable is like a numeric", {
  data("chickwts")
  class(chickwts$weight) <- c("strange", "stranger")
  input<-skim_v(chickwts$weight)
  expect_identical(input, correct)
})

# Expected response for ts  ---------------------------------------    
correct <- tibble::tribble(
  ~type,  ~stat,      ~level,     ~value,   ~formatted_value,
  "ts","missing",".all",0,"0",
  "ts","complete",".all",39,"39",
  "ts","n",".all",39,"39",
  "ts","start",".all",1962,"1962",
  "ts","end",".all",1971,"1971",
  "ts","frequency",".all",4,"4",
  "ts","deltat",".all",0.25,"0.25",
  "ts","mean",".all",mean(freeny$y),"  9.3",
  "ts","sd",".all",sd(freeny$y),"  0.3",
  "ts","min",".all",8.79137,"8.79137",
  "ts","max",".all",9.79424,"9.79424",
  "ts","median",".all",9.31378,"  9.3",
  "ts","line_graph",".all",0,"⣀⣀⣀⣀⣀⠤⠤⠤⠤⠔⠒⠒⠒⠒⠉⠉⠉⠉⠉⢁"
  
)

test_that("skim_v returns expected response for ts vectors", {
  data(freeny)
  input <- skim_v(freeny$y)
  expect_identical(input, correct)
})


correct <- tibble::tribble(
  ~type,       ~stat,       ~level,          ~value,   ~formatted_value,
  "POSIXct","missing",".all",0,"0",
  "POSIXct","complete",".all",10,"10",
  "POSIXct","n",".all",10,"10",
  "POSIXct","min",".all",1309478400,"2011-07-01",
  "POSIXct","max",".all",1309478409,"2011-07-01 00:00:09",
  "POSIXct","median",".all",1309478404.5,"2011-07-01 00:00:04",
  "POSIXct","n_unique",".all",10,"10"
)

test_that("skim_v returns expected response for POSIXct vectors", {
  dat <- seq(as.POSIXct("2011-07-01 00:00:00", tz = "UTC", origin = "1970-01-01 00:00:00 UTC"), by=1, len=10)
  input <- skim_v(dat)
  expect_identical(input, correct)
})


correct <- tibble::tribble(
  ~type,        ~stat,        ~level,    ~value,   ~formatted_value,
  "character","missing",".all",0,"0",
  "character","complete",".all",71,"71",
  "character","n",".all",71,"71",
  "character","min",".all",3,"3",
  "character","max",".all",3,"3",
  "character","empty",".all",0,"0",
  "character","n_unique",".all",66,"66"
)

test_that("skim_v returns expected character response for vectors of unknown class", {
  data("chickwts")
  class(chickwts$weight) <- "strange"
  input <- skim_v(chickwts$weight)
  expect_identical(input, correct)
})

correct <- tibble::tribble (
  ~type,    ~stat,           ~level,    ~value,   ~formatted_value,
  "list","missing",".all",1,"1",
  "list","complete",".all",5,"5",
  "list","n",".all",6,"6",
  "list","n_unique",".all",5,"5",
  "list","min_length",".all",1,"1",
  "list","median_length",".all",3,"3",
  "list","max_length",".all",4,"4"
)

test_that("skim_v returns expected response for list (not AsIs) vectors", {
  dat <- list( list("a", "b", "c"), list("d", "b", "d"), list("e", "f", "g"), d <- list("h"), 
               e <- list("i", "j", "k", "l"), f <- NA)
  input <- skim_v(dat)
  expect_identical(input, correct)
})

correct <- tibble::tribble (
  ~type,    ~stat,           ~level,    ~value,   ~formatted_value,
  "list","missing",".all",3,"3",
  "list","complete",".all",0,"0",
  "list","n",".all",3,"3",
  "list","n_unique",".all",0,"0",
  "list","min_length",".all",NA,NA,
  "list","median_length",".all",NA,NA,
  "list","max_length",".all",NA,NA
)

test_that("skim_v returns expected response for list (not AsIs) vectors that is all NA", {
  dat <- c(list(NA), list(NA), list(NA))
  input <- skim_v(dat)
  expect_identical(input, correct)
})

correct <- tibble::tribble(
  ~type,  ~stat,        ~level,    ~value,   ~formatted_value,
  "AsIs","missing",".all",1,"1",
  "AsIs","complete",".all",3,"3",
  "AsIs","n",".all",4,"4",
  "AsIs","n_unique",".all",3,"3",
  "AsIs","min_length",".all",1,"1",
  "AsIs","max_length",".all",6,"6"
)

test_that("skim_v returns expected response for asis vectors", {
  dat <- I(list(5, 5:6,5:10, NA))
  input <- skim_v(dat)
  expect_identical(input, correct)
})

