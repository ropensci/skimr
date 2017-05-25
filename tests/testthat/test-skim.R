context("Skimming a data.frame")

# Target output -----------------------------------------------------------

skim_chickwts <- data.frame(
  var = rep(c("weight", "feed"), times = c(4, 8)),
  type = rep(c("dbl", "factor"), times = c(4, 8)),
  stat = c("mean", "median", "complete", "missing", rep("count", 6), "complete",
    "missing"),
  levels = c(rep(NA, 4), "casein", "horsebean", "linseed", "meatmeal",
    "soybean", "sunflower", NA, NA),
  values = c(mean(chickwts$weight), median(chickwts$weight),
    length(chickwts$weight), 0, as.numeric(table(chickwts$feed)),
    length(chickwts$feed), 0)
)

class(skim_chickwts) <- c("skim_tbl", "data.frame")


# Begin tests -------------------------------------------------------------


