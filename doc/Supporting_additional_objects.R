## ---- eval = FALSE-------------------------------------------------------
#  library(sf)
#  nc <- st_read(system.file("shape/nc.shp", package = "sf"))

## ---- eval = FALSE-------------------------------------------------------
#  funny_sf <- function(x) {
#    length(x) + 1
#  }

## ---- eval = FALSE-------------------------------------------------------
#  skim_sf <- skim_with(
#    sfc_MULTIPOLYGON = sfl(
#      missing = n_missing,
#      complete = n_complete,
#      n = length,
#      n_unique = n_unique,
#      valid = ~ sum(sf::st_is_valid(.)),
#      funny = funny_sf
#    )
#  )

## ---- eval = FALSE-------------------------------------------------------
#  skim_sf(nc)

## ---- eval = FALSE-------------------------------------------------------
#  #' Skimming functions for `sfc_MULTIPOLYGON` objects.
#  #' @export
#  skim_sf <- skim_with(
#    sfc_MULTIPOLYGON = sfl(
#      missing = n_missing,
#      complete = n_complete,
#      n = length,
#      n_unique = n_unique,
#      valid = ~ sum(sf::st_is_valid(.)),
#      funny = funny_sf
#    )
#  )

## ---- eval = FALSE-------------------------------------------------------
#  #' @importFrom skimr get_skimmers
#  #' @export
#  get_skimmers.sfc_MULTIPOLYGON <- function(column) {
#    sfl(
#      skim_type = "sfc_MULTIPOLYGON",
#      missing = n_missing,
#      complete = n_complete,
#      n = length,
#      n_unique = n_unique,
#      valid = ~ sum(sf::st_is_valid(.)),
#      funny = funny_sf
#    )
#  }

## ---- eval = FALSE-------------------------------------------------------
#  #' @export
#  get_skimmers.sfc_POINT <- function(column) {
#    sfl(
#      skim_type = "sfc_POINT",
#      missing = n_missing,
#      complete = n_complete,
#      n = length,
#      n_unique = n_unique,
#      valid = ~ sum(sf::st_is_valid(.))
#    )
#  }

## ---- eval = FALSE-------------------------------------------------------
#  get_default_skimmer_names()

